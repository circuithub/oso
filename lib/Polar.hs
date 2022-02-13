{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Polar where

-- aeson
import Data.Aeson
  ( FromJSON( parseJSON )
  , ToJSON( toJSON )
  , (.:)
  , eitherDecodeStrict'
  , withObject
  , withText
  )
import Data.Aeson.Text ( encodeToLazyText )

-- aeson-qq
import Data.Aeson.QQ ( aesonQQ )

-- base
import Control.Applicative ( (<|>), empty )
import Control.Exception ( Exception, finally )
import Control.Monad.IO.Class ( liftIO )
import Data.Bool ( bool )
import Data.Data ( Proxy( Proxy ) )
import Data.Foldable ( msum, toList )
import Data.Functor ( (<&>) )
import Data.IORef ( IORef, atomicModifyIORef, newIORef, readIORef )
import Data.Type.Equality ( (:~:)( Refl ), testEquality )
import Data.Word ( Word64 )
import Foreign.C ( CChar, peekCString, withCString )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
import Foreign.Storable ( peekByteOff, sizeOf )
import GHC.Generics ( C1, D1, Generic, K1( K1 ), M1( M1 ), Meta( MetaSel ), Rec0, Rep, S1, type (:*:)( (:*:) ), from )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Type.Reflection ( SomeTypeRep( SomeTypeRep ), Typeable, typeOf, typeRep )

-- containers
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

-- oso
import qualified Polar.C as C
import qualified Polar.C.Types as C

-- streaming
import Data.Functor.Of ( Of )
import Streaming ( Stream )
import qualified Streaming.Prelude as S

-- text
import Data.Text ( Text, pack )
import Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text.Lazy as LT

-- transformers
import Control.Monad.Trans.State.Strict ( State, runState, state )


data PolarError
  = PolarError PolarErrorKind Text
  | CouldNotDecodeQueryEvent String
  | UnknownError String String
  deriving stock (Eq, Show)
  deriving anyclass Exception


instance FromJSON PolarError where
  parseJSON = withObject "PolarParseError" \o -> do
    kind <- o .: "kind"
    formatted <- o .: "formatted"
    return $ PolarError kind formatted


data PolarErrorKind
  = ParseError PolarParseError
  | OperationalError PolarOperationalError
  | ValidationError PolarValidationError
  | RuntimeError PolarRuntimeError
  deriving stock (Eq, Show)


instance FromJSON PolarErrorKind where
  parseJSON = withObject "PolarErrorKind" \o -> msum
    [ ParseError <$> o .: "Parse"
    , OperationalError <$> o .: "Operational"
    , ValidationError <$> o .: "Validation"
    , RuntimeError <$> o .: "Runtime"
    ]


data PolarParseError
  = UnrecognizedEOF Int
  | UnrecognizedToken String Int
  deriving stock (Eq, Show)


instance FromJSON PolarParseError where
  parseJSON = withObject "PolarParseError" \o -> msum
    [ o .: "UnrecognizedEOF" >>= withObject "UnrecognizedEOF" \o -> do
        loc <- o .: "loc"
        pure $ UnrecognizedEOF loc
    , o .: "UnrecognizedToken" >>= withObject "UnrecognizedToken" \o -> do
        token <- o .: "token"
        loc   <- o .: "loc"
        pure $ UnrecognizedToken token loc
    ]


data PolarOperationalError = UnknownOperationalError
  deriving stock (Eq, Show)


instance FromJSON PolarOperationalError where
  parseJSON = withText "PolarOperationalError" \case
    "Unknown" -> pure UnknownOperationalError
    _ -> empty


data PolarValidationError
  = UnregisteredClass PolarTerm
  deriving stock (Eq, Show)


instance FromJSON PolarValidationError where
  parseJSON = withObject "PolarValidationError" \o -> msum
    [ o .: "UnregisteredClass" >>= withObject "UnregisteredClass" \o -> do
        term <- o .: "term"
        pure $ UnregisteredClass term
    ]


data PolarRuntimeError = ApplicationError String String PolarTerm
  deriving stock (Eq, Show)


instance FromJSON PolarRuntimeError where
  parseJSON = withObject "PolarRuntimeError" \o -> msum
    [ o .: "Application" >>= withObject "Application" \o -> do
        msg <- o .: "msg"
        stackTrace <- o .: "stack_trace"
        term <- o .: "term"
        pure $ ApplicationError msg stackTrace term
    ]


data Polar = Polar
  { polarPtr :: ForeignPtr C.Polar
  , environmentRef :: IORef Environment
  }


withPolar :: Polar -> (Ptr C.Polar -> IO a) -> IO a
withPolar Polar{ polarPtr } = withForeignPtr polarPtr


polarNew :: IO Polar
polarNew = do
  polarPtr <- C.polar_new
  environmentRef <- newIORef emptyEnvironment

  polarForeignPtr <- newForeignPtr C.polar_free polarPtr

  return Polar{ polarPtr = polarForeignPtr, environmentRef }


polarLoad :: (Foldable f) => Polar -> f Text -> IO (Either PolarError ())
polarLoad polar srcs = checkResultVoid do
  withPolar polar \polarPtr ->
    withCString json \srcsPtr ->
      C.polar_load polarPtr srcsPtr
  where
    json = LT.unpack $ encodeToLazyText $ toList srcs <&> \src -> [aesonQQ|{"src": #{src}}|]


polarClearRules :: Polar -> IO (Either PolarError ())
polarClearRules polar = checkResultVoid do
  withPolar polar \polarPtr ->
    C.polar_clear_rules polarPtr


polarRegisterConstant :: Polar -> String -> PolarTerm -> IO (Either PolarError ())
polarRegisterConstant polar name value = checkResultVoid do
  withPolar polar \polarPtr ->
    withCString name \namePtr ->
      withCString (LT.unpack (encodeToLazyText value)) \valuePtr ->
        C.polar_register_constant polarPtr namePtr valuePtr


polarRegisterMro :: Polar -> String -> String -> IO (Either PolarError ())
polarRegisterMro polar name value = checkResultVoid do
  withPolar polar \polarPtr ->
    withCString name \namePtr ->
      withCString value \valuePtr ->
        C.polar_register_mro polarPtr namePtr valuePtr


polarNextInlineQuery :: Polar -> Bool -> IO (Maybe Query)
polarNextInlineQuery polar trace =
  withPolar polar \polarPtr ->
    C.polar_next_inline_query polarPtr traceInt >>= traverseNullPtr \ptr -> do
      Query <$> newForeignPtr C.query_free ptr
  where
    traceInt = bool 0 1 trace


newtype Query = Query (ForeignPtr C.Query)
  deriving stock Show


withQuery :: Query -> (Ptr C.Query -> IO a) -> IO a
withQuery (Query ptr) = withForeignPtr ptr


polarNewQuery :: Polar -> String -> Bool -> IO (Either PolarError Query)
polarNewQuery polar query trace = checkResultQuery do
  withPolar polar \polarPtr ->
    withCString query \queryPtr ->
      C.polar_new_query polarPtr queryPtr traceInt
  where
    traceInt = bool 0 1 trace


polarNewQueryFromTerm :: Polar -> PolarTerm -> Bool -> IO (Either PolarError Query)
polarNewQueryFromTerm polar term trace = checkResultQuery do
  withPolar polar \polarPtr ->
    withCString (LT.unpack (encodeToLazyText term)) \queryPtr ->
      C.polar_new_query_from_term polarPtr queryPtr traceInt
  where
    traceInt = bool 0 1 trace


polarQuestionResult :: Query -> Word64 -> Bool -> IO (Either PolarError ())
polarQuestionResult query callId yn = checkResultVoid do
  withQuery query \queryPtr ->
    C.polar_question_result queryPtr callId c'yn
  where
    c'yn = bool 0 1 yn


polarNextQueryEvent :: Query -> IO (Either PolarError QueryEvent)
polarNextQueryEvent query = do
  res <- checkResultString do
    withQuery query \queryPtr ->
      C.polar_next_query_event queryPtr

  case res of
    Left e -> pure (Left e)
    Right json ->
      case eitherDecodeStrict' (encodeUtf8 (pack json)) of
        Left e  -> pure (Left (CouldNotDecodeQueryEvent e))
        Right x -> pure (Right x)


polarCallResult :: Query -> Word64 -> PolarTerm -> IO (Either PolarError ())
polarCallResult query callId term = checkResultVoid do
  withQuery query \queryPtr ->
    withCString (LT.unpack (encodeToLazyText term)) \termPtr ->
      C.polar_call_result queryPtr callId termPtr


polarApplicationError :: Query -> String -> IO (Either PolarError ())
polarApplicationError query errorString = checkResultVoid do
  withQuery query \queryPtr ->
    withCString errorString \errorStringPtr ->
      C.polar_application_error queryPtr errorStringPtr



traverseNullPtr :: Applicative f
  => (Ptr a -> f b) -> Ptr a -> f (Maybe b)
traverseNullPtr f ptr
  | ptr == nullPtr = pure Nothing
  | otherwise      = Just <$> f ptr


data PolarTerm
  = StringLit Text
  | BoolLit Bool
  | ListLit [PolarTerm]
  | ExpressionTerm Expression
  | Variable String
  | ExternalInstanceTerm ExternalInstance
  | CallTerm Call
  deriving stock (Eq, Show)


instance FromJSON PolarTerm where
  parseJSON = withObject "PolarTerm" \o ->
    o .: "value" >>= do
      withObject "value" \o -> msum
        [ StringLit <$> o .: "String"
        , BoolLit <$> o .: "Bool"
        , ExpressionTerm <$> o .: "Expression"
        , Variable <$> o .: "Variable"
        , ExternalInstanceTerm <$> o .: "ExternalInstance"
        , ListLit <$> o .: "List"
        , CallTerm <$> o .: "Call"
        ]


instance ToJSON PolarTerm where
  toJSON x = [aesonQQ|{"value":#{value}}|] where
    value = case x of
      StringLit s -> [aesonQQ| {"String" :#{s}} |]
      ExpressionTerm t -> [aesonQQ| {"Expression": #{t} } |]
      Variable v -> [aesonQQ| {"Variable": #{v} } |]
      ExternalInstanceTerm t -> [aesonQQ| {"ExternalInstance": #{t} } |]
      CallTerm t -> [aesonQQ| {"Call": #{t} } |]
      ListLit terms -> [aesonQQ| {"List": #{terms}} |]
      BoolLit b -> [aesonQQ| {"Bool": #{b}} |]


data Expression = Expression
  { operator :: String
  , args :: [PolarTerm]
  }
  deriving stock (Eq, Show)


instance FromJSON Expression where
  parseJSON = withObject "Expression" \o -> do
    operator <- o .: "operator"
    args <- o .: "args"
    return Expression{ operator, args }


instance ToJSON Expression where
  toJSON Expression{ operator, args } = [aesonQQ|{
      "operator": #{operator},
      "args": #{args}
    }|]


data ExternalInstance = ExternalInstance
  { instanceId :: Word64
  }
  deriving stock (Eq, Show)


instance ToJSON ExternalInstance where
  toJSON ExternalInstance{ instanceId } =
    [aesonQQ|{
      "instance_id": #{instanceId}
    }|]


instance FromJSON ExternalInstance where
  parseJSON = withObject "ExternalInstance" \o -> do
    instanceId <- o .: "instance_id"
    return ExternalInstance{ instanceId }


data Call = Call
  { name :: String
  , args :: [PolarTerm]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)


data QueryEvent
  = ResultEvent Result
  | DoneEvent Done
  | ExternalIsaEvent ExternalIsa
  | ExternalIsSubclassEvent ExternalIsSubclass
  | ExternalIsaWithPathEvent ExternalIsaWithPath
  | ExternalCallEvent ExternalCall
  | ExternalOpEvent ExternalOp
  deriving stock (Eq, Show)


instance FromJSON QueryEvent where
  parseJSON = withObject "QueryEvent" \o ->
        DoneEvent <$> o .: "Done"
    <|> ResultEvent <$> o .: "Result"
    <|> ExternalIsaEvent <$> o .: "ExternalIsa"
    <|> ExternalIsSubclassEvent <$> o .: "ExternalIsSubclass"
    <|> ExternalIsaWithPathEvent <$> o .: "ExternalIsaWithPath"
    <|> ExternalCallEvent <$> o .: "ExternalCall"
    <|> ExternalOpEvent <$> o .: "ExternalOp"


data Result = Result
  { bindings :: Map String PolarTerm
  , trace :: Maybe Trace
  }
  deriving stock (Eq, Generic, Show)


newtype Trace = Trace { formatted :: String }
  deriving stock (Eq, Show)


instance FromJSON Trace where
  parseJSON = withObject "Trace" \o ->
    Trace <$> o .: "formatted"


instance FromJSON Result where
  parseJSON = withObject "Result" \o -> do
    bindings <- o .: "bindings"
    trace <- o .: "trace"

    return Result{ bindings, trace }


data Done = Done { result :: Bool }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)


data ExternalIsa = ExternalIsa
  { callId :: Word64
  , instance_ :: PolarTerm
  , classTag :: String
  }
  deriving stock (Eq, Show)


instance FromJSON ExternalIsa where
  parseJSON = withObject "ExternalIsa" \o -> do
    callId <- o .: "call_id"
    instance_ <- o .: "instance"
    classTag <- o .: "class_tag"
    return ExternalIsa{ callId, instance_, classTag }


data ExternalIsaWithPath = ExternalIsaWithPath
  { callId :: Word64 }
  deriving stock (Eq, Show)


instance FromJSON ExternalIsaWithPath where
  parseJSON = withObject "ExternalIsaWithPath" \o -> do
    callId <- o .: "call_id"
    return ExternalIsaWithPath{ callId }


data ExternalIsSubclass = ExternalIsSubclass
  { callId :: Word64 }
  deriving stock (Eq, Show)


instance FromJSON ExternalIsSubclass where
  parseJSON = withObject "ExternalIsSubclass" \o -> do
    callId <- o .: "call_id"
    return ExternalIsSubclass{ callId }


data ExternalCall = ExternalCall
  { callId :: Word64
  , instance_ :: PolarTerm
  , attribute :: String
  }
  deriving stock (Eq, Show)


instance FromJSON ExternalCall where
  parseJSON = withObject "ExternalCall" \o -> do
    callId <- o .: "call_id"
    instance_ <- o .: "instance"
    attribute <- o .: "attribute"
    return ExternalCall{ callId, instance_, attribute }


data ExternalOp = ExternalOp
  { callId :: Word64
  , operator :: String
  , args :: [PolarTerm]
  }
  deriving stock (Eq, Show)


instance FromJSON ExternalOp where
  parseJSON = withObject "ExternalOp" \o -> do
    callId <- o .: "call_id"
    operator <- o .: "operator"
    args <- o .: "args"
    return ExternalOp{ callId, operator, args }


data Environment = Environment
  { instanceMap :: Map Word64 Instance
  , types :: Map SomeTypeRep Word64
  }


emptyEnvironment :: Environment
emptyEnvironment = Environment mempty mempty


externalInstance :: PolarValue a => a -> State Environment PolarTerm
externalInstance a = state \environment -> do
  let i = maybe 0 (succ . fst) (Map.lookupMax (instanceMap environment))

  let x = Instance{ value = a }

  let t = SomeTypeRep (typeOf a)

  let environment' = environment
        { instanceMap = Map.insert i x (instanceMap environment)
        , types = Map.insertWith (\_ old -> old) t (fromIntegral (Map.size (types environment))) (types environment)
        }

  let term = ExternalInstanceTerm ExternalInstance{ instanceId = i }

  (term, environment')


data Instance where
  Instance
    :: PolarValue a
    => { value :: a }
    -> Instance


instance Eq Instance where
  Instance{ value = valueA } == Instance{ value = valueB } =
   case testEquality (typeOf valueA) (typeOf valueB) of
     Nothing   -> False
     Just Refl -> valueA == valueB


runQuery :: Polar -> State Environment PolarTerm -> Stream (Of Result) IO (Either PolarError Bool)
runQuery polar queryBuilder = do
  environment <- liftIO (readIORef (environmentRef polar))

  let (queryTerm, env) = runState queryBuilder environment

  liftIO (polarNewQueryFromTerm polar queryTerm False) >>= \case
    Left e  -> return (Left e)
    Right q -> unfoldQuery env q


unfoldQuery :: Environment -> Query -> Stream (Of Result) IO (Either PolarError Bool)
unfoldQuery env q = S.unfoldr go env where
  go env = polarNextQueryEvent q >>= \case
    Left e ->
      return $ Left (Left e)

    Right ev -> do
      -- print ev
      -- print env

      case ev of
        DoneEvent Done{ result } ->
          return $ Left (Right result)

        ResultEvent r ->
          return $ Right (r, env)

        ExternalIsaEvent ExternalIsa{ callId, instance_ = ExternalInstanceTerm ExternalInstance{ instanceId }, classTag = y } -> do
          Instance{ value = x } <- case Map.lookup instanceId (instanceMap env) of
            Nothing -> fail "Could not find instance (1)"
            Just x -> return x

          polarQuestionResult q callId (classTagOf x == y) >>= \case
            Left e -> pure $ Left $ Left e
            Right () -> go env

        ExternalIsaEvent ExternalIsa{ callId, instance_ = StringLit{}, classTag = "String" } -> do
          polarQuestionResult q callId True >>= \case
            Left e -> pure $ Left $ Left e
            Right () -> go env

        ExternalCallEvent ExternalCall{ callId, instance_ = ExternalInstanceTerm ExternalInstance{ instanceId }, attribute } -> do
          x <- case Map.lookup instanceId (instanceMap env) of
            Nothing -> fail "Could not find instance (2)"
            Just x -> return x

          let res = case x of
                Instance{ value } ->
                  flip runState env <$> call value attribute []

          case res of
            Nothing  -> do
              polarApplicationError q ("Cannot access " <> attribute) >>= \case
                Left e -> pure $ Left $ Left e
                Right () -> 
                  polarCallResult q callId (StringLit "null") >>= \case
                    Left e -> pure $ Left $ Left e
                    Right () -> go env

            Just (res, env') -> do
              polarCallResult q callId res >>= \case
                Left e -> pure $ Left $ Left e
                Right () -> go env'

        ExternalOpEvent ExternalOp{ callId, operator = "Eq", args = [ExternalInstanceTerm ExternalInstance{ instanceId = l }, ExternalInstanceTerm ExternalInstance{ instanceId = r }] } -> do
          x <- case Map.lookup l (instanceMap env) of
            Nothing -> fail "Could not find l instance"
            Just x -> return x

          y <- case Map.lookup r (instanceMap env) of
            Nothing -> fail "Could not find r instance"
            Just y -> return y

          polarQuestionResult q callId (x == y) >>= \case
            Left e -> pure $ Left $ Left e
            Right () -> go env


decodePolarError :: String -> PolarError
decodePolarError json =
  case eitherDecodeStrict' $ encodeUtf8 $ pack json of
    Left e  -> UnknownError e json
    Right a -> a


checkResultVoid :: IO (Ptr C.CResult_c_void) -> IO (Either PolarError ())
checkResultVoid io = do
  ptr <- io

  flip finally (C.result_free ptr) do
    errorPtr  <- peekByteOff ptr (sizeOf (undefined :: Ptr CChar))

    if errorPtr == nullPtr @CChar
     then return $ Right ()
     else Left . decodePolarError <$> peekCString errorPtr


checkResultQuery :: IO (Ptr C.CResult_Query) -> IO (Either PolarError Query)
checkResultQuery io = do
  ptr <- io

  flip finally (C.result_free (castPtr ptr)) do
    resultPtr <- peekByteOff ptr 0
    errorPtr  <- peekByteOff ptr (sizeOf (undefined :: Ptr CChar))

    if errorPtr == nullPtr @CChar
     then Right . Query <$> newForeignPtr C.query_free resultPtr
     else Left . decodePolarError <$> peekCString errorPtr


checkResultString :: IO (Ptr C.CResult_c_char) -> IO (Either PolarError String)
checkResultString io = do
  ptr <- io

  flip finally (C.result_free (castPtr ptr)) do
    resultPtr <- peekByteOff ptr 0
    errorPtr  <- peekByteOff ptr (sizeOf (undefined :: Ptr CChar))

    if errorPtr == nullPtr @CChar
     then Right <$> peekCString resultPtr
     else Left . decodePolarError <$> peekCString errorPtr


class (Eq a, Typeable a) => PolarValue a where
  toPolarTerm :: a -> State Environment PolarTerm
  call :: a -> String -> [PolarTerm] -> Maybe (State Environment PolarTerm)
  classTagOf :: a -> String


instance PolarValue a => PolarValue [a] where
  toPolarTerm xs = ListLit <$> traverse toPolarTerm xs
  call _ _ _ = Nothing
  classTagOf _ = "?"


instance PolarValue Text where
  toPolarTerm = pure . StringLit
  call _ _ _ = Nothing
  classTagOf _ = "String"


class Rule arg res where
  applyArgument :: ([State Environment PolarTerm] -> State Environment PolarTerm) -> arg -> res


instance PolarValue arg => Rule arg (State Environment PolarTerm) where
  applyArgument f a = f [toPolarTerm a]


instance (PolarValue arg, Rule args res) => Rule arg (args -> res) where
  applyArgument f a args = applyArgument (f . (toPolarTerm a :)) args


rule :: Rule args result => String -> args -> result
rule name = applyArgument \args -> do
  terms <- sequence args
  return $ CallTerm Call{ name, args = terms }


data RegisteredType = RegisteredType
  deriving stock (Eq, Show)


instance PolarValue RegisteredType where
  toPolarTerm = externalInstance
  call _ _ _ = Nothing
  classTagOf RegisteredType = "RegisteredType"


registerType :: forall a. PolarValue a => Polar -> IO (Either PolarError ())
registerType polar = do
  instance_ <-
    atomicModifyIORef (environmentRef polar) \environment -> do
      let instanceId = maybe 0 (succ . fst) (Map.lookupMax (instanceMap environment))

      let x = Instance{ value = RegisteredType }

      let t = SomeTypeRep (typeRep @a)

      let environment' = environment
            { instanceMap = Map.insert instanceId x (instanceMap environment)
            , types = Map.insertWith (\_ old -> old) t instanceId (types environment)
            }

      let term = ExternalInstanceTerm ExternalInstance{ instanceId }

      (environment', term)

  polarRegisterConstant polar (show (typeRep @a)) instance_


-- | A deriving-via wrapper type to derive 'PolarValue' instances for record types.
newtype GenericPolarRecord a = GenericPolarRecord a
  deriving stock (Eq, Show)


instance (GPolarFields (Rep a), Generic a, Eq a, Typeable a) => PolarValue (GenericPolarRecord a) where
  toPolarTerm = externalInstance

  call (GenericPolarRecord x) = gcall (from x)

  classTagOf _ = show (typeRep @a)


class GPolarFields f where
  gcall :: f x -> String -> [PolarTerm] -> Maybe (State Environment PolarTerm)


instance GPolarFields f => GPolarFields (D1 m f) where
  gcall (M1 a) = gcall a


instance GPolarFields f => GPolarFields (C1 m f) where
  gcall (M1 a) = gcall a


instance (GPolarFields l, GPolarFields r) => GPolarFields (l :*: r) where
  gcall (l :*: r) name args = gcall l name args <|> gcall r name args


instance (KnownSymbol name, PolarValue x) => GPolarFields (S1 ('MetaSel ('Just name) a b c) (Rec0 x)) where
  gcall (M1 (K1 a)) n [] | n == symbolVal (Proxy @name) = Just $ toPolarTerm a
  gcall (M1 _) _ _  = Nothing
