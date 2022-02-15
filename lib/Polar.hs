{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
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
{-# LANGUAGE DerivingVia #-}

module Polar where

-- aeson
import Data.Aeson
  ( FromJSON( parseJSON )
  , ToJSON( toJSON )
  , Value( String, Number )
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
import Data.Foldable ( msum, toList, fold )
import Data.Functor ( (<&>) )
import Data.IORef ( IORef, atomicModifyIORef, newIORef, readIORef )
import Data.Type.Equality ( (:~:)( Refl ), testEquality )
import Data.Word ( Word64 )
import Foreign.C ( CChar, peekCString, withCString )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
import Foreign.Storable ( peekByteOff, sizeOf )
import GHC.Generics ( C1, D1, Generic, K1( K1 ), M1( M1 ), Meta( MetaSel ), Rec0, Rep, S1, type (:*:)( (:*:) ), from, to, U1 (U1) )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Type.Reflection ( SomeTypeRep( SomeTypeRep ), Typeable, typeOf, typeRep )

-- containers
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

-- oso
import qualified Polar.C as C
import qualified Polar.C.Types as C

-- scientific
import Data.Scientific ( fromFloatDigits )

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
import Data.Bifunctor (first)
import Data.Maybe (isJust)


data PolarError
  = PolarError PolarErrorKind Text
  | CouldNotDecodeQueryEvent String String
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


data PolarRuntimeError
  = ApplicationError String String PolarTerm
  | QueryForUndefinedRule String
  deriving stock (Eq, Show)


instance FromJSON PolarRuntimeError where
  parseJSON = withObject "PolarRuntimeError" \o -> msum
    [ o .: "Application" >>= withObject "Application" \o -> do
        msg <- o .: "msg"
        stackTrace <- o .: "stack_trace"
        term <- o .: "term"
        pure $ ApplicationError msg stackTrace term
    , o .: "QueryForUndefinedRule" >>= withObject "QueryForUndefinedRule" \o -> do
        name <- o .: "name"
        return $ QueryForUndefinedRule name
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
        Left e  -> pure (Left (CouldNotDecodeQueryEvent e json))
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
  | DoubleLit Double
  | ExpressionTerm Expression
  | Variable String
  | ExternalInstanceTerm ExternalInstance
  | CallTerm Call
  | IntegerLit Integer
  | Dictionary (Map.Map String PolarTerm)
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
        , o .: "Number" >>= withObject "Number" \o -> msum
            [ IntegerLit <$> o .: "Integer"
            , DoubleLit <$> o .: "Float"
            , o .: "Float" >>= withText "Float" \case
                "Infinity"  -> pure $ DoubleLit (1/0)
                "-Infinity" -> pure $ DoubleLit (-1/0)
                "NaN"       -> pure $ DoubleLit (0/0)
                _           -> empty
            ]
        , o .: "Dictionary" >>= withObject "Dictionary" \o ->
            Dictionary <$> o .: "fields"
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
      IntegerLit x -> [aesonQQ| {"Number": {"Integer": #{x}}} |]
      DoubleLit x -> [aesonQQ| {"Number": {"Float": #{y}}} |]
        where
          y | isNaN x   = String "NaN"
            | x == 1/0  = String "Infinity"
            | x == -1/0 = String "-Infinity"
            | otherwise = Number $ fromFloatDigits x
      Dictionary xs -> [aesonQQ| {"Dictionary":{"fields":#{xs}}} |]


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
  , kwargs :: Maybe (Map String PolarTerm)
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
  | MakeExternalEvent MakeExternal
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
    <|> MakeExternalEvent <$> o .: "MakeExternal"


data Result = Result
  { bindings :: Map String PolarTerm
  , trace :: Maybe Trace
  }
  deriving stock (Eq, Generic, Show)


instance FromJSON Result where
  parseJSON = withObject "Result" \o -> do
    bindings <- o .: "bindings"
    trace <- o .: "trace"

    return Result{ bindings, trace }


newtype Trace = Trace { formatted :: String }
  deriving stock (Eq, Show)


instance FromJSON Trace where
  parseJSON = withObject "Trace" \o ->
    Trace <$> o .: "formatted"


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
  { callId :: Word64 
  , baseTag :: String
  , path :: [PolarTerm]
  , classTag :: String
  }
  deriving stock (Eq, Show)


instance FromJSON ExternalIsaWithPath where
  parseJSON = withObject "ExternalIsaWithPath" \o -> do
    callId <- o .: "call_id"
    baseTag <- o .: "base_tag"
    path <- o .: "path"
    classTag <- o .: "class_tag"
    return ExternalIsaWithPath{ callId, baseTag, path, classTag }


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


data MakeExternal = MakeExternal
  { instanceId :: Word64
  , constructor :: PolarTerm
  }
  deriving stock (Eq, Show)


instance FromJSON MakeExternal where
  parseJSON = withObject "MakeExternal" \o -> do
    instanceId <- o .: "instance_id"
    constructor <- o .: "constructor"
    return MakeExternal{ instanceId, constructor }


data Environment = Environment
  { instanceMap :: Map Word64 Instance
  , types :: Map String SomePolarType
  }
  deriving stock (Eq, Show)


data SomePolarType where
  SomePolarType :: PolarValue a => Proxy a -> SomePolarType


instance Eq SomePolarType where
  SomePolarType a == SomePolarType b = go a b
    where
      go :: forall a b. (PolarValue a, PolarValue b) => Proxy a -> Proxy b -> Bool
      go _ _ =
        isJust $ testEquality (typeRep @a) (typeRep @b)

instance Show SomePolarType where
  show (SomePolarType p) = go p
    where
      go :: forall a. PolarValue a => Proxy a -> String
      go _ = show $ typeRep @a


emptyEnvironment :: Environment
emptyEnvironment = Environment mempty mempty


externalInstance :: forall a. PolarValue a => a -> State Environment PolarTerm
externalInstance a = state \environment -> do
  let i = maybe 0 (succ . fst) (Map.lookupMax (instanceMap environment))

  let x = Instance{ value = a }

  let t = show $ SomeTypeRep (typeOf a)

  let environment' = environment
        { instanceMap = Map.insert i x (instanceMap environment)
        , types = Map.insertWith (\_ old -> old) t (SomePolarType (Proxy @a)) (types environment)
        }

  let term = ExternalInstanceTerm ExternalInstance{ instanceId = i }

  (term, environment')


data Instance where
  Instance
    :: PolarValue a
    => { value :: a }
    -> Instance


instance Show Instance where
  show _ = "Instance"


instance Eq Instance where
  Instance{ value = valueA } == Instance{ value = valueB } =
   case testEquality (typeOf valueA) (typeOf valueB) of
     Nothing   -> False
     Just Refl -> valueA == valueB


data QueryResult = QueryResult
  { bindings :: Map String PolarTerm
  , environment :: Environment
  }
  deriving stock (Eq, Show)


getResultVariable :: PolarValue a => QueryResult -> String -> Maybe a
getResultVariable QueryResult{ bindings, environment } v =
  Map.lookup v bindings >>= fromPolarTerm environment


runQuery :: Polar -> State Environment PolarTerm -> Stream (Of QueryResult) IO (Either PolarError Bool)
runQuery polar queryBuilder = do
  environment <- liftIO (readIORef (environmentRef polar))

  let (queryTerm, env) = runState queryBuilder environment

  liftIO (polarNewQueryFromTerm polar queryTerm False) >>= \case
    Left e  -> return (Left e)
    Right q -> unfoldQuery env q


runQueryString :: Polar -> String -> Stream (Of QueryResult) IO (Either PolarError Bool)
runQueryString polar queryString = do
  env <- liftIO (readIORef (environmentRef polar))

  liftIO (polarNewQuery polar queryString False) >>= \case
    Left e  -> return (Left e)
    Right q -> unfoldQuery env q


unfoldQuery :: Environment -> Query -> Stream (Of QueryResult) IO (Either PolarError Bool)
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

        ResultEvent Result{ bindings } ->
          return $ Right (QueryResult{ bindings, environment = env }, env)

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

        MakeExternalEvent MakeExternal{ instanceId, constructor = CallTerm Call{ name = constructorName, args, kwargs } } -> do
          somePolarType <- case Map.lookup constructorName (types env) of
            Nothing -> fail "Could not find constructor"
            Just x -> return x

          case somePolarType of
            SomePolarType p -> do
              let mk :: forall t. PolarValue t => Proxy t -> Either String t
                  mk _ = construct @t env ConstructorArguments
                    { positionalArguments = args
                    , namedArguments = fold kwargs
                    }

              case mk p of
                Right a -> do
                  let env' = env { instanceMap = Map.insert instanceId (Instance a) (instanceMap env) }
                  go env'

                Left e ->
                  fail $ "Could not construct: " <> e <> show args

        x -> fail $ show x <> " is not handled"


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


data ConstructorArguments = ConstructorArguments
  { positionalArguments :: [PolarTerm]
  , namedArguments :: Map String PolarTerm
  }


-- | Types that can be used in Polar rules.
class (Eq a, Typeable a) => PolarValue a where
  -- | Convert a Haskell value to a Polar term. Typically this will be
  -- 'externalInstance', which essentially sends the value as a pointer.
  toPolarTerm :: a -> State Environment PolarTerm

  fromPolarTerm :: Environment -> PolarTerm -> Maybe a

  -- | Handle attribute access or method calls. If the given value doesn't
  -- support a method or attribute, 'Nothing' can be returned.
  call
    :: a -- ^ The value to operate on.
    -> String -- ^ The name of the attribute or method to access.
    -> [PolarTerm] -- ^ Any arguments for the call (an empty list for attributes).
    -> Maybe (State Environment PolarTerm)

  classTagOf :: a -> String

  construct :: Environment -> ConstructorArguments -> Either String a


instance PolarValue a => PolarValue [a] where
  toPolarTerm xs = ListLit <$> traverse toPolarTerm xs

  fromPolarTerm e = \case
    ListLit xs -> traverse (fromPolarTerm e) xs
    _          -> Nothing

  call _ _ _ = Nothing
  classTagOf _ = "?"

  construct _ _ = Left "Cannot construct lists"


instance PolarValue Text where
  toPolarTerm = pure . StringLit

  fromPolarTerm _ = \case
    StringLit t -> Just t
    _           -> Nothing

  call _ _ _ = Nothing
  classTagOf _ = "String"

  construct _ _ = Left "Cannot construct Text"


class Rule arg res where
  applyArgument :: ([State Environment PolarTerm] -> State Environment PolarTerm) -> arg -> res


instance PolarValue arg => Rule arg (State Environment PolarTerm) where
  applyArgument f a = f [toPolarTerm a]


instance (PolarValue arg, Rule args res) => Rule arg (args -> res) where
  applyArgument f a args = applyArgument (f . (toPolarTerm a :)) args


rule :: Rule args result => String -> args -> result
rule name = applyArgument \args -> do
  terms <- sequence args
  return $ CallTerm Call{ name, args = terms, kwargs = Nothing }


data RegisteredType = RegisteredType
  deriving stock (Eq, Generic, Show)
  deriving PolarValue via GenericPolarRecord RegisteredType


registerType :: forall a. PolarValue a => Polar -> IO (Either PolarError ())
registerType polar = do
  let t = show $ SomeTypeRep (typeRep @a)

  instance_ <-
    atomicModifyIORef (environmentRef polar) \environment -> do
      let instanceId = maybe 0 (succ . fst) (Map.lookupMax (instanceMap environment))

      let x = Instance{ value = RegisteredType }

      let environment' = environment
            { instanceMap = Map.insert instanceId x (instanceMap environment)
            , types = Map.insertWith (\_ old -> old) t (SomePolarType (Proxy @a)) (types environment)
            }

      let term = ExternalInstanceTerm ExternalInstance{ instanceId }

      (environment', term)

  polarRegisterConstant polar t instance_


-- | A deriving-via wrapper type to derive 'PolarValue' instances for record types.
newtype GenericPolarRecord a = GenericPolarRecord a
  deriving stock (Eq, Show)


instance (GPolarFields (Rep a), Generic a, Eq a, Typeable a, Show a) => PolarValue (GenericPolarRecord a) where
  toPolarTerm = externalInstance

  call (GenericPolarRecord x) = gcall (from x)

  classTagOf _ = show (typeRep @a)

  construct e = fmap (GenericPolarRecord . to . fst) . gconstruct @(Rep a) @() e

  fromPolarTerm e = \case
    ExternalInstanceTerm ExternalInstance{ instanceId } -> do
      Instance{ value } <- Map.lookup instanceId (instanceMap e)
      GenericPolarRecord <$> cast value
      where
        cast :: forall x. PolarValue x => x -> Maybe a
        cast x = testEquality (typeRep @x) (typeRep @a) <&> \Refl -> x

    _ -> Nothing


class GPolarFields f where
  gcall :: f x -> String -> [PolarTerm] -> Maybe (State Environment PolarTerm)

  gconstruct :: Environment -> ConstructorArguments -> Either String (f x, ConstructorArguments)


instance GPolarFields f => GPolarFields (D1 m f) where
  gcall (M1 a) = gcall a

  gconstruct e = fmap (first M1) . gconstruct @f e


instance GPolarFields f => GPolarFields (C1 m f) where
  gcall (M1 a) = gcall a

  gconstruct e = fmap (first M1) . gconstruct @f e


instance (GPolarFields l, GPolarFields r) => GPolarFields (l :*: r) where
  gcall (l :*: r) name args = gcall l name args <|> gcall r name args

  gconstruct e args = do
    (l, args') <- gconstruct @l e args
    (r, args'') <- gconstruct @r e args'
    return (l :*: r, args'')


instance (KnownSymbol name, PolarValue x) => GPolarFields (S1 ('MetaSel ('Just name) a b c) (Rec0 x)) where
  gcall (M1 (K1 a)) n [] | n == symbolVal (Proxy @name) = Just $ toPolarTerm a
  gcall (M1 _) _ _  = Nothing

  gconstruct e args@ConstructorArguments{ positionalArguments, namedArguments }
    | x:xs <- positionalArguments =
        case fromPolarTerm e x of
          Just y -> pure (M1 (K1 y), args{ positionalArguments = xs })
          _      -> Left $ "Could not marshal argument for " <> symbolVal (Proxy @name)

    | null namedArguments =
        Left $ "Empty argument list encountered at " <> symbolVal (Proxy @name)

    | Just x <- Map.lookup (symbolVal (Proxy @name)) namedArguments =
        case fromPolarTerm e x of
          Just y -> pure (M1 (K1 y), args{ namedArguments = Map.delete (symbolVal (Proxy @name)) namedArguments })
          _      -> Left $ "Could not marshal argument for " <> symbolVal (Proxy @name)

    | otherwise =
        Left $ "Could not find named argument for " <> symbolVal (Proxy @name)



instance GPolarFields U1 where
  gcall _ _ _ = Nothing

  gconstruct _ args = pure (U1, args)
