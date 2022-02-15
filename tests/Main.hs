{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}

module Main ( main ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, Value, toJSON )
import Data.Aeson.QQ.Simple ( aesonQQ )
import Data.Aeson.Types ( Result( Success ), fromJSON )

-- base
import Control.Exception ( Exception, throw )
import GHC.Generics ( Generic )
import Type.Reflection ()

-- containers
import qualified Data.Map.Strict as Map

-- hspec
import Test.Hspec
  ( HasCallStack
  , Spec
  , before
  , beforeWith
  , describe
  , expectationFailure
  , hspec
  , it
  , shouldBe
  , shouldReturn
  , xdescribe
  )

-- oso
import Polar
  ( Call( Call, args )
  , Done( Done, result )
  , Environment
  , Expression( Expression, operator, args )
  , ExternalCall( ExternalCall, callId, instance_, attribute )
  , ExternalInstance( ExternalInstance, instanceId )
  , ExternalIsSubclass( ExternalIsSubclass, callId )
  , ExternalIsa( ExternalIsa, callId, instance_, classTag )
  , ExternalIsaWithPath( ExternalIsaWithPath, callId )
  , ExternalOp( ExternalOp, callId, args, operator )
  , GenericPolarRecord(GenericPolarRecord)
  , PolarError( PolarError )
  , PolarErrorKind( ParseError, RuntimeError )
  , PolarRuntimeError( ApplicationError, QueryForUndefinedRule )
  , PolarParseError( UnrecognizedEOF )
  , PolarTerm( BoolLit, StringLit, ListLit, ExpressionTerm, Variable, ExternalInstanceTerm, CallTerm, IntegerLit )
  , PolarValue
  , QueryEvent( ResultEvent, DoneEvent, ExternalIsaEvent, ExternalIsSubclassEvent, ExternalIsaWithPathEvent, ExternalCallEvent, ExternalOpEvent )
  , Result( Result, bindings, trace )
  , emptyEnvironment
  , getResultVariable
  , name
  , polarClearRules
  , polarLoad
  , polarNew
  , polarNewQuery
  , polarNextInlineQuery
  , polarNextQueryEvent
  , polarRegisterConstant
  , polarRegisterMro
  , registerType
  , rule
  , runQuery
  , runQueryString
  , unfoldQuery, QueryResult (QueryResult, bindings), kwargs
  )
import ShouldMatch ( shouldMatch )

-- streaming
import Streaming.Prelude ( Of( (:>) ) )
import qualified Streaming.Prelude as S

-- string-qq
import Data.String.QQ ( s )

-- text
import Data.Text ( Text )

-- transformers
import Control.Monad.Trans.State.Strict ( State )


jsonSpec :: (HasCallStack, Show a, ToJSON a, FromJSON a, Eq a) => a -> Value -> Spec
jsonSpec lit json = do
  it "can decode JSON" do
    fromJSON json `shouldBe` Success lit

  it "can encode JSON" do
    toJSON lit `shouldBe` json


main :: IO ()
main = hspec do
  describe "Oso" do
    describe "PolarTerm" do
      describe "StringLit" do
        jsonSpec
          (StringLit "Hello")
          [aesonQQ|{"value":{"String":"Hello"}}|]

      describe "BoolLit" do
        jsonSpec
          (BoolLit True)
          [aesonQQ|{"value":{"Bool":true}}|]

      describe "IntegerLit" do
        jsonSpec
          (IntegerLit 123)
          [aesonQQ|{"value":{"Number":{"Integer":123}}}|]

      describe "ListLit" do
        jsonSpec
          (ListLit [BoolLit True])
          [aesonQQ|{"value":{"List":[{"value":{"Bool":true}}]}}|]

      describe "Expression" do
        jsonSpec
          (ExpressionTerm Expression{ operator = "And", args = [BoolLit True, BoolLit False] })
          [aesonQQ|{"value":{"Expression":{"operator":"And","args":[{"value":{"Bool":true}},{"value":{"Bool":false}}]}}}|]

      describe "Variable" do
        jsonSpec
          (Variable "v")
          [aesonQQ|{"value":{"Variable":"v"}}|]

      describe "ExternalInstance" do
        jsonSpec
          (ExternalInstanceTerm ExternalInstance{ instanceId = 42 })
          [aesonQQ|{"value":{"ExternalInstance":{"instance_id":42}}}|]

      describe "Call" do
        jsonSpec
          (CallTerm Call{name = "allow", args = [BoolLit True], kwargs = Nothing})
          [aesonQQ|{
            "value":{
              "Call":{
                "name":"allow", 
                "args":[{"value":{"Bool":true}}], 
                "kwargs":null
              }
            }
          }|]

    describe "QueryEvent" do
      it "can decode Done" do
        fromJSON [aesonQQ|{"Done": {"result": true} }|] `shouldBe` do
          Success $ DoneEvent Done{ result = True }

      it "can decode Result" do
        fromJSON [aesonQQ|{"Result":{"bindings":{"x":{"value":{"String":"y"}}},"trace": null}}|] `shouldBe` do
          Success $ ResultEvent Result
            { bindings = Map.singleton "x" (StringLit "y")
            , trace = Nothing
            }

      it "can decode ExternalIsa" do
        fromJSON [aesonQQ|{"ExternalIsa":{"call_id": 123, "instance":{"value":{"Bool":true}}, "class_tag":"xyz"}}|] `shouldBe` do
          Success $ ExternalIsaEvent ExternalIsa
            { callId = 123
            , instance_ = BoolLit True
            , classTag = "xyz"
            }

      it "can decode ExternalIsSubclass" do
        fromJSON [aesonQQ|{"ExternalIsSubclass":{"call_id": 123, "instance":{"value":{"Bool":true}}, "class_tag":"xyz"}}|] `shouldBe` do
          Success $ ExternalIsSubclassEvent ExternalIsSubclass
            { callId = 123
            }

      it "can decode ExternalIsaWithPath" do
        fromJSON [aesonQQ|{"ExternalIsaWithPath":{"call_id": 123, "instance":{"value":{"Bool":true}}, "class_tag":"xyz"}}|] `shouldBe` do
          Success $ ExternalIsaWithPathEvent ExternalIsaWithPath
            { callId = 123
            }

      it "can decode ExternalCall" do
        fromJSON [aesonQQ|{"ExternalCall":{"call_id": 123, "instance":{"value":{"Bool":true}}, "attribute":"foo"}}|] `shouldBe` do
          Success $ ExternalCallEvent ExternalCall
            { callId = 123
            , instance_ = BoolLit True
            , attribute = "foo"
            }

      it "can decode ExternalOp" do
        fromJSON [aesonQQ|{"ExternalOp":{"call_id": 123, "operator":"Eq", "args":[{"value":{"Variable":"u"}}]}}|] `shouldBe` do
          Success $ ExternalOpEvent ExternalOp
            { callId = 123
            , operator = "Eq"
            , args = [Variable "u"]
            }

    describe "polarNew" do
      it "doesn't crash" do
        _ <- polarNew
        return ()

    describe "polarLoad" do
      before polarNew do
        it "loads empty files" \polar ->
          polarLoad polar [""]
            `shouldReturn` Right ()

        it "loads valid polar files" \polar ->
          polarLoad polar ["refl(x) if x == x;"]
            `shouldReturn` Right ()

        it "returns errors on invalid files" \polar -> $(
          shouldMatch
            [e| polarLoad polar ["nonsense"] |]
            [p| Left (PolarError (ParseError UnrecognizedEOF{}) _) |] )

      describe "polarClearRules" do
        before polarNew do
          it "doesn't crash" \polar -> do
            polarClearRules polar
              `shouldReturn` Right ()

          it "clears rules" \polar -> do
            expect =<< polarLoad polar ["f(1);"]
            expect =<< polarClearRules polar

            query <- expect =<< polarNewQuery polar "f(x)" False
            $( shouldMatch
                 [e| polarNextQueryEvent query |]
                 [p| Left (PolarError (RuntimeError (QueryForUndefinedRule "f")) _) |] )

      describe "polarRegisterConstant" do
        before polarNew do
          it "doesn't crash on valid definitions" \polar -> do
            polarRegisterConstant polar "foo" (StringLit "bar")
              `shouldReturn` Right ()

      xdescribe "polarRegisterMro" do
        before polarNew do
          it "doesn't crash on valid definitions" \polar -> do
            polarRegisterMro polar "foo" "bar"
              `shouldReturn` Right ()

      xdescribe "polarNextInlineQuery" do
        before polarNew do
          it "returns the next inline query" \polar -> do
            polarLoad polar ["refl(x) if x == x;"]
              `shouldReturn` Right ()

            $( shouldMatch
                 [e| polarNextInlineQuery polar False |]
                 [p| Just _ |])

      describe "polarNewQuery" do
        before polarNew do
          it "can create new queries" \polar -> $(
            shouldMatch
              [e| polarNewQuery polar "foo(x)" False |]
              [p| Right _ |] )

          it "returns an error on invalid queries" \polar -> $(
            shouldMatch
              [e| polarNewQuery polar "f o o" False |]
              [p| Left (PolarError _ _) |] )

      describe "polarNextQueryEvent" do
        before polarNew do
          it "returns the next query event" \polar -> do
            expect =<< polarLoad polar ["refl(x) if x == x;"]

            query <- expect =<< polarNewQuery polar "refl(42)" False

            $( shouldMatch
                 [e| polarNextQueryEvent query |]
                 [p| Right _ |] )

          it "throws on unknown rules" \polar -> do
            query <- expect =<< polarNewQuery polar "f(x)" False

            $( shouldMatch
                 [e| polarNextQueryEvent query |]
                 [p| Left (PolarError (RuntimeError (QueryForUndefinedRule "f")) _) |] )

          it "returns bindings" \polar -> do
            expect =<< polarLoad polar ["f(1);"]

            query <- expect =<< polarNewQuery polar "f(x)" False

            result <- expect =<< polarNextQueryEvent query

            case result of
              ResultEvent Result{ bindings } -> 
                bindings `shouldBe` Map.singleton "x" (IntegerLit 1)

              _ -> expectationFailure "Expected a result"

  describe "Functional tests" do
    before polarNew do
      describe "External instances" do
        it "can specialise" \polar -> do
          expect =<< polarLoad polar [[s| test(_: User); |]]

          let user = User{ roles = [] }
          let org = Organization{ name = "Org" }

          $( shouldMatch
               [e| S.toList $ runQuery polar (rule "test" user) |]
               [p| [QueryResult{}] :> Right True |] )
               
          $( shouldMatch
               [e| S.toList $ runQuery polar (rule "test" org) |]
               [p| [] :> Right True |] )

        it "correctly handles missing attributes" \polar -> do
          expect =<< polarLoad polar [[s| test(u: User) if u.name == "Simon"; |]]

          let user = User{ roles = [] }

          $( shouldMatch
               [e| S.toList $ runQuery polar (rule "test" user) |]
               [p| [] :> Left (PolarError (RuntimeError ApplicationError{}) _) |] )

        it "can construct external instances" \polar -> do
          expect =<< registerType @Organization polar

          S.toList_ (runQueryString polar "new Organization(\"test\") = x") >>= \case
            [result] -> 
              getResultVariable result "x" `shouldBe`
                Just (Organization "test")

            _ -> expectationFailure "Expected one result"

        it "can specialize external instances with dictionaries" \polar -> do
          expect =<< registerType @Organization polar

          expect =<< polarLoad polar [[s| named(_: { name: "Foo"}); |]]

          $( shouldMatch
               [e| S.toList_ (runQueryString polar "named(new Organization(\"Foo\"))") |]
               [p| [ QueryResult{} ] |] )

          $( shouldMatch
               [e| S.toList_ (runQueryString polar "named(new Organization(\"Not foo\"))") |]
               [p| [] |] )

        it "can specialize external instance fields" \polar -> do
          expect =<< registerType @Organization polar

          expect =<< polarLoad polar [[s| named(_: Organization{ name: "Foo"}); |]]

          $( shouldMatch
               [e| S.toList_ (runQueryString polar "named(new Organization(\"Foo\"))") |]
               [p| [ QueryResult{} ] |] )

          $( shouldMatch
               [e| S.toList_ (runQueryString polar "named(new Organization(\"Not foo\"))") |]
               [p| [] |] )

        it "can create external instances with named arguments" \polar -> do
          expect =<< registerType @Organization polar

          S.toList_ (runQueryString polar "new Organization(name: \"test\") = x") >>= \case
            [result] -> 
              getResultVariable result "x" `shouldBe`
                Just Organization{ name = "test" }

            _ -> expectationFailure "Expected one result"

  describe "Examples" do
    before polarNew do
      it "can load the first example" \polar -> do
        polarLoad polar [[s| father("Artemis", "Zeus"); |]]
          `shouldReturn` Right ()

      it "can load the second example" \polar -> do
        polarLoad polar
          [[s| father("Artemis", "Zeus");
               father("Apollo", "Zeus");
           |]]
          `shouldReturn` Right ()

      it "can show that Zeus is the father of Artemis" \polar -> do
        polarLoad polar
          [[s| father("Artemis", "Zeus");
               father("Apollo", "Zeus");
           |]]
          `shouldReturn` Right ()

        query <- expect =<< polarNewQuery polar [s| father("Artemis", "Zeus") |] False

        $( shouldMatch
             [e| S.toList_ (unfoldQuery emptyEnvironment query) |]
             [p| [ QueryResult{} ] |] )

      it "can find all children of Zeus" \polar -> do
        polarLoad polar
          [[s| father("Artemis", "Zeus");
               father("Apollo", "Zeus");
           |]]
          `shouldReturn` Right ()

        query <- expect =<< polarNewQuery polar [s| father(child, "Zeus") |] False

        S.toList_ (unfoldQuery emptyEnvironment query) >>= \case
          [ QueryResult{ bindings = bindings1 }, QueryResult{ bindings = bindings2 } ] -> do
              bindings1 `shouldBe` Map.singleton "child" (StringLit "Artemis")
              bindings2 `shouldBe` Map.singleton "child" (StringLit "Apollo")

          _ ->
            expectationFailure "Expected two query results"

      it "can find the grandfather of Ascelpius" \polar -> do
        polarLoad polar
          [[s| father("Artemis", "Zeus");
               father("Apollo", "Zeus");
               father("Asclepius", "Apollo");
               grandfather(a, b) if father(a, anyPerson) and father(anyPerson, b);
           |]]
          `shouldReturn` Right ()

        query <- expect =<< polarNewQuery polar [s| grandfather("Asclepius", grandpa) |] False

        S.toList_ (unfoldQuery emptyEnvironment query) >>= \case
          [ QueryResult{ bindings } ] -> 
              bindings `shouldBe` Map.singleton "grandpa" (StringLit "Zeus")

          _ -> expectationFailure "Expected one query result"

      describe "External instances" do
        it "can lookup fields on external instances" \polar -> do
          expect =<< polarLoad polar
            [[s| foo(o: Organization) if o.name == "Test"; |]]

          let o = Organization{ name = "Test" }

          $( shouldMatch
               [e| S.toList_ (runQuery polar (rule "foo" o)) |]
               [p| [QueryResult{}] |] )

        it "can fail to search when using external instances" \polar -> do
          expect =<< polarLoad polar
            [[s| foo(o: Organization) if o.name == "Test"; |]]

          let o = Organization{ name = "Not test" }

          $( shouldMatch
               [e| S.toList_ (runQuery polar (rule "foo" o)) |]
               [p| [] |] )

      describe "RBAC" do
        let setup polar = do
              expect =<< registerType @User polar
              expect =<< registerType @Organization polar
              expect =<< registerType @Repository polar

              expect =<< polarLoad polar
                [[s|
                   allow(actor, action, resource) if
                     has_permission(actor, action, resource);

                   has_role(user: User, name: String, resource: Resource) if
                     role in user.roles and
                     role.name = name and
                     role.resource = resource;

                   actor User {}

                   resource Organization {
                     roles = ["owner"];
                   }

                   resource Repository {
                     permissions = ["read", "push"];
                     roles = ["contributor", "maintainer"];
                     relations = { parent: Organization };

                     # An actor has the "read" permission if they have the "contributor" role.
                     "read" if "contributor";
                     # An actor has the "push" permission if they have the "maintainer" role.
                     "push" if "maintainer";

                     # An actor has the "contributor" role if they have the "maintainer" role.
                     "contributor" if "maintainer";

                     # An actor has the "maintainer" role if they have the "owner" role on the "parent" Organization.
                     "maintainer" if "owner" on "parent";
                   }

                   has_relation(organization: Organization, "parent", repository: Repository) if
                     organization = repository.organization;
                 |]]

              return polar

        -- "Import" the rule from Polar, creating a Haskell function to create 
        -- queries.
        let allow :: (PolarValue a, PolarValue b) => a -> Text -> b -> State Environment PolarTerm
            allow = rule "allow"

        -- Define our resources
        let circuitHub = Organization{ name = "CircuitHub" }
        let oso        = Organization{ name = "oso" }

        let ourRepository = Repository{ name = "circuithub", organization = circuitHub }
        let chRepository  = Repository{ name = "circuithub", organization = circuitHub }
        let osoRepository = Repository{ name = "oso", organization = oso }

        let user = User{ roles = [ Role{ name = "owner", resource = circuitHub } ] }

        beforeWith setup do
          it "allows user to read ourRepository" \polar -> do
            $( shouldMatch
                 [e|S.toList (runQuery polar (allow user "read" ourRepository)) |]
                 [p| [QueryResult{}] :> Right True |] )

          it "allows user to read chRepositry" \polar -> do
            $( shouldMatch
                 [e| S.toList (runQuery polar (allow user "read" chRepository)) |]
                 [p| [QueryResult{ }] :> Right True |] )

          it "does not allow user to read osoRepository" \polar -> do
            S.toList (runQuery polar (allow user "read" osoRepository)) `shouldReturn`
              [] :> Right True


newtype User = User
  { roles :: [Role]
  }
  deriving stock (Eq, Generic, Show)
  deriving PolarValue via GenericPolarRecord User


data Role = Role
  { name :: Text
  , resource :: Organization
  }
  deriving stock (Eq, Generic, Show)
  deriving PolarValue via GenericPolarRecord Role


data Repository = Repository
  { name :: Text
  , organization :: Organization
  }
  deriving stock (Eq, Generic, Show)
  deriving PolarValue via GenericPolarRecord Repository


newtype Organization = Organization
  { name :: Text }
  deriving stock (Eq, Generic, Show)
  deriving PolarValue via GenericPolarRecord Organization


expect :: Exception e => Either e a -> IO a
expect = either throw return

