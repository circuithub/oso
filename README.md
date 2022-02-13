# Haskell bindings to `polar`

This library contains Haskell bindings to the [Polar](https://osohq.com) query engine, and provides an API for the Oso authorization library.

## Example

```haskell
module Main where

import Control.Exception
import OSO


-- Define ordinary Haskell types, deriving PolarValue to allow them to be used
-- with Polar.
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
  deriving PolarValue via GenericPolarRecord Repository


-- A Haskell function for constructing a query against the 'allow' rule
allow :: (PolarValue a, PolarValue b) => a -> String -> b -> QueryBuilder
allow = rule "allow"


main :: IO ()
main = do
  -- Create a new handle to the Polar query engine.
  polar <- polarNew

  -- Register any types used for RBAC
  registerType @User polar
  registerType @Role polar
  registerType @Repository polar
  registerType @Organization polar

  -- Load the policy.polar file
  expect =<< polarLoad polar =<< readFile "policy.polar"

  -- Fetch data needed for authorization. For this example we'll just construct
  -- some test values.
  let circuitHub = Organization{ name = "CircuitHub" }
  let oso        = Organization{ name = "oso" }

  let ourRepository = Repository{ name = "circuithub", organization = circuitHub }
  let chRepository  = Repository{ name = "circuithub", organization = circuitHub }
  let osoRepository = Repository{ name = "oso", organization = oso }

  let chUser = User{ roles = [ Role{ name = "owner", resource = circuitHub } ] }

  -- Run some queries!
  queryRuleOnce polar $ allow chUser "read" chRepository  -- True

  queryRuleOnce polar $ allow chUser "read" osoRepository -- False


expect :: Exception e => Either e a -> IO a
expect = either throw pure
```
