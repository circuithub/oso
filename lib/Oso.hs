{-# language BlockArguments #-}

module Oso 
  ( -- $intro

    Oso
  , newOso

    -- * Loading rules
  , loadFiles
  , loadText 
  , clearRules

    -- * Querying rules
  , isAllowed

    -- * Error handling
  , OsoError(..)
  ) where

-- base
import Control.Exception ( try )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bifunctor ( first )

-- oso
import Polar ( Environment, Polar, PolarError, PolarTerm, PolarValue, polarClearRules, polarLoad, polarNew, rule, runQuery )

-- streaming
import qualified Streaming.Prelude as S

-- text
import Data.Text ( Text )
import qualified Data.Text.IO as T

-- transformers
import Control.Monad.Trans.State.Strict ( State )


-- | An opaque handle to an instance of the Oso authorization library/Polar rule engine.
--
-- @Oso@ can be created with 'newOso', and typically one is created for the
-- lifetime of an application.
--
-- Once constructed, rules can be loaded with 'loadFiles', and latter queried
-- with 'isAllowed'.
newtype Oso = Oso { polarHandle :: Polar }


-- | Errors that can occur during usage of this library.
newtype OsoError 
  = -- | The error occured in an interaction with the Polar rule engine.
    PolarError PolarError


-- | Construct a new 'Oso' instance.
newOso :: MonadIO m => m Oso
newOso = liftIO do
  Oso <$> polarNew


-- | Clear out all files and rules that have been loaded.
clearRules :: MonadIO m => Oso -> m (Either OsoError ())
clearRules oso = liftIO do
  first PolarError <$> polarClearRules (polarHandle oso)


-- | High level interface for authorization decisions. Makes an @allow@ query
-- with the given actor, action and resource and returns 'True' or 'False'.
-- 
-- You can supply arbitrary Haskell values to the @allow@ rule, so long as they
-- are instances of 'PolarValue'. See the documentation on 'PolarValue' for
-- more information on how to create these instances.
isAllowed 
  :: (MonadIO m, PolarValue actor, PolarValue action, PolarValue resource) 
  => Oso -- ^
  -> actor  -- ^ The actor performing an action.
  -> action  -- ^ The action being performed. This is often a string literal like @"read"@ .
  -> resource -- ^ The resource the action is being performed on
  -> m (Either OsoError Bool)
isAllowed oso actor action resource = liftIO do
  first PolarError <$> try do
    S.any_
      (const True)
      (runQuery (polarHandle oso) (allow actor action resource))


allow :: (PolarValue actor, PolarValue action, PolarValue resource) => actor -> action -> resource -> State Environment PolarTerm
allow = rule "allow"


-- | Load a set of files containing Polar rules.
loadFiles :: (Traversable f, MonadIO m) => Oso -> f FilePath -> m (Either OsoError ())
loadFiles oso files = liftIO do
  first PolarError <$> do
    srcs <- traverse loadFile files
    polarLoad (polarHandle oso) srcs
  where
    loadFile :: FilePath -> IO Text
    loadFile filePath = T.readFile filePath


-- | Load a string of Polar source directly.
loadText :: MonadIO m => Oso -> Text -> m (Either OsoError ())
loadText oso source = liftIO do
  first PolarError <$> do
    polarLoad (polarHandle oso) [source]
