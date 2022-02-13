{-# language BlockArguments #-}

module Oso ( Oso, newOso, clearRules, isAllowed, loadFiles, loadText ) where

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


newtype Oso = Oso { polarHandle :: Polar }


newtype OsoError = PolarError PolarError


newOso :: MonadIO m => m Oso
newOso = liftIO do
  Oso <$> polarNew


clearRules :: MonadIO m => Oso -> m (Either OsoError ())
clearRules oso = liftIO do
  first PolarError <$> polarClearRules (polarHandle oso)


isAllowed :: (MonadIO m, PolarValue actor, PolarValue action, PolarValue resource) => Oso -> actor -> action -> resource -> m (Either OsoError Bool)
isAllowed oso actor action resource = liftIO do
  first PolarError <$> try do
    S.any_
      (const True)
      (runQuery (polarHandle oso) (allow actor action resource))


allow :: (PolarValue actor, PolarValue action, PolarValue resource) => actor -> action -> resource -> State Environment PolarTerm
allow = rule "allow"


loadFiles :: (Traversable f, MonadIO m) => Oso -> f FilePath -> m (Either OsoError ())
loadFiles oso files = liftIO do
  first PolarError <$> do
    srcs <- traverse loadFile files
    polarLoad (polarHandle oso) srcs
  where
    loadFile :: FilePath -> IO Text
    loadFile filePath = T.readFile filePath


loadText :: MonadIO m => Oso -> Text -> m (Either OsoError ())
loadText oso source = liftIO do
  first PolarError <$> do
    polarLoad (polarHandle oso) [source]
