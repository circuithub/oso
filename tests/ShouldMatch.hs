{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

module ShouldMatch ( shouldMatch ) where

-- template-haskell
import Language.Haskell.TH ( ExpQ, PatQ )


-- | Check that a monadic action returns a value that matches a pattern.
-- 
-- For example:
-- 
-- > $( shouldMatch
--        [e| getAllUsers |]
--        [p| [ User{ id = 1 }, User{ id = 2, name = "Fred" } ] |] )
--
-- This Template Haskell splice expands to a pattern match. The above would
-- expand to
-- 
-- > getAllUsers >>= \case
-- >   [ User{ id = 1 }, User{ id = 2, name = "Fred" } ] -> return ()
-- >   _ -> expectationFailure ...
shouldMatch :: ExpQ -> PatQ -> ExpQ
shouldMatch m p = [e| $m >>= \case
  $(p) -> return ()
  y    -> expectationFailure $ "Did not match expected pattern: " <> show y |]
