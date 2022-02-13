{-# language DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Polar.C.Types where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar)

data Polar

data Query

data CResult_c_void = CResult_c_void
  { result :: Ptr ()
  , error :: Ptr CChar
  }

data CResult_Query = CResult_Query
  { result :: Ptr Query
  , error :: Ptr CChar
  }

data CResult_c_char = CResult_c_char
  { result :: Ptr CChar
  , error :: Ptr CChar
  }
