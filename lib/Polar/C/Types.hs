{-# language DuplicateRecordFields #-}

{-# options_ghc -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Polar.C.Types where

-- base
import Foreign.C.Types ( CChar )
import Foreign.Ptr ( Ptr )


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
