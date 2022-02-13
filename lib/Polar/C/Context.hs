{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module Polar.C.Context ( polarCtx ) where

-- containers
import qualified Data.Map.Strict as Map

-- inline-c
import Language.C.Inline as C ( Context )
import Language.C.Inline.Context ( Context(ctxTypesTable) )
import Language.C.Types ( TypeSpecifier(TypeName) )
import Polar.C.Types ( Polar, CResult_c_void, Query, CResult_Query, CResult_c_char )


polarCtx :: Context
polarCtx = mempty
  { ctxTypesTable = Map.fromList
      [ (TypeName "polar_Polar", [t| Polar |])
      , (TypeName "polar_CResult_c_void", [t| CResult_c_void |])
      , (TypeName "polar_CResult_Query", [t| CResult_Query |])
      , (TypeName "polar_CResult_c_char", [t| CResult_c_char |])
      , (TypeName "polar_Query", [t| Query |])
      ]
  }
