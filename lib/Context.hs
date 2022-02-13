{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module Context 
  ( CPolar
  , CQuery
  , CResultQuery
  , CResultString
  , CResultVoid
  , polarCtx
  ) where

-- containers
import qualified Data.Map.Strict as Map

-- inline-c
import Language.C.Inline as C ( Context )
import Language.C.Inline.Context ( Context(ctxTypesTable) )
import Language.C.Types ( TypeSpecifier(TypeName) )


data CPolar


data CResultQuery


data CResultString


data CResultVoid


data CQuery


polarCtx :: Context
polarCtx = mempty
  { ctxTypesTable = Map.fromList
      [ (TypeName "polar_Polar", [t| CPolar |])
      , (TypeName "polar_CResult_c_void", [t| CResultVoid |])
      , (TypeName "polar_CResult_Query", [t| CResultQuery |])
      , (TypeName "polar_CResult_c_char", [t| CResultString |])
      , (TypeName "polar_Query", [t| CQuery |])
      ]
  }
