module PTS.Dynamics.Binding
  (  Binding (..)
  ,  Bindings
  )  where

import PTS.Dynamics.Value (Value)
import PTS.Syntax (Name, TypedTerm)

type Bindings m = [(Name, Binding m)]

data Binding m
  = Binding
    { bindingExport :: Bool
    , bindingValue :: Value m
    , bindingType :: TypedTerm
    }
  deriving Show
