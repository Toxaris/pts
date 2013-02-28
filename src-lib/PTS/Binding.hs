module PTS.Binding
  (  Binding
  ,  Bindings
  )  where

import PTS.Value (Value)
import PTS.AST (Name, TypedTerm)

type Bindings m = [(Name, Binding m)]

type Binding m = (Value m, TypedTerm)
