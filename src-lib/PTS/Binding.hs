module PTS.Binding
  (  Binding
  ,  Bindings
  )  where

import PTS.Value (Value)
import PTS.Syntax (Name, TypedTerm)

type Bindings m = [(Name, Binding m)]

type Binding m = (Value m, TypedTerm)
