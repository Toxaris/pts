module PTS.Dynamics.Binding
  (  Binding
  ,  Bindings
  )  where

import PTS.Dynamics.Value (Value)
import PTS.Syntax (Name, TypedTerm)

type Bindings m = [(Name, Binding m)]

type Binding m = (Bool, Value m, TypedTerm)
