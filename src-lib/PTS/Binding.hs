module PTS.Binding
  (  Binding
  ,  Env'
  )  where

import PTS.Value (Value)
import PTS.AST (Name, TypedTerm)

type Env' m = [(Name, Binding m)]

type Binding m = (Value m, TypedTerm)
