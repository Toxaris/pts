module PTS.Dynamics.Binding
  (  Binding
  ,  Bindings
  )  where

import qualified Data.Map as Map

import PTS.Dynamics.Value (Value)
import PTS.Syntax (Name, TypedTerm)

type Bindings m = Map.Map Name (Binding m)

type Binding m = (Bool, Value m, TypedTerm)
