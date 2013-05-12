module PTS.Dynamics.Module where

import PTS.Dynamics.Binding (Bindings)
import PTS.Syntax (ModuleName)

data Module m
  =  Module [ModuleName] ModuleName (Bindings m)
