module PTS.Dynamics.Module where

import PTS.Syntax (ModuleName)
import PTS.Dynamics.Binding (Bindings)

data Module m
  =  Module [ModuleName] ModuleName (Bindings m)
