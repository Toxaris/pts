module PTS.Module where

import PTS.Syntax (ModuleName)
import PTS.Binding (Bindings)

data Module m
  =  Module [ModuleName] ModuleName (Bindings m)
