module PTS.Module where

import PTS.AST (ModuleName)
import PTS.Binding (Bindings)

data Module m
  =  Module [ModuleName] ModuleName (Bindings m)
