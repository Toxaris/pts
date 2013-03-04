module PTS.Module where

import PTS.AST (ModuleName, Import)
import PTS.Binding (Bindings)

data Module m
  =  Module [Import] ModuleName (Bindings m)
