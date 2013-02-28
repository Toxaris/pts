module PTS.Module where

import Control.Monad.Environment (Env)
import PTS.AST (Name)
import PTS.Binding (Binding, Bindings)

data ModuleName
  =  ModuleName [String]

data Import
  =  Import ModuleName

data Module m
  =  Module [Import] Name (Bindings m)
