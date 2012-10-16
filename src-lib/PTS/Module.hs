module PTS.Module where

import Control.Monad.Environment (Env)
import PTS.AST (Name)
import PTS.Binding (Binding, Bindings)

data Module m
  =  Module Name (Bindings m)
