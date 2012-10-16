module PTS.Module where

import Control.Monad.Environment (Env)
import PTS.AST (Name)
import PTS.Binding (Binding, Env')

data Module m
  =  Module Name (Env' m)
