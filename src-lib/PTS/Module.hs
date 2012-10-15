module PTS.Module where

import Control.Monad.Environment (Env)
import PTS.AST
import PTS.Evaluation (Value (..))

data Module t
  =  Module Name (Env Name (Value, TypedTerm))
