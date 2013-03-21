module PTS.Syntax.File
  ( File (..)
  ) where

import PTS.Syntax.Names (ModuleName)
import PTS.Syntax.Statement (Stmt)
import PTS.Syntax.Term (Term)

data File m
  =  File (Maybe ModuleName) [Stmt]

