module PTS.Syntax.File
  ( File (..)
  ) where

import PTS.Syntax.Names (ModuleName, PTSName)
import PTS.Syntax.Statement (Stmt)
import PTS.Syntax.Term (Term)

data File m
  =  File (Maybe PTSName) (Maybe ModuleName) [Stmt]

