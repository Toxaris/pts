module PTS.Syntax.Statement
  ( Stmt (..)
  ) where

import PTS.Syntax.Names (Name, ModuleName)
import PTS.Syntax.Term (Term)
import Parametric.Error (Position)

data Stmt
  = Bind Name [([Name], Term)] (Maybe Term) Term
  | Term Term
  | Export Name
  | Import ModuleName
  | StmtPos Position Stmt
  | Assertion Term (Maybe Term) (Maybe Term)
