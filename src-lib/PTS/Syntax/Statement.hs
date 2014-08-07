module PTS.Syntax.Statement
  ( Stmt (..)
  ) where

import PTS.Error (Position)
import PTS.Syntax.Names (Name, ModuleName)
import PTS.Syntax.Term (Term)

type Telescope t = [([Name], t)]

data Stmt
  = Bind Name (Telescope Term) (Maybe Term) Term
  | Term Term
  | Export Name
  | Import ModuleName
  | StmtPos Position Stmt
  | Assertion Term (Maybe Term) (Maybe Term)
