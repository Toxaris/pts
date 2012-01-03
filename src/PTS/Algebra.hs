module PTS.Algebra
  ( PreAlgebra
  , Algebra
  , fold
  , allvars
  , allvarsAlgebra
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import PTS.AST

-- algebras
type PreAlgebra alpha beta
  = TermStructure alpha -> beta

type Algebra alpha
  = PreAlgebra alpha alpha

fold :: Algebra alpha -> Term -> alpha
fold algebra term = algebra (fmap (fold algebra) (structure term))

allvars :: Term -> Names
allvars t = fold allvarsAlgebra t

allvarsAlgebra :: Algebra Names
allvarsAlgebra (Var x)            =  Set.singleton x
allvarsAlgebra (App t1 t2)        =  t1 `Set.union` t2
allvarsAlgebra (NatOp _ _ t1 t2)  =  t1 `Set.union` t2
allvarsAlgebra (IfZero t1 t2 t3)  =  t1 `Set.union` t2 `Set.union` t3
allvarsAlgebra (Lam x t1 t2)      =  Set.insert x (t1 `Set.union` t2)
allvarsAlgebra (Pi x t1 t2)       =  Set.insert x (t1 `Set.union` t2)
allvarsAlgebra (Pos p t)          =  t
allvarsAlgebra _                  =  Set.empty

