module PTS.Algebra
  ( PreAlgebra
  , Algebra
  , fold
  , strip
  , allvars
  , allvarsAlgebra
  , freevars
  , freevarsAlgebra
  , freshvar
  , depZip
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import PTS.AST

-- algebras
type PreAlgebra alpha beta
  = TermStructure alpha -> beta

type Algebra alpha
  = PreAlgebra alpha alpha

fold :: Structure term => Algebra alpha -> term -> alpha
fold algebra term = algebra (fmap (fold algebra) (structure term))

strip :: Structure term => term -> Term
strip t = fold MkTerm t

allvars :: Structure term => term -> Names
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

freevarsAlgebra :: Algebra Names
freevarsAlgebra t = case t of
  Var x            ->  Set.singleton x
  App t1 t2        ->  t1 `Set.union` t2
  NatOp _ _ t1 t2  ->  t1 `Set.union` t2
  IfZero t1 t2 t3  ->  Set.unions [t1, t2, t3]
  Lam x t1 t2      ->  t1 `Set.union` (Set.delete x t2)
  Pi x t1 t2       ->  t1 `Set.union` (Set.delete x t2)
  Pos p t          ->  t
  _                ->  Set.empty

freevars :: Structure term => term -> Names
freevars = fold freevarsAlgebra

freshvar :: Structure term => term -> Name -> Name
freshvar t x = freshvarl (freevars t) x

-- instance Arrow PreAlgebra?
depZip :: PreAlgebra alpha alpha -> PreAlgebra (alpha, beta) beta -> PreAlgebra (alpha, beta) (alpha, beta)
depZip f g x = (f (fmap fst x), g x)
