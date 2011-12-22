module PTS.Substitution 
  ( subst
  , freshCommonVar
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import PTS.AST

-- substitution (generates fresh variables if needed to prevent accidential capture)

avoidCapture :: Term -> Name -> Name -> Term -> (Name, Term)
avoidCapture s x y t = (x', s') where
  x' | x `Set.member` fvt = freshvarl fv x
     | otherwise = x

  s' | x == x' = s
     | otherwise = subst s x (mkVar x')

  fvs = freevars s
  fvt = freevars t
  fv = Set.unions [fvs, fvt, Set.singleton y]

subst :: Term -> Name -> Term -> Term
subst t x t' = case structure t of
  Nat i                 ->  mkNat i
  NatOp i f t1 t2       ->  mkNatOp i f (subst t1 x t') (subst t2 x t')
  IfZero t1 t2 t3       ->  mkIfZero (subst t1 x t') (subst t2 x t')  (subst t3 x t')
  Var y | y == x        ->  t'
  Var y | otherwise     ->  mkVar y
  Const c               ->  mkConst c
  App t1 t2             ->  mkApp (subst t1 x t') (subst t2 x t')
  Lam y t1 t2 | x /= y  ->
    let (newy, newt2) = avoidCapture t2 y x t'
    in mkLam newy (subst t1 x t') (subst newt2 x t')
  Lam y t1 t2 | x == y  ->  mkLam y (subst t1 x t') t2
  Pi y t1 t2 | x /= y   ->
    let (newy, newt2) = avoidCapture t2 y x t'
    in mkPi newy (subst t1 x t') (subst newt2 x t')
  Pi y t1 t2 | x == y   ->  mkPi y (subst t1 x t') t2
  Pos p t               ->  mkPos p (subst t x t') -- delete pos annotation here?

freshCommonVar
  :: Name -> Name -> Term -> Term -> (Name, Term, Term)

freshCommonVar n1 n2 b1 b2 = (n', b1', b2') where
  n' | n1 == n2 = n1
     | n1 `Set.notMember` fv2 = n1
     | n2 `Set.notMember` fv1 = n2
     | otherwise = freshvarl (fv1 `Set.union` fv2) n1

  b1' | n' == n1 = b1
      | otherwise = subst b1 n1 (mkVar n')

  b2' | n' == n2 = b2
      | otherwise = subst b2 n2 (mkVar n')

  fv1 = freevars b1
  fv2 = freevars b2
