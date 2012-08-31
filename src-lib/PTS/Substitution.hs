module PTS.Substitution
  ( subst
  , typedSubst
  , freshCommonVar
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import PTS.Algebra
import PTS.AST

-- substitution (generates fresh variables if needed to prevent accidental capture)

typedAvoidCapture :: TypedTerm -> Name -> TypedTerm -> Name -> TypedTerm -> (Name, TypedTerm)
typedAvoidCapture s x xt y t = (x', s') where
  x' | x `Set.member` fvt = freshvarl fv x
     | otherwise = x

  s' | x == x' = s
     | otherwise = typedSubst s x (MkTypedTerm (Var x') xt)

  fvs = freevars s
  fvt = freevars t
  fv = Set.unions [fvs, fvt, Set.singleton y]

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

typedSubst :: TypedTerm -> Name -> TypedTerm -> TypedTerm
typedSubst t x t' = case structure t of
  Nat i                 ->  MkTypedTerm (Nat i) (typeOf t)
  NatOp i f t1 t2       ->  MkTypedTerm (NatOp i f (typedSubst t1 x t') (typedSubst t2 x t')) (typeOf t)
  IfZero t1 t2 t3       ->  MkTypedTerm (IfZero (typedSubst t1 x t') (typedSubst t2 x t')  (typedSubst t3 x t')) (typeOf t)
  Var y | y == x        ->  t'
  Var y | otherwise     ->  MkTypedTerm (Var y) (typeOf t)
  Const c               ->  MkTypedTerm (Const c) (typeOf t)
  App t1 t2             ->  MkTypedTerm (App (typedSubst t1 x t') (typedSubst t2 x t')) (typeOf t)
  Lam y t1 t2 | x /= y  ->
    let (newy, newt2) = typedAvoidCapture t2 y t1 x t' 
    in MkTypedTerm (Lam newy (typedSubst t1 x t') (typedSubst newt2 x t')) (typeOf t)
  Lam y t1 t2 | x == y  ->  MkTypedTerm (Lam y (typedSubst t1 x t') t2) (typeOf t)
  Pi y t1 t2 | x /= y   ->
    let (newy, newt2) = typedAvoidCapture t2 y t1 x t'
    in MkTypedTerm (Pi newy (typedSubst t1 x t') (typedSubst newt2 x t')) (typeOf t)
  Pi y t1 t2 | x == y   ->  MkTypedTerm (Pi y (typedSubst t1 x t') t2) (typeOf t)
  Pos p t               ->  MkTypedTerm (Pos p (typedSubst t x t')) (typeOf t) -- delete pos annotation here?

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
