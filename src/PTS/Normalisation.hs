{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}
module PTS.Normalisation  where

import PTS.AST

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- select a version
normalform = normalform1

-- normalization and equivalence of terms
-- 
-- this is the original, rather slow code
--
-- benchmark: 54s
normalform1 :: Term -> Term
normalform1 t = case structure t of
  App t1 t2        ->  case structure (normalform1 t1) of
                         Lam x _ t1  ->  normalform1 $ subst t1 x t2
                         _           ->  mkApp (normalform1 t1) (normalform1 t2)
  Lam x t1 t2      ->  mkLam x (normalform1 t1) (normalform1 t2)
  Pi x t1 t2       ->  mkPi x (normalform1 t1) (normalform1 t2)
  NatOp i f t1 t2  ->  case (structure (normalform1 t1), structure (normalform1 t2)) of
                         (Nat i, Nat j)  ->  mkNat (f i j)
                         _               ->  mkNatOp i f (normalform1 t1) (normalform1 t2)
  IfZero t1 t2 t3  ->  case structure (normalform1 t1) of
                          Nat 0  ->  normalform1 t2
                          Nat _  ->  normalform1 t3
                          _      ->  mkIfZero (normalform1 t1) (normalform1 t2) (normalform1 t3)
  Pos p t          ->  normalform1 t
  _                ->  t

-- sligthly more sharing: 52s
normalform2 :: Term -> Term
normalform2 t = case structure t of
  App t1 t2
    ->  case structure t1' of
         Lam x _ body  ->  normalform2 $ subst body x t2
         _             ->  mkApp t1' t2'
    where
      t1'  =  normalform2 t1
      t2'  =  normalform2 t2
  
  Lam x t1 t2 
    ->  mkLam x t1' t2'
    where
      t1'  =  normalform2 t1
      t2'  =  normalform2 t2
  
  Pi x t1 t2
    ->  mkPi x t1' t2'
    where
      t1'  =  normalform2 t1
      t2'  =  normalform2 t2
  
  NatOp i f t1 t2
    ->  case (structure t1', structure t2') of
         (Nat i, Nat j)  ->  mkNat (f i j)
         _               ->  mkNatOp i f t1' t2'
    where
      t1'  =  normalform2 t1
      t2'  =  normalform2 t2
  
  IfZero t1 t2 t3
    ->  case structure t1' of
         Nat j  ->  if (j==0) then t2' else t3'
         _      ->  mkIfZero t1' t2' t3'
    where
      t1'  =  normalform2 t1
      t2'  =  normalform2 t2
      t3'  =  normalform2 t3
  
  Pos p t
    ->  normalform2 t
  
  _ 
    ->  t

  
-- cool version: does not work yet :(
data Context
  = Top
  | NatOp1 !Name !(Int -> Int -> Int) !Context !Term
  | NatOp2V !Name !(Int -> Int -> Int) !Int !Context
  | NatOp2T !Name !(Int -> Int -> Int) !Term !Context
  | IfZero1 !Context !Term !Term
  | IfZero2 !Term !Context !Term
  | IfZero3 !Term !Term !Context
  | App1 !Context !Term
  | App2 !Term !Context
  | Lam1 !Env !Name !Context !Term
  | Lam2 !Env !Name !Term !Context
  | Pi1 !Name !Context !Term
  | Pi2 !Env !Name !Term !Context
  | Local !Env !Context

data Env
  = Env (Set Name) (Map Name (Term, Env))

bindEnv :: Name -> Term -> Env -> Env -> Env
bindEnv n boundT boundEnv (Env ids env) = Env ids' env' where 
  env'  =  Map.insert n (boundT, boundEnv) env
  ids'  =  Set.unions [freevars boundT, ids, namesEnv boundEnv]

unbindEnv :: Name -> Env -> Env
unbindEnv n (Env ids env) = Env ids' env' where
  env'  =  Map.delete n env
  ids'  =  ids

lookupEnv :: Name -> Env -> Maybe (Term, Env)
lookupEnv n (Env ids env) = Map.lookup n env

emptyEnv :: Env
emptyEnv = Env Set.empty Map.empty

namesEnv :: Env -> Set Name
namesEnv (Env ids env) = ids

fresh :: Name -> Set Name -> Name
fresh n ns
  = if n `Set.notMember` ns then n else freshvarl ns n

normalize :: Term -> Term
normalize term = decompose emptyEnv Top term where
  decompose !env !ctx !t = case structure t of
    Nat i            ->  continueNat env i ctx
    NatOp n f t1 t2  ->  decompose env (NatOp1 n f ctx t2) t1
    IfZero t1 t2 t3  ->  decompose env (IfZero1 ctx t2 t3) t1
    Var n            ->  reduceVar env n ctx 
    Const c          ->  continueTerm env (mkConst c) ctx
    App t1 t2        ->  decompose env (App1 ctx t2) t1
    Lam n t1 t2      ->  continueLam env env n t1 t2 ctx
    Pi n t1 t2       ->  decompose env (Pi1 n ctx t2) t1
    Pos p t          ->  decompose env ctx t

  continueTerm !env !t  !Top                   =  t
  continueTerm !_   !t  !(Local env ctx)       =  continueTerm env t ctx
  continueTerm !env !t1 !(NatOp1 n f ctx t2)   =  decompose env (NatOp2T n f t1 ctx) t2 
  continueTerm !env !t2 !(NatOp2V n f i ctx)   =  continueTerm env (mkNatOp n f (mkNat i) t2) ctx
  continueTerm !env !t2 !(NatOp2T n f t1 ctx)  =  continueTerm env (mkNatOp n f t1 t2) ctx
  continueTerm !env !t1 !(IfZero1 ctx t2 t3)   =  decompose env (IfZero2 t1 ctx t3) t2
  continueTerm !env !t2 !(IfZero2 t1 ctx t3)   =  decompose env (IfZero3 t1 t2 ctx) t3
  continueTerm !env !t3 !(IfZero3 t1 t2 ctx)   =  continueTerm env (mkIfZero t1 t2 t3) ctx
  continueTerm !env !t1 !(App1 ctx t2)         =  decompose env (App2 t1 ctx) t2
  continueTerm !env !t2 !(App2 t1 ctx)         =  continueTerm env (mkApp t1 t2) ctx
  continueTerm !env !t1 !(Lam1 env' n ctx t2)  =  avoidCaptureLam env t1 env' n ctx t2
  continueTerm !_   !t2 !(Lam2 env n t1 ctx)   =  continueTerm env (mkLam n t1 t2) ctx
  continueTerm !env !t1 !(Pi1 n ctx t2)        =  avoidCapturePi env t1 n ctx t2
  continueTerm !_   !t2 !(Pi2 env n t1 ctx)    =  continueTerm env (mkPi n t1 t2) ctx
  
  continueNat !_   !j !(Local env ctx)       =  continueNat env j ctx
  continueNat !env !j !(NatOp1 n f ctx t2)   =  decompose env (NatOp2V n f j ctx) t2 
  continueNat !env !j !(NatOp2V n f i ctx)   =  reduceNatOp env n f i j ctx
  continueNat !env !j !(IfZero1 ctx t2 t3)   =  reduceIfZero env j t2 t3 ctx
  continueNat !env !j !ctx                   =  continueTerm env (mkNat j) ctx 
  
  continueLam !_   !env' !n !t1 !t2 !(Local env ctx)  =  continueLam env env' n t1 t2 ctx
  continueLam !env !env' !n !t1 !t2 !(App1 ctx t3)    =  reduceBeta env env' n t1 t2 t3 ctx
  continueLam !env !env' !n !t1 !t2 !ctx              =  reduceLam env env' n t1 t2 ctx

  avoidCaptureLam !env !t1 !env' !n !ctx !t2 
    =  decompose (bindEnv n (mkVar n') emptyEnv env) (Lam2 env' n' t1 ctx) t2
    where n' = fresh n (namesEnv env)
       
  avoidCapturePi !env !t1 !n !ctx !t2 
    =  decompose (bindEnv n (mkVar n') emptyEnv env) (Pi2 env n' t1 ctx) t2
    where n' = fresh n (namesEnv env)
    
  reduceBeta !env !env' !n !t1 !t2 !t3 !ctx
    =  decompose env'' (Local env ctx) t2
    where env'' = bindEnv n t3 env env'
    
  reduceLam !env !env' !n !t1 !t2 !ctx
    =  decompose env' (Lam1 env n ctx t2) t1
  
  reduceVar !env !n !ctx 
    =  case lookupEnv n env of
         Just (t, env')  ->  decompose env' (Local env ctx) t
         Nothing         ->  continueTerm env (mkVar n) ctx
  
  reduceNatOp !env !n !f !i !j !ctx
    =  continueNat env (f i j) ctx
  
  reduceIfZero !env !i !t2 !t3 !ctx
    =  if i == 0
         then decompose env ctx t2
         else decompose env ctx t3
  
  {-
  continueNat env i (NatOp1 n f ctx t2) 
  continueNat env i (NatOp2 n f t1 ctx)
  continueNat env i (IfZero1 ctx t2 t3)
  continueNat env i (IfZero2 t1 ctx t3)
  continueNat env i (IfZero3 t1 t2 ctx)
  continueNat env i (App1 ctx t2)
  continueNat env i (App2 t1 ctx)
  continueNat env i (Lam1 n ctx t2)
  continueNat env i (Lam2 n t1 ctx)
  continueNat env i (Pi1 n ctx t2)
  continueNat env i (Pi2 n t1 ctx)
  -}

