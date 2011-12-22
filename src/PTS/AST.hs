{-# LANGUAGE NoMonomorphismRestriction #-}
module PTS.AST
  ( Name
  , Term ()
  , TermStructure (..)
  , structure
  , Stmt (..)
  , mkNat   
  , mkNatOp 
  , mkIfZero
  , mkVar   
  , mkConst 
  , mkApp   
  , mkLam   
  , mkPi    
  , mkPos   
  -- , freevars2constants
  , freevars
  , freshvar
  , freshvarl
  , handlePos
  , allvars
  , C ()
  ) where

import Control.Applicative hiding (Const)
import Control.Monad.Reader

import Data.List (nub, (\\), intersperse)

import Data.Set (Set)
import qualified Data.Set as Set

import Parametric.Error
import Parametric.AST (Name, freshvarl)

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import PTS.Instances (C)

-- Syntax

-- the string in NatOp is an identifier for the function. It is necessary
-- to check equivalence of terms (the functions cannot be directly compared)

type Names = Set Name

-- type Tag = Int

data Term = MkTerm Names TermStructure

structure :: Term -> TermStructure
structure (MkTerm _ t) = t

data TermStructure
  = Nat     Int
  | NatOp   Name (Int -> Int -> Int) Term Term
  | IfZero  Term Term Term
  | Var     Name
  | Const   C
  | App     Term Term
  | Lam     Name Term Term
  | Pi      Name Term Term
  | Pos     Position Term

data Stmt 
  = Bind Name (Maybe Term) Term
  | Term Term
  | StmtPos Position Stmt

-- adapted from the Haskell wiki
-- http://www.haskell.org/haskellwiki/Top_level_mutable_state
-- {-# NOINLINE counter #-}
-- counter :: IORef Tag
-- counter = unsafePerformIO (newIORef 0)

mkTerm :: TermStructure -> Term
mkTerm t = result where
  result = MkTerm (mkFreevars result) t

-- mkTerm :: TermStructure -> Term
-- mkTerm t = unsafePerformIO $ do
--   old <- readIORef counter
--   writeIORef counter (succ old)
--   return (MkTerm old t)

-- smart constructors
mkNat i            =  mkTerm (Nat i)
mkNatOp n f t1 t2  =  mkTerm (NatOp n f t1 t2) 
mkIfZero t1 t2 t3  =  mkTerm (IfZero t1 t2 t3) 
mkVar n            =  mkTerm (Var n) 
mkConst c          =  mkTerm (Const c)
mkApp t1 t2        =  mkTerm (App t1 t2) 
mkLam n t1 t2      =  mkTerm (Lam n t1 t2) 
mkPi n t1 t2       =  mkTerm (Pi n t1 t2) 
mkPos p t          =  mkTerm (Pos p t) 

handlePos f p t = annotatePos p $ mkPos p <$> f t

infixl 2 >>>
(>>>) = flip (.)

freevars (MkTerm fv _) = fv

mkFreevars :: Term -> Set Name
mkFreevars t = case structure t of
  Var x            ->  Set.singleton x
  App t1 t2        ->  freevars t1 `Set.union` freevars t2
  NatOp _ _ t1 t2  ->  freevars t1 `Set.union` freevars t2
  IfZero t1 t2 t3  ->  Set.unions [freevars t1, freevars t2, freevars t3]
  Lam x t1 t2      ->  freevars t1 `Set.union` (Set.delete x (freevars t2))
  Pi x t1 t2       ->  freevars t1 `Set.union` (Set.delete x (freevars t2))
  Pos p t          ->  freevars t
  _                ->  Set.empty

freshvar :: Term -> Name -> Name
freshvar t x = freshvarl (freevars t) x

allvars :: Term -> [Name]
allvars t = case structure t of
  Var x            ->  [x]
  App t1 t2        ->  nub $ allvars t1 ++ allvars t2
  NatOp _ _ t1 t2  ->  nub $ allvars t1 ++ allvars t2
  IfZero t1 t2 t3  ->  nub $ allvars t1 ++ allvars t2  ++ allvars t3
  Lam x t1 t2      ->  nub $ allvars t1 ++ allvars t2 ++ [x]
  Pi x t1 t2       ->  nub $ allvars t1 ++ allvars t2 ++ [x]
  Pos p t          ->  allvars t
  _                ->  []
