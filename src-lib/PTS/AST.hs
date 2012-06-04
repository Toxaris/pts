{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, DeriveDataTypeable #-}
module PTS.AST
  ( Name
  , Names
  , Term (..)
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
  , mkUnquote
  , freshvarl
  , handlePos
  , C ()
  , evalOp
  , BinOp (..)
  ) where

import Control.Applicative hiding (Const)
import Control.Monad.Reader

import Data.Typeable
import Data.Data

import Data.IORef
import Data.List (nub, (\\), intersperse)
import Data.Set (Set)
import qualified Data.Set as Set

import System.IO.Unsafe (unsafePerformIO)

import Parametric.AST (Name, Names, freshvarl)
import Parametric.Error
import PTS.Instances (C)


-- Syntax

-- the string in NatOp is an identifier for the function. It is necessary
-- to check equivalence of terms (the functions cannot be directly compared)

-- type Tag = Int

data Term = MkTerm (TermStructure Term)
  deriving (Data, Typeable)

structure :: Term -> TermStructure Term
structure (MkTerm t) = t

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Data, Typeable)

evalOp :: BinOp -> (Integer -> Integer -> Integer)
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = div

data TermStructure alpha
  = Nat     Integer
  | NatOp   Name BinOp alpha alpha
  | IfZero  alpha alpha alpha
  | Var     Name
  | Const   C
  | App     alpha alpha
  | Lam     Name alpha alpha
  | Pi      Name alpha alpha
  | Pos     Position alpha
  | Unquote alpha
  deriving (Functor, Data, Typeable)

data Stmt
  = Bind Name (Maybe Term) Term
  | Term Term
  | StmtPos Position Stmt

-- adapted from the Haskell wiki
-- http://www.haskell.org/haskellwiki/Top_level_mutable_state
-- {-# NOINLINE counter #-}
-- counter :: IORef Tag
-- counter = unsafePerformIO (newIORef 0)

mkTerm :: TermStructure Term -> Term
mkTerm t = result where
  result = MkTerm t

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
mkUnquote t        =  mkTerm (Unquote t)

handlePos f p t = annotatePos p $ mkPos p <$> f t

infixl 2 >>>
(>>>) = flip (.)
