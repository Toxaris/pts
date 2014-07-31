{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module PTS.Syntax.Term
  ( Term (..)
  , AnnotatedTerm
  , TermStructure (..)
  , Structure (structure)
  , structure'
  , annotation
  , MakeTerm (mkTerm)
  , mkInt
  , mkIntOp
  , mkIfZero
  , mkVar
  , mkConst
  , mkApp
  , mkLam
  , mkPi
  , mkSortedPi
  , mkPos
  , mkUnquote
  , mkInfer
  , freshvarl
  , handlePos
  , annotatedHandlePos
  , evalOp
  , BinOp (..)
  , desugarArgs
  ) where

import Control.Applicative ((<$>))

import Data.Data (Data)
import Data.Typeable (Typeable)

import PTS.Error
import PTS.Syntax.Constants (C(C))
import PTS.Syntax.Names (Name, Names, freshvarl)


-- Syntax

data Term = MkTerm (TermStructure Term)
  deriving (Data, Typeable, Show)

data AnnotatedTerm a = MkAnnotatedTerm (TermStructure (AnnotatedTerm a)) a
  deriving (Data, Typeable, Show)

class Structure term where
  structure :: term -> TermStructure term

instance Structure Term where
  structure (MkTerm t) = t

instance Structure (AnnotatedTerm a) where
  structure (MkAnnotatedTerm t _) = t

-- | Return the actual TermStructure of a Term-like thing without Pos nodes.
structure' :: Structure term => term -> TermStructure term
structure' t = case structure t of
  Pos _ t -> structure' t
  t -> t

annotation :: AnnotatedTerm a -> a
annotation (MkAnnotatedTerm _ a) = a

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Data, Typeable, Show)

returnLift2 :: (a -> b -> c) -> a -> b -> Maybe c
returnLift2 = ((Just .) .)

evalOp :: BinOp -> (Integer -> Integer -> Maybe Integer)
evalOp Add = returnLift2 (+)
evalOp Sub = returnLift2 (-)
evalOp Mul = returnLift2 (*)
evalOp Div = safeDiv
  where
    safeDiv x 0 = Nothing
    safeDiv x y = Just $ div x y

data TermStructure alpha
  = Int     Integer
  | IntOp   BinOp alpha alpha
  | IfZero  alpha alpha alpha
  | Var     Name
  | Const   C
  | App     alpha alpha
  | Lam     Name alpha alpha
  | Pi      Name alpha alpha (Maybe C)
  | Pos     Position alpha
  | Unquote alpha
  | Infer   Integer
  deriving (Functor, Data, Typeable, Show)

-- | Desugar a binder with multiple arguments like this:
--
-- > lambda (x1 : e1) (x2 x3 : e2) . e
--
-- to a series of nested single argument binders:
--
-- > lambda x1 : e1 . lambda x2 : e2 . lambda x3 : e2 . e
desugarArgs :: (Name -> Term -> Term -> Term) -> [([Name], Term)] -> Term -> Term
desugarArgs mk [] body = body
desugarArgs mk (([], _) : args) body = desugarArgs mk args body
desugarArgs mk (((n : ns), t) : args) body = mk n t (desugarArgs mk ((ns, t) : args) body) 

-- adapted from the Haskell wiki
-- http://www.haskell.org/haskellwiki/Top_level_mutable_state
-- {-# NOINLINE counter #-}
-- counter :: IORef Tag
-- counter = unsafePerformIO (newIORef 0)

class MakeTerm a b | a -> b, b -> a where
  mkTerm :: TermStructure a -> b

instance MakeTerm Term Term where
  mkTerm t = result where
    result = MkTerm t

instance MakeTerm (AnnotatedTerm a) (a -> AnnotatedTerm a) where
  mkTerm t q = result where
    result = MkAnnotatedTerm t q

-- mkTerm :: TermStructure -> Term
-- mkTerm t = unsafePerformIO $ do
--   old <- readIORef counter
--   writeIORef counter (succ old)
--   return (MkTerm old t)

-- smart constructors
mkInt i            =  mkTerm (Int i)
mkIntOp f t1 t2    =  mkTerm (IntOp f t1 t2)
mkIfZero t1 t2 t3  =  mkTerm (IfZero t1 t2 t3)
mkVar n            =  mkTerm (Var n)
mkConst c          =  mkTerm (Const c)
mkApp t1 t2        =  mkTerm (App t1 t2)
mkLam n t1 t2      =  mkTerm (Lam n t1 t2)
mkPi n t1 t2       =  mkTerm (Pi n t1 t2 Nothing)
mkSortedPi n t1 t2 s = mkTerm (Pi n t1 t2 s)
mkPos p t          =  mkTerm (Pos p t)
mkUnquote t        =  mkTerm (Unquote t)
mkInfer i          =  mkTerm (Infer i)

handlePos f p t = annotatePos p $ mkPos p <$> f t

annotatedHandlePos f p t = annotatePos p $ (\t -> MkAnnotatedTerm (Pos p t) (annotation t)) <$> f t
