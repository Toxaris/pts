{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, KindSignatures, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, RankNTypes #-}
module Parametric.AST where

import Control.Applicative

import Control.Monad.Reader

import Data.List
import Data.Maybe

import Parametric.Error

import Tools.Errors.Class

    -- PARAMETRIC ABSTRACT SYNTAX --

data Context t n (x :: * -> *) v = Context (Maybe n) (t v)
  deriving Show

data Var (x :: * -> *) v = Var v
  deriving Show

data Pos p t v = Pos p (t v)
  deriving Show

type Name = String

    -- CONVERSION from NAME-BASED to INDEXED TERMS --

class Index t where
  index :: (MonadErrors Parametric.Error.Errors m, MonadReader [Name] m, Applicative m) => t Name -> m (t Int)

instance Index t => Index (Context t Name x) where
  index (Context (Just n) t) = Context (Just n) <$> local (n :) (index t)
  index (Context Nothing t) = fail "cannot convert unnamed abstraction into de Bruijn form"

instance Index (Var x) where
  index (Var v) = Var <$> (recover (negate 1) $ asks (findIndex (== v)) >>= maybe (fail ("unbound variable: " ++ show v)) pure)

instance Index t => Index (Pos Position t) where
  index (Pos p t) = annotatePos p $ Pos p <$> index t

    -- SHIFTING in INDEXED TERMS --

class Shift t where
  shift :: Int -> Int -> t Int -> t Int

instance Shift t => Shift (Context t Name x) where
  shift d c (Context n t) = Context n $ shift d (succ c) t

instance Shift (Var x) where
  shift d c (Var v) | v < c = Var v | otherwise = Var (v + d)

instance Shift t => Shift (Pos Position t) where
  shift d c (Pos p t) = Pos p $ shift d c t
