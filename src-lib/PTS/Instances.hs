{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Instances where

import Control.Monad

import Data.Typeable
import Data.Data

import Data.Char
import Data.List

import Parametric.Pretty

import PTS.AST
import PTS.Constants

-- This type defines a specific pure type system, see Barendregt
--
-- TODO: should be a record!
--
-- axioms, sorts, relations, nat, constants parser

data PTS = PTS
  {  axioms :: C -> Maybe TypedTerm
  ,  sorts :: C -> Bool
  ,  relations :: C -> C -> Maybe TypedTerm
  }

notExpressible = MkTypedTerm (Const (C (negate 1))) notExpressible

-- some specific pure type systems

-- simply typed lambda calculus
simplytyped :: PTS
simplytyped = PTS axioms sorts relations where
  axioms (C 0) = Just (typedStar)
  axioms (C _) = Nothing

  sorts (C 1) = True
  sorts (C _) = False

  relations (C 1) (C 1) = Just typedStar
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) notExpressible


f :: PTS
f = PTS axioms sorts relations where
  axioms (C 0) = Just (typedStar)
  axioms (C 1) = Just (typedBox)
  axioms (C _) = Nothing

  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  relations (C 1) (C 1) = Just (typedStar)
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just (typedStar)
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


lambdaPi :: PTS
lambdaPi = PTS axioms sorts relations where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just (typedStar)
  axioms (C 1) = Just (typedBox)
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


-- lower omega
lomega :: PTS
lomega = PTS axioms sorts relations where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just (typedStar)
  axioms (C 1) = Just (typedBox)
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


fpi :: PTS
fpi = PTS axioms sorts relations where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just (typedStar)
  axioms (C 1) = Just (typedBox)
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


fomega :: PTS
fomega = PTS axioms sorts relations where
  axioms (C 0) = Just (typedStar)
  axioms (C 1) = Just (typedBox)
  axioms (C _) = Nothing

  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  relations (C 1) (C 1) = Just (typedStar)
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just (typedStar)
  relations (C 2) (C 2) = Just (typedBox)
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


pilomega :: PTS
pilomega = PTS axioms sorts relations where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just (typedStar)
  axioms (C 1) = Just (typedBox)
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


-- calculus of construction
coc :: PTS
coc = PTS axioms sorts relations where
  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedStar
  axioms (C _) = Nothing

  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible

-- lambda* with * : *
lambdastar :: PTS
lambdastar = PTS axioms sorts relations where
  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedStar
  axioms (C _) = Nothing

  sorts (C 1) = True
  sorts (C _) = False

  relations (C 1) (C 1) = Just typedStar
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedStar

-- fomega* with ** : **
fomegastar :: PTS
fomegastar = PTS axioms sorts relations where
  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C 2) = Just typedBox
  axioms (C _) = Nothing

  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) typedBox

-- FOmega with an infinite hierarchy of universes
-- We call this language FOmegaOmega
fomegaomega :: PTS
fomegaomega = PTS axioms sorts relations where
  axioms (C n) = Just (typed (succ n))

  sorts (C 0) = False
  sorts (C n) = True

  relations (C 0) (C _) = Nothing
  relations (C _) (C 0) = Nothing
  relations (C a) (C b) = if a >= b then Just (typed b) else Nothing

  typed n = MkTypedTerm (Const (C n)) (typed (succ n))
