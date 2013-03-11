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
  { sorts :: C -> Bool
  , axioms :: C -> Maybe TypedTerm
  , relations :: C -> C -> Maybe TypedTerm
  , name :: [String]
  , description :: String
  }

notExpressible = MkTypedTerm (Const (C (negate 1))) notExpressible

-- some specific pure type systems

simplytyped :: PTS
simplytyped = lama

lama :: PTS
lama = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C _) (C _) = Nothing

  name = [ "stlc"
         , "simply-typed-lambda-calculus"
         , "lama"
         , "lambda-arrow" ]

  description = "lambda-calculus with simple types"

  typedStar = MkTypedTerm (Const star) notExpressible


f :: PTS
f = lam2

lam2 :: PTS
lam2 = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  name = [ "f"
         , "system-f"
         , "plc"
         , "polymorphic-lambda-calculus"
         , "solc"
         , "second-order-lambda-calculus"
         , "lam2"
         , "lambda-2" ]

  description = "lambda-calculus with simple and polymorphic types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


lamp :: PTS
lamp = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  name = [ "dtlc"
         , "dependently-typed-lambda-calculus"
         , "lf"
         , "logical-framework"
         , "lamp"
         , "lambda-pi" ]

  description = "lambda-calculus with simple and dependent types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


-- "v" is weaker than "vv", ;)
lamv :: PTS
lamv = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  name = [ "hotlc"
         , "higher-order-typed-lambda-calculus"
         , "lamv"
         , "lambda-weak-omega" ]

  description = "lambda-calculus with simple and higher-order types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


lap2 :: PTS
lap2 = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  name = [ "lap2"
         , "lambda-pi-2" ]

  description = "lambda-calculus with simple, dependent and polymorphic types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


fomega :: PTS
fomega = lamw

-- "vv" is stronger than "v", ;)
lamw :: PTS
lamw = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  name = [ "fw"
         , "f-omega"
         , "system-f-omega"
         , "lamw"
         , "lambda-omega" ]

  description = "lambda-calculus with simple, polymorphic and higher-order types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


lapv :: PTS
lapv = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  name = [ "lapv"
         , "lambda-pi-weak-omega"]

  description = "lambda-calculus with simple, dependent and higher-order types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


coc :: PTS
coc = lamc

lamc :: PTS
lamc = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedStar
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Just typedBox
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  name = [ "coc"
         , "calculus-of-construction"
         , "lamc"
         , "lambda-c"
         , "lapw"
         , "lambda-pi-omega" ]

  description = "lambda-calculus with simple, dependent, polymorphic and higher-order types"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) notExpressible


lambdastar :: PTS
lambdastar = lams

lams :: PTS
lams = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedStar
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C _) (C _) = Nothing

  name = [ "lam*"
         , "lambdastar"
         , "lambda-*"
         , "lambda-star"
         , "lams" ]

  description = "lambda-calculus with the axiom * : *"

  typedStar = MkTypedTerm (Const star) typedStar


fomegastar :: PTS
fomegastar = laws

laws :: PTS
laws = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just typedStar
  axioms (C 1) = Just typedBox
  axioms (C 2) = Just typedBox
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just typedStar
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just typedStar
  relations (C 2) (C 2) = Just typedBox
  relations (C _) (C _) = Nothing

  name = [ "fw*"
         , "fomega*"
         , "fomegastar"
         , "f-omega-star"
         , "law*"
         , "laws"
         , "lambda-omega-star" ]

  description = "fomega with the axiom ** : **"

  typedStar = MkTypedTerm (Const star) typedBox
  typedBox  = MkTypedTerm (Const box) typedBox


fomegaomega :: PTS
fomegaomega = lawu

lawu :: PTS
lawu = PTS sorts axioms relations name description where
  sorts (C 0) = False
  sorts (C n) = True

  axioms (C n) = Just (typed (succ n))

  relations (C 0) (C _) = Nothing
  relations (C _) (C 0) = Nothing
  relations (C a) (C b) = if a >= b then Just (typed b) else Nothing

  name = [ "fww"
         , "fomegaomega"
         , "law^"
         , "lawu"
         , "lambda-omega-universe" ]

  description = "fomega with an infinite hierarchy of universes"

  typed n = MkTypedTerm (Const (C n)) (typed (succ n))

instances :: [PTS]
instances = [lama, lam2, lamp, lamv, lap2, lapv, lamc, lams, laws, lawu]
