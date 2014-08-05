{-# LANGUAGE DeriveDataTypeable, Rank2Types  #-}
module PTS.Instances where

import PTS.Syntax
import PTS.Dynamics.Value

-- This type defines a specific pure type system, see Barendregt

data PTS = PTS
  { sorts :: C -> Bool
  , axioms :: C -> Maybe C
  , relations :: C -> C -> Maybe C
  , name :: [String]
  , description :: String
  }

-- some specific pure type systems

simplytyped :: PTS
simplytyped = lama

lama :: PTS
lama = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C _) (C _) = Nothing

  name = [ "stlc"
         , "simply-typed-lambda-calculus"
         , "lama"
         , "lambda-arrow" ]

  description = "lambda-calculus with simple types"


f :: PTS
f = lam2

lam2 :: PTS
lam2 = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just star
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


lamp :: PTS
lamp = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Just box
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


-- "v" is weaker than "vv", ;)
lamv :: PTS
lamv = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Just box
  relations (C _) (C _) = Nothing

  name = [ "hotlc"
         , "higher-order-typed-lambda-calculus"
         , "lamv"
         , "lambda-weak-omega" ]

  description = "lambda-calculus with simple and higher-order types"


lap2 :: PTS
lap2 = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Just box
  relations (C 2) (C 1) = Just star
  relations (C 2) (C 2) = Nothing
  relations (C _) (C _) = Nothing

  name = [ "lap2"
         , "lambda-pi-2" ]

  description = "lambda-calculus with simple, dependent and polymorphic types"


fomega :: PTS
fomega = lamw

-- "vv" is stronger than "v", ;)
lamw :: PTS
lamw = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just star
  relations (C 2) (C 2) = Just box
  relations (C _) (C _) = Nothing

  name = [ "fw"
         , "fomega"
         , "f-omega"
         , "system-f-omega"
         , "lamw"
         , "lambda-omega" ]

  description = "lambda-calculus with simple, polymorphic and higher-order types"


lapv :: PTS
lapv = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Just box
  relations (C 2) (C 1) = Nothing
  relations (C 2) (C 2) = Just box
  relations (C _) (C _) = Nothing

  name = [ "lapv"
         , "lambda-pi-weak-omega"]

  description = "lambda-calculus with simple, dependent and higher-order types"


coc :: PTS
coc = lamc

lamc :: PTS
lamc = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Just box
  relations (C 2) (C 1) = Just star
  relations (C 2) (C 2) = Just box
  relations (C _) (C _) = Nothing

  name = [ "coc"
         , "calculus-of-construction"
         , "lamc"
         , "lambda-c"
         , "lapw"
         , "lambda-pi-omega" ]

  description = "lambda-calculus with simple, dependent, polymorphic and higher-order types"


lambdastar :: PTS
lambdastar = lams

lams :: PTS
lams = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just star
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C _) (C _) = Nothing

  name = [ "lam*"
         , "lambdastar"
         , "lambda-*"
         , "lambda-star"
         , "lams" ]

  description = "lambda-calculus with the axiom * : *"


fomegastar :: PTS
fomegastar = laws

laws :: PTS
laws = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C 2) = Just box
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just star
  relations (C 2) (C 2) = Just box
  relations (C _) (C _) = Nothing

  name = [ "fw*"
         , "fomega*"
         , "fomegastar"
         , "f-omega-star"
         , "law*"
         , "laws"
         , "lambda-omega-star" ]

  description = "fomega with the axiom ** : **"


fomegaomega :: PTS
fomegaomega = lawu

lawu :: PTS
lawu = PTS sorts axioms relations name description where
  sorts (C 0) = False
  sorts (C n) = True

  axioms (C n) = Just (C (succ n))

  relations (C 0) (C _) = Nothing
  relations (C _) (C 0) = Nothing
  relations (C a) (C b) = if a >= b then Just (C b) else Nothing

  name = [ "fww"
         , "fomegaomega"
         , "law^"
         , "lawu"
         , "lambda-omega-universe" ]

  description = "fomega with an infinite hierarchy of universes"

u :: PTS
u = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C 3) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C 2) = Just triangle
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 2) (C 1) = Just star
  relations (C 2) (C 2) = Just box
  relations (C 3) (C 2) = Just box
  relations (C 3) (C 1) = Just star
  relations (C _) (C _) = Nothing

  name = [ "systemu"
         , "system-u"
         , "u"]

  description = "system U"


uu :: PTS
uu = PTS sorts axioms relations name description where
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C 3) = True
  sorts (C 4) = True
  sorts (C _) = False

  axioms (C 0) = Just star
  axioms (C 1) = Just box
  axioms (C 2) = Just triangle
  axioms (C 3) = Just circle
  axioms (C _) = Nothing

  relations (C 1) (C 1) = Just star
  relations (C 2) (C 1) = Just star
  relations (C 3) (C 1) = Just star
  relations (C 4) (C 1) = Just star

  relations (C 2) (C 2) = Just box
  relations (C 3) (C 2) = Just box
  relations (C 3) (C 3) = Just triangle
  relations (C 4) (C 3) = Just triangle
  relations (C 4) (C 2) = Just box

  relations (C _) (C _) = Nothing

  name = [ "systemuu"
         , "system-uu"
         , "uu"]

  description = "system U squared"


instances :: [PTS]
instances = [lama, lam2, lamp, lamv, lap2, lamw, lapv, lamc, lams, laws, lawu, u, uu]
