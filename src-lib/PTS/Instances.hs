{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable, Rank2Types  #-}
module PTS.Instances where

import Control.Arrow ((***))
import PTS.Syntax

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- This type defines a specific pure type system, see Barendregt

data PTS = PTS
  { sortsSet :: Either (Set C) (C -> Bool)
  , axiomsMap :: Either (Map C C) (C -> Maybe C)
  , relationsMap :: Either (Map (C, C) C) (C -> C -> Maybe C)
  , name :: [String]
  , description :: String
  }

sorts :: PTS -> C -> Bool
sorts (PTS {sortsSet = Left sorts}) c = c `Set.member` sorts
sorts (PTS {sortsSet = Right f}) c = f c

axioms :: PTS -> C -> Maybe C
axioms (PTS {axiomsMap = Left axioms}) c = c `Map.lookup` axioms
axioms (PTS {axiomsMap = Right f}) c = f c

relations :: PTS -> C -> C -> Maybe C
relations (PTS {relationsMap = Left rels}) c1 c2 = (c1, c2) `Map.lookup` rels
relations (PTS {relationsMap = Right f}) c1 c2 = f c1 c2

-- | Approximates whether the first PTS is syntactically contained in the second one.
-- Warning: we do not formally know yet whether this has any semantic meaning.
-- We believe (and Paolo sketched a simple proof) this is a conservative
-- approximation of "semantic PTS containment", PTS1 <= PTS2, that is,
--
-- If |-_PTS1 t : T and PTS1 <= PTS2, then |-_PTS2 t : T
--
-- that is, if PTS1 <= PTS2 the typing judgement in PTS2 is a conservative
-- extension of the one in PTS1.
--
-- Alternatively, PTS1 can be be "collapsable" into PTS2, but the concept is
-- trickier, and an algorithm for that in general is less obvious. Moreover, if
-- PTS1 is collapsable into PTS2, then we only know that
--
-- If |-_PTS1 t : T, then |-_PTS1 collapse(t) : collapse(T).

isSubPTS :: PTS -> PTS -> Maybe Bool
isSubPTS (PTS (Left sortsSet) (Left axiomsMap) (Left relationsMap) _ _) pts2 =
  Just (subSorts && subAxioms && subRelations)
    where
      subSorts     = isSubRel pts1SortsGraph $ sorts pts2
      subAxioms    = isSubRel pts1AxiomsGraph $ axioms pts2
      subRelations = isSubRel pts1RelationsGraph $ \(s1, s2) -> relations pts2 s1 s2

      isSubRel rel1Graph rel2 = all (\(k, v) -> rel2 k == v) rel1Graph

      pts1SortsGraph     = map (, True)      $ Set.toList sortsSet
      pts1AxiomsGraph    = map (id *** Just) $ Map.toList axiomsMap
      pts1RelationsGraph = map (id *** Just) $ Map.toList relationsMap

-- Can't check subtyping if pts1 is partly specified by a function.
isSubPTS _ _ = Nothing

-- some specific pure type systems

-- By convention, equations for sorts, axioms, relations are sorted in lexicographic ascending order of constants.

simplytyped :: PTS
simplytyped = lama

mkSorts = Left . Set.fromDistinctAscList . map C
mkAxiom n c = ((C n), c)
mkRel n1 n2 c = (((C n1), (C n2)), c)

lama :: PTS
lama = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 ]
  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 ]
  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 ]

  name = [ "stlc"
         , "simply-typed-lambda-calculus"
         , "lama"
         , "lambda-arrow" ]

  description = "lambda-calculus with simple types"


f :: PTS
f = lam2

lam2 :: PTS
lam2 = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 2 1 star
                 ]

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
lamp = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 1 2 box
                 ]

  name = [ "dtlc"
         , "dependently-typed-lambda-calculus"
         , "lf"
         , "logical-framework"
         , "lamp"
         , "lambda-pi" ]

  description = "lambda-calculus with simple and dependent types"


-- "v" is weaker than "vv", ;)
lamv :: PTS
lamv = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 2 2 box
                 ]

  name = [ "hotlc"
         , "higher-order-typed-lambda-calculus"
         , "lamv"
         , "lambda-weak-omega" ]

  description = "lambda-calculus with simple and higher-order types"


lap2 :: PTS
lap2 = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 1 2 box
                 , mkRel 2 1 star
                 ]

  name = [ "lap2"
         , "lambda-pi-2" ]

  description = "lambda-calculus with simple, dependent and polymorphic types"


fomega :: PTS
fomega = lamw

-- "vv" is stronger than "v", ;)
lamw :: PTS
lamw = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 2 1 star
                 , mkRel 2 2 box
                 ]

  name = [ "fw"
         , "fomega"
         , "f-omega"
         , "system-f-omega"
         , "lamw"
         , "lambda-omega" ]

  description = "lambda-calculus with simple, polymorphic and higher-order types"


lapv :: PTS
lapv = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 1 2 box
                 , mkRel 2 2 box
                 ]

  name = [ "lapv"
         , "lambda-pi-weak-omega"]

  description = "lambda-calculus with simple, dependent and higher-order types"


coc :: PTS
coc = lamc

lamc :: PTS
lamc = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 1 2 box
                 , mkRel 2 1 star
                 , mkRel 2 2 box
                 ]

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
lams = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 star
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 ]

  name = [ "lam*"
         , "lambdastar"
         , "lambda-*"
         , "lambda-star"
         , "lams" ]

  description = "lambda-calculus with the axiom * : *"


fomegastar :: PTS
fomegastar = laws

laws :: PTS
laws = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 , mkAxiom 2 box
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star
                 , mkRel 2 1 star
                 , mkRel 2 2 box
                 ]

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
lawu = PTS (Right sorts) (Right axioms) (Right relations) name description where
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
u = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 , 3
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 , mkAxiom 2 triangle
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star

                 , mkRel 2 1 star
                 , mkRel 2 2 box

                 , mkRel 3 1 star
                 , mkRel 3 2 box

                 ]

  name = [ "systemu"
         , "system-u"
         , "u"]

  description = "system U"


uu :: PTS
uu = PTS sortsSet axiomsMap relationsMap name description where
  sortsSet     = mkSorts
                 [ 1
                 , 2
                 , 3
                 , 4
                 ]

  axiomsMap    = Left $ Map.fromList
                 [ mkAxiom 0 star
                 , mkAxiom 1 box
                 , mkAxiom 2 triangle
                 , mkAxiom 3 circle
                 ]

  relationsMap = Left $ Map.fromList
                 [ mkRel 1 1 star

                 , mkRel 2 1 star
                 , mkRel 2 2 box

                 , mkRel 3 1 star
                 , mkRel 3 2 box
                 , mkRel 3 3 triangle

                 , mkRel 4 1 star
                 , mkRel 4 2 box
                 , mkRel 4 3 triangle

                 ]

  name = [ "systemuu"
         , "system-uu"
         , "uu"]

  description = "system U squared"


instances :: [PTS]
instances = [lama, lam2, lamp, lamv, lap2, lamw, lapv, lamc, lams, laws, lawu, u, uu]
