{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Instances where

import Control.Monad

import Data.Typeable
import Data.Data

import Data.Char
import Data.List

import Parametric.Pretty



-- This type defines a specific pure type system, see Barendregt
-- 
-- TODO: should be a record!
--
-- axioms, sorts, relations, nat, constants parser

data PTS = PTS 
  {  axioms :: C -> Maybe C
  ,  sorts :: C -> Bool
  ,  relations :: C -> C -> Maybe C
  }

-- constants
newtype C = C Int
  deriving (Eq, Data, Typeable)

nat :: C
nat = C 0

star :: C
star = C 1

box :: C
box = C 2
  
-- some specific pure type systems

-- simply typed lambda calculus
simplytyped :: PTS
simplytyped = PTS axioms sorts relations where 
  axioms (C 0) = Just (C 1)
  axioms (C _) = Nothing
  
  sorts (C 1) = True
  sorts (C _) = False
  
  relations (C 1) (C 1) = Just (C 1)
  relations (C _) (C _) = Nothing
  
fomega :: PTS
fomega = PTS axioms sorts relations where 
  axioms (C 0) = Just (C 1)
  axioms (C 1) = Just (C 2)
  axioms (C _) = Nothing
  
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False
  
  relations (C 1) (C 1) = Just (C 1)
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just (C 1)
  relations (C 2) (C 2) = Just (C 2)
  relations (C _) (C _) = Nothing
    
-- calculus of construction
coc :: PTS
coc = PTS axioms sorts relations where 
  axioms (C 0) = Just (C 1)
  axioms (C 1) = Just (C 2)
  axioms (C _) = Nothing
  
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False
  
  relations (C 1) (C 1) = Just (C 1)
  relations (C 1) (C 2) = Just (C 2)
  relations (C 2) (C 1) = Just (C 1)
  relations (C 2) (C 2) = Just (C 2)
  relations (C _) (C _) = Nothing
    
-- lambda* with * : *
lambdastar :: PTS
lambdastar = PTS axioms sorts relations where 
  axioms (C 0) = Just (C 1)
  axioms (C 1) = Just (C 1)
  axioms (C _) = Nothing
  
  sorts (C 1) = True
  sorts (C _) = False
  
  relations (C 1) (C 1) = Just (C 1)
  relations (C _) (C _) = Nothing
  
-- fomega* with ** : **
fomegastar :: PTS
fomegastar = PTS axioms sorts relations where 
  axioms (C 0) = Just (C 1)
  axioms (C 1) = Just (C 2)
  axioms (C 2) = Just (C 2)
  axioms (C _) = Nothing
  
  sorts (C 1) = True
  sorts (C 2) = True
  sorts (C _) = False
  
  relations (C 1) (C 1) = Just (C 1)
  relations (C 1) (C 2) = Nothing
  relations (C 2) (C 1) = Just (C 1)
  relations (C 2) (C 2) = Just (C 2)
  relations (C _) (C _) = Nothing
      
-- FOmega with an infinite hierarchy of universes
-- We call this language FOmegaOmega
fomegaomega :: PTS
fomegaomega = PTS axioms sorts relations where 
  axioms (C n) = Just (C (succ n))
  
  sorts (C 0) = False
  sorts (C n) = True
  
  relations (C 0) (C _) = Nothing
  relations (C _) (C 0) = Nothing
  relations (C a) (C b) = if a >= b then Just (C b) else Nothing
