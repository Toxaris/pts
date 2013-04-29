module PTS.Core.Properties where

import Data.Set

import Test.Property

import PTS.Syntax
import PTS.Syntax.Arbitrary
import PTS.Statics
import PTS.Dynamics

type Relation a = a -> a -> Bool

reflexive :: (Arbitrary a, Show a) => Relation a -> Property (a -> Bool)
reflexive (~~) = property $
  \x -> x ~~ x

symmetric :: (Arbitrary a, Show a) => Relation a -> Property (a -> a -> Bool)
symmetric (~~) = property $
  \x y -> x ~~ y ==> y ~~ x

transitive :: (Arbitrary a, Show a) => Relation a -> Property (a -> a -> a -> Bool)
transitive (~~) = property $
  \x y z -> x ~~ y && y ~~ z ==> x ~~ z

equivClosedTerm = equivTerm []

alphaEquivalenceReflexive :: Property (Term -> Bool)
alphaEquivalenceReflexive = reflexive equivClosedTerm

alphaEquivalenceSymmetric :: Property (Term -> Term -> Bool)
alphaEquivalenceSymmetric = symmetric equivClosedTerm

alphaEquivalenceTransitive :: Property (Term -> Term -> Term -> Bool)
alphaEquivalenceTransitive = transitive equivClosedTerm
