module PTS.Core.Properties where

import Data.Set

import Test.Property

import PTS.Algebra
import PTS.AST
import PTS.AST.Arbitrary
import PTS.Core

type Relation a = a -> a -> Bool

reflexive :: (Eq a, Arbitrary a, Show a) => Relation a -> Property (a -> Bool)
reflexive (~~) = property $
  \x -> x ~~ x

symmetric :: (Eq a, Arbitrary a, Show a) => Relation a -> Property (a -> a -> Bool)
symmetric (~~) = property $
  \x y -> x ~~ y ==> y ~~ x

transitive :: (Eq a, Arbitrary a, Show a) => Relation a -> Property (a -> a -> a -> Bool)
transitive (~~) = property $
  \x y z -> x ~~ y && y ~~ z ==> x ~~ z

alphaEquivalenceReflexive :: Property (Term -> Bool)
alphaEquivalenceReflexive = reflexive (==)

alphaEquivalenceSymmetric :: Property (Term -> Term -> Bool)
alphaEquivalenceSymmetric = symmetric (==)

alphaEquivalenceTransitive :: Property (Term -> Term -> Term -> Bool)
alphaEquivalenceTransitive = transitive (==)

alphaEquivalenceShareFreevars :: Property (Term -> Term -> Bool)
alphaEquivalenceShareFreevars = property $
  \a b -> a == b ==> freevars a == freevars b

ndotsLength :: Property (Int -> Bool)
ndotsLength = property $
  \n -> n >= 0 && n < 200 ==> length (ndots n) == n

ndotsContainsDots :: Property (Int -> Bool)
ndotsContainsDots = property $
  \n -> n >= 0 && n < 200 ==> all ('.' ==) (ndots n)
