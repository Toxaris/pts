module PTS.Core.Tests where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool)

import PTS.AST
import PTS.Instances
import qualified PTS.Core.Properties as Prop

import Test.Property (test)

x :: Name
x = read "x"

y :: Name
y = read "y"

z :: Name
z = read "z"

alphaEquivalenceReflexive
  =  testGroup "alpha equivalence is reflexive"
     [  testProperty "random inputs" $
          Prop.alphaEquivalenceReflexive
     ]

alphaEquivalenceSymmetric
  =  testGroup "alpha equivalence is symmetric"
     [  testProperty "random inputs" $
          Prop.alphaEquivalenceSymmetric
     ]

alphaEquivalenceTransitive
  =  testGroup "alpha equivalence is transitive"
     [  testProperty "random inputs" $
          Prop.alphaEquivalenceTransitive
     ]

alphaEquivalenceShareFreevars
  =  testGroup "alpha equivalent terms share the same free variables"
     [  testProperty "random inputs" $
          Prop.alphaEquivalenceShareFreevars
     ]

ndotsLength
  =  testGroup "n dots are n characters long"
     [  testProperty "random inputs" $
          Prop.ndotsLength
     ]

tests
  =  testGroup "PTS.Core"
     [  alphaEquivalenceReflexive
     ,  alphaEquivalenceSymmetric
     ,  alphaEquivalenceTransitive
     ,  alphaEquivalenceShareFreevars
     ,  ndotsLength
     ]
