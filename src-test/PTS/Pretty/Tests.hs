module PTS.Pretty.Tests where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)

import PTS.AST
import PTS.Constants
import PTS.Instances
import PTS.Pretty

import Test.Property (test)

x :: Name
x = read "x"

y :: Name
y = read "y"

z :: Name
z = read "z"

testPretty text term
  =  testCase text $
       assertEqual "Unexpected pretty print." text (show term)

tests
  =  testGroup "PTS.Pretty"
     [  testPretty "Nat" (mkConst nat)
     ,  testPretty "*" (mkConst star)
     ,  testPretty "**" (mkConst box)
     ,  testPretty "x" x
     ,  testPretty "y" y
     ,  testPretty "z" z
     ,  testPretty "x y" (mkApp (mkVar x) (mkVar y))
     ,  testPretty "x y z" (mkApp (mkApp (mkVar x) (mkVar y)) (mkVar z))
     ,  testPretty "x (y z)" (mkApp (mkVar x) (mkApp (mkVar y) (mkVar z)))
     ,  testPretty "x -> y" (mkPi x (mkVar x) (mkVar y))
     ,  testPretty "x -> y -> z" (mkPi x (mkVar x) (mkPi y (mkVar y) (mkVar z)))
     ,  testPretty "x y -> z" (mkPi x (mkApp (mkVar x) (mkVar y)) (mkVar z))
     ,  testPretty "x (y -> z)" (mkApp (mkVar x) (mkPi y (mkVar y) (mkVar z)))
     ,  testPretty "Pi x : x . x" (mkPi x (mkVar x) (mkVar x))
     ,  testPretty "Pi x : y . x" (mkPi x (mkVar y) (mkVar x))
     ,  testPretty "Pi x : y . Pi y : x . y" (mkPi x (mkVar y) (mkPi y (mkVar x) (mkVar y)))
     ,  testPretty "x -> Pi x : y . x" (mkPi x (mkVar x) (mkPi x (mkVar y) (mkVar x)))
     ,  testPretty "Pi x : y . x -> z" (mkPi x (mkVar y) (mkPi y (mkVar x) (mkVar z)))
     ,  testPretty "(Pi x : y . x) -> z" (mkPi x (mkPi x (mkVar y) (mkVar x)) (mkVar z))
     ,  testPretty "lambda x : x . x" (mkLam x (mkVar x) (mkVar x))
     ,  testPretty "lambda x : y . x" (mkLam x (mkVar y) (mkVar x))
     ,  testPretty "lambda x : y . lambda y : x . y" (mkLam x (mkVar y) (mkLam y (mkVar x) (mkVar y)))
     ]
