module PTS.Syntax.Substitution.Tests where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool)

import PTS.Syntax
import PTS.Instances
import qualified PTS.Syntax.Substitution.Properties as Prop

import Test.Property (test)

x :: Name
x = readName "x"

y :: Name
y = readName "y"

z :: Name
z = readName "z"

substAvoidsCapture
  =  testGroup "substitution avoids capture"
     [  testProperty "random inputs" $
          Prop.substAvoidsCapture
     ,  testCase "(\\x -> y) [y |-> x]" $ assertBool "Variable captured." $ test $
          Prop.substAvoidsCapture (mkLam x (mkConst star) (mkVar y)) y (mkVar x)
     ,  testCase "(Pi x . y) [y |-> x]" $ assertBool "Variable captured." $ test $
          Prop.substAvoidsCapture (mkPi x (mkConst star) (mkVar y)) y (mkVar x)
     ]

tests
  =  testGroup "PTS.Substitution"
     [  substAvoidsCapture
     ]
