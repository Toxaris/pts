module PTS.Parser.Tests where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual, assertFailure)

import PTS.AST
import PTS.Constants
import PTS.Parser
import PTS.Pretty

import Parametric.Error

parse text = parseTerm "PTS.Parser.Tests" text :: Either [FOmegaError] PTS.AST.Term

testParser text term = testCase text $ case parse text of
  Right parsedTerm ->  assertEqual "Unexpected parse result." (show term) (show parsedTerm)
  Left error -> assertFailure $ "Unexpected parse error: " ++ show error

x :: Name
x = read "x"

y :: Name
y = read "y"

z :: Name
z = read "z"

tests
  =  testGroup "PTS.Parser"
     [  testParser "Nat" (mkConst nat)
     ,  testParser "*" (mkConst star)
     ,  testParser "**" (mkConst box)
     ,  testParser "x" x
     ,  testParser "y" y
     ,  testParser "z" z
     ,  testParser "x y" (mkApp (mkVar x) (mkVar y))
     ,  testParser "x y z" (mkApp (mkApp (mkVar x) (mkVar y)) (mkVar z))
     ,  testParser "x (y z)" (mkApp (mkVar x) (mkApp (mkVar y) (mkVar z)))
     ,  testParser "x -> y" (mkPi x (mkVar x) (mkVar y))
     ,  testParser "x -> y -> z" (mkPi x (mkVar x) (mkPi y (mkVar y) (mkVar z)))
     ,  testParser "x y -> z" (mkPi x (mkApp (mkVar x) (mkVar y)) (mkVar z))
     ,  testParser "x (y -> z)" (mkApp (mkVar x) (mkPi y (mkVar y) (mkVar z)))
     ,  testParser "Pi x : x . x" (mkPi x (mkVar x) (mkVar x))
     ,  testParser "Pi x : y . x" (mkPi x (mkVar y) (mkVar x))
     ,  testParser "Pi x : y . Pi y : x . y" (mkPi x (mkVar y) (mkPi y (mkVar x) (mkVar y)))
     ,  testParser "x -> Pi x : y . x" (mkPi x (mkVar x) (mkPi x (mkVar y) (mkVar x)))
     ,  testParser "Pi x : y . x -> z" (mkPi x (mkVar y) (mkPi y (mkVar x) (mkVar z)))
     ,  testParser "(Pi x : y . x) -> z" (mkPi x (mkPi x (mkVar y) (mkVar x)) (mkVar z))
     ,  testParser "lambda x : x . x" (mkLam x (mkVar x) (mkVar x))
     ,  testParser "lambda x : y . x" (mkLam x (mkVar y) (mkVar x))
     ,  testParser "lambda x : y . lambda y : x . y" (mkLam x (mkVar y) (mkLam y (mkVar x) (mkVar y)))
     ]
