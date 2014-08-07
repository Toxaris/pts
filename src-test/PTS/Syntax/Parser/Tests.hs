module PTS.Syntax.Parser.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual, assertFailure)

import PTS.Error
import PTS.Syntax

parse text = parseTerm "PTS.Syntax.Parser.Tests" text :: Either [PTSError] PTS.Syntax.Term

testParser :: String -> Term -> Test
testParser text term = testCase text $ case parse text of
  Right parsedTerm ->  assertEqual "Unexpected parse result." (showPretty term) (showPretty parsedTerm)
  Left error -> assertFailure $ "Unexpected parse error: " ++ show error

x :: Name
x = read "x"

y :: Name
y = read "y"

z :: Name
z = read "z"

e :: Name
e = read "e"

f :: Name
f = read "f"


tests
  =  testGroup "PTS.Parser"
     [  testParser "Int" (mkConst int)
     ,  testParser "*" (mkConst star)
     ,  testParser "**" (mkConst box)
     ,  testParser "x" (mkVar x)
     ,  testParser "y" (mkVar y)
     ,  testParser "z" (mkVar z)
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
     ,  testParser "Pi (x : y) (y : x) . y" (mkPi x (mkVar y) (mkPi y (mkVar x) (mkVar y)))
     ,  testParser "lambda (x : y) (y : x) . y" (mkLam x (mkVar y) (mkLam y (mkVar x) (mkVar y)))
     ,  testParser "lambda (x : e) (y z : f) . x" (mkLam x (mkVar e) (mkLam y (mkVar f) (mkLam z (mkVar f) (mkVar x))))
     ,  testParser "_" (mkInfer 0)
     ,  testParser "_1" (mkInfer 1)
     ,  testParser "lambda x : _ . x" (mkLam x (mkInfer 0) (mkVar x))
     ,  testParser "lambda x . x" (mkLam x (mkInfer 0) (mkVar x))
     ,  testParser "lambda x y : _ . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 0) (mkVar x)))
     ,  testParser "lambda (x y : _) . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 0) (mkVar x)))
     ,  testParser "lambda (x y) . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 0) (mkVar x)))
     ,  testParser "lambda x (y) . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 1) (mkVar x)))
     ,  testParser "lambda (x) y . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 1) (mkVar x)))
     ,  testParser "lambda x (y : _) . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 1) (mkVar x)))
     ,  testParser "lambda (x : _) y . x" (mkLam x (mkInfer 0) (mkLam y (mkInfer 1) (mkVar x)))
     ,  testParser "_ _" (mkApp (mkInfer 0) (mkInfer 1))
     ,  testParser "_1 _" (mkApp (mkInfer 1) (mkInfer 0))
     ,  testParser "_ _2 _ _ _3 _700 _" (mkApp (mkApp (mkApp (mkApp (mkApp (mkApp (mkInfer 0) (mkInfer 2)) (mkInfer 1)) (mkInfer 4)) (mkInfer 3)) (mkInfer 700)) (mkInfer 5))
     ,  testParser "_2 _2" (mkApp (mkInfer 2) (mkInfer 2))
   ]
