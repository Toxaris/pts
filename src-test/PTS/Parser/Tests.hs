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

tests
  =  testGroup "PTS.Parser"
     [  testParser "Nat" (mkConst nat) ]
