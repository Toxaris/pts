module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import Test.QuickCheck hiding ((.&.))

import PTS.Core.Tests
import PTS.Syntax.Substitution.Tests
import PTS.Syntax.Parser.Tests
import PTS.Syntax.Pretty.Tests
import PTS.File.Tests (testFile)

main = defaultMain Main.tests

tests =
  [  PTS.Core.Tests.tests
  ,  PTS.Syntax.Parser.Tests.tests
  ,  PTS.Syntax.Pretty.Tests.tests
  ,  PTS.Syntax.Substitution.Tests.tests
  ,  testFile True ["examples"] "Arithmetics.lpts"
  ,  testFile True ["examples"] "ChurchNumbers.lpts"
  ,  testFile True ["examples"] "Functions.lpts"
  ,  testFile True ["examples"] "Inference.lpts"
  ]
