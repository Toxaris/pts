module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import Test.QuickCheck hiding ((.&.))

import PTS.Core.Tests
import PTS.Substitution.Tests
import PTS.Parser.Tests
import PTS.Pretty.Tests
import PTS.File.Tests (testFile)

main = defaultMain Main.tests

tests =
  [  PTS.Core.Tests.tests
  ,  PTS.Parser.Tests.tests
  ,  PTS.Pretty.Tests.tests
  ,  PTS.Substitution.Tests.tests
  ,  testFile True ["examples"] "Arithmetics.lpts"
  ,  testFile True ["examples"] "ChurchNumbers.lpts"
  ]
