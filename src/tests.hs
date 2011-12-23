module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import Test.QuickCheck hiding ((.&.))

import PTS.Core.Tests
import PTS.Substitution.Tests

main = defaultMain Main.tests

tests =
  [  PTS.Substitution.Tests.tests
  ,  PTS.Core.Tests.tests
  ]
