module Main where

import Test.HUnit ((@=?))
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import PTS.Substitution.Properties

main = defaultMain tests

tests =
  [  substAvoidsCapture
  ]
