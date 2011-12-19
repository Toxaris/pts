module Main where

import Test.HUnit ((@=?))
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

main = defaultMain tests

tests =
  [  {- testCase "foo" (1 @=? 1) -} ]
