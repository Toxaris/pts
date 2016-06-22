module Main where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import Test.QuickCheck hiding ((.&.))

import PTS.Syntax.Substitution.Tests
import PTS.Syntax.Parser.Tests
import PTS.Syntax.Pretty.Tests
import PTS.File.Tests (testFile, testDir)

main = Main.tests >>= defaultMain

tests = do
  examples <- testDir "examples"
  return $
        [  PTS.Syntax.Parser.Tests.tests
        ,  PTS.Syntax.Pretty.Tests.tests
        ,  PTS.Syntax.Substitution.Tests.tests
        ] ++ examples
