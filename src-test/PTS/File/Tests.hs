module PTS.File.Tests
  ( testFile
  , testFileWithOptions
  ) where

import Control.Monad.Assertions (collectAssertions)

import PTS.Error (showErrors)
import PTS.Process (processFile, runProcessFile)
import PTS.Options (Options, defaultOptions, optPath, optLiterate, optQuiet)

import System.Directory (findFile)

import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)

testFileWithOptions :: Options -> FilePath -> Test
testFileWithOptions opt file = buildTest $ do
  let path = optPath opt
  file <- findFile path file >>= maybe (fail ("file not found: " ++ file)) return
  result <- runProcessFile (collectAssertions (processFile file)) ([], []) opt
  case result of
    Left e -> fail (showErrors e)
    Right (_, assertions) -> return $ testGroup file $
      [testCase name (convert result) | (name, result) <- assertions]

convert Nothing = return ()
convert (Just e) = assertFailure (showErrors e)

testFile literate path
  = testFileWithOptions (defaultOptions
      { optQuiet = True
      , optLiterate = literate
      , optPath = path})
