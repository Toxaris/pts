module PTS.File.Tests
  ( testFile
  , testFileWithOptions
  , testDir
  ) where

import Control.Monad.Assertions (collectAssertions)
import Control.Monad.Errors

import qualified Data.Map as Map

import PTS.Error (showErrors)
import PTS.Process.File (processFile)
import PTS.Process.Main (runOptMonads, withEmptyState)
import PTS.Options (Options, defaultOptions, optPath, optLiterate, optQuiet)

import System.Directory (findFile, getDirectoryContents)
import System.FilePath (takeExtension)

import Test.Framework (Test, testGroup, buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)


testFileWithOptions :: Options -> FilePath -> Test
testFileWithOptions opt file = buildTest $ do
  let path = optPath opt
  file <- findFile path file >>= maybe (fail ("file not found: " ++ file)) return
  result <- runErrorsT . withEmptyState . runOptMonads opt . collectAssertions $ processFile file
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

testDir :: FilePath -> IO [Test]
testDir path = do
  files <- getDirectoryContents path
  return $
           [ testFile literate [path] validFile |
             file <- files,
             (validFile, literate) <-
               case takeExtension file of
                 ".lpts" ->
                   [(file, True)]
                 ".pts" ->
                   [(file, False)]
                 _ -> []
           ]
