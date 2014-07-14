{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Process.Main where

import Control.Monad.Assertions (checkAssertions)
import Control.Monad.Errors
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Log (runConsoleLogT)

import qualified Data.Set as Set
import qualified Data.Map as Map

import PTS.Dynamics
import PTS.Error
import PTS.Instances
import PTS.Options
import PTS.Process.File
import PTS.Statics
import PTS.Syntax

import System.Directory (findFile)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
infixl 2 >>>
(>>>) = flip (.)

main = do
  result <- parseCommandLine processJobs
  processErrors result

processErrors result = do
  case result of
    Left errors -> do
      liftIO $ hFlush stdout
      liftIO $ hPutStrLn stderr $ showErrors $ errors
      exitFailure
    Right result -> do
      exitSuccess

-- This shares the state across files.

processJobs jobs = do
  withEmptyState . runErrorsT $ mapM processJob jobs

processJob :: (Functor m, MonadIO m, MonadErrors [PTSError] m, MonadState (Map.Map ModuleName (Module Eval), [ModuleName], Bindings Eval) m) => (Options, FilePath) -> m (Maybe (Module Eval))
processJob (opt, file) = do
  let path = optPath opt
  file <- liftIO (findFile path file) >>= maybe (fail ("file not found: " ++ file)) return
  checkAssertions . runOptMonads opt $ processFile file

initState = (Map.empty, [], [])
withEmptyState act = evalStateT act initState

runOptMonads opt action =
  runReaderT (runConsoleLogT action (optDebugType opt)) opt
