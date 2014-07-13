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
  success <- runMainErrors $ parseCommandLine processJobs
  if success
    then exitSuccess
    else exitFailure

runMainErrors act = do
  result <- runErrorsT act
  case result of
    Left errors -> do
      liftIO $ hFlush stdout
      liftIO $ hPutStrLn stderr $ showErrors $ errors
      return False
    Right result -> do
      return True

runMainState act = evalStateT act (Map.empty, [], [])

processJobs jobs = do
  runMainState $ mapM_ processJob jobs

processJob :: (Functor m, MonadIO m, MonadErrors [PTSError] m, MonadState (Map.Map ModuleName (Module M), [ModuleName], Bindings M) m) => (Options, FilePath) -> m ()
processJob (opt, file) = do
  let path = optPath opt
  file <- liftIO (findFile path file) >>= maybe (fail ("file not found: " ++ file)) return
  mod <- processFile' file opt
  return ()

processFile' file opt =
  checkAssertions (runReaderT (runConsoleLogT (processFile file) (optDebugType opt)) opt)

processFileSimple
  :: FilePath -> IO (Either [PTSError] (Maybe (Module M)))
processFileSimple f = runErrorsT . runMainState $ processFile' f defaultOptions
