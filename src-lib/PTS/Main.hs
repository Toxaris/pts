{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Main where

import Control.Monad
import Control.Monad.Assertions (checkAssertions)
import Control.Monad.Environment
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Log
import Control.Monad.Writer

import System.Environment
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Exit (exitSuccess, exitFailure)
import System.Directory (findFile)

import Parametric.Error
import Parametric.Pretty hiding (when)

import PTS.Syntax
import PTS.Core
import PTS.Instances
import PTS.Options
import PTS.Dynamics
import PTS.File

import qualified Data.Set as Set

import Tools.Errors

infixl 2 >>>
(>>>) = flip (.)

main = parseCommandLine processJobs

runMainErrors act = do
  result <- runErrorsT act
  case result of
    Left errors -> do
      liftIO $ hFlush stdout
      liftIO $ hPutStrLn stderr $ showErrors $ errors
      return False
    Right result -> do
      return True

runMainState act = evalStateT act []

processJobs jobs = do
  success <- runMainState $ runMainErrors $ mapM_ processJob jobs
  if success
    then exitSuccess
    else exitFailure

processJob :: (Functor m, MonadIO m, MonadErrors [FOmegaError] m, MonadState [(Name, Binding M)] m) => (Options, FilePath) -> m ()
processJob (opt, file) = do
  let path = optPath opt
  file <- liftIO (findFile path file) >>= maybe (fail ("file not found: " ++ file)) return
  mod <- checkAssertions (runReaderT (runConsoleLogT (processFile file) (optDebugType opt)) opt)
  return ()
