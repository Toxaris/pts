{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Transform
  (  transform
  ,  typeOf
  ,  sortOf
  ,  C (C)
  ,  Value (Constant)
  ,  structure
  ,  Name
  ,  Term
  ,  TermStructure (..)
  ,  TypedTerm
  ,  strip
  ,  mkVar
  )  where

import Control.Applicative (Applicative)

import Control.Monad.Environment (runEnvironmentT)
import Control.Monad.Errors (runErrorsT)
import Control.Monad.Log (runConsoleLogT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)

import qualified Data.Set as Set (insert)

import PTS.Error (showErrors)
import PTS.Options (defaultOptions)
import PTS.Statics (typecheckPull)
import PTS.Syntax (TypedTerm, typeOf, sortOf, structure, Name, Term, TermStructure (..), mkVar, strip, parseTerm, multiLine, C (C))
import PTS.Dynamics (Eval, Value (Constant))

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

run p = do
  result <- runErrorsT (p `runConsoleLogT` False) `runReaderT` defaultOptions
  case result of
    Left errors -> do
      hPutStrLn stderr $ showErrors $ errors
      exitFailure
    Right result -> do
      putStrLn (multiLine 80 result)
      exitSuccess

transform :: (TypedTerm Eval -> Term) -> IO ()
transform f = run $ do
  text <- liftIO $ getContents
  term <- parseTerm "<stdin>" text
  typed <- runEnvironmentT (typecheckPull term) []
  return (f typed)
