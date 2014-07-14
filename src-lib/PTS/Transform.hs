{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Transform
  (  transform
  ,  typeOf
  ,  sortOf
  ,  asSort
  ,  structure
  ,  Name
  ,  Sort (Term, Type)
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

import Data.Monoid (mempty)
import qualified Data.Set as Set (insert)

import PTS.Error (showErrors)
import PTS.Options (defaultOptions)
import PTS.Statics (typecheckPull)
import PTS.Syntax (TypedTerm, typeOf, structure, Name, Term, TermStructure (..), mkVar, strip, parseTerm, multiLine, C (C))

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

data Sort
  = Term
  | Type

sortOf :: TypedTerm -> Sort
sortOf t = asSort (typeOf (typeOf t))

asSort :: TypedTerm -> Sort
asSort t = case structure t of
  Const (C 1) -> Term
  Const (C 2) -> Type
  Pos p t     -> asSort t

run p = do
  result <- runErrorsT (p `runConsoleLogT` False) `runReaderT` defaultOptions
  case result of
    Left errors -> do
      hPutStrLn stderr $ showErrors $ errors
      exitFailure
    Right result -> do
      putStrLn (multiLine 80 result)
      exitSuccess

transform :: (TypedTerm -> Term) -> IO ()
transform f = run $ do
  text <- liftIO $ getContents
  term <- parseTerm "<stdin>" text
  typed <- runEnvironmentT (typecheckPull term) mempty
  return (f typed)
