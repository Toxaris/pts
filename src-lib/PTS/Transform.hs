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
import Control.Monad.Log (runConsoleLogT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)

import qualified Data.Set as Set (insert)

import Parametric.Error (showErrors)
import Parametric.AST (Name, Names, freshvarl)

import PTS.Syntax (TypedTerm, typeOf, structure, Term, TermStructure (..), mkVar, strip, parseTerm, multiLine, C (C))
import PTS.Statics (typecheck)
import PTS.Options (defaultOptions)

import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)

import Tools.Errors (runErrorsT)

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
  typed <- runEnvironmentT (typecheck term) []
  return (f typed)
