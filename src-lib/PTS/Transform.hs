{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Transform
  (  transform
  ,  typeOf
  ,  sortOf
  ,  structure
  ,  Name
  ,  Sort (Term, Type)
  ,  Term
  ,  TermStructure (..)
  ,  strip
  )  where

import Control.Applicative (Applicative)
import Control.Monad.Environment (runEnvironmentT)
import Control.Monad.Log (runConsoleLogT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)

import qualified Data.Set as Set (insert)

import Parametric.Error (showErrors)
import Parametric.AST (Name, Names, freshvarl)

import PTS.Algebra (strip)
import PTS.AST (TypedTerm, typeOf, structure, Term, TermStructure (..))
import PTS.Core (typecheck)
import PTS.Options (defaultOptions)
import PTS.Parser (parseTerm)
import PTS.Pretty ()
import PTS.Constants (C (C))

import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)

import Tools.Errors (runErrorsT)

data Sort
  = Term
  | Type

sortOf :: TypedTerm -> Sort
sortOf t = case structure (typeOf (typeOf t)) of
  Const (C 1) -> Term
  Const (C 2) -> Type

run p = do
  result <- runErrorsT (p `runConsoleLogT` False) `runReaderT` defaultOptions
  case result of
    Left errors -> do
      hPutStrLn stderr $ showErrors $ errors
      exitFailure
    Right result -> do
      print result
      exitSuccess

transform :: (TypedTerm -> Term) -> IO ()
transform f = run $ do
  text <- liftIO $ getContents
  term <- parseTerm "<stdin>" text
  typed <- runEnvironmentT (typecheck term) []
  return (f typed)
