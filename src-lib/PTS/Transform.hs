module PTS.Transform
  ( transform
  ) where

import Control.Monad.Trans (liftIO)

import Parametric.Error (showErrors)

import PTS.AST (TypedTerm)
import PTS.Parser (parseTerm)
import PTS.Pretty ()

import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)

import Tools.Errors (runErrorsT)

run p = do
  result <- runErrorsT $ p
  case result of
    Left errors -> do
      hPutStrLn stderr $ showErrors $ errors
      exitFailure
    Right result -> do
      print result
      exitSuccess

transform :: (TypedTerm -> TypedTerm) -> IO ()
transform f = run $ do
  text <- liftIO $ getContents
  term <- parseTerm "<stdin>" text
  return term
