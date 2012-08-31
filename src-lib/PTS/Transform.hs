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

transform :: (TypedTerm -> TypedTerm) -> IO ()
transform f = do
  result <- runErrorsT $ do
    text <- liftIO $ getContents
    term <- parseTerm "<stdin>" text
    return term
  case result of
    Left errors -> do
      liftIO $ hPutStrLn stderr $ showErrors $ errors
      exitFailure
    Right result -> do
      print result
      exitSuccess

