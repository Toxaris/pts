module PTS.Transform
  (  transform
  ,  typeOf
  )  where

import Control.Monad.Environment (runEnvironmentT)
import Control.Monad.Log (runConsoleLogT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)

import Parametric.Error (showErrors)

import PTS.AST (TypedTerm, typeOf)
import PTS.Core (typecheck)
import PTS.Options (defaultOptions)
import PTS.Parser (parseTerm)
import PTS.Pretty ()

import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)

import Tools.Errors (runErrorsT)

run p = do
  result <- runErrorsT (p `runConsoleLogT` False) `runReaderT` defaultOptions
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
  typed <- runEnvironmentT (typecheck term) []
  return (f typed)
