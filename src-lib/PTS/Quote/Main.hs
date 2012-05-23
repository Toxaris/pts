module PTS.Quote.Main where

import Control.Monad.Environment
import Control.Monad.Error
import Control.Monad.Log
import Control.Monad.Reader

import Tools.Errors (runErrorsT)

import Parametric.Error (showErrors, annotateCode)

import PTS.AST
import PTS.Core
import PTS.Instances (fomegastar, C (C))
import PTS.Normalisation
import PTS.Options
import PTS.Parser
import PTS.Pretty
import PTS.Quote

main' = do
  text <- liftIO getContents
  let code = lines text
  e <- parseTerm "<stdin>" text
  t <- annotateCode code $ typecheck e `runEnvironmentT` []
  k <- annotateCode code $ typecheck t `runEnvironmentT` []
  case structure (normalform k) of
    Const (C 1) -> do q <- annotateCode code $ quotequote e `runEnvironmentT` []
                      liftIO (putStrLn (multiLine 80 q))
    _ -> do liftIO (putStrLn (multiLine 80 e))

main :: IO ()
main = runErrorsT $ runReaderT (runConsoleLogT (main' `catchError` \e -> liftIO $ putStrLn (showErrors e)) False) defaultOptions
