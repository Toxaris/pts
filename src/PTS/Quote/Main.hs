module Main where

import Control.Monad.Error
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
  t <- annotateCode code $ typecheck 0 [] e
  k <- annotateCode code $ typecheck 0 [] t
  case structure (normalform k) of
    Const (C 1) -> do q <- annotateCode code $ quotequote 0 [] e
                      liftIO (putStrLn (multiLine 80 q))
    _ -> do liftIO (putStrLn (multiLine 80 e))

main = runErrorsT $ runReaderT (main' `catchError` \e -> liftIO $ putStrLn (showErrors e)) defaultOptions
