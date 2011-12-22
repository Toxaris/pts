{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Main where

import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader

import Tools.Errors
import Parametric.Error
import Parametric.Pretty hiding (when)

import PTS.AST
import PTS.Substitution
import PTS.Parser
import PTS.Core
import PTS.Diff
import PTS.Instances
import PTS.Options
import PTS.Normalisation

deliterateLine ('>' : ' ' : line) = ' ' : ' ' : line
deliterateLine _                  = ""
  
deliterate text = do
  flag <- asks optLiterate
  if flag then return . unlines . map deliterateLine . lines $ text
          else return text
  
infixl 2 >>>
(>>>) = flip (.)

main = parseCommandLine processJobs

runMainErrors act = do
  result <- runErrorsT act
  case result of
    Left errors -> do
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

processJob :: (Functor m, MonadIO m, MonadErrors [FOmegaError] m, MonadState [(Name, Term)] m) => (Options, FilePath) -> m ()
processJob (opt, file) = do 
  liftIO $ putStrLn $ "process file " ++ file
  runReaderT (processFile file) opt  

processFile :: (Functor m, MonadErrors [FOmegaError] m, MonadReader Options m, MonadState [(Name, Term)] m, MonadIO m) => FilePath -> m ()
processFile file = do
  text <- liftIO (readFile file)
  text <- deliterate text
  stmts <- parseStmts file text
  processStmts (lines text, stmts)

processStmts (text, stmts) = do
  annotateCode text $ mapM_ processStmt stmts

processStmt (StmtPos p s) = annotatePos p $ processStmt s

processStmt (Term t) = recover () $ do
  pts <- asks (optInstance)
  output (text "")
  output (text "process expression")
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)])) 
  t <- prepareTerm t
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)])) 
  q <- typecheck 0 [] t
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)])) 
  let x = normalform t
  output (nest 2 (sep [text "value:", nest 2 (pretty 0 x)]))
  
processStmt (Bind n Nothing t) = recover () $ do
  pts <- asks (optInstance)
  output (text "")
  output (text "process binding of" <+> pretty 0 n)
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)])) 
  t <- prepareTerm t
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)])) 
  q <- typecheck 0 [] t
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  modify ((n, t) :)

processStmt (Bind n (Just t') t) = recover () $ do
  pts <- asks (optInstance)
  output (text "")
  output (text "process binding of" <+> pretty 0 n)
  
  -- preprocess body
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)])) 
  t <- prepareTerm t
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term", nest 2 (pretty 0 t)])) 

  -- preprocess type
  output (nest 2 (sep [text "specified type:", nest 2 (pretty 0 t')]))
  t'' <- prepareTerm t'
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full type", nest 2 (pretty 0 t'' )]))
  
  -- typecheck type
  q' <- typecheck 0 [] t''
  case structure (normalform q') of
    Const _ -> return ()
    _       -> prettyFail $  text "Type error in top-level binding of " <+> pretty 0 n
                         $$ text "  expected:" <+> text "constant"
                         $$ text "     found:" <+> pretty 0 q'
  
  -- typecheck body
  q <- typecheck 0 [] t
  
  -- compare specified and actual type
  if equiv q t''
    then output (nest 2 (sep [text "type:", nest 2 (pretty 0 t' )]))
    else let (expected, given) = showDiff 0 (diff (normalform t'' ) (normalform q)) in 
           prettyFail $ text "Type mismatch in top-level binding of" <+> pretty 0 n
                     $$ text "  specified type:" <+> pretty 0 t'
                     $$ text "     normal form:" <+> text expected
                     $$ text "     actual type:" <+> text given
  modify ((n, t) :)

prepareTerm t =
  gets (foldr substGlobal t)

substGlobal (n, e) t = subst t n e

output doc = asks (flip multiLine doc . optColumns) >>= liftIO . putStrLn
