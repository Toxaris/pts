{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Main where

import Control.Monad
import Control.Monad.Environment
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Log
import Control.Monad.Writer

import System.Environment
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Exit (exitSuccess, exitFailure)

import Parametric.Error
import Parametric.Pretty hiding (when)

import PTS.AST
import PTS.Core
import PTS.Diff
import PTS.Instances
import PTS.Normalisation
import PTS.Options
import PTS.Parser
import PTS.Substitution
import PTS.Evaluation
import PTS.Algebra
import PTS.Binding
import PTS.Module

import qualified Data.Set as Set

import Tools.Errors

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
      liftIO $ hFlush stdout
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

processJob :: (Functor m, MonadIO m, MonadErrors [FOmegaError] m, MonadState [(Name, Binding M)] m) => (Options, FilePath) -> m ()
processJob (opt, file) = do
  mod <- runReaderT (runConsoleLogT (processFile file) (optDebugType opt)) opt
  return ()

processFile :: (Functor m, MonadErrors [FOmegaError] m, MonadReader Options m, MonadState [(Name, Binding M)] m, MonadIO m, MonadLog m) => FilePath -> m (Maybe (Module M))
processFile file = do
  outputLine $ "process file " ++ file
  text <- liftIO (readFile file)
  text <- deliterate text
  File maybeName stmts <- parseFile file text
  (imports, contents) <- execWriterT (processStmts (lines text, stmts))
  return (do
    name <- maybeName
    return (Module imports name contents))

processStmts (text, stmts) = do
  annotateCode text $ mapM_ processStmt stmts

processStmt (StmtPos p s) = annotatePos p $ processStmt s

processStmt (Term t) = recover () $ do
  pts <- asks (optInstance)
  output (text "")
  output (text "process expression")
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)]))
  env <- get
  MkTypedTerm _ q <- runEnvironmentT (typecheck t) env
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  let x = nbe env t
  output (nest 2 (sep [text "value:", nest 2 (pretty 0 x)]))

processStmt (Bind n args Nothing body) = recover () $ do
  let t = desugarArgs mkLam args body
  pts <- asks (optInstance)
  output (text "")
  output (text "process binding of" <+> pretty 0 n)
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
  env <- get
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)]))
  MkTypedTerm _ q <- runEnvironmentT (typecheck t) env
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  let v = evalTerm env t
  modify ((n, (v, q)) :)

processStmt (Bind n args (Just body') body) = recover () $ do
  let t   =  desugarArgs mkLam args body
  let t'  =  desugarArgs mkPi args body'
  pts <- asks (optInstance)
  output (text "")
  output (text "process binding of" <+> pretty 0 n)

  -- preprocess body
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term", nest 2 (pretty 0 t)]))

  -- preprocess type
  output (nest 2 (sep [text "specified type:", nest 2 (pretty 0 t')]))
  let t'' = t'
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full type", nest 2 (pretty 0 t'' )]))
  env <- get

  -- typecheck type
  MkTypedTerm _ q' <- runEnvironmentT (typecheck t'') env
  case structure (nbe env (strip q')) of
    Const _ -> return ()
    _       -> prettyFail $  text "Type error in top-level binding of " <+> pretty 0 n
                         $$ text "  expected:" <+> text "constant"
                         $$ text "     found:" <+> pretty 0 q'

  -- typecheck body
  MkTypedTerm _ q <- runEnvironmentT (typecheck t) env

  -- compare specified and actual type
  if equivTerm env (strip q) t''
    then output (nest 2 (sep [text "type:", nest 2 (pretty 0 (strip t') )]))
    else let (expected, given) = showDiff 0 (diff (nbe env t'' ) (nbe env (strip q))) in
           prettyFail $ text "Type mismatch in top-level binding of" <+> pretty 0 n
                     $$ text "  specified type:" <+> pretty 0 t'
                     $$ text "     normal form:" <+> text expected
                     $$ text "     actual type:" <+> text given

  let v = evalTerm env t
  modify ((n, (v, q)) :)

processStmt (Export n) = recover () $ do
  output (text "")
  output (text "process export of" <+> pretty 0 n)

  -- construct term
  let t = mkVar n

  -- figure out type
  env <- get
  MkTypedTerm _ q <- runEnvironmentT (typecheck t) env
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))

  -- figure out value
  let v = evalTerm env t

  -- add to generated module
  tell (mempty, [(n, (v, q))])

-- Haskell's version of Scala's _ for anonymous functions. From lens.
-- I'd say more readable than point-free programming.
(??) = flip
infixl 1 ??

output :: (Pretty b, MonadIO m, MonadReader Options m) => b -> m ()
output doc =
  asks (flip multiLine doc . optColumns) >>= outputLine

-- Output doc unless --quiet was passed.
outputLine :: (MonadIO m, MonadReader Options m) => String -> m ()
outputLine doc =
  asks optQuiet >>=
    (unless ?? (liftIO . putStrLn) doc)
