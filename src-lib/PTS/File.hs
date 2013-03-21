{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.File where

import Control.Monad
import Control.Monad.Assertions (MonadAssertions (assert))
import Control.Monad.Environment
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Log
import Control.Monad.Writer

import Data.Maybe (fromMaybe)

import System.Environment
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>), (<.>), joinPath)
import System.Directory (doesFileExist)

import Parametric.Error
import Parametric.Pretty hiding (when)

import PTS.Syntax
import PTS.Syntax.Term (TypedTerm (MkTypedTerm))
import PTS.Statics
import PTS.Instances
import PTS.Options
import PTS.Dynamics

import qualified Data.Set as Set

import Tools.Errors

deliterateLine ('>' : ' ' : line) = ' ' : ' ' : line
deliterateLine _                  = ""

deliterate text = do
  flag <- asks optLiterate
  if flag then return . unlines . map deliterateLine . lines $ text
          else return text

runProcessFile action state opt =
  evalStateT (runErrorsT (runReaderT (runConsoleLogT action (optDebugType opt)) opt)) state 

processFile :: (Functor m, MonadErrors [FOmegaError] m, MonadReader Options m, MonadState [(Name, Binding M)] m, MonadIO m, MonadLog m, MonadAssertions m) => FilePath -> m (Maybe (Module M))
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

processStmt (Assertion t q' t') = recover () $ assert (showAssertion t q' t') $ do
  output (text "")
  output (text "process assertion")
  output (nest 2 (sep [text "term:", nest 2 (pretty 0 t)]))
  env <- get

  -- compare specified and actual type
  MkTypedTerm _ q <- runEnvironmentT (typecheck t) env
  case q' of
    Nothing ->
      output (nest 2 (sep [text "type:", nest 2 (pretty 0 (strip q))]))
    Just q'
      | equivTerm env (strip q) q' ->
          output (nest 2 (sep [text "type:", nest 2 (pretty 0 q')]))
      | otherwise ->
          let (expected, given) = showDiff 0 (diff (nbe env q') (nbe env (strip q))) in
            prettyFail $ text "Type mismatch in assertion"
                      $$ text "  specified type:" <+> pretty 0 q'
                      $$ text "     normal form:" <+> text expected
                      $$ text "     actual type:" <+> text given

  -- compare specified and actual result
  case t' of
    Nothing ->
      output (nest 2 (sep [text "value:", nest 2 (pretty 0 (nbe env t))]))
    Just t'
      | equivTerm env t t' ->
          output (nest 2 (sep [text "value:", nest 2 (pretty 0 t')]))
      | otherwise ->
          let (expected, given) = showDiff 0 (diff (nbe env t') (nbe env t)) in
            prettyFail $ text "Result mismatch in assertion"
                      $$ text "  specified result:" <+> pretty 0 t'
                      $$ text "       normal form:" <+> text expected
                      $$ text "     actual result:" <+> text given

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

processStmt (Import mod) = recover () $ do
  output (text "")
  output (text "process import of" <+> pretty 0 mod)

  -- find file
  path <- asks optPath
  (literate, file) <- findModule path mod

  env <- get
  put []
  result <- local (setLiterate literate) $ processFile file
  put env

  case result of
    Nothing ->
      fail $ "expected module " ++ show mod ++ " in file " ++ file ++ " but found no module statement."
    Just (Module _ name _) | name /= mod ->
      fail $ "expected module " ++ show mod ++ " inf file " ++ file ++ " but found module " ++ show name ++ "."
    Just (Module imports name bindings) ->
      modify (bindings ++)

findModule path mod = find path where
  base  =  joinPath (parts mod)

  find [] = fail ("source file for module " ++ show mod ++ " not found.")
  find (dir : path) = do
    let lpts  = dir </> base <.> "lpts"
    let pts   = dir </> base <.> "pts"
    ptsExists <- liftIO (doesFileExist pts)
    if ptsExists then return (False, pts) else do
      lptsExists <- liftIO (doesFileExist lpts)
      if lptsExists then return (True, lpts) else do
        find path

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
