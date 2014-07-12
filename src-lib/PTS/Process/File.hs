{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Process.File where

import Control.Arrow (second)

import Control.Monad (unless)
import Control.Monad.Assertions (MonadAssertions (assert))
import Control.Monad.Environment (runEnvironmentT)
import Control.Monad.Errors
import Control.Monad.Reader (MonadReader (local), runReaderT, asks)
import Control.Monad.State (MonadState, get, gets, put, modify, evalStateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Log (MonadLog, runConsoleLogT)

import Data.Monoid (mempty)

import PTS.Dynamics
import PTS.Error
import PTS.Instances
import PTS.Options
import PTS.Statics
import PTS.Syntax
import PTS.Syntax.Term (TypedTerm (MkTypedTerm))

import System.FilePath ((</>), (<.>), joinPath)
import System.Directory (doesFileExist)

import Text.PrettyPrint.HughesPJ

deliterateLine ('>' : ' ' : line) = ' ' : ' ' : line
deliterateLine _                  = ""

deliterate text = do
  flag <- asks optLiterate
  if flag then return . unlines . map deliterateLine . lines $ text
          else return text

runProcessFile action state opt =
  evalStateT (runErrorsT (runReaderT (runConsoleLogT action (optDebugType opt)) opt)) state 

processFile :: (Functor m, MonadErrors [PTSError] m, MonadReader Options m, MonadState ([ModuleName], Bindings M) m, MonadIO m, MonadLog m, MonadAssertions m) => FilePath -> m (Maybe (Module M))
processFile file = do
  outputLine $ "process file " ++ file
  text <- liftIO (readFile file)
  text <- deliterate text
  File maybeName stmts <- parseFile file text
  processStmts (lines text, stmts)
  (imports, bindings) <- get
  let contents = [(n, (False, t, v)) | (n, (True, t, v)) <- bindings]
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
  env <- gets snd
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
  env <- gets snd
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)]))
  MkTypedTerm _ q <- runEnvironmentT (typecheck t) env
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  let v = evalTerm env t
  modify $ second $ ((n, (False, v, q)) :)

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
  env <- gets snd

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
  modify $ second $ ((n, (False, v, q)) :)

processStmt (Assertion t q' t') = recover () $ assert (showAssertion t q' t') $ do
  output (text "")
  output (text "process assertion")
  output (nest 2 (sep [text "term:", nest 2 (pretty 0 t)]))
  env <- gets snd

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

  -- mark as exported
  (imports, bindings) <- get
  let bindings' = [(n', (x || n == n', t, v)) | (n', (x, t, v)) <- bindings]
  put (imports, bindings')

processStmt (Import mod) = recover () $ do
  output (text "")
  output (text "process import of" <+> pretty 0 mod)

  -- find file
  path <- asks optPath
  (literate, file) <- findModule path mod

  old <- get
  put ([], []) 
  result <- local (setLiterate literate) $ processFile file
  put old

  case result of
    Nothing ->
      fail $ "expected module " ++ show mod ++ " in file " ++ file ++ " but found no module statement."
    Just (Module _ name _) | name /= mod ->
      fail $ "expected module " ++ show mod ++ " inf file " ++ file ++ " but found module " ++ show name ++ "."
    Just (Module imports name bindings) ->
      modify $ second $ (bindings ++)

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
