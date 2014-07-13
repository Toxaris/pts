{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Process.File where

import Control.Arrow (second)

import Control.Monad (when, unless)
import Control.Monad.Assertions (MonadAssertions (assert))
import Control.Monad.Environment (runEnvironmentT)
import Control.Monad.Errors
import Control.Monad.Reader (MonadReader (local), runReaderT, asks)
import Control.Monad.State (MonadState, get, put, modify, evalStateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Log (MonadLog, runConsoleLogT)

import Data.Monoid (mempty)
import qualified Data.Map as Map

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

processFileInt file = do
  outputLine $ "process file " ++ file
  text <- liftIO (readFile file)
  text <- deliterate text
  File maybeName stmts <- parseFile file text
  processStmts (lines text, stmts)
  (cache, imports, bindings) <- get
  return (maybeName, cache, imports, bindings)

processFile :: (Functor m, MonadErrors [PTSError] m, MonadReader Options m, MonadState (Map.Map ModuleName (Module M), [ModuleName], Bindings M) m, MonadIO m, MonadLog m, MonadAssertions m) => FilePath -> m (Maybe (Module M))
processFile file = do
  fmap filterRet $ processFileInt file

filterRet res =
  let (maybeName, cache, imports, bindings) = res
      contents = [(n, (False, t, v)) | (n, (True, t, v)) <- bindings]
   in
  do
    name <- maybeName
    return (Module imports name contents)

processStmts (text, stmts) = do
  annotateCode text $ mapM_ processStmt stmts

processStmt (StmtPos p s) = annotatePos p $ processStmt s

processStmt (Term t) = recover () $ do
  pts <- asks (optInstance)
  output (text "")
  output (text "process expression")
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)]))
  (_, _, env) <- get
  MkTypedTerm _ q <- runEnvironmentT (typecheckPull t) env
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  let x = nbe env t
  output (nest 2 (sep [text "value:", nest 2 (pretty 0 x)]))

processStmt (Bind n args Nothing body) = recover () $ do
  let t = desugarArgs mkLam args body
  pts <- asks (optInstance)
  output (text "")
  output (text "process binding of" <+> pretty 0 n)
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
  (_, _, env) <- get
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)]))
  MkTypedTerm _ q <- runEnvironmentT (typecheckPull t) env
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  let v = evalTerm env t
  modify $ (\f (x, y, z) -> (x, y, f z)) $ ((n, (False, v, q)) :)

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
  (_, _, env) <- get

  -- typecheck type
  qq@(MkTypedTerm _ q') <- runEnvironmentT (typecheckPull (nbe env t'')) env
  case structure (nbe env (strip q')) of
    Const _ -> return ()
    _       -> prettyFail $  text "Type error in top-level binding of " <+> pretty 0 n
                         $$ text "  expected:" <+> text "constant"
                         $$ text "     found:" <+> pretty 0 q'

  -- use declared type to typecheck push
  MkTypedTerm _ q <- runEnvironmentT (typecheckPush t qq) env

  let v = evalTerm env t
  modify $ (\f (x, y, z) -> (x, y, f z)) $ ((n, (False, v, q)) :)

processStmt (Assertion t q' t') = recover () $ assert (showAssertion t q' t') $ do
  output (text "")
  output (text "process assertion")
  output (nest 2 (sep [text "term:", nest 2 (pretty 0 t)]))
  (_, _, env) <- get

  -- compare specified and actual type
  MkTypedTerm _ q <- runEnvironmentT (typecheckPull t) env
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
  (cache, imports, bindings) <- get
  when (and [n /= n' | (n', _) <- bindings]) $ do
    fail $ "Unbound identifier: " ++ show n
  let bindings' = [(n', (x || n == n', t, v)) | (n', (x, t, v)) <- bindings]
  put (cache, imports, bindings')

processStmt (Import mod) = recover () $ do
  output (text "")
  output (text "process import of" <+> pretty 0 mod)

  (cache, imports, bindings) <- get

  case Map.lookup mod cache of
    Just (Module _ _ bindings') -> do
      put (cache, imports, bindings ++ bindings')
    Nothing -> do
      -- find file
      path <- asks optPath
      (literate, file) <- findModule path mod

      put (cache, [], [])
      result <- local (setLiterate literate) $ processFile file
      (cache, _, _) <- get
      put (cache, imports, bindings)

      case result of
        Nothing ->
          fail $ "expected module " ++ showPretty mod ++ " in file " ++ file ++ " but found no module statement."
        Just (Module _ name _) | name /= mod ->
          fail $ "expected module " ++ showPretty mod ++ " inf file " ++ file ++ " but found module " ++ showPretty name ++ "."
        Just found@(Module _ _ bindings') ->
          put (Map.insert mod found cache, imports, bindings ++ bindings')

findModule path mod = find path where
  base  =  joinPath (parts mod)

  find [] = fail ("source file for module " ++ showPretty mod ++ " not found.")
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
