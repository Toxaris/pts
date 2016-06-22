{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Process.File where

import Control.Applicative hiding (Const)
import Control.Arrow (second)

import Control.Monad (when, unless)
import Control.Monad.Assertions (MonadAssertions (assert))
import Control.Monad.Environment (runEnvironmentT)
import Control.Monad.Errors
import Control.Monad.Reader (MonadReader (local), runReaderT, asks, ask)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Log (MonadLog, runConsoleLogT)

import Data.Traversable (traverse, for)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Map as Map

import PTS.Dynamics
import PTS.Error
import PTS.Instances
import PTS.Options
import PTS.Statics
import PTS.Syntax

import System.FilePath ((</>), (<.>), joinPath, takeExtension)
import System.Directory (doesFileExist)

import Text.PrettyPrint.HughesPJ

isSpaceOrTab c = c == ' ' || c == '\t'
deliterateLine ('>' : c : line) | isSpaceOrTab c  = ' ' : c : line
deliterateLine _                                  = ""

deliterate text = do
  flag <- asks optLiterate
  if flag then return . unlines . map deliterateLine . lines $ text
          else return text

setLiterateFromName fileName =
  case (takeExtension fileName) of
    ".lpts" -> setLiterate True
    ".pts" -> setLiterate False
    _ -> id -- Keep setting from cmd line.

data ProcessingState
  = ProcessingState
    { stateCache :: Map.Map ModuleName (Module Eval)
    , stateImports :: [ModuleName]
    , stateBindings :: Bindings Eval
    }

getCache = do
  state <- get
  return (stateCache state)

putCache cache = do
  state <- get
  put (state {stateCache = cache})

getImports = do
  state <- get
  return (stateImports state)

putImports imports = do
  state <- get
  put (state {stateImports = imports})

getBindings = do
  state <- get
  return (stateBindings state)

putBindings env = do
  state <- get
  put (state {stateBindings = env})

processFileInt fileName = do
  local (setLiterateFromName fileName) $ processFileInt' fileName

processFileInt' file = do
  outputLine $ "process file " ++ file
  text <- liftIO (readFile file)
  text <- deliterate text
  File fileLangName maybeName stmts <- parseFile file text
  let fileLang = do
        instName <- fileLangName
        lookupInstance instName

  currLang <- asks optInstance
  newLang <- maybeXor currLang fileLang =<< asks optAllowSubLang

  let setLanguage = setInstance (Just newLang)
  local setLanguage $
  {-
  let setLanguage :: CmdlineOptions -> Options = setInstance newLang
  --withReader setLanguage $
  -}
    processStmts (lines text, stmts)
  state <- get
  return (maybeName, state)

  where
    maybeXor (Just l1) (Just l2) allowSubLang | isSubPTS l2 l1 == Just True =
      if allowSubLang
       then return l2
       else prettyFail . text $
              "Sublanguage specified for module, but sublanguages for " ++
              "modules are not enabled."

    maybeXor (Just l1) (Just l2) _ =
      if l1 /= l2
       then prettyFail $ text "Different language specified for module."
       else return l1
    maybeXor (Just l) Nothing _ = return l
    maybeXor Nothing (Just l) _ = return l
    maybeXor Nothing Nothing  _ = prettyFail $ text "No language specified"

liftEval :: MonadState ProcessingState m => Eval a -> m a
liftEval action = do
  env <- getBindings
  return (runEval env action)

processFile :: (Functor m, Applicative m, MonadErrors [PTSError] m, MonadReader Options m, MonadState ProcessingState m, MonadIO m, MonadLog m, MonadAssertions m) => FilePath -> m (Maybe (Module Eval))
processFile file = do
  (maybeName, rest) <- processFileInt file
  return $ filterRet <$> maybeName <*> pure rest

filterRet name state =
  let contents = [(n, b {bindingExport = False}) | (n, b) <- stateBindings state, bindingExport b] in
    Module (stateImports state) name contents

processStmts (text, stmts) = do
  annotateCode text $ mapM_ processStmt stmts

processStmt (StmtPos p s) = annotatePos p $ processStmt s

processStmt (Term t) = recover () $ do
  pts <- getLanguage
  output (text "")
  output (text "process expression")
  output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
  whenOption optShowFullTerms $ output (nest 2 (sep [text "full term:", nest 2 (pretty 0 t)]))
  env <- getBindings
  t <- runEnvironmentT (typecheckPull t) env
  q <- liftEval (reify (typeOf t))
  output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
  x <- liftEval (eval t >>= reify) 
  output (nest 2 (sep [text "value:", nest 2 (pretty 0 x)]))

processStmt (Bind n args typeAnnot body) = recover () $ do
  let maybeT = foldTelescope mkLam args <$> body
  pts <- getLanguage
  output (text "")
  output (text "process binding of" <+> pretty 0 n)

  -- preprocess body

  for maybeT $ \t -> do
    output (nest 2 (sep [text "original term:", nest 2 (pretty 0 t)]))
    whenOption optShowFullTerms $ output (nest 2 (sep [text "full term", nest 2 (pretty 0 t)]))

  env <- getBindings
  let checkTopLevel typ =
        flip runEnvironmentT env $ do
          typ <- typecheckPull typ
          s <- checkProperType typ (text "in top-level binding of " <+> pretty 0 n) (text "")
          return (typ, s)

  (maybeT, tType, tSort) <- case typeAnnot of
    Just body' -> do
      -- preprocess type
      let t' = foldTelescope mkPi args body'
      output (nest 2 (sep [text "specified type:", nest 2 (pretty 0 t')]))
      whenOption optShowFullTerms $ output (nest 2 (sep [text "full type", nest 2 (pretty 0 t')]))

      -- typecheck type
      (qq, s) <- checkTopLevel t'

      -- use declared type to typecheck push
      qq <- liftEval (eval qq)
      maybeT <- recover Nothing $ runEnvironmentT (traverse (flip typecheckPush qq) maybeT) env
      return (maybeT, qq, s)
    Nothing -> do
      -- typecheck pull
      case maybeT of
        Just t -> do
          t <- runEnvironmentT (typecheckPull t) env
          q <- liftEval (reify (typeOf t))
          (_, s) <- checkTopLevel q

          output (nest 2 (sep [text "type:", nest 2 (pretty 0 q)]))
          return (Just t, typeOf t, s)
        Nothing ->
          prettyFail $ text "Binding for " <+> pretty 0 n <+> text "specifies neither type nor body."

  -- do binding
  let v =
        case maybeT of
          Just t -> evalTerm env t
          Nothing -> ResidualVar n
  putBindings ((n, Binding False v tType (Just tSort)) : env)

processStmt (Assertion t q' t') = recover () $ assert (showAssertion t q' t') $ do
  output (text "")
  output (text "process assertion")
  output (nest 2 (sep [text " term:", nest 2 (pretty 0 t)]))

  env <- getBindings

  let check Nothing Nothing = do
        t <- typecheckPull t
        v <- liftEval (eval t)
        return (typeOf t, v)
      check (Just q') Nothing = do
        q' <- typecheckPull q'
        checkProperType q' (text "in assertion") (text "as annotated type")
        q' <- liftEval (eval q')
        t <- typecheckPush t q'
        v <- liftEval (eval t)
        return (typeOf t, v)
      check Nothing (Just t') = do
        t' <- typecheckPull t'
        let q' = typeOf t'
        t <- typecheckPush t q'
        v' <- liftEval (eval t')
        v <- liftEval (eval t)
        same <- liftEval (equiv v v')
        unless same $ do
          t <- liftEval (reify v)
          t' <- liftEval (reify v')
          let (expected, given) = showDiff 0 (diff t' t)
          prettyFail $ text "Result mismatch in assertion"
                    $$ text "  specified result:" <+> pretty 0 t'
                    $$ text "       normal form:" <+> text expected
                    $$ text "     actual result:" <+> text given
        return (typeOf t, v)
      check (Just q') (Just t') = do
        q' <- typecheckPull q'
        checkProperType q' (text "in assertion") (text "as annotated type")
        q' <- liftEval (eval q')
        t' <- typecheckPush t' q'
        t <- typecheckPush t q'
        v' <- liftEval (eval t')
        v <- liftEval (eval t)
        same <- liftEval (equiv v v')
        unless same $ do
          t <- liftEval (reify v)
          t' <- liftEval (reify v')
          let (expected, given) = showDiff 0 (diff t' t)
          prettyFail $ text "Result mismatch in assertion"
                    $$ text "  specified result:" <+> pretty 0 t'
                    $$ text "       normal form:" <+> text expected
                    $$ text "     actual result:" <+> text given
        return (typeOf t, v)

  (t, v) <- runEnvironmentT (check q' t') env
  v <- liftEval (reify v)
  t <- liftEval (reify t)
  output (nest 2 (sep [text " type:", nest 2 (pretty 0 t)]))
  output (nest 2 (sep [text "value:", nest 2 (pretty 0 v)]))

processStmt (Export n) = recover () $ do
  output (text "")
  output (text "process export of" <+> pretty 0 n)

  -- mark as exported
  bindings <- getBindings
  when (and [n /= n' | (n', _) <- bindings]) $ do
    fail $ "Unbound identifier: " ++ show n
  let bindings' = [(n', b {bindingExport = bindingExport b || n== n'}) | (n', b) <- bindings]
  putBindings bindings'

processStmt (Import mod) = recover () $ do
  output (text "")
  output (text "process import of" <+> pretty 0 mod)

  cache <- getCache
  imports <- getImports
  bindings <- getBindings

  case Map.lookup mod cache of
    Just (Module _ _ bindings') -> do
      putBindings (bindings ++ bindings')
    Nothing -> do
      -- find file
      path <- asks optPath
      (literate, file) <- findModule path mod

      putImports []
      putBindings []
      result <- local (setLiterate literate) $ processFile file

      case result of
        Nothing ->
          fail $ "expected module " ++ showPretty mod ++ " in file " ++ file ++ " but found no module statement."
        Just (Module _ name _) | name /= mod ->
          fail $ "expected module " ++ showPretty mod ++ " inf file " ++ file ++ " but found module " ++ showPretty name ++ "."
        Just found@(Module _ _ bindings') -> do
          cache <- getCache
          putCache (Map.insert mod found cache)
          putImports imports
          putBindings (bindings ++ bindings')

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
