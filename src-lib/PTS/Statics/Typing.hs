{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PatternGuards, FlexibleInstances #-}
module PTS.Statics.Typing where

import Prelude (String)

import Control.Monad
import Control.Monad.Environment
import Control.Monad.Errors.Class
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.Trans

import Data.Bool (Bool (False, True), (&&))
import Data.Char ()
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Int (Int)
import Data.List (map, null, replicate, (++))
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (fst, snd)

import PTS.Dynamics
import PTS.Error
import PTS.Instances
import PTS.Options
import PTS.Syntax
import PTS.Syntax.Term (TypedTerm (MkTypedTerm))

import Text.PrettyPrint.HughesPJ hiding (int)
import Text.Show (Show (show))

-- implementation of pure type systems as described on p.98 in Barendregt's "Lambda Calculi with Types"
-- ftp://ftp.cs.kun.nl/pub/CompMath.Found/HBKJ.ps.Z
-- Restrictions: The PTS must be full and functional

-- some ideas regarding conversion to algorithmic style have been taken
-- from van Benthem et al's "Checking algorithms for Pure Type Systems"
-- http://www.springerlink.com/content/f510r63138068632/
--

import Debug.Trace

-- instance Ord Term where
--   compare a b = compare (show a) (show b)

-- Reduction and Evaluation of terms
isVal :: Term -> Bool
isVal t = case structure t of
  Const _    ->  True
  Lam _ _ _  ->  True
  Pi _ _ _   ->  True
  Int _      ->  True
  _          ->  False

check :: Monad m => Bool -> String -> m ()
check True _ = return ()
check False s = fail s

checkMaybe :: Monad m => Maybe a -> String -> m ()
checkMaybe (Just _) _ = return ()
checkMaybe Nothing s = fail s

lookupValue x = do
  m <- lookup x
  return (fmap fst m)

lookupType x = do
  m <- lookup x
  return (fmap snd m)

-- mytrace d ctx (Const x) | x == star = False
-- mytrace d ctx t =
--   trace ((show d) ++ (ndots d) ++ (show ctx) ++ " |- "++(show t) ++ " : ???") False

-- safe bind
safebind :: MonadEnvironment Name (Binding M) m => Name -> TypedTerm -> Term -> (Name -> Term -> m a) -> m a
safebind x t b f = do
  result <- lookupType x
  case result of
    Nothing -> do
      bind x (ResidualVar x, t) (f x b)
    Just _ -> do
      vars <- keys
      let nx = freshvarl (freevars b `Set.union` vars) x
      bind nx (ResidualVar nx, t) (f nx (subst b x (mkVar nx)))

debug :: (MonadEnvironment Name (Binding M) m, MonadLog m) => String -> Term -> m TypedTerm -> m TypedTerm
debug n t result = do
  enter n
  ctx <- getEnvironment
  log $ "Context: " ++ showCtx ctx
  log $ "Subject: " ++ show t
  x <- result
  log $ "Result:  " ++ show x
  exit
  return x

-- error messages and generic error checking

normalizeToSort t' t context info = do
  pts <- asks optInstance
  env <- getEnvironment

  case structure (nbe env (strip t')) of
    Const s | sorts pts s  ->  return s
    _                      ->  prettyFail $ msgNotProperType context info t t'

msgNotProperType context info t t'
  = text "Type Error" <+> context <+> text ": Expected proper type" <+> info <> text "." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type is not a sort, so the term is not a proper type and therefore not valid at this position.")] $$
    sep [text "Term:", nest 2 (pretty 0 t)] $$
    sep [text "Type:", nest 2 (pretty 0 t')])

normalizeToSame s' t' s t context info1 info2 = do
  let stripS' = strip s'
  let stripT' = strip t'
  env <- getEnvironment
  if (equivTerm env (stripS') (stripT'))
      then return ()
      else let  s'' = nbe env (stripS')
                t'' = nbe env (stripT')
             in prettyFail $ msgNotSame context info1 info2 s t s'' t''

msgNotSame context info1 info2 s t s' t'
  = let (s'', t'') = showDiff 0 (diff s' t')
     in text "Type Error" <+> context <> text ": Types do not match." $$ nest 2 (
        sep [text "Explanation:", nest 2 (text "The types of the" <+> info1 <+> text "and the" <+> info2 <+> text "should be beta-equivalent.")] $$
        sep [info1 <> text ":", nest 2 (pretty 0 s)] $$
        sep [info2 <> text ":", nest 2 (pretty 0 t)] $$
        sep [text "Type of" <+> info1 <> text ":", nest 2 (text s'')] $$
        sep [text "Type of" <+> info2 <> text ":", nest 2 (text t'')])

normalizeToInt :: (MonadLog m, Functor m, MonadEnvironment Name (Binding M) m, MonadReader Options m, MonadErrors Errors m) => TypedTerm -> TypedTerm -> Doc -> Doc -> m TypedTerm
normalizeToInt t' t context info = do
  let stripT' = strip t'
  env <- getEnvironment
  if equivTerm env stripT' (mkConst int)
    then typecheck (mkConst int)
    else let t'' = nbe env stripT'
          in prettyFail $ msgNotInt context info t t'' int

msgNotInt context info t t' int
  = text "Type Error" <+> context <> text ": Types do not match." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type of the" <+> info <+> text "should be beta-equivalent to" <+> pretty 0 (mkConst int) <> text ".")] $$
    sep [info <> text ":", nest 2 (pretty 0 t)] $$
    sep [text "Type of" <+> info <> text ":", nest 2 (pretty 0 t')] $$
    sep [text "Expected Type:" <+> nest 2 (pretty 0 (mkConst int))])

normalizeToPi t' t context info = do
  env <- getEnvironment
  let t'' = nbe env (strip t') in
     case structure t'' of
       result@(Pi _ _ _)  ->  return result
       _                  ->  prettyFail $ msgNotPi context info t t''

msgNotPi context info t t'
  = text "Type Error" <+> context <> text ": Not a product type." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type of the" <+> info <+> text "should be beta-equivalent to a product type.")] $$
    sep [info <> text ":", nest 2 (pretty 0 t)] $$
    sep [text "Type of" <+> info <> text ":", nest 2 (pretty 0 t')])

typecheck :: (MonadEnvironment Name (Binding M) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> m TypedTerm
--typecheck p d ctx t | mytrace d ctx t = undefined
--
prettyRelations pts s1 s2 =
  maybe
    (prettyFail $ let s1t = pretty 0 s1; s2t = pretty 0 s2 in text "in this PTS you can't abstract on expressions of sort" <+> s1t <+> text "in expressions of sort" <+> s2t <+> text ": no relation" <+> s1t <+> text ":" <+> s2t)
    return
    (relations pts s1 s2)

typecheck t = case structure t of
  -- constant
  Const c -> debug "typecheck Const" t $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (MkTypedTerm (Const c) t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  -- start
  Var x -> debug "typecheck Var" t $ do
    xt <- lookupType x
    case xt of
      Just xt -> do
        -- s <- typecheck xt
        -- normalizeToSort s xt (text "in variable") (text "as type of" <+> pretty 0 x)
        return (MkTypedTerm (Var x) xt)

      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debug "typecheck Fun" t $ do
    a'@(MkTypedTerm _ s1) <- typecheck a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ s2) <- typecheck newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'
      return (MkTypedTerm (Pi newx a' newb') s3)

  -- application
  App t1 t2 -> debug "typecheck App" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    Pi x a b <- normalizeToPi tt1 t1 (text "in application") (text "operator")

    -- TODO avoid rechecking
    a' <- typecheck a
    b' <- bind x (ResidualVar x, a') $
            typecheck b

    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    normalizeToSame a tt2 (pretty 0 x) t2 (text "in application") (text "formal parameter") (text "actual parameter")

    -- TODO get rid of subst?
    return (MkTypedTerm (App t1' t2') (typedSubst b' x t2'))

  -- abstraction
  Lam x a b -> debug "typecheck Abs" t $ do
    a'@(MkTypedTerm _ s1)  <- typecheck a
    s1' <- normalizeToSort s1 a (text "in lambda abstraction") (text "as type of" <+> pretty 0 x)

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ tb)  <- typecheck newb
      env <- getEnvironment
      let tb' = nbe env (strip tb)
      tb''@(MkTypedTerm _ s2)  <- typecheck tb'
      s2' <- normalizeToSort s2 tb' (text "in lambda abstraction") (text "as type of body")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'

      return (MkTypedTerm (Lam newx a' newb') (MkTypedTerm (Pi newx a' tb'') s3))

  -- Int
  Int i -> debug "typecheck Int" t $ do
    int' <- typecheck (mkConst int)
    return (MkTypedTerm (Int i) int')

  -- IntOp
  IntOp i f t1 t2 -> debug "typecheck IntOp" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToInt tt1 t1' (text "in" <+> pretty 0 i) (text "first argument of" <+> pretty 0 i)
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    result <- normalizeToInt tt2 t2' (text "in" <+> pretty 0 i) (text "second argument of" <+> pretty 0 i)
    return (MkTypedTerm (IntOp i f t1' t2') result)

  -- IfZero
  IfZero t1 t2 t3 -> debug "typecheck IfZero" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToInt tt1 t1' (text "in if0") (text "condition")
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    t3'@(MkTypedTerm _ tt3) <- typecheck t3
    normalizeToSame tt2 tt3 t2' t3' (text "in if0") (text "then branch") (text "else branch")
    return (MkTypedTerm (IfZero t1' t2' t3') tt2)

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos typecheck p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x


typecheckPull :: (MonadEnvironment Name (Binding M) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> m TypedTerm
typecheckPull t = case structure t of
  -- constant
  Const c -> debug "typecheck Const" t $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (MkTypedTerm (Const c) t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  Var x -> debug "typecheckPull Var" t $ do
    xt <- lookupType x
    case xt of
      Just xt -> do
        return (MkTypedTerm (Var x) xt)
      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debug "typecheck Fun" t $ do
    a'@(MkTypedTerm _ s1) <- typecheck a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ s2) <- typecheck newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'
      return (MkTypedTerm (Pi newx a' newb') s3)

  -- application
  App t1 t2 -> debug "typecheck App" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    Pi x a b <- normalizeToPi tt1 t1 (text "in application") (text "operator")

    -- TODO avoid rechecking
    a' <- typecheck a
    b' <- bind x (ResidualVar x, a') $
            typecheck b

    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    normalizeToSame a tt2 (pretty 0 x) t2 (text "in application") (text "formal parameter") (text "actual parameter")

    -- TODO get rid of subst?
    return (MkTypedTerm (App t1' t2') (typedSubst b' x t2'))

  -- abstraction
  Lam x a b -> debug "typecheck Abs" t $ do
    a'@(MkTypedTerm _ s1)  <- typecheck a
    s1' <- normalizeToSort s1 a (text "in lambda abstraction") (text "as type of" <+> pretty 0 x)

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ tb)  <- typecheck newb
      env <- getEnvironment
      let tb' = nbe env (strip tb)
      tb''@(MkTypedTerm _ s2)  <- typecheck tb'
      s2' <- normalizeToSort s2 tb' (text "in lambda abstraction") (text "as type of body")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'

      return (MkTypedTerm (Lam newx a' newb') (MkTypedTerm (Pi newx a' tb'') s3))

  -- Int
  Int i -> debug "typecheck Int" t $ do
    int' <- typecheck (mkConst int)
    return (MkTypedTerm (Int i) int')

  -- IntOp
  IntOp i f t1 t2 -> debug "typecheck IntOp" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToInt tt1 t1' (text "in" <+> pretty 0 i) (text "first argument of" <+> pretty 0 i)
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    result <- normalizeToInt tt2 t2' (text "in" <+> pretty 0 i) (text "second argument of" <+> pretty 0 i)
    return (MkTypedTerm (IntOp i f t1' t2') result)

  -- IfZero
  IfZero t1 t2 t3 -> debug "typecheck IfZero" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToInt tt1 t1' (text "in if0") (text "condition")
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    t3'@(MkTypedTerm _ tt3) <- typecheck t3
    normalizeToSame tt2 tt3 t2' t3' (text "in if0") (text "then branch") (text "else branch")
    return (MkTypedTerm (IfZero t1' t2' t3') tt2)

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos typecheck p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x


-- "Subtyping" relation
-- Question: should I use `normalizeToSame` for checking whether the actual type matches the expected type?
(<:) :: TypedTerm -> Term -> Bool
(<:) _ _ = True


typecheckPush :: (MonadEnvironment Name (Binding M) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> Term -> m TypedTerm
typecheckPush t q = case structure t of
  -- constant
  Const c -> debug "typecheck Const" t $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (MkTypedTerm (Const c) t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  Var x -> debug "typecheckPush Var" t $ do
    xt <- lookupType x
    case xt of
      Just xt -> if xt <: q
                    then do return (MkTypedTerm (Var x) xt)
                    else fail $ "Identifier " ++ show x ++ " has type " ++ show xt ++ " expected " ++ show q
      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debug "typecheck Fun" t $ do
    a'@(MkTypedTerm _ s1) <- typecheck a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ s2) <- typecheck newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'
      return (MkTypedTerm (Pi newx a' newb') s3)

  -- application
  App t1 t2 -> debug "typecheck App" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    Pi x a b <- normalizeToPi tt1 t1 (text "in application") (text "operator")

    -- TODO avoid rechecking
    a' <- typecheck a
    b' <- bind x (ResidualVar x, a') $
            typecheck b

    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    normalizeToSame a tt2 (pretty 0 x) t2 (text "in application") (text "formal parameter") (text "actual parameter")

    -- TODO get rid of subst?
    return (MkTypedTerm (App t1' t2') (typedSubst b' x t2'))

  -- abstraction
  Lam x a b -> debug "typecheck Abs" t $ do
    a'@(MkTypedTerm _ s1)  <- typecheck a
    s1' <- normalizeToSort s1 a (text "in lambda abstraction") (text "as type of" <+> pretty 0 x)

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ tb)  <- typecheck newb
      env <- getEnvironment
      let tb' = nbe env (strip tb)
      tb''@(MkTypedTerm _ s2)  <- typecheck tb'
      s2' <- normalizeToSort s2 tb' (text "in lambda abstraction") (text "as type of body")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'

      return (MkTypedTerm (Lam newx a' newb') (MkTypedTerm (Pi newx a' tb'') s3))

  -- Int
  Int i -> debug "typecheck Int" t $ do
    int' <- typecheck (mkConst int)
    return (MkTypedTerm (Int i) int')

  -- IntOp
  IntOp i f t1 t2 -> debug "typecheck IntOp" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToInt tt1 t1' (text "in" <+> pretty 0 i) (text "first argument of" <+> pretty 0 i)
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    result <- normalizeToInt tt2 t2' (text "in" <+> pretty 0 i) (text "second argument of" <+> pretty 0 i)
    return (MkTypedTerm (IntOp i f t1' t2') result)

  -- IfZero
  IfZero t1 t2 t3 -> debug "typecheck IfZero" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToInt tt1 t1' (text "in if0") (text "condition")
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    t3'@(MkTypedTerm _ tt3) <- typecheck t3
    normalizeToSame tt2 tt3 t2' t3' (text "in if0") (text "then branch") (text "else branch")
    return (MkTypedTerm (IfZero t1' t2' t3') tt2)

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos typecheck p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x
