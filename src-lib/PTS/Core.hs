{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PatternGuards, FlexibleInstances #-}
module PTS.Core where

import Prelude (fst, snd, String)

import Control.Monad
import Control.Monad.Environment
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.Trans

import Data.Bool (Bool (False, True), (&&))
import Data.Char
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Int (Int)
import Data.List (map, null, replicate, (++))
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (fst)

import Parametric.Error
import Parametric.Pretty

import PTS.Algebra
import PTS.AST
import PTS.Diff
import PTS.Instances
import PTS.Normalisation
import PTS.Options
import PTS.Pretty
import PTS.Substitution
import PTS.Evaluation
import PTS.Constants

import Text.Show (Show (show))

import Tools.Errors.Class

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
  Nat _      ->  True
  _          ->  False

check :: Monad m => Bool -> String -> m ()
check True _ = return ()
check False s = fail s

checkMaybe :: Monad m => Maybe a -> String -> m ()
checkMaybe (Just _) _ = return ()
checkMaybe Nothing s = fail s

ndots :: Int -> String
ndots n = replicate n '.'

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
safebind :: MonadEnvironment Name (Value, TypedTerm) m => Name -> TypedTerm -> Term -> (Name -> Term -> m a) -> m a
safebind x t b f = do
  result <- lookupType x
  case result of
    Nothing -> do
      bind x (ResidualVar x, t) (f x b)
    Just _ -> do
      vars <- keys
      let nx = freshvarl (freevars b `Set.union` vars) x
      bind nx (ResidualVar nx, t) (f nx (subst b x (mkVar nx)))

debug :: (MonadEnvironment Name (Value, TypedTerm) m, MonadLog m) => String -> Term -> m TypedTerm -> m TypedTerm
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

normalizeToNat :: (MonadLog m, Functor m, MonadEnvironment Name (Value, TypedTerm) m, MonadReader Options m, MonadErrors Errors m) => TypedTerm -> TypedTerm -> Doc -> Doc -> m TypedTerm
normalizeToNat t' t context info = do
  let stripT' = stripT'
  env <- getEnvironment
  if equivTerm env stripT' (mkConst nat)
    then typecheck (mkConst nat)
    else let t'' = nbe env stripT'
          in prettyFail $ msgNotNat context info t t'' nat

msgNotNat context info t t' nat
  = text "Type Error" <+> context <> text ": Types do not match." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type of the" <+> info <+> text "should be beta-equivalent to" <+> pretty 0 (mkConst nat) <> text ".")] $$
    sep [info <> text ":", nest 2 (pretty 0 t)] $$
    sep [text "Type of" <+> info <> text ":", nest 2 (pretty 0 t')] $$
    sep [text "Expected Type:" <+> nest 2 (pretty 0 (mkConst nat))])

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

typecheck :: (MonadEnvironment Name (Value, TypedTerm) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> m TypedTerm
--typecheck p d ctx t | mytrace d ctx t = undefined

typecheck t = case structure t of
  -- constant
  Const c -> debug "TypeConst" t $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (MkTypedTerm (Const c) t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  -- start
  Var x -> debug "TypeVar" t $ do
    xt <- lookupType x
    case xt of
      Just xt -> do
        -- s <- typecheck xt
        -- normalizeToSort s xt (text "in variable") (text "as type of" <+> pretty 0 x)
        return (MkTypedTerm (Var x) xt)

      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debug "TypeFun" t $ do
    a'@(MkTypedTerm _ s1) <- typecheck a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ s2) <- typecheck newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      case relations pts s1' s2' of
        Just s -> return (MkTypedTerm (Pi newx a' newb') s)
        Nothing -> prettyFail $ text "no relation" <+> pretty 0 s1' <+> text ":" <+> pretty 0 s2'

  -- application
  App t1 t2 -> debug "TypeApp" t $ do
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
  Lam x a b -> debug "TypeAbs" t $ do
    a'@(MkTypedTerm _ s1)  <- typecheck a
    s1' <- normalizeToSort s1 a (text "in lambda abstraction") (text "as type of" <+> pretty 0 x)

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ tb)  <- typecheck newb
      env <- getEnvironment
      let tb' = nbe env (strip tb)
      tb''@(MkTypedTerm _ s2)  <- typecheck tb'
      s2' <- normalizeToSort s2 tb' (text "in lambda abstraction") (text "as type of body")

      pts <- asks optInstance
      s3 <- maybe (fail $ "no relation " ++ show s1 ++ " : " ++ show s2) return (relations pts s1' s2')

      return (MkTypedTerm (Lam newx a' newb') (MkTypedTerm (Pi newx a' tb'') s3))

  -- Nat
  Nat i -> debug "TypeNat" t $ do
    nat' <- typecheck (mkConst nat)
    return (MkTypedTerm (Nat i) nat')

  -- NatOp
  NatOp i f t1 t2 -> debug "TypeNatOp" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToNat tt1 t1' (text "in" <+> pretty 0 i) (text "first argument of" <+> pretty 0 i)
    t2'@(MkTypedTerm _ tt2) <- typecheck t2
    result <- normalizeToNat tt2 t2' (text "in" <+> pretty 0 i) (text "second argument of" <+> pretty 0 i)
    return (MkTypedTerm (NatOp i f t1' t2') result)

  -- IfZero
  IfZero t1 t2 t3 -> debug "TypeIfZero" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheck t1
    normalizeToNat tt1 t1' (text "in if0") (text "condition")
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
