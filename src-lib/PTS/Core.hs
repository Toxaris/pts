{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PatternGuards, FlexibleInstances #-}
module PTS.Core where

import Prelude ()

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

-- alpha equivalence
instance Eq Term where
  t == t' = structure t == structure t'

instance Eq (TermStructure Term) where
  (Var x) == (Var y) = x == y
  (Nat i) == (Nat j) = i == j
  (NatOp i _ t1 t2) == (NatOp i' _ t1' t2') = i == i' && t1 == t1' && t2 == t2'
  (IfZero t1 t2 t3) == (IfZero t1' t2' t3') = t1==t1' && t2==t2' && t3==t3'
  (Const c) == (Const c') = c == c'
  (App t1 t2) == (App t1' t2') = t1 == t1' && t2 == t2'

  (Lam x1 q1 b1) == (Lam x2 q2 b2) = q1 == q2 && b1' == b2' where
    (_, b1', b2') = freshCommonVar x1 x2 b1 b2

  (Pi x1 q1 b1) == (Pi x2 q2 b2) = q1 == q2 && b1' == b2' where
    (_, b1', b2') = freshCommonVar x1 x2 b1 b2

  Pos _ t == Pos _ t' = t == t'
  _ == _ = False

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

equiv :: Term -> Term -> Bool
equiv t1 t2 = (normalform t1) == (normalform t2)

check :: Monad m => Bool -> String -> m ()
check True _ = return ()
check False s = fail s

checkMaybe :: Monad m => Maybe a -> String -> m ()
checkMaybe (Just _) _ = return ()
checkMaybe Nothing s = fail s

ndots :: Int -> String
ndots n = replicate n '.'

-- mytrace d ctx (Const x) | x == star = False
-- mytrace d ctx t =
--   trace ((show d) ++ (ndots d) ++ (show ctx) ++ " |- "++(show t) ++ " : ???") False

-- safe bind
safebind :: MonadEnvironment Name Term m => Name -> Term -> Term -> (Name -> Term -> m a) -> m a
safebind x t b f = do
  result <- lookup x
  case result of
    Nothing -> do
      bind x t (f x b)
    Just _ -> do
      vars <- keys
      let nx = freshvarl (freevars b `Set.union` vars) x
      bind nx t (f nx (subst b x (mkVar nx)))

debug :: (MonadEnvironment Name Term m, MonadLog m) => String -> Term -> m Term -> m Term
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

  case structure (normalform t') of
    Const s | sorts pts s  ->  return s
    _                      ->  prettyFail $ msgNotProperType context info t t'

msgNotProperType context info t t'
  = text "Type Error" <+> context <+> text ": Expected proper type" <+> info <> text "." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type is not a sort, so the term is not a proper type and therefore not valid at this position.")] $$
    sep [text "Term:", nest 2 (pretty 0 t)] $$
    sep [text "Type:", nest 2 (pretty 0 t')])

normalizeToSame s' t' s t context info1 info2
  = let s'' = normalform s'
        t'' = normalform t'
     in if s'' == t''
        then return s''
        else prettyFail $ msgNotSame context info1 info2 s t s'' t''

msgNotSame context info1 info2 s t s' t'
  = let (s'', t'') = showDiff 0 (diff s' t')
     in text "Type Error" <+> context <> text ": Types do not match." $$ nest 2 (
        sep [text "Explanation:", nest 2 (text "The types of the" <+> info1 <+> text "and the" <+> info2 <+> text "should be beta-equivalent.")] $$
        sep [info1 <> text ":", nest 2 (pretty 0 s)] $$
        sep [info2 <> text ":", nest 2 (pretty 0 t)] $$
        sep [text "Type of" <+> info1 <> text ":", nest 2 (text s'')] $$
        sep [text "Type of" <+> info2 <> text ":", nest 2 (text t'')])

normalizeToNat :: (MonadReader Options m, MonadErrors Errors m) => Term -> Term -> Doc -> Doc -> m Term
normalizeToNat t' t context info = do
  let t'' = normalform t'
  if t'' == mkConst nat
    then return (mkConst nat)
    else prettyFail $ msgNotNat context info t t'' nat

msgNotNat context info t t' nat
  = text "Type Error" <+> context <> text ": Types do not match." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type of the" <+> info <+> text "should be beta-equivalent to" <+> pretty 0 (mkConst nat) <> text ".")] $$
    sep [info <> text ":", nest 2 (pretty 0 t)] $$
    sep [text "Type of" <+> info <> text ":", nest 2 (pretty 0 t')] $$
    sep [text "Expected Type:" <+> nest 2 (pretty 0 (mkConst nat))])

normalizeToPi t' t context info
  =  let t'' = normalform t' in
       case structure t'' of
         result@(Pi _ _ _)  ->  return result
         _                  ->  prettyFail $ msgNotPi context info t t''

msgNotPi context info t t'
  = text "Type Error" <+> context <> text ": Not a product type." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type of the" <+> info <+> text "should be beta-equivalent to a product type.")] $$
    sep [info <> text ":", nest 2 (pretty 0 t)] $$
    sep [text "Type of" <+> info <> text ":", nest 2 (pretty 0 t')])

typecheck :: (MonadEnvironment Name Term m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> m Term
--typecheck p d ctx t | mytrace d ctx t = undefined

typecheck t = case structure t of
  -- constant
  Const c -> debug "TypeConst" t $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (mkConst t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  -- start
  Var x -> debug "TypeVar" t $ do
    xt <- lookup x
    case xt of
      Just xt -> do
        s <- typecheck xt
        normalizeToSort s xt (text "in variable") (text "as type of" <+> pretty 0 x)
        return xt

      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debug "TypeFun" t $ do
    s1 <- typecheck a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a b $ \newx newb -> do
      s2 <- typecheck newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      case relations pts s1' s2' of
        Just s -> return (mkConst s)
        Nothing -> prettyFail $ text "no relation" <+> pretty 0 s1' <+> text ":" <+> pretty 0 s2'

  -- application
  App t1 t2 -> debug "TypeApp" t $ do
    tt1 <- typecheck t1
    Pi x a b <- normalizeToPi tt1 t1 (text "in application") (text "operator")

    tt2 <- typecheck t2
    normalizeToSame a tt2 (pretty 0 x) t2 (text "in application") (text "formal parameter") (text "actual parameter")

    return (subst b x t2)

  -- abstraction
  Lam x a b -> debug "TypeAbs" t $ do
    s1  <- typecheck a
    s1' <- normalizeToSort s1 a (text "in lambda abstraction") (text "as type of" <+> pretty 0 x)

    safebind x a b $ \newx newb -> do
      tb  <- typecheck newb
      let tb' = normalform tb
      s2  <- typecheck tb'
      s2' <- normalizeToSort s2 tb' (text "in lambda abstraction") (text "as type of body")

      pts <- asks optInstance
      maybe (fail $ "no relation " ++ show s1 ++ " : " ++ show s2) return (relations pts s1' s2')

      return (mkPi newx a tb')

  -- Nat
  Nat i -> debug "TypeNat" t $ do
    return (mkConst nat)

  -- NatOp
  NatOp i f t1 t2 -> debug "TypeNatOp" t $ do
    tt1 <- typecheck t1
    normalizeToNat tt1 t1 (text "in" <+> pretty 0 i) (text "first argument of" <+> pretty 0 i)
    tt2 <- typecheck t2
    normalizeToNat tt2 t2 (text "in" <+> pretty 0 i) (text "second argument of" <+> pretty 0 i)

  -- IfZero
  IfZero t1 t2 t3 -> debug "TypeIfZero" t $ do
    tt1 <- typecheck t1
    normalizeToNat tt1 t1 (text "in if0") (text "condition")
    tt2 <- typecheck t2
    tt3 <- typecheck t3
    normalizeToSame tt2 tt3 t2 t3 (text "in if0") (text "then branch") (text "else branch")

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- handlePos typecheck p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x
