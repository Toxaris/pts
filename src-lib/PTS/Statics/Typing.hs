{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PatternGuards, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module PTS.Statics.Typing where

import Control.Monad
import Control.Monad.Environment
import Control.Monad.Errors.Class
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.Trans

import Data.Set (Set)
import qualified Data.Set as Set

import PTS.Dynamics
import PTS.Error
import PTS.Instances
import PTS.Options
import PTS.Syntax

import Text.PrettyPrint.HughesPJ hiding (int)
import Text.Show (Show (show))

-- implementation of pure type systems as described on p.98 in Barendregt's "Lambda Calculi with Types"
-- ftp://ftp.cs.kun.nl/pub/CompMath.Found/HBKJ.ps.Z
-- Restrictions: The PTS must be full and functional

-- some ideas regarding conversion to algorithmic style have been taken
-- from van Benthem et al's "Checking algorithms for Pure Type Systems"
-- http://www.springerlink.com/content/f510r63138068632/
--

-- Bidirectional type checking following the first part of:
-- Benjamin C. Pierce and David N. Turner. Local Type Inference. In
-- ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages
-- (POPL), San Diego, California, 1998. Full version in ACM
-- Transactions on Programming Languages and Systems (TOPLAS), 22(1),
-- January 2000, pp. 1-44.
-- Full version: http://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf

import Debug.Trace

-- safe bind
safebind :: MonadEnvironment Name (Binding Eval) m => Name -> Value Eval -> Maybe C -> Term -> (Name -> Term -> m a) -> m a
safebind x t s b f = do
  result <- lookupType x
  case result of
    Nothing -> do
      bind x (Binding False (ResidualVar x) t s) (f x b)
    Just _ -> do
      vars <- keys
      let nx = freshvarl (freevars b `Set.union` vars) x
      bind nx (Binding False (ResidualVar nx) t s) (f nx (subst b x (mkVar nx)))

debug :: (MonadEnvironment Name (Binding Eval) m, MonadLog m) => String -> Term -> m (TypedTerm Eval) -> m (TypedTerm Eval)
debug n t result = do
#ifndef DEBUG_TYPING
  result
#else
  enter n
  ctx <- getEnvironment
  -- log $ "Context: " ++ showCtx [(n, (x, y)) | (n, (_, x, y)) <- ctx]
  log $ "Subject: " ++ showPretty t
  x <- result
  log $ "Result:  " ++ showPretty x
  tx <- liftEval (reify (typeOf x))
  log $ "         : " ++ showPretty tx
  exit
  return x
#endif

debugPush ::
  (MonadEnvironment Name (Binding Eval) m, MonadLog m) =>
  String -> Term -> Value Eval -> m (TypedTerm Eval) -> m (TypedTerm Eval)
debugPush n t q result = do
#ifndef DEBUG_TYPING
  result
#else
  enter n
  -- ctx <- getEnvironment
  -- log $ "Context: " ++ showCtx [(n, (bindingValue b, bindingType b)) | (n, b) <- ctx]
  log $ "Subject: " ++ showPretty t
  q <- liftEval (reify q)
  log $ "Push type: " ++ showPretty q
  x <- result
  log $ "Result:  " ++ showPretty x
  tx <- liftEval (reify (typeOf x))
  log $ "         : " ++ showPretty tx
  exit
  return x
#endif


-- error messages and generic error checking

checkProperType t context info = do
  let t' = typeOf t
  pts <- getLanguage

  case t' of
    Constant s | sorts pts s -> return s
    _ -> do
      t' <- liftEval (reify t')
      prettyFail $ msgNotProperType context info t t'

msgNotProperType context info t t'
  = text "Type Error" <+> context <+> text ": Expected proper type" <+> info <> text "." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The following type is not a sort, so the following term is not a proper type and therefore not valid at this position.")] $$
    sep [text "Term:", nest 2 (pretty 0 t)] $$
    sep [text "Type:", nest 2 (pretty 0 t')])

checkLamBodyHasSort body s1 context = do
  pts <- getLanguage
  s2 <- case sortOf body of
    Just s | sorts pts s -> return s
    Just s -> fail "Internal Error."
    Nothing -> do
      rangeT <- liftEval . reify . typeOf $ body
      prettyFail $ msgBodyNoSort (text "in lambda abstraction") body rangeT

  s3 <- prettyRelations pts s1 s2
  return (s2, s3)

-- adapted from msgNotProperType.
msgBodyNoSort context body t
  = text "Type Error" <+> context <+> text ": Expected function body to have a sort." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The function body does not have a sort, that is, its type has no type. Therefore this term cannot be used as a function body.")] $$
    sep [text "Body:", nest 2 (pretty 0 body)] $$
    sep [text "Type:", nest 2 (pretty 0 t)])

checkIsEquiv s' t' s t context info1 info2 = do
  same <- liftEval (equiv s' t') 
  if same
    then return ()
    else do
      s'' <- liftEval (reify s')
      t'' <- liftEval (reify t')
      prettyFail $ msgNotSame context info1 info2 s t s'' t''

msgNotSame context info1 info2 s t s' t'
  = let (s'', t'') = showDiff 0 (diff (strip s') (strip t'))
     in text "Type Error" <+> context <> text ": Types do not match." $$ nest 2 (
        sep [text "Explanation:", nest 2 (text "The types of the" <+> info1 <+> text "and the" <+> info2 <+> text "should be beta-equivalent.")] $$
        sep [info1 <> text ":", nest 2 (pretty 0 s)] $$
        sep [info2 <> text ":", nest 2 (pretty 0 t)] $$
        sep [text "Type of" <+> info1 <> text ":", nest 2 (text s'')] $$
        sep [text "Type of" <+> info2 <> text ":", nest 2 (text t'')])

liftEval :: MonadEnvironment Name (Binding Eval) m => Eval a -> m a
liftEval action = do
  env <- getEnvironment
  return (runEval env action)

checkIsPi ::
  (MonadReader Options m, MonadErrors Errors m, MonadEnvironment Name (Binding Eval) m) =>
  Value Eval -> TypedTerm Eval -> Doc -> Doc -> m (Value Eval)
checkIsPi v t context info = do
  case v of
    result@(PiType _ _ _ _) ->
      return result
    _ -> do
      t' <- liftEval $ reify v
      prettyFail $ msgNotPi context info t t'

msgNotPi context info t t'
  = text "Type Error" <+> context <> text ": Not a product type." $$ nest 2 (
    sep [text "Explanation:", nest 2 (text "The type of the" <+> info <+> text "should be beta-equivalent to a product type.")] $$
    sep [info <> text ":", nest 2 (pretty 0 t)] $$
    sep [text "Type of" <+> info <> text ":", nest 2 (pretty 0 t')])

prettyRelations pts s1 s2 =
  maybe
    (prettyFail $ let s1t = pretty 0 s1; s2t = pretty 0 s2 in text "in this PTS you can't abstract on expressions of sort" <+> s1t <+> text "in expressions of sort" <+> s2t <+> text ": no relation (" <+> s1t <+> text "," <+> s2t <+> text ")")
    return
    (relations pts s1 s2)

-- Check whether actualType is beta equivalent to expectedType.
-- checkedTerm is only used for error reporting.
bidiExpected ::
  (MonadEnvironment Name (Binding Eval) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) =>
  Value Eval -> Value Eval -> Term -> String -> m ()
bidiExpected actualType expectedType checkedTerm context = do
  env <- getEnvironment
  same <- liftEval (equiv actualType expectedType)
  if same
     then return ()
     else do
       actualType <- liftEval (reify actualType)
       expectedType <- liftEval (reify expectedType)
       let (actual, expected) = showDiff 0 (diff actualType expectedType)
       prettyFail $ text "Type error: we expected two terms to be equivalent but they are not."
                 $$ text context
                 $$ text "Checked term:" <+> pretty 0 checkedTerm
                 $$ text "Actual type:" <+> pretty 0 actualType
                 $$ text "But we expected type:" <+> pretty 0 expectedType
                 $$ text "Difference"
                 $$ text "  actual:  " <+> text actual
                 $$ text "  expected:" <+> text expected


typecheckPull :: (MonadEnvironment Name (Binding Eval) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> m (TypedTerm Eval)
typecheckPull t = case structure t of
  -- constant
  Const c -> debug "typecheckPull Const" t $ do
    pts <- getLanguage
    case axioms pts c of
      Just t  ->  return (mkConst c (Constant t) (axioms pts t))
      _       ->
              if sorts pts c
                then
                  prettyFail $ text "Constant without type:" <+> pretty 0 c
                else
                  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  Var x -> debug "typecheckPull Var" t $ do
#ifdef DEBUG_TYPING
    -- ctx <- getEnvironment
    -- log $ "Context: " ++ showCtx [(n, (bindingValue b, bindingType b)) | (n, b) <- ctx]
#endif
    xt <- lookupType x
    case xt of
      Just (xt, xs) -> do
        return (mkVar x xt xs)
      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi name annotation body s -> debug "typecheckPull Fun" t $ do
    -- check annotation
    annotation <- typecheckPull annotation
    s1 <- checkProperType annotation (text "in product type") (text "as domain")
    domain <- liftEval (eval annotation)

    -- check range
    safebind name domain (Just s1) body $ \name body -> do
      body <- typecheckPull body
      s2 <- checkProperType body (text "in product type") (text "as codomain")

      -- check language support
      pts <- getLanguage
      s3 <- prettyRelations pts s1 s2
      let s4 = axioms pts s3

      -- construct result
      return (mkSortedPi name annotation body (Just s2) (Constant s3) s4)

  -- application
  App operator operand -> debug "typecheckPull App" t $ do
    -- check operator
    operator <- typecheckPull operator
    PiType name domain range kind <- checkIsPi (typeOf operator) operator (text "in application") (text "operator")

    -- check operand
    operand <- typecheckPush operand domain

    -- construct result
    result <- liftEval $ do
      operand <- eval operand
      open range operand
    return (mkApp operator operand result (Just kind))

  -- abstraction
  Lam name annotation body -> debug "typecheckPull Abs" t $ do
    -- check annotation
    annotation  <- typecheckPull annotation
    s1 <- checkProperType annotation
            (text "in lambda abstraction")
            (text "as type of" <+> pretty 0 name)
    domain <- liftEval (eval annotation)

    -- check body
    safebind name domain (Just s1) body $ \name body -> do
      body <- typecheckPull body
      let range = typeOf body

      -- check language support
      (s2, s3) <- checkLamBodyHasSort body s1 (text "in lambda abstraction")
     
      -- construct result
      range <- liftEval (close name domain (Just s1) range)
      return (mkLam name annotation body (PiType name domain range s3) (Just s3))

  -- Int
  Int i -> debug "typecheckPull Int" t $ do
    -- construct integer type
    integerType <- typecheckPull (mkConst int)
    integerType <- liftEval (eval integerType)
    pts <- getLanguage
    let integerSort = axioms pts int
    
    -- construct result
    return (mkInt i integerType integerSort)

  -- IntOp
  IntOp opFunction t1 t2 -> debug "typecheckPull IntOp" t $ do
    -- construct integer type
    integerType <- typecheckPull (mkConst int)
    integerType <- liftEval (eval integerType)
    pts <- getLanguage
    let integerSort = axioms pts int

    -- check operands
    t1 <- typecheckPush t1 integerType
    t2 <- typecheckPush t2 integerType

    -- construct result
    return (mkIntOp opFunction t1 t2 integerType integerSort)

  -- IfZero
  IfZero condition thenBranch elseBranch -> debug "typecheckPull IfZero" t $ do
    -- check condition
    integerType <- typecheckPull (mkConst int)
    integerType <- liftEval (eval integerType)
    condition <- typecheckPush condition integerType

    -- check branches
    --
    -- Then and else branch need to have the same type. We could typecheckPull
    -- the then branch and push the result into the else branch. This might be
    -- faster, but the asymmetry seems weird.
    thenBranch <- typecheckPull thenBranch
    elseBranch <- typecheckPull elseBranch
    checkIsEquiv (typeOf thenBranch) (typeOf elseBranch) thenBranch elseBranch
      (text "in if0") (text "then branch") (text "else branch")

    -- construct result
    return (mkIfZero condition thenBranch elseBranch (typeOf thenBranch) (sortOf thenBranch))

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos typecheckPull p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x

  Infer _ -> do
    prettyFail $ msgPullUnderscore t

msgPullUnderscore t =
  text "Attempted to pull pull a type from an underscore. Most likely there is an underscore at a position where type inference is impossible. The offending underscore is" <+> pretty 0 t


-- Checking rule in bidirectional type checking.
-- First argument (t) is the term to typecheck.
-- Second argument (q) is its expected type.
--   The second argument is of the form (MkTypedTerm type kind) where type is the actual expected
--   type of the first argument and kind is the type of type.
typecheckPush :: (MonadEnvironment Name (Binding Eval) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> Value Eval -> m (TypedTerm Eval)
typecheckPush t q = case structure t of
  -- constant
  Const c -> debugPush "typecheckPush Const" t q $ do
    -- check language support
    pts <- getLanguage
    case axioms pts c of
      Just ct -> do bidiExpected (Constant ct) q t "Attempted to push the wrong type onto a constant."
                    return (mkConst c (Constant ct) (axioms pts ct))
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  Var x -> debugPush "typecheckPush Var" t q $ do
    xt <- lookupType x
    case xt of
      Just (xt, xs) -> do bidiExpected xt q t "A variable from the environment has an unexpected type."
                          return (mkVar x xt xs)
      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi name annotation body _ -> debugPush "typecheckPush Fun" t q $ do
    -- check annotation
    annotation <- typecheckPull annotation
    s1 <- checkProperType annotation (text "in product type") (text "as domain")
    domain <- liftEval (eval annotation)

    -- check body
    safebind name domain (Just s1) body $ \name body -> do
      body <- typecheckPull body
      s2 <- checkProperType body (text "in product type") (text "as codomain")

      -- check language support
      pts <- getLanguage
      s3 <- prettyRelations pts s1 s2
      let s4 = axioms pts s3

      -- construct result
      return (mkSortedPi name annotation body (Just s2) (Constant s3) s4)

  -- application
  App operator operand -> debugPush "typecheckPush App" t q $ do
    -- check operator
    operator <- typecheckPull operator
    PiType name domain range sort <- checkIsPi (typeOf operator) operator (text "in application") (text "operator")

    -- check operand
    operand <- typecheckPush operand domain

    -- check that expected type matches actual type
    env <- getEnvironment
    result <- liftEval $ do
      operand <- eval operand
      open range operand
    bidiExpected result q t "The function has an unexpected codomain."

    -- construct result
    return (mkApp operator operand q (Just sort))

  -- abstraction
  Lam name annotation body -> debugPush "typecheckPush Abs" t q $ do
    -- check that expected type is a function type
    (domain, range) <- case q of
      PiType _ domain range _ -> return (domain, range)
      _ -> do
        expected <- liftEval (reify q)
        prettyFail $ text "Expected a function type for" <+> pretty 0 t <+> text "but got" <+> pretty 0 expected

    -- infer annotation if missing
    annotation <- case structure' annotation of
      Infer _ -> liftEval (reify domain)
      _ -> return annotation

    -- check annotation
    annotation <- typecheckPull annotation
    s1 <- checkProperType annotation (text "in lambda abstraction") (text "as domain")
    annotatedDomain <- liftEval (eval annotation)
    bidiExpected annotatedDomain domain t
      "In a lambda abstraction, the declared argument type does not match the expected domain."
  
    -- check body
    safebind name domain (Just s1) body $ \name body -> do
      result <- liftEval (open range (ResidualVar name))
      body <- typecheckPush body result
      return (mkLam name annotation body q (sortOf body))

  -- Int
  Int i -> debugPush "typecheckPush Int" t q $ do
    -- construct integer type
    integerType <- typecheckPull (mkConst int)
    integerType <- liftEval (eval integerType)
    pts <- getLanguage
    let integerSort = axioms pts int
    
    -- check that expected type is integer type
    bidiExpected integerType q t "An integer literal is not expected to be one."

    -- construct result
    return (mkInt i integerType integerSort)
 
  -- IntOp
  IntOp op t1 t2 -> debugPush "typecheckPush IntOp" t q $ do
    -- construct integer type
    integerType <- typecheckPull (mkConst int)
    integerType <- liftEval (eval integerType)
    pts <- getLanguage
    let integerSort = axioms pts int

    -- check that expected type is integer type
    bidiExpected integerType q t "An integer operation is not expected to be one."

    -- check operands
    t1 <- typecheckPush t1 integerType
    t2 <- typecheckPush t2 integerType

    -- construct result
    return (mkIntOp op t1 t2 integerType integerSort)

  -- IfZero
  IfZero t1 t2 t3 -> debugPush "typecheckPush IfZero" t q $ do
    -- construct integer type
    integerType <- typecheckPull (mkConst int)
    integerType <- liftEval (eval integerType)

    -- check condition
    t1 <- typecheckPush t1 integerType

    -- check operands
    t2 <- typecheckPush t2 q
    t3 <- typecheckPush t3 q

    -- construct result
    return (mkIfZero t1 t2 t3 q (sortOf t2))

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos (\ t -> typecheckPush t q) p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x

  Infer _ -> do
    expected <- liftEval (reify q)
    prettyFail $ msgPushUnderscore t expected

msgPushUnderscore t expected =
  text "Attempted to push a type on an underscore. Most likely there is an underscore at a position where type inference is impossible. The offending underscore is" <+> pretty 0 t  <+> text "which is supposed to have type" <+> pretty 0 expected

typecheckPushUntyped term expected = do
  expected <- typecheckPull expected
  expected <- liftEval (eval expected)
  typecheckPush term expected
