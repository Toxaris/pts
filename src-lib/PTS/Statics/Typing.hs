{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PatternGuards, FlexibleInstances #-}
module PTS.Statics.Typing where

import Prelude (fst, snd, String)

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
--  log $ "Context: " ++ showCtx ctx
  log $ "Subject: " ++ show t
  x <- result
  log $ "Result:  " ++ show x
  log $ "         : " ++ show (typeOf x)
  exit
  return x

debugPush :: (MonadEnvironment Name (Binding M) m, MonadLog m) => String -> Term -> TypedTerm -> m TypedTerm -> m TypedTerm
debugPush n t q result = do
  enter n
  ctx <- getEnvironment
  log $ "Context: " ++ showCtx ctx
  log $ "Subject: " ++ show t
  log $ "Push type: " ++ show q
  x <- result
  log $ "Result:  " ++ show x
  log $ "         : " ++ show (typeOf x)
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
--  _ -> do prettyFail $ text "plain typecheck called. This should not happen anymore."
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
-- typecheckPull t = do masterResult <- typecheck t
--                      result <- typecheckPull' t
--                      bidiExpected result masterResult t "typecheckPull sanity check"
--                      return result


typecheckPull t = case structure t of
  -- constant
  Const c -> debug "typecheckPull Const" t $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (MkTypedTerm (Const c) t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  Var x -> debug "typecheckPull Var" t $ do
    ctx <- getEnvironment
    log $ "Context: " ++ showCtx ctx
    xt <- lookupType x
    case xt of
      Just xt -> do
        return (MkTypedTerm (Var x) xt)
      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debug "typecheckPull Fun" t $ do
    a'@(MkTypedTerm _ s1) <- typecheckPull a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ s2) <- typecheckPull newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'
      return (MkTypedTerm (Pi newx a' newb') s3)

  -- application
  App t1 t2 -> debug "typecheckPull App" t $ do
    t1'@(MkTypedTerm _ tt1) <- typecheckPull t1
    Pi x a b <- normalizeToPi tt1 t1 (text "in application") (text "operator")

    -- TODO avoid rechecking
    a' <- typecheckPull a
    b' <- bind x (ResidualVar x, a') $
            typecheckPull b

    -- in t1 t2
    -- t1 is a function from a -> b, so
    -- t2 better be of type a
    -- check that the argument t2 actually has the type we expect
    t2'@(MkTypedTerm _ tt2) <- typecheckPush t2 a'

    -- TODO get rid of subst?
    return (MkTypedTerm (App t1' t2') (typedSubst b' x t2'))

  -- abstraction
  Lam x a b -> debug "typecheckPull Abs" t $ do
    env2 <- getEnvironment
    a'@(MkTypedTerm _ s1)  <- typecheckPull (nbe env2 a)
    s1' <- normalizeToSort s1 a (text "in lambda abstraction") (text "as type of" <+> pretty 0 x)

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ tb)  <- typecheckPull newb
      env <- getEnvironment
      let tb' = nbe env (strip tb)
      tb''@(MkTypedTerm _ s2)  <- typecheckPull tb'
      s2' <- normalizeToSort s2 tb' (text "in lambda abstraction") (text "as type of body")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'

      return (MkTypedTerm (Lam newx a' newb') (MkTypedTerm (Pi newx a' tb'') s3))

  -- Int
  Int i -> debug "typecheckPull Int" t $ do
    int' <- typecheckPull (mkConst int)
    return (MkTypedTerm (Int i) int')

  -- IntOp
  IntOp opName opFunction t1 t2 -> debug "typecheckPull IntOp" t $ do
    -- The type integers have. This might depend on the PTS instance, not so sure...
    integerType <- typecheckPull (mkConst int)
    -- Both arguments to any IntOp have to be Ints, so typecheckPush an Int in there.
    t1'@(MkTypedTerm _ tt1) <- typecheckPush t1 integerType
    t2'@(MkTypedTerm _ tt2) <- typecheckPush t2 integerType
    -- If this worked we return the IntOp with typed arguments and type Int. 
    return (MkTypedTerm (IntOp opName opFunction t1' t2') integerType)

  -- IfZero
  IfZero condition thenTerm elseTerm -> debug "typecheckPull IfZero" t $ do
    -- Condition needs to be of type integer (think boolean in a real if).
    integerType <- typecheckPull (mkConst int)
    typedCondition <- typecheckPush condition integerType
    -- Then and else branch need to have the same type. We could typecheckPull
    -- the then branch and push the result into the else branch. This might be
    -- faster, but the asymmetry seems weird.
    typedThen@(MkTypedTerm _ thenType) <- typecheckPull thenTerm
    typedElse@(MkTypedTerm _ elseType) <- typecheckPull elseTerm
    normalizeToSame thenType elseType typedThen typedElse (text "in if0") (text "then branch") (text "else branch")
    return (MkTypedTerm (IfZero typedCondition typedThen typedElse) thenType)

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos typecheckPull p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x

  Infer _ -> do
    prettyFail $ text "Attempted to pull pull a type from an underscore. Most likely there is an underscore at a position where type inference is impossible. The offending underscore is" <+> pretty 0 t

-- Check whether actualType is beta equivalent to expectedType.
-- checkedTerm is only used for error reporting.
bidiExpected :: (MonadEnvironment Name (Binding M) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => TypedTerm -> TypedTerm -> Term -> String -> m ()
bidiExpected actualType expectedType checkedTerm context = do
  let actualType' = strip actualType
  let expectedType' = strip expectedType
  env <- getEnvironment
  if equivTerm env actualType' expectedType'
     then return ()
     else do
       let (actual, expected) = showDiff 0 (diff actualType' expectedType')
       prettyFail $ text "Type error: we expected two terms to be equivalent but they are not."
                 $$ text context
                 $$ text "Checked term:" <+> pretty 0 checkedTerm
                 $$ text "Actual type:" <+> pretty 0 actualType
                 $$ text "But we expected type:" <+> pretty 0 expectedType
                 $$ text "Difference"
                 $$ text "  actual:  " <+> text actual
                 $$ text "  expected:" <+> text expected


-- Checking rule in bidirectional type checking.
-- First argument (t) is the term to typecheck.
-- Second argument (q) is its expected type.
--   The second argument is of the form (MkTypedTerm type kind) where type is the actual expected
--   type of the first argument and kind is the type of type.
typecheckPush :: (MonadEnvironment Name (Binding M) m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> TypedTerm -> m TypedTerm
-- typecheckPush t q = do masterResult <- typecheck t
--                        result <- typecheckPush' t q
--                        bidiExpected result masterResult t "typecheckPush sanity check"
--                        return result

typecheckPush t q = case structure t of
  -- constant
  Const c -> debugPush "typecheckPush Const" t q $ do
    pts <- asks optInstance
    case axioms pts c of
      Just t  ->  return (MkTypedTerm (Const c) t)
      _       ->  prettyFail $ text "Unknown constant:" <+> pretty 0 c

  Var x -> debugPush "typecheckPush Var" t q $ do
    xt <- lookupType x
    case xt of
      Just xt -> do bidiExpected xt q t "A variable from the environment has an unexpected type."
                    return (MkTypedTerm (Var x) xt)
      Nothing ->
        fail $ "Unbound identifier: " ++ show x

  -- product
  Pi x a b -> debugPush "typecheckPush Fun" t q $ do
    -- TODO This whole case is rather stupid at the moment. We could
    -- (maybe) call prettyRelations with s1 and s3 and get the s2 out
    -- of it. Maybe.
    a'@(MkTypedTerm _ s1) <- typecheckPull a
    s1' <- normalizeToSort s1 a (text "in product type") (text "as domain")

    safebind x a' b $ \newx newb -> do
      newb'@(MkTypedTerm _ s2) <- typecheckPull newb
      s2' <- normalizeToSort s2 newb (text "in product type") (text "as codomain")

      pts <- asks optInstance
      s3 <- prettyRelations pts s1' s2'
      return (MkTypedTerm (Pi newx a' newb') s3)

  -- application
  App f a -> debugPush "typecheckPush App" t q $ do
    -- 3 Things to do here:
    -- 1. Pull the function type and get a result type.
    -- 2. Push the domain of the function type (A) into the argument (a).
    -- 3. In the codomain (R) substitute the formal parameter (a') for the actual argument value (a) and check that this matches the expected type (B).

    -- Γ ⊢ f  pull  (Pi a' A -> R)    Γ ⊢ a  push  A    R[a'/a] ≡β B
    -- -------------------------------------------------------------
    -- Γ ⊢ f a  push  B

    -- 1.
    typedFunction <- typecheckPull f
    case structure' (typeOf typedFunction) of
      Pi a' typeA typeR -> do
        -- 2.
        typedArgument <- typecheckPush a typeA
        -- 3. (B is actually the pushed term q)
        bidiExpected (typedSubst typeR a' typedArgument) q t "The function has an unexpected codomain."
        return (MkTypedTerm (App typedFunction typedArgument) q)
      _ -> do
        prettyFail $ text "In type checking a push application, found the term in function position to not be of a function type."
                  $$ text "Term in function position:" <+> pretty 0 f
                  $$ text "Found type:" <+> pretty 0 (typeOf typedFunction)
                  $$ text "This might or might not be a bug in the type checker."

  -- abstraction
  Lam argumentName declaredDomain body -> debugPush "typecheckPush Abs" t q $ case structure' declaredDomain of
    -- Domain is not actually declared but needs to be inferred.
    Infer n -> do
      case structure' q of
        expectedFunctionType@(Pi _ expectedDomain expectedCodomain) -> do
          let argumentType = expectedDomain
          safebind argumentName argumentType body $ \newArgumentName newBody -> do
            typedNewBody@(MkTypedTerm _ newCodomain@(MkTypedTerm _ kind)) <- typecheckPush newBody expectedCodomain
            return (MkTypedTerm (Lam newArgumentName argumentType typedNewBody)
                                (MkTypedTerm (Pi newArgumentName expectedDomain newCodomain) kind))
        _ -> prettyFail $ text "Expected a function type for" <+> pretty 0 t <+> text "but got" <+> pretty 0 q
    -- Domain is declared, check that it is correct.
    _       -> do
      env <- getEnvironment
      argumentType <- typecheckPull (nbe env declaredDomain)
      -- Check whether we actually expect a lambda abstraction, that is a Pi-type. Fail immediately otherwise.
      case structure' q of
        expectedFunctionType@(Pi _ expectedDomain expectedCodomain) -> do
          -- Need to check two things here:
          --  1. does the declared argument type match the expected domain
          bidiExpected argumentType expectedDomain t "In a lambda abstraction, the declared argument type does not match the expected domain."
          --  2. does the body have the type of the expected codomain (typecheckPush in extended environment)
          safebind argumentName argumentType body $ \newArgumentName newBody -> do
            typedNewBody@(MkTypedTerm _ newCodomain@(MkTypedTerm _ kind)) <- typecheckPush newBody expectedCodomain
            -- Both succeed, so return the term (=lambda) with its type (=pi).
            -- This is a bit more cumbersome than expected, we actually want to just return a (MkTypedTerm t q).
            -- But the returned t is t with a new argument name and body,
            -- and the returned q is q with a new argument name and new codomain.
            return (MkTypedTerm (Lam newArgumentName argumentType typedNewBody)
                                (MkTypedTerm (Pi newArgumentName expectedDomain newCodomain) kind))
        _ -> prettyFail $ text "Expected" <+> pretty 0 q <+> text "but found" <+> pretty 0 t


  -- Int
  Int i -> debugPush "typecheckPush Int" t q $ do
    int' <- typecheckPull (mkConst int)
    bidiExpected int' q t "An integer literal is not expected to be one."
    return (MkTypedTerm (Int i) int')

  -- IntOp
  IntOp opName opFunction t1 t2 -> debugPush "typecheckPush IntOp" t q $ do
    integerType <- typecheckPull (mkConst int)
    -- Check that we actually expect an int
    bidiExpected integerType q t "An integer operation is not expected to be one."
    -- Ok, we're in the right case. Descend into arguments.
    typedT1 <- typecheckPush t1 integerType
    typedT2 <- typecheckPush t2 integerType
    -- Arguments are Integers as well, return.
    return (MkTypedTerm (IntOp opName opFunction typedT1 typedT2) integerType)

  -- IfZero
  IfZero t1 t2 t3 -> debugPush "typecheckPush IfZero" t q $ do
    -- Because of normalization, we only reach this case if the
    -- condition is not a constant, so this whole IfZero did not
    -- normalize away. Therefore the then and else branches need to be
    -- of the same type.
    -- Or does this type have to be q? I think I'm still confused about normalization.
    integerType <- typecheckPull (mkConst int)
    t1'@(MkTypedTerm _ tt1) <- typecheckPush t1 integerType
    normalizeToInt tt1 t1' (text "in if0") (text "condition")
    t2'@(MkTypedTerm _ tt2) <- typecheckPush t2 q
    t3'@(MkTypedTerm _ tt3) <- typecheckPush t3 q
    normalizeToSame tt2 tt3 t2' t3' (text "in if0") (text "then branch") (text "else branch")
    return (MkTypedTerm (IfZero t1' t2' t3') tt2)

  -- Position information
  Pos p t -> do
    -- trace ("Start: "++(show ctx) ++ " |- " ++ (show t) ++ " : ???") (return ())
    x <- typedHandlePos (\ t -> typecheckPush t q) p t
    -- trace ("End: "++(show ctx) ++ " |- " ++ (show t) ++ " : "++(show x)) (return ())
    return x

  Infer _ -> do
    prettyFail $ text "Attempted to push a type on an underscore. Most likely there is an underscore at a position where type inference is impossible. The offending underscore is" <+> pretty 0 t <+> text "which is supposed to have type" <+> pretty 0 q
