{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
module PTS.Dynamics.Evaluation where

import Control.Applicative hiding (Const)
import Control.Monad.Environment
import Control.Monad.State

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

import PTS.Dynamics.Binding
import PTS.Dynamics.Value
import PTS.Syntax
import PTS.Syntax.Names

newtype Eval a = Eval (EnvironmentT Name (Binding Eval) (State NamesMap) a)
  deriving (Functor, Monad, MonadState NamesMap, MonadEnvironment Name (Binding Eval))

runEval :: Bindings Eval -> Eval a -> a
runEval env (Eval p) = evalState (runEnvironmentT p env) (envToNamesMap env)

close :: Name -> Value Eval -> Maybe C -> Value Eval -> Eval (Function Eval)
close name typ sort value = do
  term <- reify value
  abstract (\arg -> do
    withEnvironment [] $ do
      bind name (Binding False arg typ sort) $ do
        eval term)

equivTerm :: Bindings Eval -> TypedTerm Eval -> TypedTerm Eval -> Bool
equivTerm env t1 t2 = runEval env $ do
  v1 <- eval t1
  v2 <- eval t2
  equiv v1 v2

equiv :: Value Eval -> Value Eval -> Eval Bool
equiv (Function n v1 f) (Function _ v1' f') = do
  r1 <- equiv v1 v1'
  n'   <- fresh n
  v2   <- open f   (ResidualVar n')
  v2'  <- open f'  (ResidualVar n')
  r2 <- equiv v2 v2'
  return (r1 && r2)
equiv (Number n) (Number n') = do
  return (n == n')
equiv (Constant c) (Constant c') = do
  return (c == c')
equiv (PiType n v1 f s1) (PiType _ v1' f' s2) = do
  r1   <- equiv v1 v1'
  n'   <- fresh n
  v2   <- open f   (ResidualVar n')
  v2'  <- open f'  (ResidualVar n')
  r2   <- equiv v2 v2'
  return (r1 && r2 && s1 == s2)
equiv (ResidualIntOp op v1 v2) (ResidualIntOp op' v1' v2') = do
  let r1 = op == op'
  r2 <- equiv v1 v1'
  r3 <- equiv v2 v2'
  return (r1 && r2 && r3)
equiv (ResidualIfZero v1 v2 v3) (ResidualIfZero v1' v2' v3') = do
  r1 <- equiv v1 v1'
  r2 <- equiv v2 v2'
  r3 <- equiv v3 v3'
  return (r1 && r2 && r3)
equiv (ResidualVar n) (ResidualVar n') = do
  return (n == n')
equiv (ResidualApp v1 v2) (ResidualApp v1' v2') = do
  r1 <- equiv v1 v1'
  r2 <- equiv v2 v2'
  return (r1 && r2)
equiv _ _ = do
  return False

nbe :: Structure t => Bindings Eval -> t -> Term
nbe env e = runEval env $ do
  v   <- eval e
  e'  <- reify v
  return e'

fresh :: Name -> Eval Name
fresh n = do
  ns <- get
  let (n', ns') = freshvarlMap ns n
  put ns'
  return n'

reify :: Value Eval -> Eval Term
reify (Function n v1 f) = do
  e1 <- reify v1
  n' <- fresh n
  v2 <- open f (ResidualVar n')
  e2 <- reify v2
  return (mkLam n' e1 e2)
reify (Number n) = do
  return (mkInt n)
reify (Constant c) = do
  return (mkConst c)
reify (PiType n v1 f s) = do
  e1 <- reify v1
  n' <- fresh n
  v2 <- open f (ResidualVar n')
  e2 <- reify v2
  return (mkSortedPi n' e1 e2 (Just s))
reify (ResidualIntOp op v1 v2) = do
  e1 <- reify v1
  e2 <- reify v2
  return (mkIntOp op e1 e2)
reify (ResidualIfZero v1 v2 v3) = do
  e1 <- reify v1
  e2 <- reify v2
  e3 <- reify v3
  return (mkIfZero e1 e2 e3)
reify (ResidualVar n) = do
  return (mkVar n)
reify (ResidualApp v1 v2) = do
  e1 <- reify v1
  e2 <- reify v2
  return (mkApp e1 e2)

evalTerm :: Bindings Eval -> TypedTerm Eval -> Value Eval
evalTerm env t = runEval env $ do
  eval t

apply :: Monad m => Value m -> Value m -> m (Value m)
apply (Function n t f) v2 = open f v2
apply v1 v2 = return (ResidualApp v1 v2)

eval :: Structure t => t -> Eval (Value Eval)
eval t = case structure t of
  Int n -> do
    return (Number n)
  IntOp op e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
      (Number n1, Number n2) | Just n3 <- evalOp op n1 n2 -> do
        return (Number n3)
      _ -> return (ResidualIntOp op v1 v2)
  IfZero e1 e2 e3 -> do
    v1 <- eval e1
    case v1 of
      Number 0 -> do
        eval e2
      Number _ -> do
        eval e3
      _ -> do
        v2   <- eval e2
        v3   <- eval e3
        return (ResidualIfZero v1 v2 v3)
  Var n -> do
    binding <- lookupValue n
    case binding of
      Just v -> return v
      Nothing -> return (ResidualVar n)
  Const c -> do
    return (Constant c)
  App e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    apply v1 v2
  Lam n e1 e2 -> do
    v1 <- eval e1
    env <- getEnvironment
    f <- abstract (\v -> do
      withEnvironment env $ do
        bind n (Binding False v v1 Nothing) $ do
          eval e2)
    return (Function n v1 f)
  Pi n e1 e2 (Just s) -> do
    v1 <- eval e1
    env <- getEnvironment
    f <- abstract (\v -> do
      withEnvironment env $ do
        bind n (Binding False v v1 Nothing) $ do
          eval e2)
    return (PiType n v1 f s)
  Pos _ e -> do
    eval e
  Infer _ -> error "Encountered type inference marker during evaluation. You either have an underscore in your code that cannnot be decided or you have discovered a bug in the interpreter."
  Unquote _ -> error "During evaluation, there should be no unquote left."
