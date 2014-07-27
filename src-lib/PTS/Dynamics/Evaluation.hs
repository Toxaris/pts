{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Dynamics.Evaluation where

import Control.Applicative hiding (Const)
import Control.Monad.State

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

import PTS.Dynamics.Binding
import PTS.Dynamics.Value
import PTS.Syntax
import PTS.Syntax.Names

type Env m = [(Name, Value m)]

dropTypes :: Bindings m -> Env m
dropTypes = map (\(x, (_, y, z)) -> (x, y))

newtype Eval a = Eval (State NamesMap a)
  deriving (Functor, Monad, MonadState NamesMap)

runEval :: NamesMap -> Eval a -> a
runEval names (Eval p) = evalState p names

equivTerm :: Bindings Eval -> Term -> Term -> Bool
equivTerm env' t1 t2 = runEval (envToNamesMap env) $ do
  v1 <- eval t1 env
  v2 <- eval t2 env
  equiv v1 v2
 where env = dropTypes env'

equiv :: Value Eval -> Value Eval -> Eval Bool
equiv (Function n v1 (ValueFunction f)) (Function _ v1' (ValueFunction f')) = do
  r1 <- equiv v1 v1'
  n'   <- fresh n
  v2   <- f   (ResidualVar n')
  v2'  <- f'  (ResidualVar n')
  r2 <- equiv v2 v2'
  return (r1 && r2)
equiv (Number n) (Number n') = do
  return (n == n')
equiv (Constant c) (Constant c') = do
  return (c == c')
equiv (PiType n v1 (ValueFunction f)) (PiType _ v1' (ValueFunction f')) = do
  r1   <- equiv v1 v1'
  n'   <- fresh n
  v2   <- f   (ResidualVar n')
  v2'  <- f'  (ResidualVar n')
  r2   <- equiv v2 v2'
  return (r1 && r2)
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

nbe :: Bindings Eval -> Term -> Term
nbe env' e = runEval (envToNamesMap env) $ do
  v   <- eval e env
  e'  <- reify v
  return e'
 where env = dropTypes env'

fresh :: Name -> Eval Name
fresh n = do
  ns <- get
  let (n', ns') = freshvarlMap ns n
  put ns'
  return n'

reify :: Value Eval -> Eval Term
reify (Function n v1 (ValueFunction f)) = do
  e1 <- reify v1
  n' <- fresh n
  v2 <- f (ResidualVar n')
  e2 <- reify v2
  return (mkLam n' e1 e2)
reify (Number n) = do
  return (mkInt n)
reify (Constant c) = do
  return (mkConst c)
reify (PiType n v1 (ValueFunction f)) = do
  e1 <- reify v1
  n' <- fresh n
  v2 <- f (ResidualVar n')
  e2 <- reify v2
  return (mkPi n' e1 e2)
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

evalTerm :: Bindings Eval -> Term -> Value Eval
evalTerm env' t = runEval (envToNamesMap env) $ do
  eval t env
 where env = dropTypes env'


eval :: Term -> Env Eval -> Eval (Value Eval)
eval t env = case structure t of
  Int n -> do
    return (Number n)
  IntOp op e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return $
      fromMaybe (ResidualIntOp op v1 v2)
        (case (v1, v2) of
            (Number n1, Number n2) -> do
              Number <$> evalOp op n1 n2
            _ -> Nothing)
  IfZero e1 e2 e3 -> do
    v1 <- eval e1 env
    case v1 of
      Number 0 -> do
        eval e2 env
      Number _ -> do
        eval e3 env
      _ -> do
        v2   <- eval e2 env
        v3   <- eval e3 env
        return (ResidualIfZero v1 v2 v3)
  Var n -> do
    case lookup n env of
      Just v -> return v
      Nothing -> return (ResidualVar n)
  Const c -> do
    return (Constant c)
  App e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case v1 of
      Function n t (ValueFunction f) -> do
        f v2
      _ -> do
        return (ResidualApp v1 v2)
  Lam n e1 e2 -> do
    v1 <- eval e1 env
    return (Function n v1 (ValueFunction (\v -> eval e2 ((n, v) : env))))
  Pi n e1 e2 -> do
    v1 <- eval e1 env
    return (PiType n v1 (ValueFunction (\v -> eval e2 ((n, v) : env))))
  Pos _ e -> do
    eval e env
  Infer _ -> error "Encountered type inference marker during evaluation. You either have an underscore in your code that cannnot be decided or you have discovered a bug in the interpreter."
  Unquote _ -> error "During evaluation, there should be no unquote left."
