{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Evaluation where

import PTS.AST

import qualified Data.Set as Set

import Control.Monad.State

type Env = [(Name, Value)]

data Value
  = Function  Name Term (Value -> M Value)
  | Number    Integer
  | Constant  C
  | Residual  Term

newtype M a = M (State Names a)
  deriving (Functor, Monad, MonadState Names)

runM :: M a -> a
runM (M p) = evalState p Set.empty

nbe :: Term -> Term
nbe e = runM $ do
  v   <- eval e []
  e'  <- reify v
  return e'
  

fresh :: Name -> M Name
fresh n = do
  ns <- get
  let n' = freshvarl ns n
  put (Set.insert n' ns)
  return n

reify :: Value -> M Term
reify (Function n t f) = do
  n' <- fresh n
  v <- f (Residual (mkVar n'))
  e <- reify v
  return (mkLam n' t e)
reify (Number n) = do
  return (mkNat n)
reify (Constant c) = do
  return (mkConst c)
reify (Residual e) = do
  return e

eval :: Term -> Env -> M Value
eval t env = case structure t of
  Nat n -> do
    return (Number n)
  NatOp n op e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1, v2) of
      (Number n1, Number n2) -> do
        return (Number (evalOp op n1 n2))
      _ -> do
        e1'  <- reify v1
        e2'  <- reify v2
        return (Residual (mkNatOp n op e1' e2'))
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
        e1'  <- reify v1
        e2'  <- reify v2
        e3'  <- reify v3
        return (Residual (mkIfZero e1' e2' e3'))
  Var n -> do
    case lookup n env of
      Just v -> return v
      Nothing -> fail ("unbound variable '" ++ show n ++ "'")
  Const c -> do
    return (Constant c)
  App e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case v1 of
      Function n t f -> do
        f v2
      _ -> do
        e1'  <- reify v1
        e2'  <- reify v2
        return (Residual (mkApp e1' e2'))
  Lam n e1 e2 -> do
    return (Function n e1 (\v -> eval e2 ((n, v) : env)))
  Pi n e1 e2 -> do
    n'   <- fresh n
    v1   <- eval e1 env
    v2   <- eval e2 ((n, Residual (mkVar n')) : env)
    e1'  <- reify v1
    e2'  <- reify v2
    return (Residual (mkPi n' e1' e2'))
  Pos _ e -> do
    eval e env

{-
data TermStructure alpha
  = Nat     Integer
  | NatOp   Name BinOp alpha alpha
  | IfZero  alpha alpha alpha
  | Var     Name
  | Const   C
  | App     alpha alpha
  | Lam     Name alpha alpha
  | Pi      Name alpha alpha
  | Pos     Position alpha
  | Unquote alpha
  deriving (Functor, Data, Typeable)
-}
