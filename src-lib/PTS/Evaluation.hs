{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Evaluation where

import PTS.AST

import qualified Data.Set as Set

import Control.Monad.State

type Env = [(Name, Value)]

data Value
  = Function  Name Value (Value -> M Value)
  | Number    Integer
  | Constant  C
  | PiType    Name Value (Value -> M Value)
  | ResidualNatOp  Name BinOp Value Value
  | ResidualIfZero Value Value Value
  | ResidualVar    Name
  | ResidualApp    Value Value

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
reify (Function n v1 f) = do
  e1 <- reify v1
  n' <- fresh n
  v2 <- f (ResidualVar n')
  e2 <- reify v2
  return (mkLam n' e1 e2)
reify (Number n) = do
  return (mkNat n)
reify (Constant c) = do
  return (mkConst c)
reify (PiType n v1 f) = do
  e1 <- reify v1
  n' <- fresh n
  v2 <- f (ResidualVar n')
  e2 <- reify v2
  return (mkPi n' e1 e2)
reify (ResidualNatOp n op v1 v2) = do
  e1 <- reify v1
  e2 <- reify v2
  return (mkNatOp n op e1 e2)
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
        return (ResidualNatOp n op v1 v2)
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
      Function n t f -> do
        f v2
      _ -> do
        return (ResidualApp v1 v2)
  Lam n e1 e2 -> do
    v1 <- eval e1 env
    return (Function n v1 (\v -> eval e2 ((n, v) : env)))
  Pi n e1 e2 -> do
    v1 <- eval e1 env
    return (PiType n v1 (\v -> eval e2 ((n, v) : env)))
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
