{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTS.Evaluation where

import PTS.AST
import PTS.Value
import PTS.Binding

import qualified Data.Set as Set

import Control.Applicative hiding (Const)
import Data.Maybe(fromMaybe,)
import Control.Monad.State

type Env m = [(Name, Value m)]

envToNames :: Env m -> Names
envToNames env = Set.fromList (map fst env)

dropTypes :: Bindings m -> Env m
dropTypes = map (\(x, (y, z)) -> (x, y))

newtype M a = M (State Names a)
  deriving (Functor, Monad, MonadState Names)

runM :: Names -> M a -> a
runM names (M p) = evalState p names

equivTerm :: Bindings M -> Term -> Term -> Bool
equivTerm env' t1 t2 = runM (envToNames env) $ do
  v1 <- eval t1 env
  v2 <- eval t2 env
  equiv v1 v2
 where env = dropTypes env'

equiv :: Value M -> Value M -> M Bool
equiv (Function n v1 f) (Function _ v1' f') = do
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
equiv (PiType n v1 f) (PiType _ v1' f') = do
  r1   <- equiv v1 v1'
  n'   <- fresh n
  v2   <- f   (ResidualVar n')
  v2'  <- f'  (ResidualVar n')
  r2   <- equiv v2 v2'
  return (r1 && r2)
equiv (ResidualNatOp n op v1 v2) (ResidualNatOp n' op' v1' v2') = do
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

nbe :: Bindings M -> Term -> Term
nbe env' e = runM (envToNames env) $ do
  v   <- eval e env
  e'  <- reify v
  return e'
 where env = dropTypes env'

fresh :: Name -> M Name
fresh n = do
  ns <- get
  let n' = freshvarl ns n
  put (Set.insert n' ns)
  return n'

reify :: Value M -> M Term
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

evalTerm :: Bindings M -> Term -> Value M
evalTerm env' t = runM (envToNames env) $ do
  eval t env
 where env = dropTypes env'


eval :: Term -> Env M -> M (Value M)
eval t env = case structure t of
  Nat n -> do
    return (Number n)
  NatOp n op e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return $
      fromMaybe (ResidualNatOp n op v1 v2)
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
