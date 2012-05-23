{-# LANGUAGE FlexibleContexts #-}
module PTS.Quote where

import Prelude hiding (div, log)

import Control.Monad.Environment
import Control.Monad.Identity
import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.Trans

import qualified Data.Set as Set

import System.IO.Unsafe

import Parametric.Error
import Parametric.Pretty(multiLine)

import PTS.Algebra
import PTS.AST
import PTS.Core
import PTS.Instances (C (C), fomegastar)
import PTS.Normalisation
import PTS.Options
import PTS.Parser (parseTerm)
import PTS.Pretty

import Tools.Errors (runErrorsT)
import Tools.Errors.Class

rep = (!! 0)
lam = (!! 1)
app = (!! 2)
tlam = (!! 3)
tapp = (!! 4)
ifzero = (!! 5)
nat = (!! 6)
add = (!! 7)
sub = (!! 8)
mul = (!! 9)
div = (!! 10)

varnames :: [Name]
varnames = map read ["R", "lam", "app", "tlam", "tapp", "ifzero", "nat", "add", "sub", "mul", "div"]

unsafeParse :: String -> Term
unsafeParse text =
  case runIdentity (runErrorsT (runReaderT (parseTerm "<Quote.hs>" text) defaultOptions)) of
    Right x -> x
    Left e -> error (showErrors e)

interface r =
  [ p "* -> *"
  , p "Pi S : * . Pi T : * . (R S -> R T) -> R (S -> T)"
  , p "Pi S : * . Pi T : * . R (S -> T) -> R S -> R T"
  , p "Pi S : ** . Pi T : S -> * . (Pi X : S . R (T X)) -> R (Pi X : S . T X)"
  , p "Pi S : ** . Pi T : S -> * . R (Pi X : S . T X) -> (Pi X : S . R (T X))"
  , p "Pi T : * . R Nat -> R T -> R T -> R T"
  , p "Nat -> R Nat"
  , p "R Nat -> R Nat -> R Nat"
  , p "R Nat -> R Nat -> R Nat"
  , p "R Nat -> R Nat -> R Nat"
  , p "R Nat -> R Nat -> R Nat"
  ] where

  p text = unsafeParse $ text >>= \x -> if x == 'R' then show r else return x

unsafeQuote :: Term -> Term
unsafeQuote t = unsafePerformIO $ do
  res <- runErrorsT (((quote (map mkVar varnames) t `runEnvironmentT` []) `runConsoleLogT` False) `runReaderT` defaultOptions)
  case res of
    Left e -> fail (showErrors e)
    Right q -> return q

unsafeQuoteQuote :: Term -> Term
unsafeQuoteQuote t = unsafePerformIO $ do
  res <- runErrorsT (((quotequote t `runEnvironmentT` []) `runConsoleLogT` False) `runReaderT` defaultOptions)
  case res of
    Left e -> fail (showErrors e)
    Right q -> return q

quotequote :: (MonadEnvironment Name Term m, MonadReader Options m, MonadErrors Errors m, Functor m, MonadLog m) => Term -> m Term
quotequote t = do
  let vars = map (freshvarl (allvars t)) varnames
  q <- quote (map mkVar vars) t
  return $ foldr (uncurry mkLam) q (zip vars (interface (rep vars)))

quote :: (MonadEnvironment Name Term m, MonadErrors Errors m, Functor m, MonadReader Options m, MonadLog m) => [Term] -> Term -> m Term

quote vars q = case structure q of
  Nat n -> debug "QuoteNat" q $ do
    return $ nat vars `mkApp` mkNat n

  Var x -> debug "QuoteVar" q $ do
    return q

  Lam x a b -> debug "QuoteLam" q $ do
    Const s1 <- (structure . normalform) `fmap` typecheck a
    safebind x a b $ \newx newb -> do
      tb <- typecheck newb
      Const s2 <- (structure . normalform) `fmap` typecheck tb
      b' <- quote vars newb

      r <- case (s1, s2) of
        (C 1, C 1) -> return $ lam vars `mkApp` a `mkApp` tb `mkApp` mkLam newx (rep vars `mkApp` a) b'
        (C 2, C 1) -> return $ tlam vars `mkApp` a `mkApp` mkLam newx a tb `mkApp` mkLam newx a b'
        _ -> fail $ "cannot quote non-value-level term " ++ show q

      return r

  App f x -> debug "QuoteApp" q $ do
    Pi v a b <- (structure . normalform) `fmap` typecheck f
    Const s1 <- (structure . normalform) `fmap` typecheck a
    Const s2 <- safebind v a b $ \newv newb -> do
       (structure . normalform) `fmap` typecheck newb
    f' <- quote vars f
    case (s1, s2) of
        (C 1, C 1) -> do
          x' <- quote vars x
          return $ app vars `mkApp` a `mkApp` b `mkApp` f' `mkApp` x'
        (C 2, C 1) -> return $ tapp vars `mkApp` a `mkApp` mkLam v a b `mkApp` f' `mkApp` x
        _ -> fail $ "cannot quote non-value-level term" ++ show q

  NatOp n f x y -> debug "QuoteNatOp" q $ do
    x' <- quote vars x
    y' <- quote vars y
    case show n of
      "add" -> return $ add vars `mkApp` x' `mkApp` y'
      "sub" -> return $ sub vars `mkApp` x' `mkApp` y'
      "mul" -> return $ mul vars `mkApp` x' `mkApp` y'
      "div" -> return $ div vars `mkApp` x' `mkApp` y'

  IfZero c t e -> debug "QuoteIfZero" q $ do
    c' <- quote vars c
    t' <- quote vars t
    e' <- quote vars e
    return $ ifzero vars `mkApp` c' `mkApp` t' `mkApp` e'

  Pos p t -> do
    t' <- quote vars t
    return $ mkPos p t'

  _ -> do
    fail $ "cannot quote non-value-level term" ++ show q
