{-# LANGUAGE FlexibleContexts #-}
module PTS.Quote where

import Prelude hiding (div)

import PTS.AST
import PTS.Instances (C (C))
import PTS.Core hiding (debug)
import PTS.Parser (parseTerm)
import Parametric.Error
import Tools.Errors.Class
import Tools.Errors(runErrorsT)
import Parametric.Pretty(multiLine)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import PTS.Pretty
import PTS.Instances (fomegastar)
import PTS.Options
import System.IO.Unsafe
import PTS.Normalisation

import qualified Data.Set as Set

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

varnames = ["R", "lam", "app", "tlam", "tapp", "ifzero", "nat", "add", "sub", "mul", "div"]

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
  
  p text = unsafeParse $ text >>= \x -> if x == 'R' then r else return x

unsafeQuote :: Term -> Term
unsafeQuote t = unsafePerformIO $ do
  res <- runErrorsT (quote (map mkVar varnames) 0 [] t `runReaderT` defaultOptions)
  case res of 
    Left e -> fail (showErrors e)
    Right q -> return q 

unsafeQuoteQuote :: Term -> Term
unsafeQuoteQuote t = unsafePerformIO $ do
  res <- runErrorsT (quotequote 0 [] t `runReaderT` defaultOptions)
  case res of
    Left e -> fail (showErrors e)
    Right q -> return q 


quotequote :: (MonadIO m, MonadReader Options m, MonadErrors Errors m, Functor m) => Int -> [(String, Term)] -> Term -> m Term
quotequote d ctx t = do
  let vars = map (freshvarl (Set.fromList (allvars t))) varnames 
  q <- quote (map mkVar vars) 0 [] t
  return $ foldr (uncurry mkLam) q (zip vars (interface (rep vars)))  

debug :: MonadIO m => Int -> String -> [(String, Term)] -> Term -> m Term -> m Term 
debug d n ctx t result | False {- DebugQuote `elem` options -} = do
  liftIO $ putStrLn $ 
    replicate (d+d) ' ' ++ 
    "--> " ++ n ++ ": " ++
    (let gamma = showCtx ctx in if null gamma then "" else gamma ++ " ") ++
    "|- <" ++ show t ++ "> = ???"
  x <- result
  liftIO $ putStrLn $ 
    replicate (d+d) ' ' ++ 
    "<-- " ++ n ++ ": " ++
    (let gamma = showCtx ctx in if null gamma then "" else gamma ++ " ") ++
    "|- <" ++ show t ++ "> = " ++ show x
  return x
  
debug d n ctx t result = result


quote :: (MonadIO m, MonadErrors Errors m, Functor m, MonadReader Options m) => [Term] -> Int -> [(String, Term)] -> Term -> m Term

quote vars d ctx q = case structure q of
  Nat n -> debug d "QuoteNat" ctx q $ do 
    return $ nat vars `mkApp` mkNat n

  Var x -> debug d "QuoteVar" ctx q $ do
    return q

  Lam x a b -> debug d "QuoteLam" ctx q $ do
    let (newx, newb, newctx) = bind ctx x a b
    Const s1 <- (structure . normalform) `fmap` typecheck (succ d) ctx a
    tb <- typecheck (succ d) newctx newb
    Const s2 <- (structure . normalform) `fmap` typecheck (succ d) newctx tb
    b' <- quote vars (succ d) newctx newb
  
    r <- case (s1, s2) of
      (C 1, C 1) -> return $ lam vars `mkApp` a `mkApp` tb `mkApp` mkLam newx (rep vars `mkApp` a) b' 
      (C 2, C 1) -> return $ tlam vars `mkApp` a `mkApp` mkLam newx a tb `mkApp` mkLam newx a b' 
      _ -> fail $ "cannot quote non-value-level term " ++ show q
  
    return r

  App f x -> debug d "QuoteApp" ctx q $ do
    Pi v a b <- (structure . normalform) `fmap` typecheck (succ d) ctx f
    Const s1 <- (structure . normalform) `fmap` typecheck (succ d) ctx a
    let (newv, newb, newctx) = bind ctx v a b
    Const s2 <- (structure . normalform) `fmap` typecheck (succ d) newctx newb
    f' <- quote vars (succ d) ctx f
    case (s1, s2) of
      (C 1, C 1) -> do 
        x' <- quote vars (succ d) ctx x
        return $ app vars `mkApp` a `mkApp` b `mkApp` f' `mkApp` x'
      (C 2, C 1) -> return $ tapp vars `mkApp` a `mkApp` mkLam v a b `mkApp` f' `mkApp` x
      _ -> fail $ "cannot quote non-value-level term" ++ show q

  NatOp n f x y -> debug d "QuoteNatOp" ctx q $ do
    x' <- quote vars d ctx x
    y' <- quote vars d ctx y
    case n of
      "add" -> return $ add vars `mkApp` x' `mkApp` y'
      "sub" -> return $ sub vars `mkApp` x' `mkApp` y'
      "mul" -> return $ mul vars `mkApp` x' `mkApp` y'
      "div" -> return $ div vars `mkApp` x' `mkApp` y'

  IfZero c t e -> debug d "QuoteIfZero" ctx q $ do
    c' <- quote vars d ctx c
    t' <- quote vars d ctx t
    e' <- quote vars d ctx e
    return $ ifzero vars `mkApp` c' `mkApp` t' `mkApp` e'

  Pos p t -> do
    t' <- quote vars d ctx t
    return $ mkPos p t'

  _ -> do
    fail $ "cannot quote non-value-level term" ++ show q
