{-# LANGUAGE NoMonomorphismRestriction #-}
module PTS.Pretty 
  ( singleLine 
  , multiLine
  , showCtx ) where

import Control.Arrow(first)

import Data.List(intersperse)
import Data.Set (Set)
import qualified Data.Set as Set

import Parametric.Pretty

import PTS.AST
import PTS.Instances

-- priorities
pAppR = 3
pApp  = 2
pArrL = 2
pArr  = 1
pLam  = 0
pPi   = 0
pIf0  = 2

-- finding chains of similiar terms
appChain = appChain' [] where
  appChain' xs t = case structure t of
    App f x  ->  appChain' (x:xs) f
    Pos p t  ->  appChain' xs t
    _        ->  (t, xs)

lamChain t = case structure t of
  Lam n q t  ->  first ((n, q) :) $ lamChain t
  Pos p t    ->  lamChain t
  _          ->  ([], t)

piChain t = case structure t of
  Pi n q t | n `Set.member` freevars t  ->  first ((n, q) :) $ piChain t
  Pos p t                               ->  piChain t
  _                                     ->  ([], t)

arrChain t = case structure t of
  Pi n q t | n `Set.notMember` freevars t  ->  first (q :) $ arrChain t
  Pos p t                                  ->  arrChain t
  _                                        ->  ([], t)

-- pretty printing
instance Pretty Name where
  pretty _ name = text (show name)

instance Pretty C where
  pretty _ (C 0) = text "Nat"
  pretty _ (C n) = text (replicate n '*')

instance Pretty Term where
  pretty p t = case structure t of 
    Nat n -> 
      int n
    
    NatOp n _ a b -> parens `when` (pApp < p) $ 
      pretty 0 n <+> pretty pAppR a <+> pretty pAppR b
      
    IfZero c t e -> parens `when` (pIf0 < p) $ 
      sep [ text "if" <+> pretty pIf0 c
          , nest p $ text "then" <+> pretty pIf0 t
          , nest p $ text "else" <+> pretty pIf0 e ]
          
    Var n ->
      pretty p n
      
    Const c -> 
      pretty p c
      
    App _ _ -> parens `when` (pApp < p) $ 
      let (f, xs) = appChain t in
        (case structure f of 
           Var _   -> hsep
           Const _ -> hsep
           _       -> sep)
          [ pretty pApp f
          , nest 2 . sep . map (pretty pAppR) $ xs ]
      
    Lam _ _ _ -> parens `when` (pLam < p) $ 
      let (lams, body) = lamChain t in
        sep [ sep . map (\(n, q) -> text "lambda" <+> pretty 0 n <+> text ":" <+> pretty pLam q <+> text ".") $ lams
            , nest 2 $ pretty pLam body ]
    
    Pi n _ t' | n `Set.member` freevars t' -> parens `when` (pPi < p) $
      let (pis, body) = piChain t in
       sep [ sep . map (\(n, q) -> text "Pi" <+> pretty 0 n <+> text ":" <+> pretty pPi q <+> pretty p ".") $ pis
           , nest 2 $ pretty pPi body ]
    
    Pi _ _ _ -> parens `when` (pArr < p) $
      let (arrs, body) = arrChain t in
       sep $ [ sep . map (\q -> pretty pArrL q <+> pretty p "->") $ arrs] ++ [ pretty pArr body ]
    
    Pos _ t -> pretty p t

instance Pretty Stmt where
  pretty p (Bind n Nothing t)   = pretty 0 n <+> text "=" <+> pretty 0 t
  pretty p (Bind n (Just t') t) = pretty 0 n <+> text ":" <+> pretty 0 t' <+> text "=" <+> pretty 0 t
  pretty p (Term t) = pretty 0 t

instance Show Term where
  show t = singleLine t
  
showCtx :: [(Name, Term)] -> String
showCtx = concat . intersperse ", " . map (\(n, t) -> show n ++ " : " ++ show t) 
