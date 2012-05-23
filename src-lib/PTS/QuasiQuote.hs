{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module PTS.QuasiQuote (pts) where

import Parametric.Error (Position (..))
import Parametric.AST
import PTS.Instances
import PTS.AST
import PTS.Parser (parseTermAtPos)

import Data.Generics
import Language.Haskell.TH as TH hiding (Name)
import Language.Haskell.TH.Quote

pts  :: QuasiQuoter
pts  =  QuasiQuoter
  quoteExprExp
  quoteExprPat
  (error "cannot use pts quasiquoter in types")
  (error "cannot use pts quasiquoter in declarations")

quoteExprExp :: String -> TH.ExpQ
quoteExprExp text = do
  loc <- TH.location
  let  file  =  TH.loc_filename loc
       line  =  fst (TH.loc_start loc)
       col   =  snd (TH.loc_start loc)
  term <- parseTermAtPos file line col text
  lift term

class LiftList t where
  liftListDefault :: Lift e t => [e] -> Q t

instance LiftList Exp where
  liftListDefault es = listE (map lift es)

class LiftList t => Lift e t where
  lift :: e -> Q t

  liftList :: [e] -> Q t
  liftList = liftListDefault

instance Lift e t => Lift [e] t where
  lift = liftList

instance Lift Position Exp where
  lift (Position file i1 i2 i3 i4) = [| Position $(lift file) $(lift i1) $(lift i2) $(lift i3) $(lift i4) |]

instance Lift Char Exp where
  lift c = litE (charL c)
  liftList s = litE (stringL s)

instance Lift Integer Exp where
  lift n = litE (integerL n)

instance Lift Int Exp where
  lift n = litE (integerL (toInteger n))

instance Lift BinOp Exp where
  lift Add = [| Add |]
  lift Sub = [| Add |]
  lift Mul = [| Add |]
  lift Div = [| Add |]

instance Lift C Exp where
  lift (C n) = [| C $(lift n) |]

instance Lift Name Exp where
  lift (PlainName s) = [| PlainName $(lift s) |]
  lift (IndexName s i) = [| IndexName $(lift s) $(lift i) |]

instance Lift Term Exp where
  lift t = case structure t of
    Nat     n           ->  [| mkNat $(lift n) |]
    NatOp   v op e1 e2  ->  [| mkNatOp $(lift v) $(lift op) $(lift e1) $(lift e2) |]
    IfZero  c t e       ->  [| mkIfZero $(lift c) $(lift t) $(lift e) |]
    Var     v           ->  [| mkVar $(lift v) |]
    Const   c           ->  [| mkConst $(lift c) |]
    App     f a         ->  [| mkApp $(lift f) $(lift a) |]
    Lam     v t e       ->  [| mkLam $(lift v) $(lift t) $(lift e) |]
    Pi      v t e       ->  [| mkPi $(lift v) $(lift t) $(lift e) |]
    Pos     p e         ->  [| mkPos $(lift p) $(lift e) |]
    Unquote v           ->  TH.varE (TH.mkName (show v))

quoteExprPat :: String -> TH.PatQ
quoteExprPat text = do
  loc <- TH.location
  let  file  =  TH.loc_filename loc
       line  =  fst (TH.loc_start loc)
       col   =  snd (TH.loc_start loc)
  expr <- parseTermAtPos file line col text
  dataToPatQ (const Nothing `extQ` antiExprPat) expr

antiExprPat :: Term -> Maybe (TH.Q TH.Pat)
antiExprPat t = case structure t of
  (Unquote v)  -> Just $ TH.varP  (TH.mkName (show v))
  _            -> Nothing
