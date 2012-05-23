{-# LANGUAGE TemplateHaskell #-}
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
  liftE term

class Lift e where
  liftE :: e -> ExpQ
  -- liftP :: e -> PatQ

  liftListE :: [e] -> ExpQ
  liftListE es = listE (map liftE es)

  -- liftListP :: [e] -> PatQ
  -- liftListP = listP (map liftP es)

instance Lift e => Lift [e] where
  liftE = liftListE
  -- liftP = liftListP

instance Lift Position where
  liftE (Position file i1 i2 i3 i4) = [| Position $(liftE file) $(liftE i1) $(liftE i2) $(liftE i3) $(liftE i4) |]

instance Lift Char where
  liftE c = litE (charL c)
  liftListE s = litE (stringL s)

instance Lift Integer where
  liftE n = litE (integerL n)

instance Lift Int where
  liftE n = litE (integerL (toInteger n))

instance Lift BinOp where
  liftE Add = [| Add |]
  liftE Sub = [| Add |]
  liftE Mul = [| Add |]
  liftE Div = [| Add |]

instance Lift C where
  liftE (C n) = [| C $(liftE n) |]

instance Lift Name where
  liftE (PlainName s) = [| PlainName $(liftE s) |]
  liftE (IndexName s i) = [| IndexName $(liftE s) $(liftE i) |]

instance Lift Term where
  liftE t = case structure t of
    Nat     n           ->  [| mkNat $(liftE n) |]
    NatOp   v op e1 e2  ->  [| mkNatOp $(liftE v) $(liftE op) $(liftE e1) $(liftE e2) |]
    IfZero  c t e       ->  [| mkIfZero $(liftE c) $(liftE t) $(liftE e) |]
    Var     v           ->  [| mkVar $(liftE v) |]
    Const   c           ->  [| mkConst $(liftE c) |]
    App     f a         ->  [| mkApp $(liftE f) $(liftE a) |]
    Lam     v t e       ->  [| mkLam $(liftE v) $(liftE t) $(liftE e) |]
    Pi      v t e       ->  [| mkPi $(liftE v) $(liftE t) $(liftE e) |]
    Pos     p e         ->  [| mkPos $(liftE p) $(liftE e) |]
    Unquote v           ->  TH.varE (TH.mkName v)

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
