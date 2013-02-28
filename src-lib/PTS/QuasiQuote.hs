{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module PTS.QuasiQuote (pts) where

import Parametric.Error (Position (..))
import Parametric.AST
import PTS.Instances
import PTS.AST
import PTS.Parser (parseTermAtPos)
import PTS.Constants

import Data.Char

import Data.Generics
import Language.Haskell.TH hiding (Name)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

pts  :: QuasiQuoter
pts  =  QuasiQuoter
  quoteTerm
  quoteTerm
  (error "cannot use pts quasiquoter in types")
  (error "cannot use pts quasiquoter in declarations")

quoteTerm :: PatOrExp t => String -> Q t
quoteTerm text = do
  loc <- TH.location
  let  file  =  TH.loc_filename loc
       line  =  fst (TH.loc_start loc)
       col   =  snd (TH.loc_start loc)
  term <- parseTermAtPos file line col text
  lift term

class PatOrExp t where
  list  ::  [Q t] -> Q t
  con   ::  TH.Name -> [Q t] -> Q t
  lit   ::  Lit -> Q t
  var   ::  TH.Name -> Q t
  app   ::  Q t -> [Q t] -> Q t

instance PatOrExp Pat where
  list  =  listP
  con   =  conP
  lit   =  litP
  var   =  varP
  app   =  error "cannot apply in patterns"

instance PatOrExp Exp where
  list  =  listE
  con   =  \name -> foldl appE (conE name)
  lit   =  litE
  var   =  varE
  app   =  foldl appE

class Lift e where
  lift :: PatOrExp t => e -> Q t

  liftList :: PatOrExp t => [e] -> Q t
  liftList es = list (map lift es)

instance Lift e => Lift [e] where
  lift = liftList

instance Lift Position where
  lift (Position file i1 i2 i3 i4) = con 'Position [lift file, lift i1, lift i2, lift i3, lift i4]

instance Lift Char where
  lift c = lit (charL c)
  liftList s = lit (stringL s)

instance Lift Integer where
  lift n = lit (integerL n)

instance Lift Int where
  lift n = lit (integerL (toInteger n))

instance Lift BinOp where
  lift Add = con 'Add []
  lift Sub = con 'Sub []
  lift Mul = con 'Mul []
  lift Div = con 'Div []

instance Lift C where
  lift (C n) = con 'C [lift n]

instance Lift Name where
  lift (PlainName  s)    = con 'PlainName [lift s]
  lift (IndexName  s i)  = con 'IndexName [lift s, lift i]
  lift (MetaName s)      = var (TH.mkName s)

instance Lift Term where
  lift (MkTerm (Nat     n))              =  con 'MkTerm  [con 'Nat     [lift n]]
  lift (MkTerm (NatOp   v  op  e1  e2))  =  con 'MkTerm  [con 'NatOp   [lift v,  lift op,  lift e1,  lift e2]]
  lift (MkTerm (IfZero  c  t   e))       =  con 'MkTerm  [con 'IfZero  [lift c,  lift t,   lift e]]
  lift (MkTerm (Var     v))              =  con 'MkTerm  [con 'Var     [lift v]]
  lift (MkTerm (Const   c))              =  con 'MkTerm  [con 'Const   [lift c]]
  lift (MkTerm (App     f  a))           =  con 'MkTerm  [con 'App     [lift f, lift a]]
  lift (MkTerm (Lam     v  t  e))        =  con 'MkTerm  [con 'Lam     [lift v, lift t, lift e]]
  lift (MkTerm (Pi      v  t  e))        =  con 'MkTerm  [con 'Pi      [lift v, lift t, lift e]]
  lift (MkTerm (Unquote t))              =  unquote t
  lift (MkTerm (Pos     p  e))           =  lift e

class Unquote e where
  unquote :: PatOrExp t => e -> Q t

instance Unquote Term where
  unquote (MkTerm (App f a)) = app (unquote f) [unquote a]
  unquote (MkTerm (Var v))
    | isUpper (head (show v))  =  con (TH.mkName (show v)) []
    | otherwise                =  var (TH.mkName (show v))
  unquote (MkTerm (Pos p e)) = unquote e
