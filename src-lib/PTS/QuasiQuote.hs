{-# LANGUAGE TemplateHaskell #-}
module PTS.QuasiQuote (pts) where

import Parametric.Error (Position (..))
import Parametric.AST
import PTS.Instances
import PTS.AST
import PTS.Parser (parseTermAtPos)

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

instance PatOrExp Pat where
  list  =  listP
  con   =  conP
  lit   =  litP
  var   =  varP

instance PatOrExp Exp where
  list  =  listE
  con   =  \name -> foldl appE (conE name)
  lit   =  litE
  var   =  varE

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

instance Lift Term where
  lift (MkTerm t) = con 'MkTerm [lift t]

instance Lift t => Lift (TermStructure t) where
  lift (Nat     n)              =  con 'Nat     [lift n]
  lift (NatOp   v  op  e1  e2)  =  con 'NatOp   [lift v,  lift op,  lift e1,  lift e2]
  lift (IfZero  c  t   e)       =  con 'IfZero  [lift c,  lift t,   lift e]
  lift (Var     v)              =  con 'Var     [lift v]
  lift (Const   c)              =  con 'Const   [lift c]
  lift (App     f  a)           =  con 'App     [lift f, lift a]
  lift (Lam     v  t  e)        =  con 'Lam     [lift v, lift t, lift e]
  lift (Pi      v  t  e)        =  con 'Pi      [lift v, lift t, lift e]
  lift (Pos     p  e)           =  lift e
  lift (Unquote v)              =  var (TH.mkName v)
