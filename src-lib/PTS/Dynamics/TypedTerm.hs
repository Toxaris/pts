{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module PTS.Dynamics.TypedTerm
  ( TypedTerm
  , typeOf
  , typedHandlePos
  ) where

import Control.Applicative ((<$>))

import PTS.Error
import PTS.Syntax.Constants
import PTS.Syntax.Term

newtype TypedTerm = MkTypedTerm (AnnotatedTerm TypedTerm)

instance Show TypedTerm where
  showsPrec d t =
    let struct = structure t; typ = typeOf t in
    showParen (d > app_prec) $
      showString "MkTypedTerm (MkAnnotatedTerm " . showsPrec (app_prec + 1) struct .
        if loops t
         then showString " <self>" -- This is the only non-default behavior.
         else showString " " . showsPrec (app_prec + 1) typ
      . showString ")"
   where
     app_prec = 10

loops t1 = sameConst (structure' t1) (structure' (typeOf t1))
 where
   sameConst (Const (C c1)) (Const (C c2)) = c1 == c2
   sameConst _ _ = False

instance Structure TypedTerm where
  structure (MkTypedTerm t) = fmap MkTypedTerm (structure t)

typeOf :: TypedTerm -> TypedTerm
typeOf (MkTypedTerm t) = annotation t

instance MakeTerm TypedTerm (TypedTerm -> TypedTerm) where
  mkTerm t q = result where
    result = MkTypedTerm (mkTerm (fmap (\(MkTypedTerm t) -> t) t) q)

typedHandlePos f p t = annotatePos p $ (\t -> mkPos p t (typeOf t)) <$> f t
