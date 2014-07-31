{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, UndecidableInstances #-}
module PTS.Dynamics.TypedTerm
  ( TypedTerm
  , typeOf
  , sortOf
  , typedHandlePos
  ) where

import Control.Applicative ((<$>))

import PTS.Error
import PTS.Syntax.Constants
import PTS.Syntax.Term
import PTS.Dynamics.Value

newtype TypedTerm m = MkTypedTerm (AnnotatedTerm (Value m, Maybe C))
  deriving (Show)

instance Structure (TypedTerm m) where
  structure (MkTypedTerm t) = fmap MkTypedTerm (structure t)

typeOf :: TypedTerm m -> Value m
typeOf (MkTypedTerm t) = fst (annotation t)

sortOf :: TypedTerm m -> Maybe C
sortOf (MkTypedTerm t) = snd (annotation t)

instance (a ~ Value m, b ~ Maybe C) => MakeTerm (TypedTerm m) (a -> b -> TypedTerm m) where
  mkTerm t q k = result where
    result = MkTypedTerm (mkTerm (fmap (\(MkTypedTerm t) -> t) t) (q, k))

typedHandlePos f p t = annotatePos p $ (\t -> mkPos p t (typeOf t) (sortOf t)) <$> f t
