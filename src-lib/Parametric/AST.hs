{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, KindSignatures, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, RankNTypes, DeriveDataTypeable #-}
module Parametric.AST where

import Data.Typeable
import Data.Data

import Data.Char (isLetter, isDigit, isAlphaNum)
import Data.Set (Set)
import qualified Data.Set as Set

data Name
  = PlainName String
  | IndexName String Int
  deriving (Eq, Ord, Data, Typeable)

type Names = Set Name

instance Show Name where
  showsPrec _ (PlainName text) = showString text
  showsPrec _ (IndexName text i) = showString text . shows i

instance Read Name where
  readsPrec _ (c:cs) | isLetter c = [plainName [c] cs] where
    plainName text (c:cs) | isDigit c = indexName text [c] cs
    plainName text (c:cs) | isAlphaNum c = plainName (c : text) cs
    plainName text rest = (PlainName (reverse text), rest)

    indexName text index (c:cs) | isDigit c = indexName text (c : index) cs
    indexName text index (c:cs) | isAlphaNum c = plainName (index ++ text) cs
    indexName text index rest = (IndexName (reverse text) (read (reverse index)), rest)

  readsPrec _ _ = []

nextIndex :: Name -> Name
nextIndex (PlainName text) = IndexName text 0
nextIndex (IndexName text index) = IndexName text (index + 1)

freshvarl :: Names -> Name -> Name
freshvarl xs x
  =  if x `Set.member` xs
     then freshvarl xs (nextIndex x)
     else x
