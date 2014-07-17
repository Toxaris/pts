{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Syntax.Names
  ( Name (PlainName, IndexName, MetaName)
  , Names
  , freshvarl
  , ModuleName (ModuleName)
  , parts
  ) where

import Data.Char (isAlphaNum, isDigit, isLetter, isLower)
import Data.Data (Data)
import Data.Set (Set, member)
import Data.Typeable (Typeable)

data ModuleName
  =  ModuleName [String]
  deriving (Eq, Ord, Show)

parts :: ModuleName -> [String]
parts (ModuleName xs) = xs

data Name
  = PlainName String
  | IndexName Int String
  | MetaName String
  deriving (Eq, Ord, Data, Typeable)

type Names = Set Name

instance Show Name where
  showsPrec _ (PlainName text) = showString text
  showsPrec _ (IndexName i text) = showString text . shows i
  showsPrec _ (MetaName text) = showChar '$' . showString text

instance Read Name where
  readsPrec _ (c:cs) | isLetter c = [plainName [c] cs] where
    plainName text (c:cs) | isDigit c = indexName text [c] cs
    plainName text (c:cs) | isAlphaNum c = plainName (c : text) cs
    plainName text rest = (PlainName (reverse text), rest)

    indexName text index (c:cs) | isDigit c = indexName text (c : index) cs
    indexName text index (c:cs) | isAlphaNum c = plainName (index ++ text) cs
    indexName text index rest = (IndexName (read (reverse index)) (reverse text), rest)

  readsPrec _ ('$':c:cs) | isLower c = [metaName [c] cs] where
    metaName text (c:cs) | isAlphaNum c = metaName (c : text) cs
    metaName text rest = (MetaName (reverse text), rest)

  readsPrec _ _ = []

nextIndex :: Name -> Name
nextIndex (PlainName text) = IndexName 0 text
nextIndex (IndexName index text) = IndexName (index + 1) text

freshvarl :: Names -> Name -> Name
freshvarl xs x
  =  if x `member` xs
     then freshvarl xs (nextIndex x)
     else x
