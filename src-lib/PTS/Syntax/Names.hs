{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Syntax.Names
  ( Name (PlainName, IndexName, MetaName)
  , Names
  , NamesMap
  , freshvarl
  , freshvarlMap
  , envToNamesMap
  , ModuleName (ModuleName)
  , LanguageName
  , parts
  , showName
  ) where

import Data.Char (isAlphaNum, isDigit, isLetter, isLower)
import Data.Data (Data)
import Data.Set (Set, member)
import qualified Data.Map as Map
import Data.Typeable (Typeable)

type LanguageName = String

data ModuleName
  =  ModuleName [String]
  deriving (Eq, Ord, Show)

parts :: ModuleName -> [String]
parts (ModuleName xs) = xs

data Name
  = PlainName {-# UNPACK #-} !Char String
  | IndexName {-# UNPACK #-} !Int {-# UNPACK #-}!Char String
  | MetaName String
  deriving (Eq, Ord, Data, Typeable)

type Names = Set Name

instance Show Name where
  showsPrec _ (PlainName c text) = showString (c : text)
  showsPrec _ (IndexName i c text) = showString (c : text) . shows i
  showsPrec _ (MetaName text) = showChar '$' . showString text

showName :: Name -> String
showName = show

instance Read Name where
  readsPrec _ (first:cs) | isLetter first = [plainName [] cs] where
    plainName text (c:cs) | isDigit c = indexName text [c] cs
    plainName text (c:cs) | isAlphaNum c = plainName (c : text) cs
    plainName text rest = (PlainName first (reverse text), rest)

    indexName text index (c:cs) | isDigit c = indexName text (c : index) cs
    indexName text index (c:cs) | isAlphaNum c = plainName (c : index ++ text) cs
    indexName text index rest = (IndexName (read (reverse index)) first (reverse text), rest)


  readsPrec _ ('$':c:cs) | isLower c = [metaName [c] cs] where
    metaName text (c:cs) | isAlphaNum c = metaName (c : text) cs
    metaName text rest = (MetaName (reverse text), rest)

  readsPrec _ _ = []

nextIndex :: Name -> Name
nextIndex (PlainName char text) = IndexName 0 char text
nextIndex (IndexName index char text) = IndexName (index + 1) char text

freshvarl :: Names -> Name -> Name
freshvarl xs x
  =  if x `member` xs
     then freshvarl xs (nextIndex x)
     else x

-- Map name to its current max index.
type NamesMap = Map.Map (Char, String) Int

-- Merge with fresh, this is just a state monad.
freshvarlMap :: NamesMap -> Name -> (Name, NamesMap)
freshvarlMap names n =
  (case oldLookup of
     Nothing -> n
     Just idx ->
       let
         -- Here we reupdate the index, because insertLookupWithKey
         -- returns the old content. However, that's still cheap,
         -- especially compared to doing a new lookup.
         newIdx = transformIdx idx
       in IndexName newIdx rawC rawN
  , newNames)
    where
      raw@(rawC, rawN) = rawName n
      oldIdx = getIdx n
      transformIdx = (+1)
      updateVal key badReplacementValue oldValue = transformIdx oldValue
      (oldLookup, newNames) = Map.insertLookupWithKey updateVal raw oldIdx names

rawName (PlainName c text)   = (c, text)
rawName (IndexName _ c text) = (c, text)

getIdx (PlainName _ _) = -1
getIdx (IndexName idx _ _) = idx

envToNamesMap :: [(Name, a)] -> NamesMap
envToNamesMap = Map.fromListWith max . map (\(name, _) -> (rawName name, getIdx name))
