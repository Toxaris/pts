{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Parser ( parseStmt, parseStmts, parseTerm ) where

import Prelude hiding (abs, pi, elem, notElem, const)

import Control.Applicative hiding (many, Const)
import Control.Monad
import Control.Monad.Reader

import Data.Char
import Data.Either
import Data.Eq
import Data.Foldable

import System.IO

import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec as Parsec

import Tools.Errors.Class
import Tools.Instances

import Parametric.Error
import Parametric.Parser
import Parametric.Parser.Error

import PTS.AST
import PTS.Instances
import PTS.Options

     -----------------
    -- PTS PARSER --
     ----------------- 

natop n f x = mkNatOp n f <$> (keyword (show n) *> x) <*> (x <?> "second argument of '" ++ show n ++ "'")

expr = term simple rec mkPos "expression" where
  simple = withPos mkPos $ asum 
    [ parens expr
    , brackets expr
    , abs mkLam lambda ident colon1 expr dot expr
    , abs mkPi  pi     ident colon1 expr dot expr
    , mkIfZero <$> (keyword "if0" *> expr) 
             <*> (keyword "then" *> expr) 
             <*> (keyword "else" *> expr)
    , natop (read "add") (+) simple
    , natop (read "sub") (-) simple
    , natop (read "mul") (*) simple
    , natop (read "div") div simple
    , mkConst <$> const
    , var mkVar ident
    , mkNat <$> number ]
    
  rec = asum
    [ app mkApp simple
    , arr (\a b -> mkPi (freshvar b (read "unused")) a b) arrow (expr )]

stmt = withPos StmtPos $ asum
  [ Bind <$> try (ident <* colon1) <*> (Just <$> expr) <* assign <*> expr
  , Bind <$> try (ident <* assign) <*> pure Nothing <*> expr
  , Term <$> expr]

stmts = stmt `endBy` semi

     ---------
    -- LEXER --
     ---------

keywords = ["Lambda", "lambda", "Pi", "if0", "then", "else", "->", "add", "mul", "sub", "div"]

identChar x = not (isSpace x) && x `notElem` ".:=;/()[]"

ident = lexem (do name <- many1 (satisfy identChar) 
                  when (name `elem` keywords) $
                    unexpected ("keyword " ++ name)
                             
                  when (Prelude.all (`elem` ['0'..'9']) name) $
                    unexpected ("numeric literal " ++ name)
                  
                  when (isDigit (head name)) $
                    pzero
                  
                  when (Prelude.all (== '*') name) $
                    unexpected ("constant")
                  
                  when (name == "Nat") $
                    unexpected ("constant")
                  
                  return (read name))
          <?> "variable name"

number = read <$> lexem (many1 (satisfy isDigit) <* notFollowedBy (satisfy identChar))

const = lexem (do name <- many1 (satisfy identChar)
                  if name == "Nat" 
                    then return (C 0)
                    else if (Prelude.all ('*' ==) name)
                      then return (C (length name))
                      else pzero)
          <?> "constant"

keyword s  = lexem (string s <* notFollowedBy (satisfy identChar))

comment = string "/*" <* manyTill anyChar (try (string "*/"))

skipSpace = skipMany ((space *> pure () <|> comment *> pure ()) <?> "")
lexem p = try p <* skipSpace 

dot    = lexem (char '.')
colon1 = lexem (char ':')
lambda = keyword "lambda" <|> keyword "Lambda"
pi     = keyword "Pi"
if0    = lexem (keyword "if0")
assign = lexem (char '=')
semi   = lexem (char ';')
arrow  = lexem (string "->")

parens p   = lparen *> p <* rparen
brackets p = lbracket *> p <* rbracket

lparen = lexem(char '(')
rparen = lexem(char ')')
lbracket = lexem(char '[')
rbracket = lexem(char ']')

     ----------------------
    -- RUNNING the PARSER --
     ----------------------

parseInternal parser file text = do
  case parse (skipSpace *> parser <* eof) file text of
    Left e -> throwError . pure . formatError text $ e 
    Right r -> return r 

parseStmt = parseInternal stmt

parseStmts = parseInternal stmts
parseTerm = parseInternal expr
