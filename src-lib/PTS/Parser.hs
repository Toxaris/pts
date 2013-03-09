{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Parser ( parseFile, parseStmt, parseStmts, parseTerm, parseTermAtPos ) where

import Prelude hiding (abs, pi, elem, notElem, const, mod)

import Control.Applicative hiding (many, Const, optional)
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

import PTS.Algebra
import PTS.AST
import PTS.Instances
import PTS.Options
import PTS.Constants

     -----------------
    -- PTS PARSER --
     -----------------

natop n f x = mkNatOp n f <$> (keyword (show n) *> x) <*> (x <?> "second argument of '" ++ show n ++ "'")

expr = term simple rec mkPos "expression" where
  simple = withPos mkPos $ asum
    [ termParens expr
    , brackets expr
    , abs mkLam lambda identOrMeta colon1 expr dot expr
    , abs mkPi  pi     identOrMeta colon1 expr dot expr
    , mkIfZero <$> (keyword "if0" *> expr)
             <*> (keyword "then" *> expr)
             <*> (keyword "else" *> expr)
    , natop (read "add") Add simple
    , natop (read "sub") Sub simple
    , natop (read "mul") Mul simple
    , natop (read "div") Div simple
    , mkConst <$> const
    , mkUnquote <$> unquote
    , var mkVar ident
    , mkNat <$> number ]

  rec = asum
    [ app mkApp simple
    , arr (\a b -> mkPi (freshvar b (read "unused")) a b) arrow (expr )]

unquote = char '$' *> asum
  [ var mkVar ident
  , parens expr]

stmt = withPos StmtPos $ asum
  [ Export <$> (keyword "export" *> ident <* semi)
  , Import <$> (keyword "import" *> modname <* semi)
  , Assertion <$> (keyword "assert" *> expr) <*> optionMaybe (colon1 *> expr) <*> optionMaybe (assign *> expr) <* semi
  , try (Term <$> expr <* semi)
  , Bind <$> ident <*> args <*> optionMaybe (colon1 *> expr) <* assign <*> expr <* semi]

stmts = many (optional pragma *> stmt)

args = many (parens argGroup)

argGroup = (,) <$> names <* colon1 <*> expr

file = File <$> optionMaybe (keyword "module" *> modname <* semi) <*> stmts

names = many ident

     ----------------
    -- LINE PRAGMAS --
     ----------------

pragma = lexem $ do
  string "{-# LINE "
  line <- read <$> many1 digit
  string " \""
  name <- many1 ((char '\\' *> char '"') <|> noneOf "\"")
  string "\" #-}"

  pos <- getPosition
  setPosition (setSourceName (setSourceLine pos (pred line)) name)

     ---------
    -- LEXER --
     ---------

keywords = ["Lambda", "lambda", "Pi", "if0", "then", "else", "->", "add", "mul", "sub", "div", "module", "import", "export", "assert"]

identChar x = not (isSpace x) && x `notElem` ".:=;/()[]$"

identOrMeta = ident <|> meta

meta = lexem (do char '$'
                 first <- satisfy (\c -> isLetter c && isLower c)
                 rest <- many (satisfy (\c -> isAlphaNum c || c `elem` "'_"))
                 return (read ('$' : first : rest)))
         <?> "meta variable name"

ident = lexem (do name <- namepart
                  when (Prelude.all (== '*') name) $
                    unexpected ("constant")

                  when (name == "Nat") $
                    unexpected ("constant")

                  return (read name))
          <?> "variable name"

namepart = lexem (do
  name <- many1 (satisfy identChar)
  when (name `elem` keywords) $
    unexpected ("keyword " ++ name)
  when (Prelude.all (`elem` ['0'..'9']) name) $
    unexpected ("numeric literal " ++ name)
  when (isDigit (head name)) $
    pzero
  return name)

modname = lexem (do
  names <- namepart `sepBy` dot
  return (ModuleName names)) <?> "module name"

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

termParens p = parens p <|> underscore_lparen *> p <* rparen
parens p   = lparen *> p <* rparen
brackets p = lbracket *> p <* rbracket

underscore_lparen = lexem(keyword "_(")
lparen = lexem(char '(')
rparen = lexem(char ')')
lbracket = lexem(char '[')
rbracket = lexem(char ']')

     ----------------------
    -- RUNNING the PARSER --
     ----------------------

parseInternal parser file text = do
  case parse (skipSpace *> parser <* eof) file text of
    Left e -> throwError . pure . formatError file text $ e
    Right r -> return r

parseStmt = parseInternal stmt

parseStmts = parseInternal stmts
parseTerm = parseInternal expr
parseFile = parseInternal file

parseTermAtPos :: Monad m => String -> Int -> Int -> String -> m Term
parseTermAtPos file line col text =
    case parse p file text of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            skipSpace
            e <- expr
            eof
            return e
