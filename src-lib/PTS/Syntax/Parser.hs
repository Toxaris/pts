{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PTS.Syntax.Parser ( parseFile, parseStmt, parseStmts, parseTerm, parseTermAtPos ) where

import Prelude hiding (abs, pi, elem, notElem, const, mod)

import Control.Applicative hiding (many, Const, optional)
import Control.Monad
import Control.Monad.Errors.Class
import Control.Monad.Reader

import Data.Char
import Data.Either
import Data.Eq
import Data.Foldable
import Data.List ((\\))

import System.IO

import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Error

import PTS.Error
import PTS.Syntax.Algebra
import PTS.Syntax.Constants
import PTS.Syntax.File
import PTS.Syntax.Names
import PTS.Syntax.Statement
import PTS.Syntax.Term

     ---------------------
    -- PARAMETRIC PARSER --
     ---------------------

-- left-recursion handling
term simple rec pos msg = result where
  result = combine <$> getPosition <*> simple <*> many ((,) <$> rec <*> getPosition)
  combine p = foldl' (\x (f, q) -> setPos pos p (f x) q)

-- right-recursive syntax pattern: "lambda ident : qualifier . body"
abs cons lambda ident colon qualifier dot body
  = cons <$> try (lambda *> ident <* colon) <*> qualifier <*> (dot *> body)

-- left-recursive syntax pattern: "x -> y"
arr cons arrow simple = flip cons <$> (arrow *> simple)

-- left-recursive syntax pattern: "x y"
app cons simple = flip cons <$> simple

-- non-recursive syntax pattern: "ident"
var cons ident = cons <$> ident

-- non-recursive syntax pattern: "constant"
con cons constant = cons <$ constant

withPos f p = setPos f <$> getPosition <*> p <*> getPosition where

setPos f p1 x p2 = f (Position (sourceName p1) (sourceLine p1) (sourceLine p2) (sourceColumn p1) (pred $ sourceColumn p2)) x

     -----------------
    -- PTS PARSER --
     -----------------

intop n f x = mkIntOp n f <$> (keyword (show n) *> x) <*> (x <?> "second argument of '" ++ show n ++ "'")

expr = term simple rec mkPos "expression" where
  simple = withPos mkPos $ asum
    [ termParens expr
    , brackets expr
    , abs mkLam lambda identOrMeta colon1 expr dot expr
    , abs mkPi  pi     identOrMeta colon1 expr dot expr
    , multAbs mkLam lambda
    , multAbs mkPi pi
    , mkIfZero <$> (keyword "if0" *> expr)
             <*> (keyword "then" *> expr)
             <*> (keyword "else" *> expr)
    , intop (read "add") Add simple
    , intop (read "sub") Sub simple
    , intop (read "mul") Mul simple
    , intop (read "div") Div simple
    , mkConst <$> const
    , mkUnquote <$> unquote
    , mkInfer <$> infer
    , var mkVar ident
    , mkInt <$> number ]

  rec = asum
    [ app mkApp simple
    , arr (\a b -> mkPi (freshvar b (read "unused")) a b) arrow (expr )]

-- parse abstractions with multiple parameters, like this:
-- lambda (x1 : e1) (x2 x3 : e2) . e
multAbs constructor parser = desugarArgs constructor <$> (parser *> args) <*> (dot *> expr)

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

infer = lexem (do char '_'
                  n <- number
                  ~(available, explicits) <- getState
                  setState (available, n:explicits)
                  return n)
        <|>
        lexem (do char '_'
                  ~(available, explicits) <- getState
                  setState (tail available, explicits)
                  return $ head available)
        

ident = lexem (do name <- namepart
                  when (Prelude.all (== '*') name) $
                    unexpected ("constant")

                  when (name == "Int") $
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
                  if name == "Int"
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

     ----------------
    -- ERROR OUTPUT --
     ----------------

formatError :: FilePath -> String -> ParseError -> PTSError
formatError expectedName src err
  = Error (Just file) (Just "Syntax Error") (lines msg) maybeSrc where

  -- extract information
  messages = errorMessages err
  pos = errorPos err
  name = sourceName pos

  (line, column) = convert (sourceLine pos) (sourceColumn pos)
  convert l c = if l > srcLineCount
                  then (srcLineCount, succ srcLineLength)
                  else (l, min c (length srcLine))

  maybeSrc = if name == expectedName
               then Just (lines src)
               else Nothing

  srcLines = lines src
  srcLineCount = length srcLines
  srcLine = srcLines !! (pred line)
  srcLineLength = length srcLine

  file = Position name line line column column
  msg = tail $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" messages

     ----------------------
    -- RUNNING the PARSER --
     ----------------------

parseInternal parser file text = do
  let result = runParser (do result <- (skipSpace *> parser <* eof)
                             state <- getState
                             return (result, state)) initialState file text
      initialState = case result of
                       ~(Right (_, (available, used))) -> ([0..] \\ used, [])
  case result of
    Left e -> throwError . pure . formatError file text $ e
    Right (ast, state) -> return ast

parseStmt = parseInternal stmt

parseStmts = parseInternal stmts
parseTerm = parseInternal expr
parseFile = parseInternal file

parseTermAtPos :: Monad m => String -> Int -> Int -> String -> m Term
parseTermAtPos file line col text =
  let result = runParser (do result <- p
                             state <- getState
                             return (result, state)) initialState file text
      initialState = case result of
                       ~(Right (_, (available, used))) -> ([0..] \\ used, [])
  in case result of
      Left err  -> fail $ show err
      Right (e, state) -> return e
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
