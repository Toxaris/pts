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
import Data.Functor.Identity
import Data.List ((\\))

import System.IO

import Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos (initialPos)

import Text.Parsec (ParsecT)

import PTS.Error
import PTS.Syntax.Algebra
import PTS.Syntax.Constants
import PTS.Syntax.File
import PTS.Syntax.Names
import PTS.Syntax.Statement
import PTS.Syntax.Telescope
import PTS.Syntax.Term

     ---------------
    -- PARSER TYPE --
     ---------------

type UserState = (SourcePos, [Integer], [Integer])
type Parser = ParsecT String UserState Identity

     ---------------------
    -- PARAMETRIC PARSER --
     ---------------------

-- left-recursion handling
term :: Parser a -> Parser (a -> b) -> (Position -> b -> a) -> Parser a
term simple rec pos = result where
  result = combine <$> getPosition <*> simple <*> many ((,) <$> rec <*> getPosition)
  combine p = foldl' (\x (f, q) -> setPos pos p (f x) q)

-- left-recursive syntax pattern: "x -> y"
arr cons arrow simple = flip cons <$> (arrow *> simple)

-- left-recursive syntax pattern: "x y"
app cons simple = flip cons <$> simple

-- non-recursive syntax pattern: "ident"
var cons ident = cons <$> ident

-- non-recursive syntax pattern: "constant"
con cons constant = cons <$ constant

withPos f p = setPos f <$> getPosition <*> p <*> getEndPosition where

getEndPosition = do
  (pos, available, explicits) <- getState
  return pos

setPos f p1 x p2 = f (Position (sourceName p1) (sourceLine p1) (sourceLine p2) (sourceColumn p1) (pred $ sourceColumn p2)) x

     -----------------
    -- PTS PARSER --
     -----------------

intop n f x = mkIntOp f <$> (keyword n *> x) <*> (x <?> "second argument of '" ++ n ++ "'")

expr = term simple rec mkPos where
  simple = withPos mkPos $ asum
    [ termParens expr
    , brackets expr
    , multAbs mkLam lambda
    , multAbs mkPi pi
    , mkIfZero <$> (keyword "if0" *> expr)
             <*> (keyword "then" *> expr)
             <*> (keyword "else" *> expr)
    , intop "add" Add simple
    , intop "sub" Sub simple
    , intop "mul" Mul simple
    , intop "div" Div simple
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
multAbs :: (Name -> Term -> Term -> Term) -> Parser a -> Parser Term
multAbs constructor parser = foldTelescope constructor <$> (parser *> telescopeOrArgGroup) <*> (dot *> expr)

unquote = char '$' *> asum
  [ var mkVar ident
  , parens expr]

stmt = withPos StmtPos $ asum
  [ Export <$> (keyword "export" *> ident <* semi)
  , Import <$> (keyword "import" *> modname <* semi)
  , Assertion <$> (keyword "assert" *> expr) <*> optionMaybe (colon1 *> expr) <*> optionMaybe (assign *> expr) <* semi
  , try (Term <$> expr <* semi)
  , Bind <$> ident <*> telescope <*> optionMaybe (colon1 *> expr) <* assign <*> (Just <$> expr) <* semi]

stmts = many (optional pragma *> stmt)

telescope :: Parser (Telescope Term)
telescope = many (parens argGroup)

inferredOrExplicitType = (colon1 *> expr) <|> (mkInfer <$> nextInfer)

argGroup = (,) <$> names <*> inferredOrExplicitType

argGroupOrNames = asum
  [ parens argGroup
  , (,) <$> names <*> (mkInfer <$> nextInfer)
  ]

telescopeOrArgGroup :: Parser (Telescope Term)
telescopeOrArgGroup = asum
  [ do ns <- names
       asum
         [ do colon1
              typ <- expr
              return [(ns, typ)]
         , do typ <- mkInfer <$> nextInfer
              rest <- many argGroupOrNames
              return ((ns, typ) : rest)
         ]
  , many argGroupOrNames
  ]

langName :: Parser String
langName = show <$> ident

file = File <$> optionMaybe (keyword "language" *> langName <* semi) <*> optionMaybe (keyword "module" *> modname <* semi) <*> stmts

names = many1 identOrMeta

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

keywords = ["Lambda", "lambda", "Pi", "if0", "then", "else", "->", "add", "mul", "sub", "div", "module", "import", "export", "assert", "language", "_("]

identChar x = not (isSpace x) && x `notElem` ".:=;/()[]$"

identOrMeta = ident <|> meta

meta = lexem (do char '$'
                 first <- satisfy (\c -> isLetter c && isLower c)
                 rest <- many (satisfy (\c -> isAlphaNum c || c `elem` "'_"))
                 return (read ('$' : first : rest)))
         <?> "meta variable name"

nextInfer = do ~(pos, available, explicits) <- getState
               setState (pos, tail available, explicits)
               return $ head available

infer = lexem (do char '_'
                  n <- number
                  ~(pos, available, explicits) <- getState
                  setState (pos, available, n:explicits)
                  return n)
        <|>
        lexem (do char '_'
                  nextInfer)
        

ident :: Parser Name
ident = lexem (do name <- namepart
                  when (Prelude.all (== '*') name) $
                    unexpected ("constant")

                  when (name == "Int") $
                    unexpected ("constant")

                  return (read name))
          <?> "variable name"

namepart = do
  name <- many1 (satisfy identChar)
  when (name `elem` keywords) $
    unexpected ("keyword " ++ name)
  when (Prelude.all (`elem` ['0'..'9']) name) $
    unexpected ("numeric literal " ++ name)
  when (isDigit (head name)) $
    pzero
  return name

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

keyword s | s `elem` keywords = lexem (string s <* notFollowedBy (satisfy identChar))
keyword s  = error $ "Keyword '" ++ s ++ "' not in keywords"

comment = string "/*" <* manyTill anyChar (try (string "*/"))

skipSpace = skipMany ((space *> pure () <|> comment *> pure ()) <?> "")
lexem p = try p <* do
  pos <- getPosition
  (_, available, explicits) <- getState
  setState (pos, available, explicits)
  skipSpace

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
                       ~(Right (_, (_, available, used))) -> (initialPos file, [0..] \\ used, [])
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
                       ~(Right (_, (_, available, used))) -> (initialPos file, [0..] \\ used, [])
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
