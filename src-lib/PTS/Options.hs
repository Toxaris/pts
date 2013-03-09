{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module PTS.Options where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans

import Data.Char
import Data.List (mapAccumL)

import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath (splitSearchPath)

import Parametric.Pretty (Pretty, Doc, multiLine)

import PTS.Instances

-- options record
data Options = Options
  { optColumns :: Int
  , optInstance :: PTS
  , optLiterate :: Bool
  , optShowFullTerms :: Bool
  , optDebugQuote :: Bool
  , optDebugType :: Bool
  , optQuiet :: Bool
  , optPath :: [FilePath]
  }

-- monadic option combinators
whenOption :: (MonadReader Options m) => (Options -> Bool) -> m () -> m ()
whenOption  f act = ask >>= \opt            -> when (f opt) act

prettyFail :: MonadReader Options m => Doc -> m a
prettyFail doc = asks (flip multiLine doc . optColumns) >>= fail

-- default options
defaultOptions = Options
  { optColumns = 80
  , optInstance = fomegastar
  , optLiterate = False
  , optShowFullTerms = False
  , optDebugQuote = False
  , optDebugType = False
  , optQuiet = False
  , optPath = ["."]
  }

setColumns    x options = options {optColumns = x}
setInstance   x options = options {optInstance = x}
setLiterate   x options = options {optLiterate = x}
setDebugTerms x options = options {optShowFullTerms = x}
setDebugQuote x options = options {optDebugQuote = x}
setDebugType  x options = options {optDebugType = x}
setQuiet      x options = options {optQuiet = x}
setPath       x options = options {optPath = x}

extendPath p options
  =  options {optPath = optPath options ++ splitSearchPath p}

data Flag
  = Error String
  | Help
  | Global (Options -> Options)
  | Local (Options -> Options)
  | FilePath FilePath

-- option descriptions
options =
  [ Option ['c'] ["columns"]         (ReqArg handleColumns  "c"     ) "wrap output at specified column"
  , Option ['p'] ["pts", "instance"] (ReqArg handlePTS      "i"     ) "implement specified pure type systems instance"
  , Option ['l'] ["literate"]        (OptArg handleLiterate "b"     ) "treat input as literate source files"
  , Option ['d'] ["debug"]           (ReqArg handleDebug   "option" ) "activate specified debug options"
  , Option ['q'] ["quiet"]           (NoArg  handleQuiet            ) "don't print so much"
  , Option ['i'] []                  (OptArg handlePath "paths"     ) "add paths to search path, or reset search path" 
  , Option "?h"  ["help"]            (NoArg  handleHelp             ) "display this help"
  ]

-- option processing
handleHelp         = Help

handleColumns  arg = case reads arg of
                       [(n, "")]     -> Local  (setColumns    n         )
                       _             -> Error  ("Error: columns option expects integer instead of " ++ arg)

handlePTS      arg = case map toLower arg of
                       str | inname $ name lama -> Global (setInstance lama)
                           | inname $ name lam2 -> Global (setInstance lam2)
                           | inname $ name lamp -> Global (setInstance lamp)
                           | inname $ name lamv -> Global (setInstance lamv)
                           | inname $ name lap2 -> Global (setInstance lap2)
                           | inname $ name lapv -> Global (setInstance lapv)
                           | inname $ name lamc -> Global (setInstance lamc)
                           | inname $ name lams -> Global (setInstance lams)
                           | inname $ name laws -> Global (setInstance laws)
                           | inname $ name lawu -> Global (setInstance lawu)
                           | otherwise        -> Error  ("Error: Unknown pure type system instance " ++ arg)
                           where inname = elem str

handleLiterate arg = case fmap (map toLower) arg of
                       Nothing       -> Local  (setLiterate   True      )
                       Just "yes"    -> Local  (setLiterate   True      )
                       Just "no"     -> Local  (setLiterate   False     )
                       Just other    -> Error  ("Error: literate option expects 'yes' or 'no' instead of " ++ other)

handleDebug arg    = case map toLower arg of
                       "toplevel"    -> Local  (setDebugTerms True       )
                       "typing"      -> Local  (setDebugType  True       )
                       "quoting"     -> Local  (setDebugQuote True       )
                       _             -> Error  ("Error: debug option expects 'toplevel', 'typing' or 'quoting' instead of " ++ arg)

handleQuiet        = Global (setQuiet True)

handlePath Nothing = Local (setPath [])
handlePath (Just p) = Local (extendPath p)

-- order requirements
argOrder = ReturnInOrder FilePath

-- flag processing

processFlagsHelp []         = return ()
processFlagsHelp (Help : _) = liftIO printHelp
processFlagsHelp (_ : rest) = processFlagsHelp rest

processFlagsErrors     []                   = return ()
processFlagsErrors     (Error msg  : flags) = fail msg
processFlagsErrors     (_          : flags) = processFlagsErrors flags

processFlagsGlobal opt []                   = return opt
processFlagsGlobal opt (Global f   : flags) = processFlagsGlobal (f opt) flags
processFlagsGlobal opt (_          : flags) = processFlagsGlobal opt flags

processFlagsLocal  opt []                   = return []
processFlagsLocal  opt (Local f    : flags) = processFlagsLocal (f opt) flags
processFlagsLocal  opt (FilePath p : flags) = fmap ((opt, p) :) (processFlagsLocal opt flags)
processFlagsLocal  opt (_          : flags) = processFlagsLocal opt flags

printHelp = putStrLn (usageInfo "PTS interpreter" options)

-- main entry point
parseCommandLine :: (Functor m, MonadIO m) => ([(Options, FilePath)] -> m a) -> m ()
parseCommandLine handler = do
  cmdline <- liftIO getArgs
  let (flags, [], errors) = getOpt argOrder options cmdline
  mapM_ (fail . ("Syntax Error in command line: " ++)) errors
  processFlagsHelp flags
  processFlagsErrors flags
  global <- processFlagsGlobal defaultOptions flags
  jobs <- processFlagsLocal global flags
  handler jobs
  return ()
