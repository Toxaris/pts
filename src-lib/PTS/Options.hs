{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module PTS.Options where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans

import Data.Char
import Data.List (mapAccumL)

import System.Console.GetOpt
import System.Environment (getArgs)

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
  }

setColumns    f1 (Options _  f2 f3 f4 f5 f6) = Options f1 f2 f3 f4 f5 f6
setInstance   f2 (Options f1 _  f3 f4 f5 f6) = Options f1 f2 f3 f4 f5 f6
setLiterate   f3 (Options f1 f2 _  f4 f5 f6) = Options f1 f2 f3 f4 f5 f6
setDebugTerms f4 (Options f1 f2 f3 _  f5 f6) = Options f1 f2 f3 f4 f5 f6
setDebugQuote f5 (Options f1 f2 f3 f4 _  f6) = Options f1 f2 f3 f4 f5 f6
setDebugType  f6 (Options f1 f2 f3 f4 f5 _ ) = Options f1 f2 f3 f4 f5 f6

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
  , Option "?h"  ["help"]            (NoArg  handleHelp             ) "display this help"
  ]

-- option processing
handleHelp         = Help

handleColumns  arg = case reads arg of
                       [(n, "")]     -> Local  (setColumns    n         )
                       _             -> Error  ("Error: columns option expects integer instead of " ++ arg)
handlePTS      arg = case map toLower arg of
                       "stlc"        -> Global (setInstance simplytyped )
                       "simplytyped" -> Global (setInstance simplytyped )
                       "fomega"      -> Global (setInstance fomega      )
                       "coc"         -> Global (setInstance coc         )
                       "lambdastar"  -> Global (setInstance lambdastar  )
                       "fomegastar"  -> Global (setInstance fomegastar  )
                       "fomegaomega" -> Global (setInstance fomegaomega )
                       other         -> Error  ("Error: Unknown pure type system instance " ++ arg)

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
