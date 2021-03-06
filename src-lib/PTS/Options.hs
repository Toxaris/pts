{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module PTS.Options where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Char (toLower)
import Data.List (mapAccumL, intercalate)
import Data.Maybe (fromJust)

import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath (splitSearchPath)

import Text.PrettyPrint.HughesPJ hiding (render)

import PTS.Instances

import Paths_pts (getDataFileName)

-- options record
data OptionsStruct t = Options
  { optColumns :: Int
  , optInstance :: t
  , optAllowSubLang :: Bool
  , optLiterate :: Bool
  , optShowFullTerms :: Bool
  , optDebugQuote :: Bool
#ifdef DEBUG_TYPING
  , optDebugType :: Bool
#endif
  , optQuiet :: Bool
  , optPath :: [FilePath]
  }

type CmdlineOptions = OptionsStruct (Maybe PTS)
type Options = OptionsStruct (Maybe PTS) -- XXX remove Maybe

-- | Gets the current object language.
-- You're supposed to only call it when it
-- is guaranteed that a language is configured.
getLanguage :: MonadReader Options m => m PTS
getLanguage = asks (fromJust . optInstance)

-- monadic option combinators
whenOption :: (MonadReader Options m) => (Options -> Bool) -> m () -> m ()
whenOption f act = ask >>= \opt -> when (f opt) act

prettyFail :: MonadReader Options m => Doc -> m a
prettyFail doc = asks (flip render doc . optColumns) >>= fail

-- default options
defaultOptions = Options
  { optColumns = 80
  , optInstance = Nothing
  , optAllowSubLang = True
  , optLiterate = False
  , optShowFullTerms = False
  , optDebugQuote = False
#ifdef DEBUG_TYPING
  , optDebugType = False
#endif
  , optQuiet = True
  , optPath = ["."]
  }

#ifndef DEBUG_TYPING
optDebugType :: Options -> Bool
optDebugType _ = False
#endif


setColumns      x options = options {optColumns = x}
setInstance     x options = options {optInstance = x}
setAllowSubLang x options = options {optAllowSubLang = x}
setLiterate     x options = options {optLiterate = x}
setDebugTerms   x options = options {optShowFullTerms = x}
setDebugQuote   x options = options {optDebugQuote = x}
#ifdef DEBUG_TYPING
setDebugType    x options = options {optDebugType = x}
#endif
setQuiet        x options = options {optQuiet = x}
setPath         x options = options {optPath = x}

extendPath p options
  =  options {optPath = optPath options ++ splitSearchPath p}

data Flag
  = Error String
  | Help
  | Flag (Options -> Options)
  | FilePath FilePath
  | ShowInsts Bool
  | LocateEmacsMode

-- option descriptions
options =
  [ Option ['c'] ["columns"]              (ReqArg handleColumns  "c"     ) "wrap output at specified column"
  , Option ['p'] ["pts", "instance"]      (ReqArg handlePTS      "i"     ) "implement specified pure type systems instance"
  , Option ['s'] ["sub-langs"]            (OptArg handleSubLang  "yes/no") "allow modules to use sublanguages"
  , Option ['l'] ["literate"]             (OptArg handleLiterate "yes/no") "treat input as literate source files"
  , Option ['d'] ["debug"]                (ReqArg handleDebug    "option") "activate specified debug options (for debugging PTS itself)"
  , Option ['q'] ["quiet"]                (NoArg  handleQuiet            ) "be quiet (default)"
  , Option ['v'] ["verbose"]              (NoArg  handleVerbose          ) "print lots of info (to debug PTS programs)"
  , Option ['i'] []                       (OptArg handlePath     "paths" ) "add paths to search path, or reset search path"
  , Option ['e'] ["enumerate-instances"]  (OptArg handleShowInsts "format") "enumerate built-in pure-type-system instances"
  , Option []    ["locate-emacs-mode"]    (NoArg  handleLocateEmacsMode  ) "locate the bundled emacs-mode"
  , Option "?h"  ["help"]                 (NoArg  handleHelp             ) "display this help"
  ]

-- option processing
handleHelp         = Help

handleColumns  arg = case reads arg of
                       [(n, "")]     -> Flag   (setColumns    n         )
                       _             -> Error  ("Error: --columns option expects integer instead of " ++ arg)

handlePTS      arg = case lookupInstance arg of
                       Just inst -> Flag (setInstance $ Just inst)
                       Nothing   -> Error $ show $ text "Error: Unknown pure type system instance" <+> text arg $$
                                      text "" $$
                                      supported $$
                                      text "" $$
                                      text "To learn more about the instances, run: pts --enumerate-instances"

getFlag arg optName = case fmap (map toLower) arg of
                       Nothing       -> return True
                       Just "yes"    -> return True
                       Just "no"     -> return False
                       Just other    -> fail ("Error: --" ++ optName ++ " option expects 'yes' or 'no' instead of " ++ other)

handleLiterate arg = case getFlag arg "literate" of
                       Right b       -> Flag   (setLiterate b            )
                       Left err      -> Error  err

handleSubLang arg = case getFlag arg "sub-langs" of
                       Right b       -> Flag   (setAllowSubLang b        )
                       Left err      -> Error  err

handleDebug arg    = case map toLower arg of
                       "toplevel"    -> Flag   (setDebugTerms True       )
                       "quoting"     -> Flag   (setDebugQuote True       )
#ifdef DEBUG_TYPING
                       "typing"      -> Flag   (setDebugType  True       )
                       _             -> Error  ("Error: --debug option expects 'toplevel', 'typing' or 'quoting' instead of " ++ arg)
#else
                       "typing"      -> Error  ("Error: this version of PTS was compiled without support for --debug=typing")
                       _             -> Error  ("Error: --debug option expects 'toplevel' or 'quoting' instead of " ++ arg)
#endif

handleQuiet        = Flag (setQuiet True)
handleVerbose      = Flag (setQuiet False)

handlePath Nothing  = Flag (setPath [])
handlePath (Just p) = Flag (extendPath p)

handleShowInsts Nothing = ShowInsts False
handleShowInsts (Just "machine-readable") = ShowInsts True
handleShowInsts (Just other) = Error ("Error: --enumerate-instances option expects 'machine-readable' or nothing instead of " ++ other)

handleLocateEmacsMode = LocateEmacsMode


-- flag processing

processFlagsHelp []         = return ()
processFlagsHelp (Help : _) = liftIO printHelp
processFlagsHelp (_ : rest) = processFlagsHelp rest

processFlagsErrors     []                   = return ()
processFlagsErrors     (Error msg  : flags) = fail msg
processFlagsErrors     (_          : flags) = processFlagsErrors flags

processFlags opt []                   = return opt
processFlags opt (Flag f     : flags) = processFlags (f opt) flags
processFlags opt (_          : flags) = processFlags opt flags

processFlagsShowInsts []              = return ()
processFlagsShowInsts (ShowInsts False : _) = liftIO printInstances
processFlagsShowInsts (ShowInsts True : _) = liftIO printInstancesMachineReadable
processFlagsShowInsts (_ : rest)      = processFlagsShowInsts rest

processFlagsLocateEmacsMode []                    = return ()
processFlagsLocateEmacsMode (LocateEmacsMode : _) = liftIO locateEmacsMode
processFlagsLocateEmacsMode (_ : rest)            = processFlagsLocateEmacsMode rest

printHelp = putStrLn (usageInfo (render 80 header) options) where
  header =
    programName $$ supported $$ optionsText
  programName =
    text "PTS interpreter."
  optionsText =
    text "Options:"

supported :: Doc
supported = fsep $ concat
  [  [ text "Supported instances:"]
  ,  punctuate (text ",")
       [text (head (name i)) | i <- instances]
  ,  map text $ words
       "(and synonyms)"
  ]

printInstancesMachineReadable :: IO ()
printInstancesMachineReadable = putStrLn $ render 80 $ fsep
  [ text n | i <- instances, n <- name i ]

printInstances :: IO ()
printInstances = putStrLn (render 80 info) where
  info = text "Available instances:" $$ text "" $$
         (vcat $ punctuate (text "" $+$ text "")
           [ (text $ head $ name i) $$
                (nest 8 $
                   (fsep $ map text $ words $ description i) $$
                   (fsep $
                     text "Synonyms:" : (punctuate (text ",") $ map text $ tail $ name i)))
           | i <- instances ])

locateEmacsMode :: IO ()
locateEmacsMode = do
  path <- getDataFileName "emacs"
  putStr path

-- printing
render :: Int -> Doc -> String
render n = renderStyle (Style PageMode n 1)

-- main entry point
parseCommandLine :: (Functor m, MonadIO m) => m (Options, [FilePath])
parseCommandLine = do
  cmdline <- liftIO getArgs
  let (flags, fileNames, errors) = getOpt Permute options cmdline
  mapM_ (fail . ("Syntax Error in command line: " ++)) errors
  processFlagsHelp flags
  processFlagsErrors flags
  processFlagsLocateEmacsMode flags
  processFlagsShowInsts flags
  flags <- processFlags defaultOptions flags
  return (flags, fileNames)
