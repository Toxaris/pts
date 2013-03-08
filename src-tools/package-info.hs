-- print information about the current package
-- (reads the cached build info, so only works after 'cabal configure')

import Prelude hiding (print)

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Text
import System.Environment

readLocalBuildInfo :: IO LocalBuildInfo
readLocalBuildInfo = do
  text <- readFile "dist/setup-config"
  let body = dropWhile (/= '\n') text
  return (read body)

print :: Text a => a -> IO ()
print x = putStrLn (display x)

main = do
  arg <- getArgs
  lbi <- readLocalBuildInfo
  let lpc = localPkgDescr lbi
  let pid = packageId lpc
  case arg of
    ["--package"]          ->  print pid
    ["--package-name"]     ->  print (pkgName pid)
    ["--package-version"]  ->  print (pkgVersion pid)
