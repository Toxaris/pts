-- print information about the current package
-- (reads the cached build info, so only works after 'cabal configure')

import Prelude hiding (print)

import Distribution.Simple (packageId, pkgName, pkgVersion)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Text
import System.Environment

readLocalBuildInfo :: IO LocalBuildInfo
readLocalBuildInfo = getPersistBuildConfig "dist"

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
