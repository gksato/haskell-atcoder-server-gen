{-# LANGUAGE LambdaCase #-}
module FetchGHCPkg where
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.Maybe (fromMaybe)
import System.IO (hPutStrLn, stderr)
import Data.List (groupBy)

askGHCPkgForModuleList
  :: Maybe String -- ^ `ghc-pkg` executable
  -> String -- ^ package name
  -> IO (Maybe (String, [String]))
askGHCPkgForModuleList mbGhcPkg pkg = do
  (exitCode, out, outErr) <- readProcessWithExitCode
    (fromMaybe "ghc-pkg" mbGhcPkg)
    ["field", "--simple-output", pkg, "version,exposed-modules"]
    ""
  case exitCode of
    ExitFailure _ -> do
      hPutStrLn stderr $ "ghc-pkg failed for package " ++ pkg ++ " with:\n" ++ outErr
      return Nothing
    ExitSuccess -> do
      let [verLine, modLine] = lines out
          modules = if ',' `elem` modLine
                      then map (head . words) $ filter (/= ",")
                           $ groupBy (\x y -> x /= ',' && y /= ',') modLine
                      else words modLine
      return $ Just (verLine, modules) 
