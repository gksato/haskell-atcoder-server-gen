
{-# LANGUAGE RecordWildCards #-}
module App (run) where

import qualified Cabal.Plan as Plan
import Config (Config(..), Workaround(..), Workarounds)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as Text
import Distribution.Pretty ( prettyShow )
import FetchCabal.WithPlan
import System.IO (hPutStrLn, stderr)
import Network.HTTP.Client (  Manager )
import Distribution.Types.PackageDescription (PackageDescription(PackageDescription, package, library))
import Distribution.Types.Library (Library(exposedModules))
import FetchGHCPkg (askGHCPkgForModuleList)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Network.HTTP.Client.TLS as TLS
import GenSource (genSourceWithExposedImports)


exposedModulesFromPackageDescr
  :: PackageDescription
  -> (String, [String])
exposedModulesFromPackageDescr PackageDescription {package, library}
  = (prettyShow $ package,
     map prettyShow $ maybe [] exposedModules library)

getModuleList
  :: Manager
  -> Workaround
  -> Plan.PlanJson
  -> Plan.Unit
  -> IO (String, [String])
getModuleList mgr NoWorkaround pj unit = do
    pd <- askHackageForUnitPD' mgr pj unit
    return $ exposedModulesFromPackageDescr pd
getModuleList mgr (AddFlagSettings flags) pj unit = do
    pd <- askHackageForUnitPDWithExtraFlags' mgr pj unit flags
    return $ exposedModulesFromPackageDescr pd
getModuleList _mgr FetchGHCPkg _pj Plan.Unit { uPId = pid@(Plan.PkgId (Plan.PkgName name) ver) } = do
    Just (pkgver, modules) <- askGHCPkgForModuleList Nothing (T.unpack name)
    if pkgver /= (T.unpack $ Plan.dispVer ver)
        then hPutStrLn stderr
                $ "Warning: version mismatch for package "
                ++ T.unpack name
                ++ ": plan.json has "
                ++ T.unpack (Plan.dispVer ver)
                ++ ", ghc-pkg has "
                ++ pkgver
        else return ()
    return (T.unpack $ Plan.dispPkgId pid, modules)

takeWorkaround
  :: Workarounds
  -> Plan.Unit
  -> Workaround
takeWorkaround workarounds Plan.Unit { uPId = Plan.PkgId (Plan.PkgName name) _ver }
  = fromMaybe NoWorkaround $ workarounds Map.!? T.unpack name

getAllModuleLists
  :: Manager
  -> Workarounds
  -> Plan.PlanJson
  -> [Plan.Unit]
  -> IO [(String, [String])]
getAllModuleLists mgr workarounds pj units
 = mapM (\ unit -> getModuleList mgr (takeWorkaround workarounds unit) pj unit) units

run :: Config -> IO () 
run Config {plan, rootPkgName, rootComponent, workarounds} = do
  pj@Plan.PlanJson {..}
     <- Plan.findAndDecodePlanJson plan
    
  let mainComps = do
        Plan.Unit { uPId = Plan.PkgId (Plan.PkgName pName) _ver, uComps} <- Map.elems pjUnits
        guard $ rootPkgName == T.unpack pName
        maybeToList $ uComps Map.!? rootComponent
  mainComp <- case mainComps of
        [] -> error $ "cannot find component " ++ show rootComponent
                     ++ " in package " ++ rootPkgName
        [mainComp] -> return mainComp
        (mainComp:_:_) -> hPutStrLn stderr
                           ("Warning: multiple components named "
                            ++ show rootComponent
                            ++ " in package "
                            ++ rootPkgName
                            ++ ", using one of them")
                           >> return mainComp
  
  mgr <- TLS.newTlsManager
  modls <- getAllModuleLists mgr workarounds pj
    $ map (pjUnits Map.!)
    $ Set.toList $ Plan.ciLibDeps mainComp

  Text.putStr
    $ genSourceWithExposedImports modls