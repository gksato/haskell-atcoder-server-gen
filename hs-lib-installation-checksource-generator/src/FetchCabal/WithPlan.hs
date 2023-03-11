{-# LANGUAGE RecordWildCards #-}

module FetchCabal.WithPlan where


import FetchCabal ( askHackageForGPD, mainLibSpec )
import qualified Cabal.Plan as Plan
import           Cabal.Plan ( PlanJson(PlanJson) )
import Network.HTTP.Client (  Manager )
import Distribution.Types.GenericPackageDescription
    ( GenericPackageDescription )
import Distribution.Parsec.Warning (PWarning)
import qualified Data.Text as Text
import Distribution.Types.Dependency (Dependency)
import Distribution.PackageDescription (PackageDescription, FlagAssignment, mkFlagAssignment, mkFlagName)
import Distribution.PackageDescription.Configuration (finalizePD)
import qualified Data.Map.Strict as Map
import Distribution.System (Platform(Platform), classifyArch, ClassificationStrictness (Permissive), classifyOS)
import Distribution.Compiler (unknownCompilerInfo, CompilerId (CompilerId), classifyCompilerFlavor, AbiTag (NoAbiTag))
import qualified Distribution.Types.Version as CVer
import Data.Either (fromRight)

askHackageForUnitGPD
  :: Manager
  -> Plan.Unit
  -> IO (GenericPackageDescription, [PWarning])
askHackageForUnitGPD mgr Plan.Unit { .. }
  = askHackageForGPD mgr (Text.unpack pkgname)
    $ Just $ Text.unpack $ Plan.dispVer ver
  where
    Plan.PkgId (Plan.PkgName pkgname) ver = uPId

finalizePDWithPlan
  :: PlanJson
  -> Plan.Unit
  -> GenericPackageDescription
  -> Either [Dependency] (PackageDescription, FlagAssignment)
finalizePDWithPlan PlanJson { .. } Plan.Unit { .. }
  = finalizePD flagAss mainLibSpec (const True) platform compilerInfo []
  where
    flagAss = mkFlagAssignment
      $ map (\ (Plan.FlagName !name, !val) ->
               (mkFlagName $ Text.unpack name, val))
      $ Map.toList uFlags
    platform = Platform arch os
    arch = classifyArch Permissive $ Text.unpack pjArch
    os = classifyOS Permissive $ Text.unpack pjOs
    compilerInfo = unknownCompilerInfo compilerId NoAbiTag
    compilerId = CompilerId compilerFlavor compilerVer
    compilerFlavor = classifyCompilerFlavor $ Text.unpack pjCompilerName
    compilerVer = CVer.mkVersion pjCompilerVer
    Plan.PkgId (Plan.PkgName pjCompilerName) (Plan.Ver pjCompilerVer)
      = pjCompilerId


askHackageForUnitPD
  :: Manager
  -> PlanJson
  -> Plan.Unit
  -> IO (PackageDescription, FlagAssignment, [PWarning])
askHackageForUnitPD mgr pj unit = do
  (gpd, warns) <- askHackageForUnitGPD mgr unit
  let (res, fa') = fromRight (error "Impossible")
        $ finalizePDWithPlan pj unit gpd
  return (res, fa', warns)

