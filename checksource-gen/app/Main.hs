{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cabal.Plan
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Network.HTTP.Client.TLS as TLS

import FetchCabal.WithPlan
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as Text
import GenSource (genSourceWithExposedImports)
import qualified Cabal.Plan as Plan

main :: IO ()
main = do
  pj@PlanJson {..} <- findAndDecodePlanJson
        $ ProjectRelativeToDir "/home/runner/submission"
  let rootUnit = (pjUnits Map.!)
        . fromJust
        . L.find (\(Plan.UnitId text) -> T.isPrefixOf (T.pack "submission") text)
        . Set.toList
        $ planJsonIdRoots pj
  let deps = Set.toList
        $ ciLibDeps $ uComps rootUnit Map.! CompNameExe "main"
  mgr <- TLS.newTlsManager
  pds <- mapM (askHackageForUnitPD' mgr pj)
    $ filter (\ Plan.Unit { .. } -> case uPId of
                 Plan.PkgId (Plan.PkgName pkgname) _
                   -> pkgname /= "ghc-boot-th")
    $ map (\dep -> fromJust (Map.lookup dep pjUnits)) deps
  Text.putStr
    $ genSourceWithExposedImports pds
    [("ghc-boot-th-9.8.4", ["GHC.LanguageExtensions.Type",
            "GHC.ForeignSrcLang.Type",
            "GHC.Lexeme"])]
  return ()
