{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cabal.Plan
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Network.HTTP.Client.TLS as TLS

import FetchCabal.WithPlan
import qualified Data.Text.Lazy.IO as Text
import GenSource (genSourceWithExposedImports)

main :: IO ()
main = do
  pj@PlanJson {..} <- findAndDecodePlanJson
        $ ProjectRelativeToDir "/home/runner/submission"
  let rootUnit = (pjUnits Map.!) $ head $ Set.toList $ planJsonIdRoots pj
  let deps = Set.toList
        $ ciLibDeps $ uComps rootUnit Map.! CompNameExe "main"
  mgr <- TLS.newTlsManager
  pds <- mapM (askHackageForUnitPD' mgr pj . (pjUnits Map.!)) deps
  Text.putStr $ genSourceWithExposedImports pds
  return ()
