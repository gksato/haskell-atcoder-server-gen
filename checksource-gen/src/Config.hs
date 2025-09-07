module Config (Config(..), defaultConfig, Workarounds, Workaround(..)) where

import Cabal.Plan (SearchPlanJson(ProjectRelativeToDir), CompName(CompNameExe))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Config = Config {
    plan :: SearchPlanJson,
    rootPkgName :: String,
    rootComponent :: CompName,
    workarounds :: Workarounds 
}
defaultConfig :: Config
defaultConfig = Config { plan = ProjectRelativeToDir "/home/runner/submission",
                         rootPkgName = "submission",
                         rootComponent = CompNameExe "main",
                         workarounds = Map.empty }

type Workarounds = Map String Workaround

data Workaround
    = NoWorkaround -- ^ apply no workaround. 
    | FetchGHCPkg
    | AddFlagSettings [(String, Bool)]