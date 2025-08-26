{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (run)
import Config (Workaround(..), defaultConfig, Config(workarounds))
import qualified Data.Map.Strict as Map

main :: IO ()
main = run config

config :: Config
config = defaultConfig { workarounds = Map.fromList [
      ("ghc-boot-th", FetchGHCPkg),
      ("base", FetchGHCPkg),
      ("text", AddFlagSettings [("simdutf", False)])
  ]}