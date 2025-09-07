{-# LANGUAGE TemplateHaskell #-}

module FetchCabal where

import Network.HTTP.Types.Header ( hAccept )
import Network.HTTP.Client
    ( Request(checkResponse, requestHeaders),
      Response(responseBody),
      Manager,
      brConsume,
      httpLbs,
      withResponse,
      parseRequest,
      throwErrorStatusCodes )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Control.Monad.Catch ( Exception, MonadThrow(..) )
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec.Warning (PWarning)
import qualified Distribution.Types.Version as CVer
import Data.List.NonEmpty (NonEmpty)
import Distribution.Parsec.Error (PError)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (OneComponentRequestedSpec))
import Distribution.PackageDescription (ComponentName(CLibName), defaultLibName)
import Data.Map (Map)
import Data.Aeson.TH (Options(..), deriveJSON, defaultOptions)
import Data.Char (toLower)
import Data.Aeson (decode)
import qualified Data.Version as BVer

httpBSMgrIO :: Manager -> Request -> IO (Response ByteString)
httpBSMgrIO req mgr = withResponse mgr req $ \ resp -> do
  bss <- brConsume $ responseBody resp
  return resp { responseBody = BS.concat bss }

type PackageString = String
type VersionString = String

hackageCabalURI :: PackageString -> Maybe VersionString -> String
hackageCabalURI pkg ver
  = "https://hackage.haskell.org/package/" ++ pkg
  ++ maybe [] ('-':) ver ++ "/" ++ pkg ++ ".cabal"

hackageCabalRequest
  :: MonadThrow m => PackageString -> Maybe VersionString -> m Request
hackageCabalRequest pkg ver = parseRequest $ "GET " ++ uri
  where
    uri = hackageCabalURI pkg ver

data CabalParseError
  = CabalParseError (Maybe CVer.Version) (NonEmpty PError) [PWarning]
  deriving Show

instance Exception CabalParseError

askHackageForGPD
  :: Manager
  -> PackageString
  -> Maybe VersionString
  -> IO (GenericPackageDescription, [PWarning])
askHackageForGPD mgr pkg ver = do
  req <- hackageCabalRequest pkg ver
  resp <- httpBSMgrIO mgr req { checkResponse = throwErrorStatusCodes }
  let (warns, parseRes) = runParseResult
        $ parseGenericPackageDescription
        $ responseBody resp
  case parseRes of
    Left (mver, errors) -> throwM $ CabalParseError mver errors warns
    Right res -> return (res, warns)



mainLibSpec :: ComponentRequestedSpec
mainLibSpec = OneComponentRequestedSpec $ CLibName defaultLibName


type FlagNameString = String

data PackagePreferrence
  = PPNormal | PPUnpreferred | PPDeprecated
  deriving (Show)
$(deriveJSON
  defaultOptions { constructorTagModifier = map toLower . drop 2 }
 ''PackagePreferrence)
  
hackagePackageURI :: PackageString -> String
hackagePackageURI pkg = "https://hackage.haskell.org/package/" ++ pkg

hackagePkgVersionsRequest
  :: MonadThrow m => PackageString -> m Request
hackagePkgVersionsRequest pkg
  = (\req -> req { requestHeaders = [(hAccept, "application/json")] })
    <$> parseRequest ("GET " ++ hackagePackageURI pkg)

data JSONError = JSONError
  deriving (Show)

instance Exception JSONError

askHackageForPkgVersions
  :: Manager
  -> PackageString
  -> IO (Map BVer.Version PackagePreferrence)
askHackageForPkgVersions mgr pkg = do
  req <- hackagePkgVersionsRequest pkg
  resp <- httpLbs req { checkResponse = throwErrorStatusCodes } mgr
  let res = responseBody resp
  case decode res of
    Nothing -> throwM JSONError
    Just a -> return a
