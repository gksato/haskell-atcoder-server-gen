module GenSource where
import Distribution.Types.PackageDescription (PackageDescription(package, library, licenseFiles), license)
import Distribution.Types.Library (Library(exposedModules))
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Builder as TLazyB
import Distribution.Pretty ( prettyShow )
import Distribution.Types.PackageId (PackageIdentifier(..))

genSourceWithExposedImports
  :: [PackageDescription]
  -> [(String, [String])]
  -> TLazy.Text
genSourceWithExposedImports pds others = TLazyB.toLazyText
  $ moduleDecl
  <> foldr
     (\(pname, modnames) rest ->
         "-- " <> TLazyB.fromString pname
         <> newLine
         <> foldr
            (\modname rest' ->
               importQualified
               <> TLazyB.fromString modname
               <> newLine
               <> rest')
            (newLine <> rest)
            modnames
     )
     mainFct modLists
  where
    modLists
      = map (\ pdescr ->
                (prettyShow $ package pdescr,
                  map prettyShow $ maybe [] exposedModules
                  $ library pdescr))
        pds
      ++ others
    moduleDecl = "{-# OPTIONS_GHC -Wno-unused-imports #-}\n" <>
                 "module Main where\n\n\n"
    mainFct = "\nmain :: Prelude.IO ()\nmain = Prelude.return ()\n"
    importQualified = "import qualified "
    newLine = TLazyB.singleton '\n'

genLicenses
  :: [PackageDescription]
  -> TLazy.Text
genLicenses packages = TLazyB.toLazyText
  $ foldr
  (\descr rest ->
     let urlprefix = TLazyB.fromString
           "https://hackage.haskell.org/package/"
           <> TLazyB.fromString (prettyShow $ pkgName $ package descr)
           <> "/src/" in
     TLazyB.fromString (prettyShow $ pkgName $ package descr)
     <> TLazyB.singleton '('
     <> TLazyB.fromString (prettyShow $ license descr)
     <> TLazyB.fromString "): "
     <> foldr (\ path rest' -> urlprefix <> TLazyB.fromString (prettyShow path) <> TLazyB.singleton ',' <> rest')
        (newLine <> rest)
        (licenseFiles descr))
  mempty
  packages
  where
    newLine = TLazyB.singleton '\n'
