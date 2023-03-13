module GenSource where
import Distribution.Types.PackageDescription (PackageDescription(package, library, licenseFiles), license)
import Distribution.Types.Library (Library(exposedModules))
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Builder as TLazyB
import Distribution.Pretty ( prettyShow )
import Distribution.Types.PackageId (PackageIdentifier(..))

genSourceWithExposedImports
  :: [PackageDescription]
  -> TLazy.Text
genSourceWithExposedImports packages = TLazyB.toLazyText
  $ moduleDecl
  <> foldr
     (\descr rest ->
         "-- " <> TLazyB.fromString (prettyShow $ package descr)
         <> newLine
         <> foldr
            (\modl rest' ->
               importQualified
               <> TLazyB.fromString (prettyShow modl)
               <> newLine
               <> rest')
            (newLine <> rest)
            (maybe [] exposedModules $ library descr)
     )
     mainFct packages
  where
    moduleDecl = "module Main where\n\n\n"
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
