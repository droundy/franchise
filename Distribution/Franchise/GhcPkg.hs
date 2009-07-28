{- Copyright (c) 2008 David Roundy

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE. -}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module Distribution.Franchise.GhcPkg
    ( readPkgMappings, addToGhcPath,
      -- we don't really want to export
      -- the following, but it stops ghc
      -- from displaying warnings.
      InstalledPackageInfo(..), PackageIdentifier(..),
      Version(..), License(..) )
    where

import System.Directory ( doesFileExist, createDirectoryIfMissing )
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.Trie
import Distribution.Franchise.Env ( setEnv )

readPkgMappings :: C (Trie [String])
readPkgMappings =
    do x <- getPackageConfs
       confs <- mapM cat x
       let pinfos = map readlist confs ++ map (map fixIPI . readlist) confs
           readlist str = case reads str of
                          [(y,_)] -> y
                          _ -> []
           mods :: InstalledPackageInfo String -> [(String,String)]
           mods ipi = zip (exposedModules ipi)
                      (repeat $ showPackage $ package ipi)
           addmods [] trie = trie
           addmods ((m,p):r) trie = addmods r $ alterT m (mycons p) trie
               where mycons pp Nothing = Just [pp]
                     mycons pp (Just ps) = Just (pp:ps)
       -- putS $ unlines $ ("HERE ARE THE MAPS" :) $ map show $
       --    toListT $ addmods (concatMap mods $ concat pinfos) emptyT
       return $ addmods (concatMap mods $ concat pinfos) emptyT

getPackageConfs :: C [String]
getPackageConfs =
    do list <- systemOut "ghc-pkg" ["list"]
       return $ map (init . filter (/='\r')) $ filter ((/= ' ') . head) $
              filter (not . null) $ lines list

addToGhcPath :: FilePath -> C ()
addToGhcPath d = do amw <- amInWindows
                    oldpath <- reverse `fmap` getPackageConfs
                    if d `elem` oldpath
                       then return ()
                       else setEnv "GHC_PACKAGE_PATH" $
                            if amw then drop 1 $ concatMap (';':) (d:oldpath)
                                   else drop 1 $ concatMap (':':) (d:oldpath)
                    unlessC (io $ doesFileExist d) $
                            io $ do createDirectoryIfMissing True (dirname d)
                                    writeFile d "[]"

fixIPI :: InstalledPackageInfo PackageName -> InstalledPackageInfo String
fixIPI ipi = InstalledPackageInfo { package = fixPI $ package ipi,
                                    exposedModules = exposedModules ipi,
                                    depends = map fixPI $ depends ipi,
                                    license = license ipi,
                                    copyright = copyright ipi,
                                    maintainer = maintainer ipi,
                                    author = author ipi,
                                    stability = stability ipi,
                                    homepage = homepage ipi,
                                    pkgUrl = pkgUrl ipi,
                                    description = description ipi,
                                    category = category ipi,
                                    exposed = exposed ipi,
                                    hiddenModules = hiddenModules ipi,
                                    importDirs = importDirs ipi,
                                    libraryDirs = libraryDirs ipi,
                                    hsLibraries = hsLibraries ipi,
                                    extraLibraries = extraLibraries ipi,
                                    extraGHCiLibraries = extraGHCiLibraries ipi,
                                    includeDirs = includeDirs ipi,
                                    includes = includes ipi,
                                    hugsOptions = hugsOptions ipi,
                                    ccOptions = ccOptions ipi,
                                    ldOptions = ldOptions ipi,
                                    frameworkDirs = frameworkDirs ipi,
                                    frameworks = frameworks ipi,
                                    haddockInterfaces = haddockInterfaces ipi,
                                    haddockHTMLs = haddockHTMLs ipi
                                  }

data InstalledPackageInfo pn
   = InstalledPackageInfo {
        package           :: PackageIdentifier pn,
        license           :: License,
        copyright         :: String,
        maintainer        :: String,
        author            :: String,
        stability         :: String,
        homepage          :: String,
        pkgUrl            :: String,
        description       :: String,
        category          :: String,
        -- these parts are required by an installed package only:
        exposed           :: Bool,
        exposedModules    :: [String],
        hiddenModules     :: [String],
        importDirs        :: [FilePath],
        libraryDirs       :: [FilePath],
        hsLibraries       :: [String],
        extraLibraries    :: [String],
        extraGHCiLibraries:: [String],
        includeDirs       :: [FilePath],
        includes          :: [String],
        depends           :: [PackageIdentifier pn],
        hugsOptions       :: [String],
        ccOptions         :: [String],
        ldOptions         :: [String],
        frameworkDirs     :: [FilePath],
        frameworks        :: [String],
        haddockInterfaces :: [FilePath],
        haddockHTMLs      :: [FilePath]
    } deriving (Read, Show)

data PackageName = PackageName String deriving ( Read, Show, Eq, Ord )

pnToString :: PackageName -> String
pnToString (PackageName n) = n

fixPI :: PackageIdentifier PackageName -> PackageIdentifier String
fixPI (PackageIdentifier pn v) = PackageIdentifier (pnToString pn) v

data PackageIdentifier pn =
    PackageIdentifier { pkgName :: pn, pkgVersion :: Version }
     deriving (Read, Show, Eq, Ord)

data License = GPL | LGPL | BSD3 | BSD4 | PublicDomain | AllRightsReserved
             | OtherLicense | UnknownLicense String
  deriving (Read, Show, Eq)

data Version = 
  Version { versionBranch :: [Int], versionTags :: [String] }
  deriving (Read,Show,Eq,Ord)

showPackage :: PackageIdentifier String -> String
showPackage (PackageIdentifier n v) = n ++ '-': showVersion v

showVersion :: Version -> String
showVersion (Version branch tags) =
    dotsBetween (map show branch)++concatMap ('-':) tags
    where dotsBetween [x] = x
          dotsBetween (x:xs) = x++'.':dotsBetween xs
          dotsBetween [] = ""
