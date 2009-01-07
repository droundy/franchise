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

module Distribution.Franchise.GhcState
    ( ghcFlags, ldFlags, cFlags, addPackages, removePackages, packageName,
      rmGhcFlags,
      setOutputDirectory,
      pkgFlags, copyright, license, version,
      getGhcFlags, getCFlags, getLdFlags,
      define, undefine, defineAs, needDefinitions,
      isDefined, getDefinitions,
      getLibDir, getBinDir,
      getVersion, packages, getPackageVersion,
      getPkgFlags, getMaintainer )
        where

import Control.Monad ( mplus )
import System.Directory ( getAppUserDataDirectory )
import Data.List ( (\\) )
import Data.Maybe ( catMaybes )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Env ( getEnv )
import Distribution.Franchise.ListUtils ( stripPrefix )

addPackages :: [String] -> C ()
addPackages = addExtra "packages"

removePackages :: [String] -> C ()
removePackages x = do p <- getExtra "packages"
                      putExtra "packages" $ p \\ x

pkgFlags :: [String] -> C ()
pkgFlags = addExtraUnique "pkgFlags"

ghcFlags :: [String] -> C ()
ghcFlags = addExtraUnique "ghcFlags"

setOutputDirectory :: String -> C ()
setOutputDirectory odir =
    do fs <- getGhcFlags
       let fs' = rmoldodirs oldodirs fs
           oldodirs = filter (/=".") $ filter (not.null) $
                      catMaybes $ map (stripPrefix "-odir") fs
           rmoldodirs [] x = x
           rmoldodirs (o:os) x = rmoldodirs os (x \\ ["-odir"++o, "-hidir"++o,"-stubdir"++o, "-i"++o])
       if odir == "."
         then putExtra "ghcFlags" fs'
         else putExtra "ghcFlags" $ ["-odir"++odir,"-hidir"++odir,"-stubdir"++odir,"-i"++odir]++fs'

cFlags :: [String] -> C ()
cFlags = addExtraUnique "cflags"

rmGhcFlags :: [String] -> C ()
rmGhcFlags x = do f <- getExtra "ghcFlags"
                  putExtra "ghcFlags" $ f \\ x

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update k v [] = [(k,v)]
update k v ((k',v'):xs) | k'==k     = (k,v):xs
                        | otherwise = (k',v'):update k v xs

define :: String -> C ()
define x = defineAs x ""

undefine :: String -> C ()
undefine x = do ds <- getDefinitions
                putExtra "definitions" $ filter ((/=x).fst) ds

needDefinitions :: C ()
needDefinitions = getDefinitions >>= putExtra "definitions"

defineAs :: String -> String -> C ()
defineAs x y = do ds <- getDefinitions
                  putExtra "definitions" $ update x y ds

copyright, license, version :: String -> C ()
copyright = addExtraData "copyright"
license = addExtraData "license"
version v = do addExtraData "version" v
               writeF "config.d/version" v
                      `catchC` \_ -> return ()

getGhcFlags :: C [String]
getGhcFlags = getExtra "ghcFlags"

getCFlags :: C [String]
getCFlags = getExtra "cflags"

getLdFlags :: C [String]
getLdFlags = getExtra "ldflags"

packages :: C [String]
packages = getExtra "packages"

getPkgFlags :: C [String]
getPkgFlags = getExtra "pkgFlags"

getDefinitions :: C [(String,String)]
getDefinitions = getExtra "definitions"

isDefined :: String -> C Bool
isDefined x = (not . null . filter ((==x).fst)) `fmap` getDefinitions

getVersion :: C String
getVersion = maybe "0.0" id `fmap` getExtraData "version"

getMaintainer :: C String
getMaintainer = do ema <- getEnv "EMAIL"
                   mai <- getExtraData "maintainer"
                   return $ maybe "???" id (mai `mplus` ema)

packageName :: String -> C ()
packageName = addExtraData "packageName"

getPackageName :: C (Maybe String)
getPackageName = getExtraData "packageName"

getPackageVersion :: C (Maybe String)
getPackageVersion = do ver <- getVersion
                       pn <- getPackageName
                       return $ fmap (++("-"++ver)) pn

getPrefix :: C String
getPrefix =
    do prf <- getExtraData "prefix"
       amwindows <- amInWindows
       case prf of
         Just x -> return x
         Nothing -> do pkgflgs <- getPkgFlags
                       if "--user" `elem` pkgflgs
                         then io $ getAppUserDataDirectory "cabal"
                         else if amwindows
                              then maybe "C:\\Program Files\\Haskell" (++ "\\Haskell") `fmap` getEnv "ProgramFiles"
                              else return "/usr/local"

getLibDir :: C String
getLibDir = do prefix <- getPrefix
               maybe (prefix++"/lib") id `fmap` getExtraData "libdir"

getBinDir :: C String
getBinDir = do prefix <- getPrefix
               maybe (prefix++"/bin") id `fmap` getExtraData "bindir"

ldFlags :: [String] -> C ()
ldFlags = addExtraUnique "ldflags"
