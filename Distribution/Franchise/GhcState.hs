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
    ( hcFlags, ghcFlags, jhcFlags,
      ldFlags, cFlags, addPackages, removePackages, packageName,
      rmGhcFlags,
      setOutputDirectory,
      pkgFlags, version,
      getGhcFlags, getJhcFlags, getCFlags, getLdFlags,
      define, undefine, defineAs, needDefinitions,
      isDefined, getDefinitions,
      getLibDir, getBinDir, getEtcDir,
      getDataDir, getDocDir, getHtmlDir, getManDir,
      getVersion, packages, getPackageVersion,
      getPkgFlags, getMaintainer )
        where

import Control.Monad ( mplus )
import System.Directory ( getAppUserDataDirectory )
import Data.List ( nub, (\\) )
import Data.Maybe ( catMaybes )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Env ( getEnv )
import Distribution.Franchise.ListUtils ( stripPrefix )
import Distribution.Franchise.Trie ( toListT, fromListT )

addPackages :: [String] -> C ()
addPackages = addExtra "packages"

removePackages :: [String] -> C ()
removePackages x = do p <- getExtra "packages"
                      putExtra "packages" $ p \\ x

-- | Add the specified flags to the list of flags passed to ghc-pkg.
pkgFlags :: [String] -> C ()
pkgFlags = addExtraUnique "pkgFlags"

-- | Add the specified flags to the list of flags passed to ghc.  Note
-- that it is preferred for flags such as -i or -I to use 'hcFlags'
-- instead.
ghcFlags :: [String] -> C ()
ghcFlags = addExtraUnique "ghcFlags"

-- | Add the specified flags to the list of flags specific to jhc.
jhcFlags :: [String] -> C ()
jhcFlags = addExtraUnique "jhcFlags"

-- | Add the specified flags to the list of flags passed to any
-- Haskell compiler.
hcFlags :: [String] -> C ()
hcFlags = addExtraUnique "hcFlags"

setOutputDirectory :: String -> C ()
setOutputDirectory odir =
    do mkdir odir
       fs <- getGhcFlags
       let fs' = rmoldodirs oldodirs fs
           oldodirs = filter (/=".") $ filter (not.null) $
                      catMaybes $ map (stripPrefix "-odir") fs
           rmoldodirs [] x = x
           rmoldodirs (o:os) x =
               rmoldodirs os (x\\["-odir"++o,"-hidir"++o,"-stubdir"++o,"-i"++o])
       if odir == "."
         then putExtra "ghcFlags" fs'
         else putExtra "ghcFlags" $ ["-odir"++odir,"-hidir"++odir,
                                     "-stubdir"++odir,"-i"++odir]++fs'

-- | Add the specified flags to the list of flags to be passed to the
-- C compiler for compiling C files.
cFlags :: [String] -> C ()
cFlags = addExtraUnique "cflags"

rmGhcFlags :: [String] -> C ()
rmGhcFlags x = do f <- getExtra "ghcFlags"
                  putExtra "ghcFlags" $ f \\ x

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update k v [] = [(k,v)]
update k v ((k',v'):xs) | k'==k     = (k,v):xs
                        | otherwise = (k',v'):update k v xs

-- | The 'define' function allows you to create C preprocessor
-- definitions.  e.g.
--
-- @
--         'whenC' 'amInWindows' $ 'define' \"WINDOWS\"
-- @
--
-- Then in your actual source code you would have blocks such as
--
-- @
-- #ifdef WINDOW
--     putStrLn \"You've got a lame operating system!\"
-- #else
--     putStrLn \"You've got a cool operating system!\"
-- #endif
-- @

define :: String -> C ()
define x = defineAs x ""

undefine :: String -> C ()
undefine x = do ds <- getDefinitions
                putExtra "definitions" $ fromListT $ filter ((/=x).fst) ds

needDefinitions :: C ()
needDefinitions = getDefinitions >>= (putExtra "definitions" . fromListT)


-- | The 'defineAs' function is like 'define', but it allows you to
-- specify the actual value of the defined C preprocessor macro:
-- 
-- @
--         'defineAs' \"COOLNESS\" \"cool\"
--         'whenC' 'amInWindows' $ 'defineAs' \"COOLNESS\" \"lame\"
-- @
--
-- with your source code containing:
--
-- @
--    'putStrLn' (\"Your operating system is \"++COOLNESS)
-- @

defineAs :: String -> String -> C ()
defineAs x y = do ds <- getDefinitions
                  putExtra "definitions" $ fromListT $ update x y ds

-- | Define the version, which affects any package or cabal file that
-- is generated.
version :: String -> C ()
version v = do "version" <<= v
               writeF "config.d/version" v
                      `catchC` \_ -> return ()

getGhcFlags :: C [String]
getGhcFlags = do x <- getExtra "ghcFlags"
                 y <- getExtra "hcFlags"
                 return $ nub (x++y)

getJhcFlags :: C [String]
getJhcFlags = do x <- getExtra "jhcFlags"
                 y <- getExtra "hcFlags"
                 return $ nub (x++y)

getCFlags :: C [String]
getCFlags = getExtra "cflags"

getLdFlags :: C [String]
getLdFlags = getExtra "ldflags"

packages :: C [String]
packages = getExtra "packages"

getPkgFlags :: C [String]
getPkgFlags = getExtra "pkgFlags"

getDefinitions :: C [(String,String)]
getDefinitions = toListT `fmap` getExtra "definitions"

-- | You can find out if you've previously defined something with 'isDefined':
--
-- @
--     cfiles <- 'whenC' ('isDefined' \"WINDOWS\") $ return [\"windows.c\"]
-- @
isDefined :: String -> C Bool
isDefined x = (not . null . filter ((==x).fst)) `fmap` getDefinitions

getVersion :: C String
getVersion = maybe "137.0" id `fmap` getExtraData "version"

getMaintainer :: C String
getMaintainer = do ema <- getEnv "EMAIL"
                   mai <- getExtraData "maintainer"
                   return $ maybe "???" id (mai `mplus` ema)

packageName :: String -> C ()
packageName nm = "packageName" <<= nm

getPackageName :: C (Maybe String)
getPackageName = getExtraData "packageName"

getPackageVersion :: C (Maybe String)
getPackageVersion = do ver <- cleanVersionForPackage `fmap` getVersion
                       pn <- getPackageName
                       return $ fmap (++("-"++ver)) pn
    where cleanVersionForPackage = takeWhile (`elem` ('.':['0'..'9']))

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
                              then maybe "C:\\Program Files\\Haskell"
                                    (++"\\Haskell") `fmap` getEnv "ProgramFiles"
                              else return "/usr/local"

getLibDir :: C String
getLibDir = do prefix <- getPrefix
               maybe (prefix++"/lib") id `fmap` getExtraData "libdir"

getBinDir :: C String
getBinDir = do prefix <- getPrefix
               maybe (prefix++"/bin") id `fmap` getExtraData "bindir"

getDataDir :: C String
getDataDir = do prefix <- getPrefix
                maybe (prefix++"/share") id `fmap` getExtraData "datadir"

getDocDir :: C String
getDocDir = do dd <- getDataDir
               pn <- maybe "" id `fmap` getPackageName
               maybe (dd++"/doc/"++pn) id `fmap` getExtraData "docdir"

getHtmlDir :: C String
getHtmlDir = do dd <- getDocDir
                maybe dd id `fmap` getExtraData "htmldir"

getManDir :: Int -> C String
getManDir section = do datadir <- getDataDir
                       maybe (datadir++"/man/man"++show section)
                             (++("/man"++show section))
                             `fmap` getExtraData "mandir"

getEtcDir :: C String
getEtcDir = do metc <- getExtraData "bindir"
               case metc of
                 Just etc -> return etc
                 Nothing -> do prefix <- getPrefix
                               case prefix of
                                 "/usr" -> return "/etc"
                                 _ -> return (prefix++"/etc")

-- | Add the specified flags to the list of flags to be passed to the
-- linker.
ldFlags :: [String] -> C ()
ldFlags = addExtraUnique "ldflags"
