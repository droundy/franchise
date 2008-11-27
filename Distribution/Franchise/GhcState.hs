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
    ( handleArgs, flag, unlessFlag,
      configureFlagWithDefault, FranchiseFlag,
      ghcFlags, ldFlags, cFlags, addPackages, removePackages, packageName,
      rmGhcFlags,
      pkgFlags, copyright, license, version,
      getGhcFlags, getCFlags, getLdFlags,
      define, undefine, defineAs,
      isDefined, getDefinitions,
      getLibDir, getBinDir,
      getVersion, packages, getPackageVersion,
      getPkgFlags, getMaintainer )
        where

import qualified System.Environment as E ( getEnv )
import Control.Monad ( mplus )
import System.Exit ( exitWith, ExitCode(..) )
import System.Directory ( getAppUserDataDirectory )
import System.Environment ( getProgName )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..),
                               usageInfo, getOpt )
import Data.List ( delete, (\\) )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Env ( getEnv )

type FranchiseFlag = OptDescr (C ())

configureFlagWithDefault :: String -> String -> String
                         -> C () -> (String -> C ()) -> C FranchiseFlag
configureFlagWithDefault n argname h defaultaction j =
 do addHook n defaultaction
    return $ Option [] [n] (ReqArg (addHook n . j') argname) h
    where j' v = do putV $ "handling configure flag --"++n++" "++v; j v

flag :: String -> String -> C () -> C FranchiseFlag
flag n h j = return $ Option [] [n] (NoArg j') h
    where j' = do putV $ "handling flag --"++n; j

unlessFlag :: String -> String -> C () -> C FranchiseFlag
unlessFlag n h j = do addHook n j'
                      flag n h (removeHook n)
    where j' = do putV $ "handling missing flag --"++n; j

withEnv :: String -> (String -> C ()) -> C ()
withEnv x j = do e <- io $ E.getEnv x
                 j e
              `catchC` \_ -> return ()

handleArgs :: [C FranchiseFlag] -> C [String]
handleArgs optsc =
    do args <- getExtra "commandLine"
       myname <- io $ getProgName
       withEnv "GHCFLAGS" (ghcFlags . words)
       withEnv "PACKAGES" (addPackages . words)
       withEnv "LDFLAGS" (ldFlags . words)
       withEnv "CFLAGS" (cFlags . words)
       withEnv "LIBDIR" (addExtraData "libdir")
       withEnv "BINDIR" (addExtraData "bindir")
       withEnv "PREFIX" (addExtraData "prefix")
       opts <- sequence optsc
       let header = unwords (myname:map inbrackets validCommands) ++" OPTIONS"
           validCommands = ["configure","build","clean","install"] -- should be in monad
           inbrackets x = "["++x++"]"
           defaults = [ Option ['h'] ["help"] (NoArg showUsage)
                                   "show usage info",
                        Option [] ["user"]
                          (NoArg $ pkgFlags ["--user"]) "install as user",
                        Option [] ["disable-optimization"]
                          (NoArg $ rmGhcFlags ["-O2","-O"]) "disable optimization",
                        Option [] ["verbose"] (OptArg setVerbose "VERBOSITY")
                          ("Control verbosity (default verbosity level is 1)"),
                        Option [] ["debug"] (NoArg $ setVerbose $ Just "2")
                          ("Enable debug output (verbosity level 2)"),
                        Option [] ["no-remove"] (NoArg $ putExtra "noRemove" [()])
                          ("Prevent deletion of temporary files"),
                        Option [] ["prefix"]
                          (ReqArg (addExtraData "prefix") "PATH")
                          "install under prefix",
                        Option [] ["bindir"]
                          (ReqArg (addExtraData "bindir") "PATH")
                          "install in bindir",
                        Option [] ["libdir"]
                          (ReqArg (addExtraData "libdir") "PATH")
                          "install in libdir",
                        Option [] ["libsubdir"]
                          (ReqArg (addExtraData "libsubdir") "PATH")
                          "install in libsubdir",
                        Option ['j'] ["jobs"]
                          (OptArg (\v -> setNumJobs $ maybe 1000 id (v >>= readM) ) "N")
                          "run N jobs in parallel; infinite jobs with no arg.",
                        Option [] ["package"]
                                 (ReqArg (\p -> addPackages [p]) "PACKAGE-NAME")
                          "use a particular ghc package",
                        Option [] ["enable-hpc"] (NoArg $ ghcFlags ["-fhpc"])
                          "enable program coverage",
                        Option ['V'] ["version"] (NoArg showVersion)
                                   "show version number"
                      ]
           readM s = case reads s of [(x,"")] -> Just x
                                     _ -> Nothing
           putAndExit x = do io $ putStrLn x
                             io $ exitWith ExitSuccess
           showVersion = putAndExit "version 0.0"
           showUsage = putAndExit (usageInfo header options)
           options = opts++defaults
       eviloptions <- sequence [ flag "ghc" "use ghc" $ return (),
                                 flag "global" "not --user" $ return (),
                                 flag "disable-optimize" "disable optimization" $
                                      rmGhcFlags ["-O2","-O"],
                                 return $ Option [] ["constraint"]
                                 (ReqArg (const (return ())) "ugh") "ignored" ]
       case getOpt Permute (options++eviloptions) args of
         (flags, commands, []) -> do sequence_ flags
                                     return $ delete "configure" commands
         (_, _, msgs)   -> fail $ concat msgs ++ usageInfo header options

addPackages :: [String] -> C ()
addPackages = addExtra "packages"

removePackages :: [String] -> C ()
removePackages x = do p <- getExtra "packages"
                      putExtra "packages" $ p \\ x

pkgFlags :: [String] -> C ()
pkgFlags = addExtra "pkgFlags"

ghcFlags :: [String] -> C ()
ghcFlags = addExtra "ghcFlags"

cFlags :: [String] -> C ()
cFlags = addExtra "cflags"

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
ldFlags = addExtra "ldflags"
