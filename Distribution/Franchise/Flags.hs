{-# LANGUAGE CPP #-}
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
module Distribution.Franchise.Flags
    ( handleArgs, flag, unlessFlag,
      configureFlagWithDefault, FranchiseFlag )
        where

import qualified System.Environment as E ( getEnv )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getProgName )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..),
                               usageInfo, getOpt )
import Data.List ( delete )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.GhcState ( ghcFlags, ldFlags, cFlags, pkgFlags,
                                         rmGhcFlags, addPackages )

franchiseVersion :: String
#ifdef VERSION
franchiseVersion = VERSION
#else
franchiseVersion = "unknown"
#endif

-- |At heart, a FranchiseFlag is just a getopt OptDescr, but it is
-- kept abstract so I can change the API.
newtype FranchiseFlag = FF (OptDescr (C ()))

unFF :: FranchiseFlag -> OptDescr (C ())
unFF (FF x) = x

configureFlagWithDefault :: String -> String -> String
                         -> C () -> (String -> C ()) -> C FranchiseFlag
configureFlagWithDefault n argname h defaultaction j =
 do addHook n defaultaction
    return $ FF $ Option [] [n] (ReqArg (addHook n . j') argname) h
    where j' v = do putV $ "handling configure flag --"++n++" "++v; j v

flag :: String -> String -> C () -> C FranchiseFlag
flag n h j = return $ FF $ Option [] [n] (NoArg j') h
    where j' = do putV $ "handling flag --"++n; j

unlessFlag :: String -> String -> C () -> C FranchiseFlag
unlessFlag n h j = do addHook n j'
                      flag n h (addExtra flagn "here")
    where j' = unlessC (haveExtraData flagn) (putD ("handling missing flag --"++n) >> j)
          flagn = "flag-"++n

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
       opts <- map unFF `fmap` sequence optsc
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
                        Option [] ["debug"] (NoArg $ setVerbose $ Just "3")
                          ("Enable debug output (verbosity level 3)"),
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
           showVersion = putAndExit franchiseVersion
           showUsage = putAndExit (usageInfo header options)
           options = opts++defaults
       eviloptions <- sequence [ flag "ghc" "use ghc" $ return (),
                                 flag "global" "not --user" $ return (),
                                 flag "disable-optimize" "disable optimization" $
                                      rmGhcFlags ["-O2","-O"],
                                 return $ FF $ Option [] ["constraint"]
                                 (ReqArg (const (return ())) "ugh") "ignored" ]
       case getOpt Permute (options++map unFF eviloptions) args of
         (flags, commands, []) -> do sequence_ flags
                                     return $ delete "configure" commands
         (_, _, msgs)   -> fail $ concat msgs ++ usageInfo header options
