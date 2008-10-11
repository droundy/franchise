{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Distribution.Franchise.ConfigureState
    ( runWithArgs, whenNotConfigured, setConfigured,
      ghcFlags, ldFlags, cFlags, addPackages, packageName,
      rmGhcFlags,
      pkgFlags, copyright, license, version,
      getGhcFlags, getCFlags, getLdFlags,
      getLibDir, getBinDir,
      replace, replacements,
      getVersion, packages, getPackageVersion,
      getExtraData, addExtraData, haveExtraData,
      getPkgFlags, getCopyright, getLicense,
      getMaintainer,
      flag,
      getNumJobs, addCreatedFile, getCreatedFiles,
      CanModifyState(..),
      C, ConfigureState(..), runC, io, catchC, forkC,
      unlessC,
      putS, putV, amVerbose,
      put, get, gets, modify )
        where

import qualified System.Environment as E ( getEnv )
import Prelude hiding ( catch )
import Control.Exception ( Exception(AssertionFailed), throw, catch )
import Control.Monad ( when, unless, mplus )
import Control.Concurrent ( forkIO, Chan, readChan, writeChan, newChan )

import System.Exit ( exitWith, ExitCode(..) )
import System.Directory ( getAppUserDataDirectory )
import System ( getArgs, getProgName )
import System.Info ( os )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..),
                               usageInfo, getOpt )
import List ( (\\) )
import Maybe ( isJust )

flag :: String -> String -> C () -> OptDescr (C ())
flag n h j = Option [] [n] (NoArg j) h

runWithArgs :: [OptDescr (C ())] -> [String] -> (String -> C ()) -> C ()
runWithArgs opts validCommands runCommand =
    do args <- gets commandLine
       myname <- io $ getProgName
       withEnv "GHCFLAGS" (ghcFlags . words)
       withEnv "LDFLAGS" (ldFlags . words)
       withEnv "CFLAGS" (cFlags . words)
       let header = unwords (myname:map inbrackets validCommands) ++" OPTIONS"
           inbrackets x = "["++x++"]"
           defaults = [ Option ['h'] ["help"] (NoArg showUsage)
                                   "show usage info",
                        flag "user" "install as user" (pkgFlags ["--user"]),
                        Option [] ["verbose"]
                          (OptArg (const $ modify (\c -> c { amVerboseC = True })) "VERBOSITY")
                          "be verbose",
                        Option [] ["prefix"]
                          (ReqArg (\v -> modify (\c -> c { prefixC = Just v })) "PATH")
                          "install under prefix",
                        Option [] ["bindir"]
                          (ReqArg (\v -> modify (\c -> c { bindirC = Just v })) "PATH")
                          "install in bindir",
                        Option [] ["libdir"]
                          (ReqArg (\v -> modify (\c -> c { libdirC = Just v })) "PATH")
                          "install in libdir",
                        Option [] ["libsubdir"]
                          (ReqArg (\v -> modify (\c -> c { libsubdirC = Just v })) "PATH")
                          "install in libsubdir",
                        Option ['j'] ["jobs"]
                          (OptArg (\v -> setNumJobs $ maybe 1000 id (v >>= readM) ) "N")
                          "Allow N jobs at once; infinite jobs with no arg.",
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
           eviloptions = [ flag "ghc" "use ghc" $ return (),
                           flag "global" "not --user" $ return (),
                           Option [] ["constraint"] (ReqArg (const (return ())) "ugh")
                                      "ignored"
                         ]
       case getOpt Permute (options++eviloptions) args of
         (flags, commands, []) ->
             case commands \\ validCommands of
               [] -> do v <- getEnv "VERBOSE"
                        when (v /= Just "" && v /= Just "0" && v /= Nothing) $
                             modify $ \c -> c { amVerboseC = True }
                        sequence_ flags
                        mapM_ runCommand commands
               invalid -> failNicely $ "unrecognized arguments: " ++ unwords invalid
         (_, _, msgs)   -> failNicely $ concat msgs ++ usageInfo header options

failNicely :: String -> C ()
failNicely x = do io $ putStrLn $ "Error:  "++x
                  io $ exitWith $ ExitFailure 1

addPackages :: [String] -> C ()
addPackages x = modify $ \c -> c { packagesC = packagesC c ++ x }

pkgFlags :: [String] -> C ()
pkgFlags x = modify $ \c -> c { pkgFlagsC = pkgFlagsC c ++ x }

ghcFlags :: [String] -> C ()
ghcFlags x = modify $ \c -> c { ghcFlagsC = ghcFlagsC c ++ x }

cFlags :: [String] -> C ()
cFlags x = modify $ \c -> c { cFlagsC = cFlagsC c ++ x }

rmGhcFlags :: [String] -> C ()
rmGhcFlags x = modify $ \c -> c { ghcFlagsC = ghcFlagsC c \\ x }

copyright, license, version :: String -> C ()
copyright x = modify $ \c -> c { copyrightC = Just x }
license x = modify $ \c -> c { licenseC = Just x }
version x = modify $ \c -> c { versionC = x }

getGhcFlags :: C [String]
getGhcFlags = gets ghcFlagsC

getCFlags :: C [String]
getCFlags = gets cFlagsC

getLdFlags :: C [String]
getLdFlags = gets ldFlagsC

packages :: C [String]
packages = gets packagesC

getPkgFlags :: C [String]
getPkgFlags = gets pkgFlagsC

getVersion :: C String
getVersion = gets versionC

getLicense :: C String
getLicense = maybe "OtherLicense" id `fmap` gets licenseC

getCopyright :: C String
getCopyright = maybe "???" id `fmap` gets copyrightC

getMaintainer :: C String
getMaintainer = do ema <- getEnv "EMAIL"
                   mai <- gets maintainerC
                   return $ maybe "???" id (mai `mplus` ema)

getExtraData :: String -> C (Maybe String)
getExtraData d = lookup d `fmap` gets extraDataC

unlessC :: C Bool -> C () -> C ()
unlessC predicate job = do doit <- predicate
                           unless doit job

haveExtraData :: String -> C Bool
haveExtraData d = isJust `fmap` getExtraData d

addExtraData :: String -> String -> C ()
addExtraData d v = modify $ \c -> c { extraDataC = (d,v):extraDataC c }

packageName :: String -> C ()
packageName x = modify $ \c -> c { packageNameC = Just x }

getPackageName :: C (Maybe String)
getPackageName = gets packageNameC

getPackageVersion :: C (Maybe String)
getPackageVersion = do ver <- getVersion
                       pn <- getPackageName
                       return $ fmap (++("-"++ver)) pn

getPrefix :: C String
getPrefix =
    do prf <- gets prefixC
       case prf of
         Just x -> return x
         Nothing -> do pkgflgs <- getPkgFlags
                       if "--user" `elem` pkgflgs
                         then io $ getAppUserDataDirectory "cabal"
                         else if os == "mingw32"
                              then return "C:\\Program Files\\Haskell"
                              else return "/usr/local"

getLibDir :: C String
getLibDir = do prefix <- getPrefix
               maybe (prefix++"/lib") id `fmap` gets libdirC

getBinDir :: C String
getBinDir = do prefix <- getPrefix
               maybe (prefix++"/bin") id `fmap` gets bindirC

ldFlags :: [String] -> C ()
ldFlags x = modify $ \c -> c { ldFlagsC = ldFlagsC c ++ x }

data ConfigureState = CS { commandLine :: [String],
                           createdFiles :: [String],
                           ghcFlagsC :: [String],
                           pkgFlagsC :: [String],
                           cFlagsC :: [String],
                           ldFlagsC :: [String],
                           packagesC :: [String],
                           replacementsC :: [(String,String)],
                           amConfigured :: Bool,
                           amVerboseC :: Bool,
                           prefixC :: Maybe String,
                           bindirC :: Maybe String,
                           libdirC :: Maybe String,
                           libsubdirC :: Maybe String,
                           versionC :: String,
                           packageNameC :: Maybe String,
                           maintainerC :: Maybe String,
                           licenseC :: Maybe String,
                           extraDataC :: [(String,String)],
                           copyrightC :: Maybe String }
                      deriving ( Read, Show )

data TotalState = TS { numJobs :: Int,
                       outputChan :: Chan String,
                       syncChan :: Chan (),
                       configureState :: ConfigureState }

newtype C a = C (TotalState -> IO (a,TotalState))

unC :: C a -> TotalState -> IO (a,TotalState)
unC (C f) = f

instance Functor C where
    f `fmap` x = x >>= (return . f)

instance Monad C where
    (C f) >>= g = C $ \cs -> do (a,cs') <- f cs
                                unC (g a) cs'
    return x = C (\cs -> return (x,cs))
    fail e = C (\_ -> throw $ AssertionFailed e)

get :: C ConfigureState
get = C $ \ts -> return (configureState ts,ts)

put :: ConfigureState -> C ()
put cs = C $ \ts -> return ((),ts { configureState=cs })

gets :: (ConfigureState -> a) -> C a
gets f = f `fmap` get

modify :: (ConfigureState -> ConfigureState) -> C ()
modify f = C $ \ts -> return ((),ts { configureState = f $ configureState ts })

setNumJobs :: Int -> C ()
setNumJobs n = C $ \ts -> return ((), ts { numJobs = n })

getNumJobs :: C Int
getNumJobs = C $ \ts -> return (numJobs ts, ts)

addCreatedFile :: String -> C ()
addCreatedFile f = modify (\cs -> cs { createdFiles = f:createdFiles cs })

getCreatedFiles :: C [String]
getCreatedFiles = gets createdFiles

runC :: C a -> IO a
runC (C a) =
    do x <- getArgs
       ch <- newChan
       ch2 <- newChan
       let writethread = do s <- readChan ch
                            putStrLn s
                            writeChan ch2 ()
                            writethread
       forkIO writethread
       fst `fmap` a (TS { outputChan = ch,
                          syncChan = ch2,
                          numJobs = 1,
                          configureState = defaultConfiguration { commandLine = x } })

defaultConfiguration :: ConfigureState
defaultConfiguration = CS { commandLine = [],
                            createdFiles = [],
                            ghcFlagsC = [],
                            pkgFlagsC = [],
                            cFlagsC = [],
                            ldFlagsC = [],
                            packagesC = [],
                            replacementsC = [],
                            amConfigured = False,
                            amVerboseC = False,
                            prefixC = Nothing,
                            bindirC = Nothing,
                            libdirC = Nothing,
                            libsubdirC = Nothing,
                            versionC = "0.0",
                            packageNameC = Nothing,
                            maintainerC = Nothing,
                            licenseC = Nothing,
                            extraDataC = [],
                            copyrightC = Nothing }

whenNotConfigured :: C () -> C ()
whenNotConfigured j = do amc <- gets amConfigured
                         when (not amc) $ j


setConfigured :: C ()
setConfigured = modify $ \c -> c { amConfigured = True }

io :: IO a -> C a
io x = C $ \cs -> do a <- x
                     return (a,cs)

catchC :: C a -> (String -> C a) -> C a
catchC (C a) b = C $ \ts -> a ts `catch` \err -> unC (b $ show err) ts

forkC :: CanModifyState -> C () -> C ()
forkC CannotModifyState (C j) = C (\ts -> do forkIO (j ts >> return())
                                             return ((),ts))
forkC _ j = j

getEnv :: String -> C (Maybe String)
getEnv x = fmap Just (io (E.getEnv x)) `catchC` \_ -> return Nothing

withEnv :: String -> (String -> C ()) -> C ()
withEnv x j = do e <- io $ E.getEnv x
                 j e
              `catchC` \_ -> return ()

data CanModifyState = CanModifyState | CannotModifyState deriving (Eq)

replace :: Show a => String -> a -> C ()
replace a b = do r <- gets replacementsC
                 if a `elem` map fst r
                    then return ()
                    else modify $ \c -> c { replacementsC = (a,show b):r }

replacements :: C [(String,String)]
replacements = gets replacementsC

putS :: String -> C ()
putS str = C $ \ts -> do writeChan (outputChan ts) str
                         readChan (syncChan ts)
                         return ((),ts)

putV :: String -> C ()
putV str = do amv <- amVerbose
              if amv then putS str
                     else return ()

amVerbose :: C Bool
amVerbose = gets amVerboseC
