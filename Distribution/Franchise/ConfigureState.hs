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
      ghcFlags, ldFlags, addPackages, packageName,
      pkgFlags, copyright, license, version,
      getGhcFlags, getCFlags, getLdFlags,
      getLibDir, getBinDir,
      replace, replacements,
      getVersion, packages, getPackageVersion,
      getPkgFlags, getCopyright, getLicense,
      getMaintainer,
      flag,
      CanModifyState(..),
      C, ConfigureState(..), runC, io, catchC, forkC )
        where

import System.Exit ( ExitCode(..) )
import qualified System.Environment as E ( getEnv )
import Control.Monad ( when, msum, mplus )
import Control.Concurrent ( forkIO, forkOS )
import Control.Monad.State ( StateT, MonadIO, MonadState,
                             runStateT, liftIO, get, gets, put, modify )

import System.Exit ( exitWith, ExitCode(..) )
import System ( getArgs, getProgName )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..),
                               usageInfo, getOpt )
import Data.List ( (\\) )

flag :: String -> String -> C () -> OptDescr (C ())
flag n h j = Option [] [n] (NoArg j) h

runWithArgs :: [OptDescr (C ())] -> [String] -> (String -> C ()) -> C ()
runWithArgs opts validCommands runCommand =
    do args <- io $ getArgs
       myname <- io $ getProgName
       let header = unwords (myname:map inbrackets validCommands) ++" OPTIONS"
           inbrackets x = "["++x++"]"
           defaults = [ Option ['h'] ["help"] (NoArg showUsage)
                                   "show usage info",
                        flag "user" "install as user" (pkgFlags ["--user"]),
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
                        Option ['V'] ["version"] (NoArg showVersion)
                                   "show version number"
                      ]
           putAndExit x = do io $ putStrLn x
                             io $ exitWith ExitSuccess
           showVersion = putAndExit "version 0.0"
           showUsage = putAndExit (usageInfo header options)
           options = opts++defaults
       case getOpt Permute options args of
         (flags, commands, []) ->
             case commands \\ validCommands of
               [] -> do sequence_ flags
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

packageName :: String -> C ()
packageName x = modify $ \c -> c { packageNameC = Just x }

getPackageName :: C (Maybe String)
getPackageName = gets packageNameC

getPackageVersion :: C (Maybe String)
getPackageVersion = do ver <- getVersion
                       pn <- getPackageName
                       return $ fmap (++("-"++ver)) pn

getPrefix :: C String
getPrefix = maybe "/usr/local/" id `fmap` gets prefixC

getLibDir :: C String
getLibDir = do prefix <- getPrefix
               maybe (prefix++"/lib") id `fmap` gets libdirC

getBinDir :: C String
getBinDir = do prefix <- getPrefix
               maybe (prefix++"/bin") id `fmap` gets bindirC

ldFlags :: [String] -> C ()
ldFlags x = modify $ \c -> c { ldFlagsC = ldFlagsC c ++ x }

data ConfigureState = CS { ghcFlagsC :: [String],
                           pkgFlagsC :: [String],
                           cFlagsC :: [String],
                           ldFlagsC :: [String],
                           packagesC :: [String],
                           replacementsC :: [(String,String)],
                           amConfigured :: Bool,
                           prefixC :: Maybe String,
                           bindirC :: Maybe String,
                           libdirC :: Maybe String,
                           libsubdirC :: Maybe String,
                           versionC :: String,
                           packageNameC :: Maybe String,
                           maintainerC :: Maybe String,
                           licenseC :: Maybe String,
                           copyrightC :: Maybe String }
                      deriving ( Read, Show )

newtype C a = C ((StateT ConfigureState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState ConfigureState)

runC :: C a -> IO a
runC (C a) = fst `fmap` runStateT a defaultConfiguration

defaultConfiguration :: ConfigureState
defaultConfiguration = CS { ghcFlagsC = [],
                            pkgFlagsC = [],
                            cFlagsC = [],
                            ldFlagsC = [],
                            packagesC = [],
                            replacementsC = [],
                            amConfigured = False,
                            prefixC = Nothing,
                            bindirC = Nothing,
                            libdirC = Nothing,
                            libsubdirC = Nothing,
                            versionC = "0.0",
                            packageNameC = Nothing,
                            maintainerC = Nothing,
                            licenseC = Nothing,
                            copyrightC = Nothing }

whenNotConfigured :: C () -> C ()
whenNotConfigured j = do amc <- gets amConfigured
                         when (not amc) $ j


setConfigured :: C ()
setConfigured = modify $ \c -> c { amConfigured = True }

io :: MonadIO m => IO a -> m a
io = liftIO

catchC :: C a -> (String -> C a) -> C a
catchC (C a) b =
    do c <- get
       (out,c') <- io (runStateT a c `catch` \err -> runStateT (unC $ b $ show err) c)
       put c'
       return out
    where unC (C x) = x

forkC :: CanModifyState -> C () -> C ()
forkC CannotModifyState (C j) = do c <- get
                                   io $ forkOS $ do runStateT j c; return ()
                                   return ()
forkC _ j = j

getEnv :: String -> C (Maybe String)
getEnv x = fmap Just (io (E.getEnv x)) `catchC` \_ -> return Nothing

data CanModifyState = CanModifyState | CannotModifyState deriving (Eq)

replace :: Show a => String -> a -> C ()
replace a b = do r <- gets replacementsC
                 modify $ \c -> c { replacementsC = (a,show b):r }

replacements :: C [(String,String)]
replacements = gets replacementsC
