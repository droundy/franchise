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
    ( runWithArgs,
      amInWindows,
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
      flag, unlessFlag, configureFlag, configureUnlessFlag,
      runConfigureHooks, runPostConfigureHooks,
      getNumJobs, addCreatedFile, getCreatedFiles,
      CanModifyState(..),
      C, ConfigureState(..), runC, io, catchC, forkC,
      unlessC, whenC,
      putS, putV, putD, putSV,
      put, get, gets, modify )
        where

import qualified System.Environment as E ( getEnv )
import Control.Monad ( mplus )
import Data.Monoid ( Monoid, mempty )
import Control.Concurrent ( forkIO, Chan, killThread, threadDelay,
                            readChan, writeChan, newChan )

import System.Exit ( exitWith, ExitCode(..) )
import System.Directory ( getAppUserDataDirectory, getCurrentDirectory )
import System.Environment ( getArgs, getProgName )
import System.IO ( BufferMode(..), IOMode(..), openFile, hSetBuffering, hPutStrLn, stdout )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..),
                               usageInfo, getOpt )
import Data.List ( (\\) )
import Data.Maybe ( isJust )

flag :: String -> String -> C () -> C (OptDescr (C ()))
flag n h j = return $ Option [] [n] (NoArg $ addHook Postconfigure n j') h
    where j' = do putV $ "handling flag --"++n; j

unlessFlag :: String -> String -> C () -> C (OptDescr (C ()))
unlessFlag n h j = do addHook Postconfigure n j'
                      flag n h (removeHook Postconfigure n)
    where j' = do putV $ "handling missing flag --"++n; j

configureFlag :: String -> String -> C () -> C (OptDescr (C ()))
configureFlag n h j = return $ Option [] [n] (NoArg $ addHook Preconfigure n j') h
    where j' = do putV $ "handling configure flag --"++n; j

configureUnlessFlag :: String -> String -> C () -> C (OptDescr (C ()))
configureUnlessFlag n h j = do addHook Preconfigure n j'
                               flag n h (removeHook Preconfigure n)
    where j' = do putV $ "handling missing configure flag --"++n; j

runWithArgs :: [C (OptDescr (C ()))] -> [String] -> (String -> C ()) -> C ()
runWithArgs optsc validCommands runCommand =
    do args <- gets commandLine
       myname <- io $ getProgName
       withEnv "GHCFLAGS" (ghcFlags . words)
       withEnv "PACKAGES" (addPackages . words)
       withEnv "LDFLAGS" (ldFlags . words)
       withEnv "CFLAGS" (cFlags . words)
       opts <- sequence optsc
       let header = unwords (myname:map inbrackets validCommands) ++" OPTIONS"
           inbrackets x = "["++x++"]"
           defaults = [ Option ['h'] ["help"] (NoArg showUsage)
                                   "show usage info",
                        Option [] ["user"]
                          (NoArg $ do let m = pkgFlags ["--user"]
                                      m; addHook Postconfigure "user" m) "install as user",
                        Option [] ["verbose"]
                          (OptArg (\v -> C $ \ts -> return $
                                         Right ((), ts { verbosity = readVerbosity Verbose v }))
                           "VERBOSITY")
                          ("Control verbosity (default verbosity level is 1)"),
                        Option [] ["prefix"]
                          (ReqArg (\v -> addHook Postconfigure "prefix" $
                                         modify (\c -> c { prefixC = Just v })) "PATH")
                          "install under prefix",
                        Option [] ["bindir"]
                          (ReqArg (\v -> do let m = modify (\c -> c { bindirC = Just v })
                                            m; addHook Postconfigure "bindir" m) "PATH")
                          "install in bindir",
                        Option [] ["libdir"]
                          (ReqArg (\v -> do let m = modify (\c -> c { libdirC = Just v })
                                            m; addHook Postconfigure "libdir" m) "PATH")
                          "install in libdir",
                        Option [] ["libsubdir"]
                          (ReqArg (\v -> do let m = modify (\c -> c { libsubdirC = Just v })
                                            m; addHook Postconfigure "libsubdir" m) "PATH")
                          "install in libsubdir",
                        Option ['j'] ["jobs"]
                          (OptArg (\v -> setNumJobs $ maybe 1000 id (v >>= readM) ) "N")
                          "run N jobs in parallel; infinite jobs with no arg.",
                        Option [] ["package"]
                                 (ReqArg (\p -> addPackages [p]) "PACKAGE-NAME")
                          "use a particular ghc package",
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
                                 return $ Option [] ["constraint"]
                                 (ReqArg (const (return ())) "ugh") "ignored" ]
       case getOpt Permute (options++eviloptions) args of
         (flags, commands, []) -> do sequence_ flags
                                     mapM_ runCommand commands
         (_, _, msgs)   -> fail $ concat msgs ++ usageInfo header options

addPackages :: [String] -> C ()
addPackages x = modify $ \c -> c { packagesC = (packagesC c \\ x) ++ x }

pkgFlags :: [String] -> C ()
pkgFlags x = modify $ \c -> c { pkgFlagsC = (pkgFlagsC c \\ x) ++ x }

ghcFlags :: [String] -> C ()
ghcFlags x = modify $ \c -> c { ghcFlagsC = (ghcFlagsC c \\ x) ++ x }

cFlags :: [String] -> C ()
cFlags x = modify $ \c -> c { cFlagsC = (cFlagsC c \\ x) ++ x }

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

unlessC :: Monoid a => C Bool -> C a -> C a
unlessC predicate job = do doit <- predicate
                           if doit then return mempty else job

whenC :: Monoid a => C Bool -> C a -> C a
whenC predicate job = do doit <- predicate
                         if doit then job else return mempty

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

-- | amInWindows is a hokey function to identify windows systems.  It's
-- probably more portable than checking System.Info.os, which isn't saying
-- much.
amInWindows :: C Bool
amInWindows = (not . elem '/') `fmap` io getCurrentDirectory

getPrefix :: C String
getPrefix =
    do prf <- gets prefixC
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

data LogMessage = Stdout String | Logfile String
data HookTime = Preconfigure | Postconfigure
data Verbosity = Quiet | Normal | Verbose | Debug deriving ( Eq, Ord, Enum )

data TotalState = TS { numJobs :: Int,
                       verbosity :: Verbosity,
                       outputChan :: Chan LogMessage,
                       syncChan :: Chan (),
                       configureHooks :: [(String,C ())],
                       postConfigureHooks :: [(String,C ())],
                       configureState :: ConfigureState }

tsHook :: HookTime -> TotalState -> [(String,C ())]
tsHook Preconfigure = configureHooks
tsHook Postconfigure = postConfigureHooks

modifyHooks :: HookTime -> ([(String,C ())] -> [(String,C ())]) -> C ()
modifyHooks Preconfigure f =
    C $ \ts -> return $ Right ((), ts { configureHooks = f $ configureHooks ts })
modifyHooks Postconfigure f =
    C $ \ts -> return $ Right ((), ts { postConfigureHooks = f $ postConfigureHooks ts })

addHook :: HookTime -> String -> C () -> C ()
addHook ht n h = do removeHook ht n
                    modifyHooks ht ((n,h):)

removeHook :: HookTime -> String -> C ()
removeHook ht n = modifyHooks ht $ filter ((/=n) . fst)

runHooks :: HookTime -> C ()
runHooks ht = do hks <- C $ \ts -> return $ Right (tsHook ht ts, ts)
                 mapM_ snd $ reverse hks

runConfigureHooks :: C ()
runConfigureHooks = runHooks Preconfigure

runPostConfigureHooks :: C ()
runPostConfigureHooks = runHooks Postconfigure

newtype C a = C (TotalState -> IO (Either String (a,TotalState)))

unC :: C a -> TotalState -> IO (Either String (a,TotalState))
unC (C f) = f

instance Functor C where
    f `fmap` x = x >>= (return . f)

instance Monad C where
    (C f) >>= g = C $ \cs -> do macs' <- f cs
                                case macs' of
                                  Left e -> return (Left e)
                                  Right (a,cs') -> unC (g a) cs'
    return x = C (\cs -> return $ Right (x, cs))
    fail e = do putV $ "failure: "++ e
                C (\_ -> return $ Left e)

get :: C ConfigureState
get = C $ \ts -> return $ Right (configureState ts,ts)

put :: ConfigureState -> C ()
put cs = C $ \ts -> return $ Right ((),ts { configureState=cs })

gets :: (ConfigureState -> a) -> C a
gets f = f `fmap` get

modify :: (ConfigureState -> ConfigureState) -> C ()
modify f = C $ \ts -> return $ Right ((),ts { configureState = f $ configureState ts })

setNumJobs :: Int -> C ()
setNumJobs n = C $ \ts -> return $ Right ((), ts { numJobs = n })

getNumJobs :: C Int
getNumJobs = C $ \ts -> return $ Right (numJobs ts, ts)

addCreatedFile :: String -> C ()
addCreatedFile f = modify (\cs -> cs { createdFiles = f:createdFiles cs })

getCreatedFiles :: C [String]
getCreatedFiles = gets createdFiles

runC :: C a -> IO a
runC (C a) =
    do x <- getArgs
       ch <- newChan
       ch2 <- newChan
       h <- if "configure" `elem` x then openFile "config.log" WriteMode
                                    else openFile "build.log" WriteMode
       hSetBuffering h LineBuffering
       hSetBuffering stdout LineBuffering
       let writethread = do mess <- readChan ch
                            case mess of Stdout s -> putStrLn s
                                         Logfile s -> hPutStrLn h s
                            writeChan ch2 ()
                            writethread
       thid <- forkIO writethread
       v <- Just `fmap` E.getEnv "VERBOSE" `catch` \_ -> return Nothing
       xxx <- a (TS { outputChan = ch,
                      syncChan = ch2,
                      numJobs = 1,
                      configureHooks = [],
                      postConfigureHooks = [],
                      verbosity = readVerbosity Normal v,
                      configureState = defaultConfiguration { commandLine = x } })
       case xxx of
         Left e -> do -- give print thread a chance to do a bit more writing...
                      threadDelay 1000000
                      killThread thid
                      putStrLn $ "Error:  "++e
                      exitWith $ ExitFailure 1
         Right (out,_) -> return out

defaultConfiguration :: ConfigureState
defaultConfiguration = CS { commandLine = [],
                            createdFiles = [],
                            ghcFlagsC = [],
                            pkgFlagsC = [],
                            cFlagsC = [],
                            ldFlagsC = [],
                            packagesC = [],
                            replacementsC = [],
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

io :: IO a -> C a
io x = C $ \cs -> do a <- x
                     return $ Right (a,cs)

catchC :: C a -> (String -> C a) -> C a
catchC (C a) b = C $ \ts ->
                 do out <- (Right `fmap` a ts) `catch` \err -> return (Left $ show err)
                    case out of
                      Left e -> unC (b e) ts
                      Right (Left e) -> unC (b e) ts
                      Right x -> return x

forkC :: CanModifyState -> C () -> C ()
forkC CannotModifyState (C j) = C (\ts -> do forkIO (j ts >> return())
                                             return $ Right ((),ts))
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
putS str = whenC ((>= Normal) `fmap` getVerbosity) $
           do putM Stdout str
              putM Logfile str

putV :: String -> C ()
putV str = do amv <- (> Normal) `fmap` getVerbosity
              if amv then putS str
                     else putM Logfile str

putD :: String -> C ()
putD str = whenC ((> Verbose) `fmap` getVerbosity) $ putS str

putSV :: String -> String -> C ()
putSV str vstr = do v <- getVerbosity
                    case v of
                      Normal -> putM Stdout str
                      Verbose -> putM Stdout vstr
                      _ -> return ()
                    putM Logfile vstr

putM :: (String -> LogMessage) -> String -> C ()
putM m str = C $ \ts -> do writeChan (outputChan ts) (m $ chomp str)
                           readChan (syncChan ts)
                           return $ Right ((),ts)
    where chomp x = case reverse x of '\n':rx -> reverse rx
                                      _ -> x

getVerbosity :: C Verbosity
getVerbosity = C $ \ts -> return $ Right (verbosity ts, ts)

readVerbosity :: Verbosity -> Maybe String -> Verbosity
readVerbosity defaultV s = case (reads `fmap` s) :: Maybe [(Int,String)] of
                           Just [(0,"")] -> Quiet
                           Just [(1,"")] -> Normal
                           Just [(2,"")] -> Verbose
                           Just [(3,"")] -> Debug
                           _ -> defaultV
