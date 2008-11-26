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
      ghcFlags, ldFlags, cFlags, addPackages, removePackages, packageName,
      getModulePackageMap, setModulePackageMap,
      rmGhcFlags,
      pkgFlags, copyright, license, version,
      getGhcFlags, getCFlags, getLdFlags,
      define, undefine, defineAs,
      isDefined, getDefinitions,
      getLibDir, getBinDir,
      replace, replaceLiteral, replacements,
      getVersion, packages, getPackageVersion,
      getExtraData, getAllExtraData, addExtraData, haveExtraData,
      getPkgFlags, getLicense,
      getMaintainer,
      flag, unlessFlag, configureFlag, configureUnlessFlag,
      configureFlagWithDefault, FranchiseFlag,
      runConfigureHooks, runPostConfigureHooks,
      getNumJobs,
      CanModifyState(..),
      Target(..),
      getTargets, modifyTargets, setBuilt, clearBuilt, isBuilt,
      C, ConfigureState(..), runC, io, catchC, forkC,
      writeConfigureState, readConfigureState,
      cd, rm_rf, mkdir, writeF, splitPath,
      dirname, basename,
      withDirectory, withRootdir, rememberDirectory, getCurrentSubdir, processFilePath,
      quietly, silently,
      unlessC, whenC, getNoRemove,
      putS, putV, putD, putSV, putL,
      put, get, gets, modify )
        where

import qualified System.Environment as E ( getEnv )
import Control.Monad ( MonadPlus, mplus, mzero )
import Data.Monoid ( Monoid, mempty )
import Control.Concurrent ( forkIO, Chan, killThread, threadDelay,
                            readChan, writeChan, newChan )

import System.Exit ( exitWith, ExitCode(..) )
import System.Directory ( getAppUserDataDirectory, getCurrentDirectory,
                          doesDirectoryExist,
                          removeFile, removeDirectory, createDirectory,
                          getDirectoryContents )
import System.Environment ( getProgName )
import System.IO ( BufferMode(..), IOMode(..), openFile,
                   hSetBuffering, hPutStrLn, stdout )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), ArgDescr(..),
                               usageInfo, getOpt )
import Data.List ( isPrefixOf, (\\) )
import Data.Maybe ( isJust, catMaybes )

import Distribution.Franchise.StringSet
import Distribution.Franchise.Trie

type FranchiseFlag = OptDescr (C ())

configureFlagWithDefault :: String -> String -> String
                         -> C () -> (String -> C ()) -> C FranchiseFlag
configureFlagWithDefault n argname h defaultaction j =
 do addHook Preconfigure n defaultaction
    return $ Option [] [n] (ReqArg (addHook Preconfigure n . j') argname) h
    where j' v = do putV $ "handling configure flag --"++n; j v

flag :: String -> String -> C () -> C FranchiseFlag
flag n h j = return $ Option [] [n] (NoArg $ addHook Postconfigure n j') h
    where j' = do putV $ "handling flag --"++n; j

unlessFlag :: String -> String -> C () -> C FranchiseFlag
unlessFlag n h j = do addHook Postconfigure n j'
                      flag n h (removeHook Postconfigure n)
    where j' = do putV $ "handling missing flag --"++n; j

configureFlag :: String -> String -> C () -> C FranchiseFlag
configureFlag n h j = return $ Option [] [n] (NoArg $ addHook Preconfigure n j') h
    where j' = do putV $ "handling configure flag --"++n; j

configureUnlessFlag :: String -> String -> C () -> C FranchiseFlag
configureUnlessFlag n h j = do addHook Preconfigure n j'
                               flag n h (removeHook Preconfigure n)
    where j' = do putV $ "handling missing configure flag --"++n; j

runWithArgs :: [C FranchiseFlag] -> [String] -> (String -> C ()) -> C ()
runWithArgs optsc validCommands runCommand =
    do args <- gets commandLine
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
           inbrackets x = "["++x++"]"
           defaults = [ Option ['h'] ["help"] (NoArg showUsage)
                                   "show usage info",
                        Option [] ["user"]
                          (NoArg $ do let m = pkgFlags ["--user"]
                                      m; addHook Postconfigure "user" m) "install as user",
                        Option [] ["disable-optimization"]
                          (NoArg $ addHook Postconfigure "disable-optimization" $
                                 rmGhcFlags ["-O2","-O"]) "disable optimization",
                        Option [] ["verbose"]
                          (OptArg (\v -> C $ \ts -> return $
                                         Right ((), ts { verbosity = readVerbosity Verbose v }))
                           "VERBOSITY")
                          ("Control verbosity (default verbosity level is 1)"),
                        Option [] ["no-remove"]
                          (NoArg (C $ \ts -> return $
                                      Right ((), ts { noRemove = True })))
                          ("Prevent deletion of temporary files"),
                        Option [] ["prefix"]
                          (ReqArg (addHook Postconfigure "prefix"
                                   . addExtraData "prefix") "PATH")
                          "install under prefix",
                        Option [] ["bindir"]
                          (ReqArg (\v -> do let m = addExtraData "bindir" v
                                            m; addHook Postconfigure "bindir" m) "PATH")
                          "install in bindir",
                        Option [] ["libdir"]
                          (ReqArg (\v -> do let m = addExtraData "libdir" v
                                            m; addHook Postconfigure "libdir" m) "PATH")
                          "install in libdir",
                        Option [] ["libsubdir"]
                          (ReqArg (\v -> do let m = addExtraData "libsubdir" v
                                            m; addHook Postconfigure "libsubdir" m) "PATH")
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
                                     mapM_ runCommand commands
         (_, _, msgs)   -> fail $ concat msgs ++ usageInfo header options

addPackages :: [String] -> C ()
addPackages x = modify $ \c -> c { packagesC = (packagesC c \\ x) ++ x }

removePackages :: [String] -> C ()
removePackages x = modify $ \c -> c { packagesC = packagesC c \\ x }

pkgFlags :: [String] -> C ()
pkgFlags x = modify $ \c -> c { pkgFlagsC = (pkgFlagsC c \\ x) ++ x }

ghcFlags :: [String] -> C ()
ghcFlags x = modify $ \c -> c { ghcFlagsC = (ghcFlagsC c \\ x) ++ x }

cFlags :: [String] -> C ()
cFlags x = modify $ \c -> c { cFlagsC = (cFlagsC c \\ x) ++ x }

rmGhcFlags :: [String] -> C ()
rmGhcFlags x = modify $ \c -> c { ghcFlagsC = ghcFlagsC c \\ x }

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update k v [] = [(k,v)]
update k v ((k',v'):xs) | k'==k     = (k,v):xs
                        | otherwise = (k',v'):update k v xs

define :: String -> C ()
define x = defineAs x ""

undefine :: String -> C ()
undefine x = modify $ \c -> c { definitionsC = filter ((/=x).fst) $ definitionsC c }

defineAs :: String -> String -> C ()
defineAs x y = modify $ \c -> c { definitionsC = update x y $ definitionsC c }

copyright, license, version :: String -> C ()
copyright = addExtraData "copyright"
license = addExtraData "license"
version v = do addExtraData "version" v
               writeF "config.d/X-version" v
                      `catchC` \_ -> return ()

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

getDefinitions :: C [(String,String)]
getDefinitions = gets definitionsC

isDefined :: String -> C Bool
isDefined x = (not . null . filter ((==x).fst)) `fmap` getDefinitions

getVersion :: C String
getVersion = maybe "0.0" id `fmap` getExtraData "version"

getLicense :: C String
getLicense = maybe "OtherLicense" id `fmap` getExtraData "license"

getMaintainer :: C String
getMaintainer = do ema <- getEnv "EMAIL"
                   mai <- getExtraData "maintainer"
                   return $ maybe "???" id (mai `mplus` ema)

getExtraData :: String -> C (Maybe String)
getExtraData d = lookup d `fmap` getAllExtraData

getAllExtraData :: C [(String, String)]
getAllExtraData = gets extraDataC

unlessC :: Monoid a => C Bool -> C a -> C a
unlessC predicate job = do doit <- predicate
                           if doit then return mempty else job

whenC :: Monoid a => C Bool -> C a -> C a
whenC predicate job = do doit <- predicate
                         if doit then job else return mempty

haveExtraData :: String -> C Bool
haveExtraData d = isJust `fmap` getExtraData d

addExtraData :: String -> String -> C ()
addExtraData d v =
    modify $ \c -> c { extraDataC = (d,v): filter ((/=d).fst) (extraDataC c) }

packageName :: String -> C ()
packageName = addExtraData "packageName"

getPackageName :: C (Maybe String)
getPackageName = getExtraData "packageName"

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
ldFlags x = modify $ \c -> c { ldFlagsC = ldFlagsC c ++ x }

data ConfigureState = CS { commandLine :: [String],
                           currentSubDirectory :: Maybe String,
                           ghcFlagsC :: [String],
                           pkgFlagsC :: [String],
                           cFlagsC :: [String],
                           ldFlagsC :: [String],
                           packagesC :: [String],
                           replacementsC :: [(String,String)],
                           definitionsC :: [(String,String)],
                           extraDataC :: [(String,String)] }

readConfigureState :: String -> C ConfigureState
readConfigureState d =
    do cl <- readf "commandLine"
       ghc <- readf "ghcFlags"
       pkg <- readf "pkgFlags"
       c <- readf "cFlags"
       ld <- readf "ldFlags"
       packs <- readf "packages"
       repl <- readf "replacements"
       defs <- readf "definitions"
       alles <- readDirectory d'
       let es = catMaybes $ map afterX alles
           afterX ('X':'-':r) = Just r
           afterX _ = Nothing
       vs <- mapM (\e -> io $ readFile (d'++"X-"++e)) es
       let extr = zip es vs
       seq (length $ concat vs) $ return $
           defaultConfiguration { commandLine = cl,
                                  ghcFlagsC = ghc,
                                  pkgFlagsC = pkg,
                                  cFlagsC = c,
                                  ldFlagsC = ld,
                                  packagesC = packs,
                                  replacementsC = repl,
                                  definitionsC = defs,
                                  extraDataC = extr }
      where d' = case reverse d of ('/':_) -> d
                                   _ -> d++"/"
            readf x = do s <- io $ readFile (d'++x)
                         case reads s of
                           [(dat,"")] -> return dat
                           _ -> fail $ "couldn't read "++x

writeConfigureState :: String -> C ()
writeConfigureState d =
    do cs <- get
       writeF (d'++"commandLine") $ show $ commandLine cs
       writeF (d'++"ghcFlags") $ show $ ghcFlagsC cs
       writeF (d'++"pkgFlags") $ show $ pkgFlagsC cs
       writeF (d'++"cFlags") $ show $ cFlagsC cs
       writeF (d'++"ldFlags") $ show $ ldFlagsC cs
       writeF (d'++"packages") $ show $ packagesC cs
       writeF (d'++"replacements") $ show $ replacementsC cs
       writeF (d'++"definitions") $ show $ definitionsC cs
       mapM_ writeExtra $ extraDataC cs
       allextras <- filter ("X-" `isPrefixOf`) `fmap` readDirectory d
       let toberemoved = allextras \\ map (("X-"++) . fst) (extraDataC cs)
       mapM_ (rm_rf . (d'++)) toberemoved
    where d' = case reverse d of ('/':_) -> d
                                 _ -> d++"/"
          writeExtra (e,v) = writeF (d'++"X-"++e) v

writeF :: String -> String -> C ()
writeF x0 y = do x <- processFilePath x0
                 mkdir $ dirname x0
                 y' <- io (readFile x) `catchC` \_ -> return ('x':y)
                 whenC (return $ length y /= length y' || y /= y') $ io $ writeFile x y

readDirectory :: String -> C [String]
readDirectory d =
    do d' <- processFilePath d
       io $ filter (not . (`elem` [".",".."])) `fmap` getDirectoryContents d'

-- | mkdir makes a directory and its parents if it doesn't exist.
mkdir :: FilePath -> C ()
mkdir "" = return ()
mkdir d0 = do d <- processFilePath d0
              unlessC (io $ doesDirectoryExist d) $ do mkdir $ dirname d0
                                                       putV $ "mkdir "++d0
                                                       io $ createDirectory d

splitPath :: FilePath -> (FilePath, FilePath)
splitPath p = (dirname p, basename p)

basename :: FilePath -> FilePath
basename p = reverse (takeWhile isSep rp++ takeWhile (not.isSep) (dropWhile isSep rp))
    where rp = reverse p

isSep :: Char -> Bool
isSep c = c `elem` "/\\"

dirname :: FilePath -> FilePath
dirname = reverse . drop 1 . dropWhile (not . isSep) . dropWhile isSep . reverse

rm_rf :: FilePath -> C ()
rm_rf d0 = do d <- processFilePath d0
              rm_rf' d
  where
   rm_rf' d =
    do catchC (io $ removeFile d) $ \_ -> return ()
       whenC (io $ doesDirectoryExist d) $
             do fs <- readDirectory d
                mapM_ (rm_rf' . ((d++"/")++)) fs
                putV $ "rm -rf "++d
                io $ removeDirectory d
    `catchC` \e -> putV $ "rm -rf failed: "++e

data LogMessage = Stdout String | Logfile String
data HookTime = Preconfigure | Postconfigure
data Verbosity = Quiet | Normal | Verbose | Debug deriving ( Eq, Ord, Enum )
data Target = Target { fellowTargets :: !StringSet,
                       dependencies :: !StringSet,
                       rule :: !(C ()) }

data TotalState = TS { numJobs :: Int,
                       verbosity :: Verbosity,
                       noRemove :: Bool,
                       outputChan :: Chan LogMessage,
                       syncChan :: Chan (),
                       configureHooks :: [(String,C ())],
                       postConfigureHooks :: [(String,C ())],
                       targets :: Trie Target,
                       built :: StringSet,
                       packageModuleMap :: Maybe (Trie [String]),
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

-- ErrorState is for returning errors along with any cached data we might
-- have.  Currently, the only data we cache is the mapping from packages to
-- modules.
data ErrorState = Err String (Maybe (Trie [String]))

newtype C a = C (TotalState -> IO (Either ErrorState (a,TotalState)))

unC :: C a -> TotalState -> IO (Either ErrorState (a,TotalState))
unC (C f) = f

instance Functor C where
    f `fmap` x = x >>= (return . f)

instance Monad C where
    (C f) >>= g = C $ \cs ->
        do macs' <- f cs
           case macs' of
             Left e -> return (Left e)
             Right (a,cs') -> unC (g a) cs'
                              `catch` \err -> return (Left $ Err (show err) $ packageModuleMap cs')
    return x = C (\cs -> return $ Right (x, cs))
    fail e = do putV $ "failure: "++ e
                C (\ts -> return $ Left $ Err e (packageModuleMap ts))

instance MonadPlus C where
    mplus f g = catchC f $ \_ -> g
    mzero = fail "mzero"

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

-- | Change current subdirectory
cd :: String -> C ()
cd d = modify (\cs -> cs { currentSubDirectory = cdd $ currentSubDirectory cs })
    where cdd Nothing = Just d
          cdd (Just oldd) = Just (oldd++"/"++d)

withDirectory :: String -> C a -> C a
withDirectory d f = do oldd <- gets currentSubDirectory
                       cd d
                       x <- f
                       modify $ \cs -> cs { currentSubDirectory = oldd }
                       return x

withRootdir :: C a -> C a
withRootdir f = do oldd <- gets currentSubDirectory
                   modify $ \cs -> cs { currentSubDirectory = Nothing }
                   x <- f
                   modify $ \cs -> cs { currentSubDirectory = oldd }
                   return x

rememberDirectory :: C (C a -> C a)
rememberDirectory = do mcwd <- getCurrentSubdir
                       case mcwd of
                         Just cwd -> return (withRootdir . withDirectory cwd)
                         Nothing -> return withRootdir

-- | getCurrentSubdir returns the current subdirectory, and also ensures
-- that it exists.
getCurrentSubdir :: C (Maybe String)
getCurrentSubdir = do sd <- gets currentSubDirectory
                      case sd of Just d -> withRootdir $ mkdir d
                                 Nothing -> return ()
                      return sd

processFilePath :: String -> C String
processFilePath ('*':f) = return ('*':f) -- This is a phony target
processFilePath f = do sd <- gets currentSubDirectory
                       return $ maybe f (++('/':f)) sd

runC :: [String] -> C a -> IO a
runC args (C a) =
    do ch <- newChan
       ch2 <- newChan
       h <- if "configure" `elem` args then openFile "config.log" WriteMode
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
                      noRemove = False,
                      targets = defaultTargets,
                      built = emptyS,
                      packageModuleMap = Nothing,
                      configureState = defaultConfiguration { commandLine = args } })
       case xxx of
         Left (Err e _) -> do -- give print thread a chance to do a bit more writing...
                      threadDelay 1000000
                      killThread thid
                      putStrLn $ "Error:  "++e
                      exitWith $ ExitFailure 1
         Right (out,_) -> return out

defaultTargets :: Trie Target
defaultTargets =
    insertT "*clean*" (Target emptyS emptyS $ putS "cleaning...") $
    insertT "*install*" (Target emptyS (fromListS ["*build*"]) $ putS "installing...") $
    insertT "*build*" (Target emptyS emptyS $ putS "finished building.") $
    emptyT

defaultConfiguration :: ConfigureState
defaultConfiguration = CS { commandLine = [],
                            currentSubDirectory = Nothing,
                            ghcFlagsC = [],
                            pkgFlagsC = [],
                            cFlagsC = [],
                            ldFlagsC = [],
                            packagesC = [],
                            replacementsC = [],
                            definitionsC = [],
                            extraDataC = [] }

getTargets :: C (Trie Target)
getTargets = C $ \ts -> return $ Right (targets ts, ts)

modifyTargets :: (Trie Target -> Trie Target) -> C ()
modifyTargets f = C $ \ts -> return $ Right ((), ts { targets = f $ targets ts })

getModulePackageMap :: C (Maybe (Trie [String]))
getModulePackageMap = C $ \ts -> return $ Right (packageModuleMap ts, ts)

setModulePackageMap :: Trie [String] -> C ()
setModulePackageMap mpm =
    C $ \ts -> return $ Right ((), ts { packageModuleMap = Just mpm })

isBuilt :: String -> C Bool
isBuilt t = C $ \ts -> return $ Right (t `elemS` built ts || ('*':t++"*") `elemS` built ts, ts)

setBuilt :: String -> C ()
setBuilt t = C $ \ts -> return $ Right ((), ts { built = addS t $ built ts })

clearBuilt :: String -> C ()
clearBuilt t = C $ \ts -> return $ Right ((), ts { built = delS t $ built ts })

io :: IO a -> C a
io x = C $ \cs -> do a <- x
                     return $ Right (a,cs)

catchC :: C a -> (String -> C a) -> C a
catchC (C a) b = C $ \ts ->
                 do out <- (Right `fmap` a ts) `catch` \err -> return (Left $ show err)
                    case out of
                      Left e -> unC (b e) ts
                      Right (Left (Err e modmap)) -> unC (b e) $
                                                     ts { packageModuleMap = modmap `mplus`
                                                                             packageModuleMap ts }
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
replace a = replaceLiteral a . show

replaceLiteral :: String -> String -> C ()
replaceLiteral a b = do r <- gets replacementsC
                        if a `elem` map fst r
                          then return ()
                          else modify $ \c -> c { replacementsC = (a,b):r }

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

getNoRemove :: C Bool
getNoRemove = C $ \ts -> return $ Right (noRemove ts, ts)

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

putL :: String -> C ()
putL = putM Logfile

getVerbosity :: C Verbosity
getVerbosity = C $ \ts -> return $ Right (verbosity ts, ts)

quietly :: C a -> C a
quietly j = do v <- getVerbosity
               C $ \ts -> return $ Right ((), ts { verbosity = Quiet })
               x <- j
               C $ \ts -> return $ Right ((), ts { verbosity = v })
               return x

-- silently guarantees that the command it's passed won't write anything
-- either to the screen or to the log file, unless we're in debug mode.

silently :: C a -> C a
silently (C j) =
  C $ \ts ->
  if verbosity ts == Debug
  then j ts
  else
    do ch <- newChan
       ch2 <- newChan
       let silentthread = do readChan ch
                             writeChan ch2 ()
                             silentthread
       forkIO silentthread
       v <- j $ ts { outputChan = ch, syncChan = ch2 }
       case v of
         Right (a, ts') -> return $ Right (a, ts' { outputChan = outputChan ts,
                                                    syncChan = syncChan ts })
         Left x -> return $ Left x

readVerbosity :: Verbosity -> Maybe String -> Verbosity
readVerbosity defaultV s = case (reads `fmap` s) :: Maybe [(Int,String)] of
                           Just [(0,"")] -> Quiet
                           Just [(1,"")] -> Normal
                           Just [(2,"")] -> Verbose
                           Just [(3,"")] -> Debug
                           _ -> defaultV
