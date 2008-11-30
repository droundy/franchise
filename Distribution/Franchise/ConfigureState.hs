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
    ( amInWindows,
      getModulePackageMap, setModulePackageMap,
      getExtra, addExtra, addExtraUnique, putExtra, persistExtra,
      getExtraData, getAllExtraData, addExtraData, haveExtraData,
      addHook, removeHook, runHooks,
      getNumJobs, setNumJobs, oneJob,
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
      putSnoln, putS, putV, putD, putSV, putL, setVerbose,
      put, get )
        where

import qualified System.Environment as E ( getEnv )
import Control.Monad ( mplus )
import Data.Monoid ( Monoid, mempty, mappend )
import Control.Concurrent ( forkIO, Chan, killThread, threadDelay,
                            readChan, writeChan, newChan )

import System.Exit ( exitWith, ExitCode(..) )
import System.Directory ( getCurrentDirectory,
                          doesDirectoryExist,
                          removeFile, removeDirectory, createDirectory,
                          getDirectoryContents )
import System.IO ( BufferMode(..), IOMode(..), openFile,
                   hSetBuffering, hFlush, hPutStr, stdout )
import Data.List ( delete, (\\) )
import Data.Maybe ( isJust, catMaybes )

import Distribution.Franchise.StringSet
import Distribution.Franchise.Trie

putExtra :: Show a => String -> a -> C ()
putExtra d v = addExtraData d $ show v

addExtraUnique :: (Eq a, Show a, Read a) => String -> [a] -> C ()
addExtraUnique d v = do vold <- getExtra d
                        putExtra d $ v ++ (vold \\ v)

addExtra :: (Monoid a, Show a, Read a) => String -> a -> C ()
addExtra d v = do vold <- getExtra d
                  putExtra d $ mappend v vold

getExtra :: (Monoid a, Read a) => String -> C a
getExtra d = do mv <- getExtraData d
                return $ case reads `fmap` mv of
                           Just [(v,_)] -> v
                           _ -> mempty

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

-- | amInWindows is a hokey function to identify windows systems.  It's
-- probably more portable than checking System.Info.os, which isn't saying
-- much.
amInWindows :: C Bool
amInWindows = (not . elem '/') `fmap` io getCurrentDirectory

data ConfigureState = CS { currentSubDirectory :: Maybe String,
                           extraDataC :: [(String,String)] }

readConfigureState :: String -> C ConfigureState
readConfigureState d =
    do alles <- readDirectory d'
       let es = filter ((/= '.') . head) alles
       vs <- mapM (\e -> io $ readFile (d'++e)) es
       let extr = zip es vs
       seq (length $ concat vs) $ return $
           defaultConfiguration { extraDataC = extr }
      where d' = case reverse d of ('/':_) -> d
                                   _ -> d++"/"

writeConfigureState :: String -> C ()
writeConfigureState d =
    do cs <- get
       mapM_ writeExtra $ extraDataC cs
       allextras <- filter ((/= '.') . head) `fmap` readDirectory d
       let toberemoved = allextras \\ map fst (extraDataC cs)
       mapM_ (rm_rf . (d'++)) toberemoved
    where d' = case reverse d of ('/':_) -> d
                                 _ -> d++"/"
          writeExtra (e,v) = writeF (d'++e) v

persistExtra :: String -> C ()
persistExtra v =
    C $ \ts -> return $ Right ((), ts { persistentThings = v : delete v (persistentThings ts) })

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
data Verbosity = Quiet | Normal | Verbose | Debug deriving ( Eq, Ord, Enum )
data Target = Target { fellowTargets :: !StringSet,
                       dependencies :: !StringSet,
                       rule :: !(C ()) }

data TotalState = TS { numJobs :: Int,
                       verbosity :: Verbosity,
                       outputChan :: Chan LogMessage,
                       syncChan :: Chan (),
                       hooks :: [(String,C ())],
                       targets :: Trie Target,
                       built :: StringSet,
                       persistentThings :: [String],
                       packageModuleMap :: Maybe (Trie [String]),
                       configureState :: ConfigureState }

modifyHooks :: ([(String,C ())] -> [(String,C ())]) -> C ()
modifyHooks f = C $ \ts -> return $ Right ((), ts { hooks = f $ hooks ts })

addHook :: String -> C () -> C ()
addHook n h = do removeHook n
                 modifyHooks ((n,h):)

removeHook :: String -> C ()
removeHook n = modifyHooks $ filter ((/=n) . fst)

runHooks :: C ()
runHooks = do hks <- C $ \ts -> return $ Right (hooks ts, ts)
              mapM_ snd $ reverse hks

-- ErrorState is for returning errors along with any cached data we might
-- have.  Currently, the only data we cache is the mapping from packages to
-- modules.
data ErrorState = Err { failMsg :: String,
                        persistentExtras :: [(String,String)],
                        moduleMap :: Maybe (Trie [String]) }

newtype C a = C (TotalState -> IO (Either ErrorState (a,TotalState)))

unC :: C a -> TotalState -> IO (Either ErrorState (a,TotalState))
unC (C f) = f

instance Functor C where
    f `fmap` x = x >>= (return . f)

instance Monad C where
    (C f) >>= g = C $ \ts ->
        do mats' <- f ts
           case mats' of
             Left e -> return (Left e)
             Right (a,ts') -> unC (g a) ts'
                              `catch` \err -> return (Left $ Err (show err)
                                                                 (getPersistentStuff ts')
                                                                 (packageModuleMap ts'))
    return x = C (\ts -> return $ Right (x, ts))
    fail e = do putV $ "failure: "++ e
                C (\ts -> return $ Left $ Err e (getPersistentStuff ts) (packageModuleMap ts))

getPersistentStuff :: TotalState -> [(String,String)]
getPersistentStuff ts = catMaybes $ map lookupone $ persistentThings ts
    where lookupone d = do v <- lookup d $ extraDataC (configureState ts)
                           Just (d,v)

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

oneJob :: C Bool
oneJob = (==1) `fmap` getNumJobs

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

runC :: C a -> IO a
runC (C a) =
    do ch <- newChan
       ch2 <- newChan
       h <- openFile "franchise.log" WriteMode
       hSetBuffering h LineBuffering
       hSetBuffering stdout LineBuffering
       let writethread = do mess <- readChan ch
                            case mess of Stdout s -> do putStr s
                                                        hFlush stdout
                                         Logfile s -> hPutStr h s
                            writeChan ch2 ()
                            writethread
       thid <- forkIO writethread
       v <- Just `fmap` E.getEnv "VERBOSE" `catch` \_ -> return Nothing
       xxx <- a (TS { outputChan = ch,
                      syncChan = ch2,
                      numJobs = 1,
                      hooks = [],
                      persistentThings = [],
                      verbosity = readVerbosity Normal v,
                      targets = defaultTargets,
                      built = emptyS,
                      packageModuleMap = Nothing,
                      configureState = defaultConfiguration })
       case xxx of
         Left e -> do -- give print thread a chance to do a bit more writing...
                      threadDelay 1000000
                      killThread thid
                      putStrLn $ "Error:  "++ failMsg e
                      exitWith $ ExitFailure 1
         Right (out,_) -> return out

defaultTargets :: Trie Target
defaultTargets =
    insertT "*clean*" (Target emptyS emptyS $ putS "cleaning...") $
    insertT "*install*" (Target emptyS (fromListS ["*build*"]) $ putS "installing...") $
    insertT "*build*" (Target emptyS emptyS $ putS "finished building.") $
    emptyT

defaultConfiguration :: ConfigureState
defaultConfiguration = CS { currentSubDirectory = Nothing,
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
                      Right (Left err) ->
                          unC (b $ failMsg err) $
                          ts { packageModuleMap = moduleMap err `mplus` packageModuleMap ts,
                               configureState = (configureState ts) {
                                     extraDataC = addextras (persistentExtras err) $
                                                  extraDataC (configureState ts) }}
                              where addextras [] x = x
                                    addextras ((d,v):r) ed = addextras r $ (d,v):filter ((/=d).fst) ed
                      Right x -> return x

forkC :: CanModifyState -> C () -> C ()
forkC CannotModifyState (C j) = C (\ts -> do forkIO (j ts >> return())
                                             return $ Right ((),ts))
forkC _ j = j

data CanModifyState = CanModifyState | CannotModifyState deriving (Eq)

putSnoln :: String -> C ()
putSnoln str = whenC ((>= Normal) `fmap` getVerbosity) $
               do putMnoln Stdout str
                  putMnoln Logfile str

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

getNoRemove :: C [()]
getNoRemove = getExtra "noRemove"

putSV :: String -> String -> C ()
putSV str vstr = do v <- getVerbosity
                    case v of
                      Quiet -> return ()
                      Normal -> putM Stdout str
                      _ -> putM Stdout vstr
                    putM Logfile vstr

putM :: (String -> LogMessage) -> String -> C ()
putM _ "" = return ()
putM m str = putMnoln m $ chomp str ++ "\n"
    where chomp x = case reverse x of '\n':rx -> reverse rx
                                      _ -> x

putMnoln :: (String -> LogMessage) -> String -> C ()
putMnoln _ "" = return ()
putMnoln m str = C $ \ts -> do writeChan (outputChan ts) (m str)
                               readChan (syncChan ts)
                               return $ Right ((),ts)

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

setVerbose :: Maybe String -> C ()
setVerbose v = C $ \ts -> return $ Right ((), ts { verbosity = readVerbosity Verbose v })

readVerbosity :: Verbosity -> Maybe String -> Verbosity
readVerbosity defaultV s = case (reads `fmap` s) :: Maybe [(Int,String)] of
                           Just [(0,"")] -> Quiet
                           Just [(1,"")] -> Normal
                           Just [(2,"")] -> Verbose
                           Just [(3,"")] -> Debug
                           _ -> defaultV
