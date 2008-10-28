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

module Distribution.Franchise.Buildable
    ( Buildable(..), build, buildWithArgs, installBin, replace, createFile,
      define, defineAs, isDefined,
      findAnExecutable,
      defaultRule, buildName, build', cleanIt, rm,
      addTarget, getBuildable, (|<-),
      extraData )
    where

import Data.Maybe ( isJust )
import Data.List ( isPrefixOf, isSuffixOf )
import System.Environment ( getProgName, getArgs )
import System.Directory ( doesFileExist, removeFile, copyFile,
                          getModificationTime, findExecutable )
import Control.Concurrent ( readChan, writeChan, newChan )

import System.Console.GetOpt ( OptDescr(..) )

import Distribution.Franchise.Util
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.StringSet
import Distribution.Franchise.Trie

data Buildable = Dependency :<- BuildRule

instance Eq Buildable where
    (xs:<_:<-_) == (ys:<_:<-_) = fromListS xs == fromListS ys

(|<-) :: Dependency -> BuildRule -> Buildable
(|<-) = (:<-)

infix 1 :<-
infix 1 |<-

defaultRule :: BuildRule
defaultRule = BuildRule (const $ return ()) (const $ return ()) cleanIt

extraData :: String -> String
extraData x = "config.d/X-"++x

cleanIt :: Dependency -> [String]
cleanIt (_:<[]) = []
cleanIt (xs:<_) = xs

rm :: String -> C ()
rm f | "/" `isSuffixOf` f = return ()
rm f = do noRm <- getNoRemove
          f' <- processFilePath f
          if noRm then putV $ "#rm "++f
                  else do io $ removeFile f'
                          putV $ "rm "++f
                         `catchC` \_ -> return ()

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d

build :: [C (OptDescr (C ()))] -> C () -> C String -> IO ()
build opts doconf mkbuild =
    do args <- getArgs
       buildWithArgs args opts doconf mkbuild

buildWithArgs :: [String] -> [C (OptDescr (C ()))] -> C () -> C String -> IO ()
buildWithArgs args opts doconf mkbuild =
       runC args $ runWithArgs opts myargs runcommand
    where myargs = ["configure","build","clean","install"]
          runcommand "configure" = configure
          runcommand "clean" = do b <- mkbuild
                                  clean' b
          runcommand "build" = do reconfigure
                                  b <- mkbuild
                                  build' CannotModifyState b
          runcommand "install" = do reconfigure
                                    b <- mkbuild
                                    build' CannotModifyState b
                                    install' b
          runcommand t = do reconfigure
                            mkbuild
                            build' CannotModifyState t
          configure = do putS "configuring..."
                         rm "config.d/commandLine"
                         runConfigureHooks
                         doconf
                         mkbuild
                         runPostConfigureHooks
                         writeConfigureState "config.d"
                         putS "configure successful."
          reconfigure = do (readConfigureState "config.d" >>= put)
                              `catchC` \_ -> do putV "Couldn't read old config.d"
                                                rm_rf "config.d"
                           setupname <- io $ getProgName
                           putV "checking whether we need to reconfigure"
                           let needreconf = do clmt <- io $ getModificationTime "config.d/commandLine"
                                               setupmt <- io $ getModificationTime setupname
                                               if setupmt < clmt
                                                  then putV "reconfiguring due to timestamps"
                                                  else return ()
                                               return (setupmt < clmt)
                                            `catchC` \_ -> do putV "reconfiguring due to missing file"
                                                              return True
                           whenC needreconf $ makeConfState
                           runPostConfigureHooks
          makeConfState = do fs <- gets commandLine
                             putV $ "reconfiguring with flags " ++ unwords fs
                             runWithArgs opts myargs (const configure)

install' :: String -> C ()
install' t0 = inst emptyS [t0]
    where inst _ [] = return ()
          inst done (t:ts) | t `elemS` done = inst done ts
          inst done (t:ts) =
              do mt <- getBuildable t
                 case mt of
                   Nothing -> inst (addS t done) ts
                   Just (xs:<ds:<-how) -> do install how (xs :< ds)
                                             inst (addS t done) (filter (not . (`elemS` done)) ds++ts)

clean' :: String -> C ()
clean' t0 = cl emptyS [t0]
  where
    cl _ [] = return ()
    cl done (t:ts) | t `elemS` done = cl done ts
    cl done (t:ts) =
        do mb <- getBuildable t
           case mb of
             Nothing -> cl (addS t done) ts
             Just (xs :< ds :<- how) ->
                 do mapM_ rm $ clean how (xs :< ds)
                    cl (addS t done) (filter (not . (`elemS` done)) ds++ts)

needsWork :: String -> C Bool
needsWork t =
    do mtt <- getTarget t
       case mtt of
         Nothing -> return False
         Just (Target _ ds _)
             | nullS ds -> return True -- no dependencies means it always needs work!
         Just (Target ts ds _) ->
           do mmt <- ((Just . maximum) `fmap` io (mapM getModificationTime (t:toListS ts)))
                     `catchC` \_ -> return Nothing
              case mmt of
                Nothing -> do putD $ "need work because " ++ t ++ " doesn't exist (or a friend)"
                              return True
                Just mt -> anyM latertime $ toListS ds
                      where latertime y = do ye <- io $ doesFileExist y
                                             if not ye
                                               then do putD $ "Need work cuz "++y++" don't exist"
                                                       return True
                                               else do mty <- io $ getModificationTime y
                                                       if mty > mt
                                                         then putD $ "I need work since "++ y ++
                                                                  " is newer than " ++ t
                                                         else return ()
                                                       return (mty > mt)
                            anyM _ [] = return False
                            anyM f (z:zs) = do b <- f z
                                               if b then return True else anyM f zs
                        

build' :: CanModifyState -> String -> C ()
build' cms b =
        do --put $S unwords ("I'm thinking of recompiling...": buildName b)
           w <- toListS `fmap` findWork b
           case w of
             [] -> putD "I see nothing here to recompile"
             _ -> putD $ "I want to recompile all of "++ unwords w
           case length w of
             0 -> putD $ "Nothing to recompile for "++b++"."
             l -> putD $ unwords $ ["Need to recompile ",show l,"for"]++b:["."]
           chan <- io $ newChan
           buildthem chan emptyS w
    where buildthem _ _ [] = return ()
          buildthem chan inprogress w =
              do putD $ unwords ("I am now wanting to compile":w)
                 loadavgstr <- cat "/proc/loadavg" `catchC` \_ -> return ""
                 let loadavg = case reads loadavgstr of
                               ((n,_):_) -> max 0.0 (n :: Double)
                               _ -> 0.0
                     fixNumJobs nj =
                         if nj > 1 && loadavg >= 0.5+fromIntegral nj
                         then do putV $ "Throttling jobs with load "++show loadavg
                                 return 1
                         else return nj
                 njobs <- getNumJobs >>= fixNumJobs
                 (canb',depb') <- partitionM (canBuildNow (w `addsS` inprogress)) w
                 let jobs = max 0 (njobs - lengthS inprogress)
                     canb = take jobs canb'
                     depb = drop jobs canb' ++ depb'
                     buildone ttt =
                         forkC cms $
                         do stillneedswork <- needsWork ttt
                            Just (Target ts xs0 how) <- getTarget ttt
                            if stillneedswork
                              then do putD $ unlines
                                        ["I am making "++ ttt,
                                         "  This depends on "++ unwords (toListS xs0)]
                                      make how (ttt:toListS ts :< toListS xs0)
                                               `catchC`
                                               \e -> do putV $ errorBuilding e ttt
                                                        putV e
                                                        io $ writeChan chan $ Left e
                                      io $ writeChan chan (Right ttt)
                              else do putD $ "I get to skip one! " ++ ttt
                                      io $ writeChan chan (Right ttt)
                 case filter (".o" `isSuffixOf`) canb of
                   [] -> return ()
                   [_] -> return ()
                   tb -> putD $ "I can now build "++ unwords tb
                 mapM_ buildone canb
                 md <- io $ readChan chan
                 case md of
                   Left e -> do let estr = errorBuilding e b
                                putV (estr++'\n':e)
                                fail estr
                   Right d -> do putD $ "Done building "++ d
                                 buildthem chan (delS d (addsS canb $ inprogress)) depb
          errorBuilding e "config.d/commandLine" = "configure failed:\n"++e
          errorBuilding e f | ".depend" `isSuffixOf` f = e
          errorBuilding e bn = "Error building "++bn++'\n':e

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ [] = return ([],[])
partitionM f (x:xs) = do amok <- f x
                         (ok,notok) <- partitionM f xs
                         return $ if amok then (x:ok,notok) else (ok,x:notok)

canBuildNow :: StringSet -> String -> C Bool
canBuildNow needwork t = do mt <- getTarget t
                            case dependencies `fmap` mt of
                              Just d -> return $ not $ any (`elemS` needwork) $ toListS d
                              _ -> return True

getBuildable :: String -> C (Maybe Buildable)
getBuildable t = do mt <- getTarget t
                    case mt of
                      Nothing -> return Nothing
                      Just (Target ts ds how) -> return $ Just (t:toListS ts :< toListS ds :<- how)

getTarget :: String -> C (Maybe Target)
getTarget t = lookupT t `fmap` getTargets

findWork :: String -> C StringSet
findWork zzz = do putD $ "findWork called on "++zzz
                  keysT `fmap` fw emptyT zzz
    where fw :: Trie Bool -> String -> C (Trie Bool)
          fw nw t | isJust $ t `lookupT` nw = return nw
          fw nw t = do mt <- getTarget t
                       case mt of
                         Nothing -> return nw
                         Just (Target _ ds _)
                             | nullS ds -> -- no dependencies means it always needs work!
                                           return $ insertT t True nw
                         Just (Target _ ds00 _) ->
                             do let ds0 = toListS ds00
                                nwds <- lookAtDeps nw ds0
                                if any (\d -> Just True == d `lookupT` nwds) ds0
                                   then return $ insertT t True nwds
                                   else do tooold <- needsWork t
                                           --putD$"These need work: "++unwords(toListS$keysT$filterT id nwds)
                                           --putD$"These are FINE!: "++unwords(toListS$keysT$filterT not nwds)
                                           return $ insertT t tooold nwds
                             where lookAtDeps nw' [] = return nw'
                                   lookAtDeps nw' (d:ds) = do nw2 <- fw nw' d
                                                              lookAtDeps nw2 ds

installBin :: Dependency -> C ()
installBin (xs:<_) = do pref <- getBinDir
                        mapM_ (\x -> io $ copyFile x (pref++"/"++x)) xs

createFile :: String -> C ()
createFile fn = do addTarget $ [fn] :< [fn++".in"] :<-
                             defaultRule { make = \_ -> actuallyCreateFile fn }
                   actuallyCreateFile fn

actuallyCreateFile :: String -> C ()
actuallyCreateFile fn = do x <- cat (fn++".in")
                           r <- replacements
                           io $ writeFile fn $ repl r x
    where repl [] x = x
          repl ((a,b):rs) x = repl rs $ r1 a b x
          r1 a b x@(x1:xs) | a `isPrefixOf` x = b ++ r1 a b (drop (length a) x)
                           | otherwise = x1 : r1 a b xs
          r1 _ _ "" = ""

define :: String -> C ()
define x = do ghcFlags ["-D"++x]
              cFlags ["-D"++x]

defineAs :: String -> String -> C ()
defineAs x y = do ghcFlags ["-D"++x++"=\""++y++"\""]
                  cFlags ["-D"++x++"=\""++y++"\""]

isDefined :: String -> C Bool
isDefined x = elem ("-D"++x) `fmap` getGhcFlags

-- throw exception on failure to find something
findAnExecutable :: String -> [String] -> C String
findAnExecutable e xs = fe (e:xs)
    where fe [] = fail $ "Couldn't find executable "++e
          fe (y:ys) = do me <- io $ findExecutable y
                         case me of
                           Just _ -> return y
                           Nothing -> fe ys

addTarget :: Buildable -> C ()
addTarget (ts :< ds :<- r) =
    do ts' <- mapM processFilePath ts
       ds' <- fromListS `fmap` mapM processFilePath ds
       let fixt t = (t, delS t allts)
           allts = fromListS ts'
           ts'' = map fixt ts'
           addt (t,otherTs) = modifyTargets $ insertT t (Target otherTs ds' r)
       mapM_ addt ts''
