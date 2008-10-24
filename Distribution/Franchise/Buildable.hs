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
    ( build, buildWithArgs, installBin, replace, createFile,
      define, defineAs, isDefined,
      findAnExecutable,
      defaultRule, buildName, build', cleanIt, rm,
      addTarget,
      printBuildableDeep, (|<-),
      source, extraData, combineBuildables, emptyBuildable )
    where

import Control.Monad ( when, msum )
import Data.List ( nub, partition, intersect, isPrefixOf, isSuffixOf )
import System.Environment ( getProgName, getArgs )
import System.Directory ( doesFileExist, removeFile, copyFile,
                          getModificationTime, findExecutable )
import Control.Concurrent ( readChan, writeChan, newChan )

import System.Console.GetOpt ( OptDescr(..) )

import Distribution.Franchise.Util
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.StringSet

(|<-) :: Dependency -> BuildRule -> Buildable
(|<-) = (:<-)

infix 1 |<-

defaultRule :: BuildRule
defaultRule = BuildRule (const $ return ()) (const $ return ()) cleanIt

source :: String -> Buildable
source = Unknown

extraData :: String -> Buildable
extraData x = Unknown ("config.d/X-"++x)

combineBuildables :: [Buildable] -> Buildable
combineBuildables bs = [] :< bs :<- defaultRule

emptyBuildable :: Buildable
emptyBuildable = [] :< [] :<- defaultRule

fixDependenciesBetweenPair :: Buildable -> Buildable -> (Buildable, Buildable)
fixDependenciesBetweenPair a b = (a', b')
    where a' = fixbuild b a
          b' = fixbuild a b
          fixbuild x (Unknown y) = maybe (Unknown y) id $ lookupB y x
          fixbuild x (xs:<xds:<-h) = xs :< map (fixbuild x) xds :<- h

lookupB :: String -> Buildable -> Maybe Buildable
lookupB f b0 = msum $ mapBuildable lu b0
    where lu (Unknown _) = Nothing
          lu b | f `elem` buildName b = Just b
               | otherwise = Nothing

cleanIt :: Dependency -> [String]
cleanIt (_:<[]) = []
cleanIt (xs:<_) = xs

printBuildableDeep :: Buildable -> C ()
printBuildableDeep b@(xs :< ds:<-_) =
    do putS $ unwords xs
       putS $ showBuild b
       putS "Depends on:\n\n"
       let pbd i (x:<d:<-_) = do mapM_ (putS . (take i (repeat ' ')++)) x
                                 mapM_ (pbd (i+1)) d
           pbd i (Unknown x) = putS $ take i (repeat ' ')++"Source:"++x
       mapM_ (pbd 0) ds
printBuildableDeep (Unknown _) = error "bug in printBuildableDeep"

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
buildName (Unknown d) = [d]

buildDeps :: Buildable -> [Buildable]
buildDeps (_:<ds:<-_) = ds
buildDeps _ = []

build :: [C (OptDescr (C ()))] -> C () -> C Buildable -> IO ()
build opts doconf mkbuild =
    do args <- getArgs
       buildWithArgs args opts doconf mkbuild

buildWithArgs :: [String] -> [C (OptDescr (C ()))] -> C () -> C Buildable -> IO ()
buildWithArgs args opts doconf mkbuild =
       runC args $ runWithArgs opts myargs runcommand
    where myargs = ["configure","build","clean","install"]
          runcommand "configure" = configure
          runcommand "clean" = do b <- mkbuild
                                  mapM_ rm $ clean' b
          runcommand "build" = do reconfigure
                                  b <- mkbuild
                                  build' CannotModifyState b
          runcommand "install" = do reconfigure
                                    b <- mkbuild
                                    build' CannotModifyState b
                                    install' b
          runcommand t = do reconfigure
                            b <- mkbuild
                            ts <- getTargets
                            case msum $ map (lookupB t) $ b : ts of
                              Just b' -> build' CannotModifyState b'
                              Nothing -> fail $ "unrecognized target "++t
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
                           build' CanModifyState $ ["config.d/commandLine"]
                                      :< [source setupname]
                                      :<- defaultRule { make = makeConfState }
                           runPostConfigureHooks
          makeConfState _ = do fs <- gets commandLine
                               putV $ "reconfiguring with flags " ++ unwords fs
                               runWithArgs opts myargs (const configure)

install' :: Buildable -> C ()
install' ((x :< ds) :<- how) = do mapM_ install' ds
                                  install how (x :< ds)
install' (Unknown _) = return ()

mapBuildable :: (Buildable -> a) -> Buildable -> [a]
mapBuildable f b = reverse $ mb emptyS [b]
    where mb done (x:xs) | x `elemB` done = mb done xs
                         | otherwise = mb ([x] `addB` done) (requirements x ++ xs) ++ [f x]
          mb _ [] = []
          requirements (_:<ds:<-_) = ds
          requirements _ = []

clean' :: Buildable -> [String]
clean' b = concat $ mapBuildable c b
    where c (d:<-how) = clean how d
          c _ = []

needsWork :: Dependency -> C Bool
needsWork ([]:<_) = return True
needsWork ((x:_) :< ds) =
    do fe <- io $ doesFileExist x
       if not fe
         then do putD $ "need work because " ++ x ++ " doesn't exist"
                 return True
         else do mt <- io $ getModificationTime x
                 let latertime y = do ye <- io $ doesFileExist y
                                      if not ye
                                        then do putD $ "Need work cuz "++y++" don't exist"
                                                return True
                                        else do mty <- io $ getModificationTime y
                                                if mty > mt
                                                   then putD $ "I need work since "++ y ++
                                                            " is newer than " ++ x
                                                   else return ()
                                                return (mty > mt)
                     anyM _ [] = return False
                     anyM f (z:zs) = do b <- f z
                                        if b then return True
                                             else anyM f zs
                 anyM latertime $ concatMap buildName ds

build' :: CanModifyState -> Buildable -> C ()
build' _ (Unknown f) = do e <- io $ doesFileExist f
                          when (not e) $ fail $ "Source file "++f++" does not exist!"
build' cms b =
        do --put $S unwords ("I'm thinking of recompiling...": buildName b)
           w <- reverse `fmap` findWork b
           case w of
             [] -> putD "I see nothing here to recompile"
             _ -> putD $ "I want to recompile all of "++ unwords (concatMap buildName w)
           case length w of
             0 -> putD $ "Nothing to recompile for "++unwords (buildName b)++"."
             l -> putD $ unwords $ ["Need to recompile ",show l,"for"]
                                       ++buildName b++["."]
           chan <- io $ newChan
           buildthem chan emptyS w
    where buildthem _ _ [] = return ()
          buildthem chan inprogress w =
              do putD $ unwords ("I am now wanting to compile":concatMap buildName w)
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
                 let (canb',depb') = partition (canBuildNow (w `addB` inprogress)) w
                     jobs = max 0 (njobs - lengthS inprogress)
                     canb = take jobs canb'
                     depb = drop jobs canb' ++ depb'
                     buildone (d:<-how) =
                         forkC cms $
                         do stillneedswork <- needsWork d
                            if stillneedswork
                              then do let _ :< xs = d
                                      putD $ unlines
                                        ["I am making "++ unwords (depName d),
                                         "  This depends on "++
                                         unwords (concatMap buildName xs),
                                         "  These depend on "++
                                         unwords (concatMap
                                              (concatMap buildName . buildDeps) xs)]
                                      make how d
                                               `catchC`
                                               \e -> do putV $ errorBuilding e $ unwords(depName d)
                                                        putV e
                                                        io $ writeChan chan $ Left e
                                      io $ writeChan chan (Right (d:<-how))
                              else do putD $ "I get to skip one! " ++ unwords (depName d)
                                      io $ writeChan chan (Right (d:<-how))
                     buildone (Unknown _) = error "bug in buildone"
                 case filter (".o" `isSuffixOf`) $ concatMap buildName canb of
                   [] -> return ()
                   [_] -> return ()
                   tb -> putD $ "I can now build "++ unwords tb
                 mapM_ buildone canb
                 md <- io $ readChan chan
                 case md of
                   Left e -> do let estr = errorBuilding e $ unwords (buildName b)
                                putV (estr++'\n':e)
                                fail estr
                   Right d -> do putD $ "Done building "++ unwords (buildName d)
                                 buildthem chan (delB d (addB canb $ inprogress)) depb
          delB done x = delsS (buildName done) x
          errorBuilding _ "config.d/commandLine" = "configure failed"
          errorBuilding e f | ".depend" `isSuffixOf` f = e
          errorBuilding _ bn = "Error building "++bn

showBuild :: Buildable -> String
showBuild (xs:<ds:<-_) = unwords (xs++ [":"]++nub (concatMap buildName ds))
showBuild _ = error "bug in showBuild"

elemB :: Buildable -> StringSet -> Bool
elemB b s = any (`elemS` s) $ buildName b

addB :: [Buildable] -> StringSet -> StringSet
addB b s = addsS (concatMap buildName b) s

canBuildNow :: StringSet -> Buildable -> Bool
canBuildNow _ (Unknown _) = True
canBuildNow needwork (_:<d:<-_) = not $ any (`elemB` needwork) d

findWork :: Buildable -> C [Buildable]
findWork (Unknown _) = return []
findWork zzz = do putD $ "findWork called on "++unwords (concatMap buildName $ mapBuildable id zzz)
                  fw [] [] $ reverse $ mapBuildable id zzz
    where -- The second and third arguments ought to be sets!
          fw :: [Buildable] -> [Buildable] -> [Buildable] -> C [Buildable]
          fw nw _ [] = return nw
          fw nw ok (Unknown x:r) | Unknown x `elem` (ok++nw) = fw nw ok r
                                 | otherwise = fw nw (Unknown x:ok) r
          fw nw ok (b@(xs:<ds:<-_):r) =
              if b `elem` (ok++nw)
              then do if b `elem` ok
                         then putD $ "I already have "++ unwords (buildName b)
                         else putD $ "I already know I must compile "++
                                       unwords (buildName b)
                      fw nw ok r
              else
                if all (`elem` (ok++nw)) ds
                then
                   do ineedwork <- case nw `intersect` ds of
                                   (_z:_) -> do putD $ "Must compile "++ unwords (buildName b) ++
                                                             " because of " ++ unwords (buildName _z)
                                                return True
                                   [] -> needsWork (xs:<ds)
                      if ineedwork then fw (b:nw) ok r
                                   else fw nw (b:ok) r
                else fw nw ok (ds++b:r)

installBin :: Dependency -> C ()
installBin (xs:<_) = do pref <- getBinDir
                        mapM_ (\x -> io $ copyFile x (pref++"/"++x)) xs

createFile :: String -> C ()
createFile fn = do addTarget $ [fn] :< [source $ fn++".in"] :<-
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

addTarget :: Buildable -> C Buildable
addTarget b0 = do modifyTargets $ addb b0
                  last `fmap` getTargets
    where addb b [] = [b]
          addb b (x:xs) | b == x = b:xs -- override targets with same name
          addb b (x:xs) = x' : addb b' xs
              where (b',x') = fixDependenciesBetweenPair b x

