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
    ( build, installBin, replace, createFile,
      define, defineAs, isDefined,
      findAnExecutable,
      -- The constructors are exported so users
      -- can construct arbitrarily complex build
      -- systems, hopefully.
      Dependency(..), Buildable(..), (|<-), BuildRule(..),
      defaultRule, buildName, build', cleanIt, rm,
      printBuildableDeep,
      -- useful for user-oriented messages.
      putS,
      -- semi-automatic rule generation
      source, (.&), combineBuildables )
    where

import Control.Monad ( when, msum )
import Data.List ( nub, partition, delete, intersect )
import System.Environment ( getProgName )
import System.Directory ( doesFileExist, removeFile, copyFile,
                          getModificationTime, findExecutable )
import Control.Concurrent ( readChan, writeChan, newChan )

import System.Console.GetOpt ( OptDescr(..) )

import Distribution.Franchise.Util
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.StringSet

data Dependency = [String] :< [Buildable]
data Buildable = Dependency :<- BuildRule
               | Unknown String
(|<-) :: Dependency -> BuildRule -> Buildable
(|<-) = (:<-)

data BuildRule = BuildRule { make :: Dependency -> C (),
                             install :: Dependency -> C (),
                             clean :: Dependency -> [String] }

defaultRule :: BuildRule
defaultRule = BuildRule (const $ return ()) (const $ return ()) cleanIt

infix 2 :<
infix 1 :<-, |<-

source :: String -> Buildable
source = Unknown

combineBuildables :: [Buildable] -> Buildable
combineBuildables bs = [] :< bs :<- defaultRule

(.&) :: Buildable -> Buildable -> Buildable
infixr 3 .&
a .& b = [unwords (buildName a++"and":buildName b)] :< [a',b'] :<- defaultRule
    where a' = fixbuild b a
          b' = fixbuild a b
          fixbuild x (Unknown y) = maybe (Unknown y) id $ lookupB y x
          fixbuild x (xs:<xds:<-h) = xs :< map (fixbuild x) xds :<- h

lookupB :: String -> Buildable -> Maybe Buildable
lookupB _ (Unknown _) = Nothing
lookupB f (xs:<xds:<-h) | f `elem` xs = Just (xs:<xds:<-h)
                        | otherwise = msum (map (lookupB f) xds)

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
rm f | endsWith "/" f = return ()
rm f = io (removeFile f) `catchC` \_ -> return ()

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d
buildName (Unknown d) = [d]

buildDeps :: Buildable -> [Buildable]
buildDeps (_:<ds:<-_) = ds
buildDeps _ = []

saveConf :: C ()
saveConf = do s <- show `fmap` get
              io $ writeFile "conf.state" s

restoreConf :: C ()
restoreConf = do s <- cat "conf.state"
                 case reads s of
                   ((c,_):_) -> put c
                   _ -> fail "Couldn't read conf.state"

build :: [OptDescr (C ())] -> C () -> C Buildable -> IO ()
build opts doconf mkbuild =
    runC $ runWithArgs opts myargs runcommand
    where myargs = ["configure","build","clean","install"]
          mkbuild' = do b <- mkbuild
                        b2 <- buildCreatedFiles
                        return (b .& b2)
          runcommand "configure" = configure
          runcommand "clean" = do b <- mkbuild'
                                  mapM_ rm $ clean' b
          runcommand "build" = do reconfigure
                                  b <- mkbuild
                                  build' CannotModifyState b
          runcommand "install" = do reconfigure
                                    b <- mkbuild'
                                    build' CannotModifyState b
                                    install' b
          runcommand x = fail $ "nonexistent command "++x
          configure = do putS "Configuring..."
                         doconf
                         saveConf
                         setConfigured
                         putS "Configure successful."
          reconfigure = do restoreConf `catchC` \_ -> rm "conf.state"
                           setupname <- io $ getProgName
                           build' CanModifyState $ ["conf.state"] :< [source setupname]
                                      :<- defaultRule { make = makeConfState }
          makeConfState _ = do fs <- gets commandLine
                               runWithArgs opts myargs (const configure)
                               modify $ \s -> s { commandLine=fs }

install' :: Buildable -> C ()
install' ((x :< ds) :<- how) = do mapM_ install' ds
                                  install how (x :< ds)
install' (Unknown _) = return ()

mapBuildable :: (Buildable -> a) -> Buildable -> [a]
mapBuildable f b = reverse $ mb [] [b]
    where mb done (x:xs) | x `elem` done = mb done xs
                         | otherwise = mb (x:done) (requirements x ++ xs) ++ [f x]
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
         then do --putS $ "need work because " ++ x ++ " doesn't exist"
                 return True
         else do mt <- io $ getModificationTime x
                 let latertime y = do ye <- io $ doesFileExist y
                                      if not ye
                                        then do --putS $ "Need work cuz "++y++" don't exist"
                                                return True
                                        else do mty <- io $ getModificationTime y
                                                --if mty > mt
                                                --   then putS $ "I need work since "++ y ++
                                                --            " is too new versus " ++ x
                                                --   else return ()
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
           --putS $ "I want to recompile all of "++ unwords (concatMap buildName w)
           --case length w of
           --  0 -> putS $ "Nothing to recompile for "++unwords (buildName b)++"."
           --  l -> putS $ unwords $ ["Need to recompile ",show l,"for"]
           --                            ++buildName b++["."]
           chan <- io $ newChan
           buildthem chan emptyS w
    where buildthem _ _ [] = return ()
          buildthem chan inprogress w =
              do --putS $ unwords ("I am now wanting to compile":concatMap buildName w)
                 loadavgstr <- cat "/proc/loadavg" `catchC` \_ -> return ""
                 let loadavg = case reads loadavgstr of
                               ((n,_):_) -> max 0 (round (n :: Double))
                               _ -> 0
                 njobs <- (max 1 . (\x -> x-loadavg)) `fmap` getNumJobs
                 let (canb',depb') = partition (canBuildNow (w `addB` inprogress)) w
                     jobs = max 0 (njobs - lengthS inprogress)
                     canb = take jobs canb'
                     depb = drop jobs canb' ++ depb'
                     buildone (d:<-how) =
                         forkC cms $
                         do stillneedswork <- needsWork d
                            if stillneedswork
                              then do --let _ :< xs = d
                                      --putS $ unlines
                                      --  ["I am making "++ unwords (depName d),
                                      --   "  This depends on "++
                                      --   unwords (concatMap buildName xs),
                                      --   "  These depend on "++
                                      --   unwords (concatMap
                                      --        (concatMap buildName . buildDeps) xs)]
                                      make how d
                                               `catchC`
                                               (io . writeChan chan . Left)
                                      io $ writeChan chan (Right (d:<-how))
                              else do --putS $ "I get to skip one! " ++ unwords (depName d)
                                      io $ writeChan chan (Right (d:<-how))
                     buildone (Unknown _) = error "bug in buildone"
                 --case filter (endsWith ".o") $ concatMap buildName canb of
                 --  [] -> return ()
                 --  [_] -> return ()
                 --  tb -> putS $ "I can now build "++ unwords tb
                 mapM_ buildone canb
                 md <- io $ readChan chan
                 case md of
                   Left e -> fail $ "Failure building " ++ unwords (buildName b)
                                  ++"\n" ++ e
                   Right d -> do --putS $ "Done building "++ unwords (buildName d)
                                 buildthem chan (delB d (addB canb $ inprogress)) depb
          delB done x = delsS (buildName done) x

showBuild :: Buildable -> String
showBuild (xs:<ds:<-_) = unwords (xs++ [":"]++nub (concatMap buildName ds))
showBuild _ = error "bug in showBuild"

instance Eq Buildable where
    Unknown x == Unknown y = x == y
    Unknown x == (ys:<_:<-_) = x `elem` ys
    (ys:<_:<-_) == Unknown x = x `elem` ys
    (xs:<_:<-_) == (ys:<_:<-_) = eqset xs ys
        where eqset [] [] = True
              eqset [] _ = False
              eqset _ [] = False
              eqset (z:zs) bs = z `elem` bs && zs `eqset` (delete z bs)

elemB :: Buildable -> StringSet -> Bool
elemB b s = buildName b `anyElemS` s

addB :: [Buildable] -> StringSet -> StringSet
addB b s = addsS (concatMap buildName b) s

canBuildNow :: StringSet -> Buildable -> Bool
canBuildNow _ (Unknown _) = True
canBuildNow needwork (_:<d:<-_) = not $ any (`elemB` needwork) d

findWork :: Buildable -> C [Buildable]
findWork (Unknown _) = return []
findWork zzz = do -- putS $ "findWork called on "++unwords (concatMap buildName $ mapBuildable id zzz)
                  fw [] [] $ reverse $ mapBuildable id zzz
    where -- The second and third arguments ought to be sets!
          fw :: [Buildable] -> [Buildable] -> [Buildable] -> C [Buildable]
          fw nw _ [] = return nw
          fw nw ok (Unknown x:r) | Unknown x `elem` (ok++nw) = fw nw ok r
                                 | otherwise = fw nw (Unknown x:ok) r
          fw nw ok (b@(xs:<ds:<-_):r) =
              if b `elem` (ok++nw)
              then do --if b `elem` ok
                      --   then putS $ "I already have "++ unwords (buildName b)
                      --   else putS $ "I already know I must compile "++
                      --                 unwords (buildName b)
                      fw nw ok r
              else
                if all (`elem` (ok++nw)) ds
                then
                   do ineedwork <- case nw `intersect` ds of
                                   (_z:_) -> do --putS $ "Must compile "++ unwords (buildName b) ++
                                                --             " because of " ++ unwords (buildName _z)
                                                return True
                                   [] -> needsWork (xs:<ds)
                      if ineedwork then fw (b:nw) ok r
                                   else fw nw (b:ok) r
                else fw nw ok (ds++b:r)

installBin :: Dependency -> C ()
installBin (xs:<_) = do pref <- getBinDir
                        mapM_ (\x -> io $ copyFile x (pref++"/"++x)) xs

createFile :: String -> C ()
createFile fn = do addCreatedFile fn
                   actuallyCreateFile fn

buildCreatedFiles :: C Buildable
buildCreatedFiles = do cfs <- getCreatedFiles
                       let bcfs = map bcf cfs
                           bcf fn = [fn] :< [source $ fn++".in"]
                                    :<- defaultRule { make = \_ -> actuallyCreateFile fn }
                       return $ ["created files"] :< bcfs :<- defaultRule

actuallyCreateFile :: String -> C ()
actuallyCreateFile fn = do x <- cat (fn++".in")
                           r <- replacements
                           io $ writeFile fn $ repl r x
    where repl [] x = x
          repl ((a,b):rs) x = repl rs $ r1 a b x
          r1 a b x@(x1:xs) | startsWith a x = b ++ r1 a b (drop (length a) x)
                           | otherwise = x1 : r1 a b xs
          r1 _ _ "" = ""
          startsWith [] _ = True
          startsWith (c:cs) (d:ds) = c == d && startsWith cs ds
          startsWith _ _ = False

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
