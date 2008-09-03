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
    ( build, installBin, replace, createFile, define,
      -- The constructors are exported so users
      -- can construct arbitrarily complex build
      -- systems, hopefully.
      Dependency(..), Buildable(..), (|<-), BuildRule(..),
      defaultRule, buildName, build', cleanIt, rm,
      -- useful for user-oriented messages.
      putS,
      -- semi-automatic rule generation
      source, (.&) )
    where

import Control.Monad ( when, mplus, msum )
import Data.Maybe ( catMaybes, listToMaybe )
import Data.Set ( fromList, toList )
import Data.List ( nub, partition, delete, intersect, (\\) )
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist, removeFile )
import System.Posix.Files ( getFileStatus, modificationTime )
import Control.Concurrent ( readChan, writeChan, newChan )

import Control.Monad.State ( modify, put, get )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )

import Distribution.Franchise.Util
import Distribution.Franchise.ConfigureState

data Dependency = [String] :< [Buildable]
data Buildable = Dependency :<- BuildRule
               | Unknown String
(|<-) :: Dependency -> BuildRule -> Buildable
(|<-) = (:<-)

data BuildRule = BuildRule { make :: Dependency -> C (),
                             install :: Dependency -> C (),
                             clean :: Dependency -> [String] }

defaultRule = BuildRule (const $ return ()) (const $ return ()) cleanIt

infix 2 :<
infix 1 :<-, |<-

source :: String -> Buildable
source = Unknown

(.&) :: Buildable -> Buildable -> Buildable
infixr 3 .&
a .& b = [] :< [a',b'] :<- defaultRule
    where a' = fixbuild b a
          b' = fixbuild a b
          fixbuild x (Unknown y) = maybe (Unknown y) id $ lookupB y x
          fixbuild x (xs:<xds:<-h) = xs :< map (fixbuild x) xds :<- h

lookupB :: String -> Buildable -> Maybe Buildable
lookupB f (Unknown _) = Nothing
lookupB f (xs:<xds:<-h) | f `elem` xs = Just (xs:<xds:<-h)
                        | otherwise = msum (map (lookupB f) xds)

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

commaWords :: [String] -> String
commaWords [] = ""
commaWords [x] = x
commaWords (x:xs) = x++", "++commaWords xs

rm :: String -> C ()
rm f | endsWith "/" f = return ()
rm f = io (removeFile f) `catchC` \_ -> return ()

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d
buildName (Unknown d) = [d]

saveConf :: C ()
saveConf = do s <- show `fmap` get
              io $ writeFile "conf.state" s

restoreConf :: C ()
restoreConf = do s <- io $ readFile "conf.state"
                 case reads s of
                   ((c,_):_) -> put c
                   _ -> fail "Couldn't read conf.state"

build :: [OptDescr (C ())] -> C () -> C Buildable -> IO ()
build opts doconf mkbuild =
    runC $ runWithArgs opts ["configure","build","clean","install"] runcommand
    where runcommand "configure" = configure
          runcommand "clean" = do b <- mkbuild
                                  mapM_ rm $ clean' b
          runcommand "build" = do reconfigure
                                  b <- mkbuild
                                  build' CannotModifyState b
          runcommand "install" = do reconfigure
                                    b <- mkbuild
                                    build' CannotModifyState b
                                    install' b
          configure = do putS "Configuring..."
                         doconf
                         saveConf
                         setConfigured
          reconfigure = do restoreConf `catchC` \_ -> rm "conf.state"
                           setupname <- io $ getProgName
                           build' CanModifyState $ ["conf.state"] :< [source setupname]
                                      :<- defaultRule { make = \_ -> configure }

install' :: Buildable -> C ()
install' ((x :< ds) :<- how) = do mapM_ install' ds
                                  install how (x :< ds)
install' (Unknown _) = return ()

nubsort :: (Eq a, Ord a) => [a] -> [a]
nubsort = toList . fromList

mapBuildable :: (Buildable -> a) -> Buildable -> [a]
mapBuildable f b = reverse $ mb [] [b]
    where mb done (b:bs) | b `elem` done = mb done bs
                         | otherwise = mb (b:done) (requirements b ++ bs) ++ [f b]
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
         else do s <- io $ getFileStatus x
                 let mt = modificationTime s
                     latertime y = do ye <- io $ doesFileExist y
                                      if not ye
                                        then do --putS $ "Need work cuz "++y++" don't exist"
                                                return True
                                        else do sy <- io $ getFileStatus y
                                                --if (modificationTime sy > mt)
                                                --   then putS $ "I need work since "++ y ++
                                                --            " is too new versus " ++ x
                                                --   else return ()
                                                return (modificationTime sy > mt)
                     anyM _ [] = return False
                     anyM f (z:zs) = do b <- f z
                                        if b then return True
                                             else anyM f zs
                 anyM latertime $ concatMap buildName ds

build' :: CanModifyState -> Buildable -> C ()
build' _ (Unknown f) = do e <- io $ doesFileExist f
                          when (not e) $ fail $ "Source file "++f++" does not exist!"
build' cms b =
        do -- putS $ unwords ("I'm thinking of recompiling...": buildName b)
           w <- reverse `fmap` findWork b
           --putS $ "I want to recompile all of "++ unwords (concatMap buildName w)
           case length w of
             0 -> putS $ "Nothing to recompile for "++unwords (buildName b)++"."
             l -> putS $ unwords $ ["Need to recompile ",show l,"for"]
                                       ++buildName b++["."]
           chan <- io $ newChan
           buildthem chan [] w
    where buildthem _ [] [] = return ()
          buildthem chan inprogress w =
              do let (canb',depb') = partition (canBuildNow (inprogress++w)) w
                     jobs = max 0 (4 - length inprogress)
                     canb = take jobs canb'
                     depb = drop jobs canb' ++ depb'
                     buildone (d:<-how) = forkC cms $
                                          do make how d
                                               `catchC`
                                               (io . writeChan chan . Left . show)
                                             io $ writeChan chan (Right (d:<-how))
                 case filter (endsWith ".o") $ concatMap buildName canb of
                   [] -> return ()
                   [_] -> return ()
                   tb -> putS $ "I can now build "++ unwords tb
                 mapM_ buildone canb
                 md <- io $ readChan chan
                 case md of
                   Left e -> fail $ "Failure building " ++ unwords (buildName b)
                                  ++"\n" ++ e
                   Right d -> buildthem chan (delB d (inprogress++canb)) depb
          delB done = filter (/= done)

showBuild :: Buildable -> String
showBuild (xs:<ds:<-_) = unwords (xs++ [":"]++nub (concatMap buildName ds))

instance Eq Buildable where
    Unknown x == Unknown y = x == y
    Unknown x == (ys:<_:<-_) = x `elem` ys
    (ys:<_:<-_) == Unknown x = x `elem` ys
    (xs:<_:<-_) == (ys:<_:<-_) = eqset xs ys
        where eqset [] [] = True
              eqset [] _ = False
              eqset _ [] = False
              eqset (x:xs) ys = x `elem` ys && xs `eqset` (delete x ys)

canBuildNow :: [Buildable] -> Buildable -> Bool
canBuildNow _ (Unknown _) = True
canBuildNow needwork (_:<d:<-_) = not $ any (`elem` needwork) d

findWork :: Buildable -> C [Buildable]
findWork (Unknown _) = return []
findWork zzz = fw [] [] $ mapBuildable id zzz
    where fw nw _ [] = return nw
          fw nw ok (Unknown _:r) = fw nw ok r
          fw nw ok (b@(xs:<ds:<-_):r) =
              if b `elem` (ok++nw)
              then do --putS $ "I already know about "++ unwords (buildName b)
                      fw nw ok r
              else do ineedwork <- case nw `intersect` ds of
                                   (z:_) -> do --putS $ "Must compile "++ unwords (buildName b) ++
                                               --             " because of " ++ unwords (buildName z)
                                               return True
                                   [] -> needsWork (xs:<ds)
                      if ineedwork then fw (b:nw) ok r
                                   else fw nw (b:ok) r

installBin :: Dependency -> C ()
installBin (xs:<_) = do pref <- getBinDir
                        let inst x = system "cp" [x,pref++"/"]
                        mapM_ inst xs

putS :: String -> C ()
putS = io . putStrLn

createFile :: String -> C ()
createFile fn = do x <- io $ readFile (fn++".in")
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
define x = ghcFlags ["-D"++x]
