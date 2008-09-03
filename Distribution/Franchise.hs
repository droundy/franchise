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

module Distribution.Franchise ( build, executable, privateExecutable,
                                installBin,
                                -- The constructors are exported so users
                                -- can construct arbitrarily complex build
                                -- systems, hopefully.
                                Dependency(..), Buildable, (|<-), BuildRule(..),
                                -- Handy module-searching
                                requireModule, checkLib, findPackagesFor,
                                -- defining package properties
                                package, copyright, license, version,
                                -- setting compile parameters
                                ghcFlags,
                                -- utility for running external code
                                systemOut,
                                -- semi-automatic rule generation
                                (<:), source, (.&) )
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
import System.Console.GetOpt ( OptDescr(..) )

import Distribution.Franchise.Util

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

infix 2 <:
(<:) :: [String] -> [Buildable] -> Buildable
[x] <: y | endsWith ".a" x = [x] :< y :<- defaultRule { make = objects_to_a }
[x] <: y | endsWith ".o" x && all (all (endsWith ".a") . buildName) y
             = [x] :< y :<- defaultRule { make = a_to_o }
x <: y | all (endsWithOneOf [".o",".hi"]) x &&
         any (any (endsWithOneOf [".hs",".lhs"]) . buildName) y
             = x :< y :<- defaultRule { make = ghc_hs_to_o }
[x] <: [y] | endsWith ".o" x && all (endsWith ".c") (buildName y)
               = [x] :< [y] :<- defaultRule { make = ghc_c }
xs <: ys = error $ "Can't figure out how to build "++ show xs++" from "++ show (map buildName ys)

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

executable :: String -> String -> [String] -> C Buildable
executable exname src cfiles =
    do x :< y :<- b <- privateExecutable exname src cfiles
       return $ x :< y :<- b { install = installBin }

findPackagesFor :: String -> C ()
findPackagesFor src = do rm "temp.depend"
                         whenJust (directoryPart src) $ \d -> ghcFlags ["-i"++d,
                                                                        "-I"++d]
                         ghcDeps "temp.depend" [src] >>= build' CanModifyState
                         rm "temp.depend"

ghcDeps :: String -> [String] -> C Buildable
ghcDeps dname src =
    do x <- io (readFile dname) `catchC` \_ -> return ""
       let cleandeps = filter (not . endsWith ".hi") .
                       filter (not . endsWith ".o") .
                       filter (/=":") . words . unlines .
                       filter notcomment . lines
           notcomment ('#':_) = False
           notcomment _ = True
       return $ [dname] :< map source ("conf.state":cleandeps x) :<- defaultRule { make = builddeps }
  where builddeps _ = do x <- seekPackages (ghc systemErr $ ["-M","-optdep-f","-optdep"++dname] ++ src)
                         case x of
                           [] -> return ()
                           [_] -> putS $ "Added package "++ unwords x++"..."
                           _ -> putS $ "Added packages "++ unwords x++"..."

-- privateExecutable is used for executables used by the build system but
-- not to be installed.

privateExecutable :: String -> String -> [String] -> C Buildable
privateExecutable  exname src cfiles =
    do whenJust (directoryPart src) $ \d -> ghcFlags ["-i"++d, "-I"++d]
       let depend = exname++".depend"
       ghcDeps depend [src] >>= build' CanModifyState
       mods <- parseDeps `fmap` io (readFile depend)
       let objs = filter (endsWith ".o") $ concatMap buildName mods
           mk _ = do ghc system (objs++ concatMap buildName cobjs ++ ["-o",exname])
           cobjs = map (\f -> [take (length f - 2) f++".o"] <: [source f]) cfiles
       return $ [exname] :< (source src:mods++cobjs)
                  :<- defaultRule { make = mk, clean = \b -> depend : cleanIt b }

whenJust :: Maybe a -> (a -> C ()) -> C ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

directoryPart :: String -> Maybe String
directoryPart f = case reverse $ drop 1 $ dropWhile (/= '/') $ reverse f of
                  "" -> Nothing
                  d -> Just d

printBuildableDeep :: Buildable -> C ()
printBuildableDeep b@(xs :< ds:<-_) =
    do putS $ unwords xs
       putS $ showBuild b
       putS "Depends on:\n\n"
       let pbd i (x:<d:<-_) = do mapM_ (putS . (take i (repeat ' ')++)) x
                                 mapM_ (pbd (i+1)) d
           pbd i (Unknown x) = putS $ take i (repeat ' ')++"Source:"++x
       mapM_ (pbd 0) ds

package :: String -> [String] -> C Buildable
package pn modules =
    do packageName pn
       let depend = pn++".depend"
       ghcDeps depend modules >>= build' CanModifyState
       mods <- parseDeps `fmap` io (readFile depend)
       pre <- getLibDir
       ver <- getVersion
       let lib = ["lib"++pn++".a"] <: mods
           obj = [pn++".o"] <: [lib]
           cabal = [pn++".cabal"] :< [source depend] :<- defaultRule { make = makecabal }
           destination = pre++"/"++pn++"-"++ver++"/"
           makecabal _ = do lic <- getLicense
                            cop <- getCopyright
                            mai <- getMaintainer
                            deps <- packages
                            io $ writeFile (pn++".cabal") $ unlines
                                          ["name: "++pn,
                                           "version: "++ver,
                                           "license: "++lic,
                                           "copyright: "++cop,
                                           "maintainer: "++mai,
                                           "import-dirs: "++ destination,
                                           "library-dirs: "++ destination,
                                           "exposed-modules: "++unwords modules,
                                           "hidden-modules: "++unwords (concatMap modName mods \\ modules),
                                           "hs-libraries: "++pn,
                                           "exposed: True",
                                           "depends: "++commaWords deps]
           installme _ = do system "mkdir" ["-p",destination]
                            let inst x = system "cp" ["--parents",x,destination]
                                his = filter (endsWith ".hi") $ concatMap buildName mods
                            mapM_ inst ["lib"++pn++".a",pn++".o"]
                            mapM_ inst his
                            pkgflags <- getPkgFlags
                            system "ghc-pkg" $ pkgflags ++ ["update",pn++".cabal"]
       return $ [destination] :< (lib:obj:cabal:mods)
                  :<- defaultRule { install = installme,
                                    clean = \b -> depend : cleanIt b}

commaWords :: [String] -> String
commaWords [] = ""
commaWords [x] = x
commaWords (x:xs) = x++", "++commaWords xs

rm :: String -> C ()
rm f | endsWith "/" f = return ()
rm f = io (removeFile f) `catchC` \_ -> return ()

modName :: Buildable -> [String]
modName (xs :< _ :<- _) = map toMod $ filter (endsWith ".o") xs
    where toMod x = map todots $ take (length x - 2) x
          todots '/' = '.'
          todots x = x

parseDeps :: String -> [Buildable]
parseDeps x = builds
    where builds = map makeBuild $ pd $ catMaybes $ map breakdep $ filter notcomm $ lines x
          notcomm ('#':_) = False
          notcomm x = True
          breakdep (' ':':':' ':r) = Just ("",r)
          breakdep (x:y) = do (a,b) <- breakdep y
                              Just (x:a,b)
          breakdep [] = Nothing
          pd :: [(String,String)] -> [([String],[String])]
          pd [] = []
          pd ((x,y):r) | endsWith ".o" x =
                           ([x,take (length x-2) x++".hi"], y : map snd ys) : pd r'
              where (ys,r') = partition ((==x).fst) r
          makeBuild :: ([String],[String]) -> Buildable
          makeBuild (xs,xds) = xs <: map (fb builds) xds
          fb _ x | endsWithOneOf [".lhs",".hs"] x = source x
          fb [] x = error $ "Couldn't find build for "++ x
          fb ((xs:<xds:<-h):r) x | x `elem` xs = xs :< xds :<- h
                                 | otherwise = fb r x

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
                                               `catchC` \_ -> io (writeChan chan Nothing)
                                             io $ writeChan chan (Just (d:<-how))
                 case filter (endsWith ".o") $ concatMap buildName canb of
                   [] -> return ()
                   [_] -> return ()
                   tb -> putS $ "I can now build "++ unwords tb
                 mapM_ buildone canb
                 md <- io $ readChan chan
                 case md of
                   Nothing -> fail "Ooops..."
                   Just d -> buildthem chan (delB d (inprogress++canb)) depb
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

data Foo a = Foo Int a deriving (Eq)
instance Eq a => Ord (Foo a) where
  compare (Foo a _) (Foo b _) = compare a b
unfoo (Foo _ x) = x

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

ghc :: (String -> [String] -> C a) -> [String] -> C a
ghc sys args = do pn <- getPackageVersion
                  packs <- concatMap (\p -> ["-package",p]) `fmap` packages
                  fl <- getGhcFlags
                  cf <- map ("-optc"++) `fmap` getCFlags
                  ld <- map ("-optl"++) `fmap` getLdFlags
                  let opts = fl ++ (if "-c" `elem` args then [] else ld)
                                ++ (if any (endsWith ".c") args then cf else packs)
                  case pn of
                    Just p -> sys "ghc" $ opts++["-hide-all-packages","-package-name",p]++args
                    Nothing -> sys "ghc" $ opts++"-hide-all-packages":packs++args

ghc_hs_to_o :: Dependency -> C ()
ghc_hs_to_o (_:<ds) = case filter (endsWithOneOf [".hs",".lhs"]) $ concatMap buildName ds of
                      [d] -> ghc system ["-c",d]
                      [] -> fail "error 1"
                      _ -> fail "error 2"

ghc_c :: Dependency -> C ()
ghc_c (_:<ds) = case filter (endsWith ".c") $ concatMap buildName ds of
                [d] -> ghc system ["-c","-cpp",d]
                [] -> fail "error 4"
                _ -> fail "error 5"

installBin :: Dependency -> C ()
installBin (xs:<_) = do pref <- getBinDir
                        let inst x = system "cp" [x,pref++"/"]
                        mapM_ inst xs

objects_to_a :: Dependency -> C ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (endsWith ".o") (concatMap buildName ds))

a_to_o :: Dependency -> C ()
a_to_o ([outname]:<ds) = system "ld" ("-r":"--whole-archive":"-o":outname:
                                   filter (endsWith ".a") (concatMap buildName ds))
tryModule :: String -> C String
tryModule m = do let fn = "Try"++m++".hs"
                 io $ writeFile fn ("import "++m++"\nmain:: IO ()\nmain = undefined\n")
                 e <- ghc systemErr ["-c",fn]
                 mapM_ rm [fn,"Try"++m++".hi","Try"++m++".o"]
                 return e

tryLib :: String -> String -> String -> C String
tryLib l h func = do let fn = "try-lib"++l++".c"
                         fo = "try-lib"++l++".o"
                         fh = "try-lib"++l++".h"
                         hf = "try-lib.hs"
                     io $ writeFile fh $ unlines ["void foo();"]
                     io $ writeFile hf $ unlines ["foreign import ccall unsafe \""++
                                             fh++" foo\" foo :: IO ()",
                                             "main :: IO ()",
                                             "main = foo"]
                     io $ writeFile fn $ unlines ["#include <stdio.h>",
                                             "#include \""++h++"\"",
                                             "void foo();",
                                             "void foo() {",
                                             "  "++func++";",
                                             "}"]
                     e1 <- ghc systemErr ["-c","-cpp",fn]
                     e2 <- ghc systemErr ["-fffi","-o","try-lib",fo,hf]
                     mapM_ rm [fh,fn,fo,"try-lib"++l++".hi","try-lib","try-lib.o",hf]
                     return (e1++e2)

checkLib :: String -> String -> String -> C ()
checkLib l h func =
    do e <- tryLib l h func
       case e of
         "" -> putS $ "found library "++l++" without any extra flags."
         _ -> do ldFlags ["-l"++l]
                 e2 <- tryLib l h func
                 case e2 of
                   "" -> putS $ "found library "++l++" with -l"++l
                   _ -> do putS e2
                           fail $ "Couldn't find library "++l

requireModule :: String -> C ()
requireModule m = do x <- seekPackages $ tryModule m
                     case x of
                       [] -> putS $ "found module "++m
                       [_] -> putS $ "found module "++m++" in package "++unwords x
                       _ -> putS $ "found module "++m++" in packages "++unwords x
                  `catchC` \e -> do putS e
                                    fail $ "Can't use module "++m

seekPackages :: C String -> C [String]
seekPackages runghcErr = runghcErr >>= lookForPackages
    where lookForPackages "" = return []
          lookForPackages e =
              case catMaybes $ map findOption $ lines e of
              [] -> fail e
              ps -> do addPackages ps
                       e2 <- runghcErr
                       if e2 == e
                          then fail e
                          else do x <- lookForPackages e2
                                  return (ps++x)

findOption :: String -> Maybe String
findOption x | take (length foo) x == foo = listToMaybe $
                                            map (takeWhile (/=',')) $
                                            map (takeWhile (/=' ')) $
                                            words $ drop (length foo) x
             where foo = "member of package "
findOption (_:x) = findOption x
findOption [] = Nothing

putS :: String -> C ()
putS = io . putStrLn
