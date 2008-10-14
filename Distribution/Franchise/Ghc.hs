{-# LANGUAGE CPP #-}
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

module Distribution.Franchise.Ghc
    ( executable, privateExecutable,
      -- Handy module-searching
      requireModule, lookForModule, checkLib, findPackagesFor,
      -- defining package properties
      package,
      (<:) ) where

import Control.Monad ( when )
import Data.Maybe ( catMaybes, listToMaybe )
import Data.List ( partition, (\\) )
import System.Directory ( createDirectoryIfMissing, copyFile )

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState

infix 2 <:
(<:) :: [String] -> [Buildable] -> Buildable
[x] <: y | endsWith ".o" x && any (any (endsWithOneOf [".hs",".lhs"]) . buildName) y
             = [x] :< y :<- defaultRule { make = ghc_hs_to_o }
[x] <: [y] | endsWith ".o" x && all (endsWith ".c") (buildName y)
               = [x] :< [y] :<- defaultRule { make = ghc_c }
[x] <: [y] | endsWith ".hi" x && endsWith ".o" yy &&
             drop 3 (reverse x) == drop 2 (reverse yy)
                 = [x] :< [y] :<- defaultRule -- hokey trick
                   where yy = concat $ buildName y
[stubo] <: [y] | endsWith "_stub.o" stubo = [stubo] :< [y] :<- defaultRule -- hokey!
xs <: ys = error $ "Can't figure out how to build "++ show xs++" from "++ show (map buildName ys)

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
  where builddeps _ = do x <- seekPackages (ghc systemErr $ ["-M"
#if __GLASGOW_HASKELL__ >= 610
                                                            ,"-dep-makefile"
#else
                                                            ,"-optdep-f"
#endif
                                                            ,"-optdep"++dname] ++ src)
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
       --putS $ "privateExecutable: "++exname
       --printBuildableDeep ([exname] :< (source src:mods++cobjs) |<- defaultRule)
       return $ [exname] :< (source src:mods++cobjs)
                  :<- defaultRule { make = mk, clean = \b -> depend : cleanIt b }

whenJust :: Maybe a -> (a -> C ()) -> C ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

directoryPart :: String -> Maybe String
directoryPart f = case reverse $ drop 1 $ dropWhile (/= '/') $ reverse f of
                  "" -> Nothing
                  d -> Just d

package :: String -> [String] -> C Buildable
package pn modules =
    do packageName pn
       let depend = pn++".depend"
       ghcDeps depend modules >>= build' CanModifyState
       mods <- parseDeps `fmap` io (readFile depend)
       pre <- getLibDir
       ver <- getVersion
       let config = [pn++".config"] :< [source depend] :<- defaultRule { make = makeconfig }
           destination = pre++"/"++pn++"-"++ver++"/"
           guessVersion = takeWhile (/='-') -- crude heuristic for dependencies
           appendExtra f d = do mv <- getExtraData d
                                case mv of
                                  Nothing -> return ()
                                  Just v -> io $ appendFile f $ d++": "++v++"\n"
           makeconfig _ =do lic <- getLicense
                            cop <- getCopyright
                            mai <- getMaintainer
                            cat <- getExtraData "category"
                            deps <- packages
                            io $ writeFile (pn++".config") $ unlines
                                          ["name: "++pn,
                                           "version: "++ver,
                                           "license: "++lic,
                                           "copyright: "++cop,
                                           "maintainer: "++mai,
                                           "import-dirs: "++ show destination,
                                           "library-dirs: "++ show destination,
                                           "exposed-modules: "++unwords modules,
                                           "hidden-modules: "++unwords (concatMap modName mods \\ modules),
                                           "hs-libraries: "++pn,
                                           "exposed: True",
                                           "depends: "++commaWords deps]
                            io $ writeFile (pn++".cabal") $ unlines
                                          ["name: "++pn,
                                           "version: "++ver,
                                           "license: "++lic,
                                           "copyright: "++cop,
                                           "maintainer: "++mai,
                                           "exposed-modules: "++unwords modules,
                                           "",
                                           "build-type: Custom",
                                           "build-depends: "++commaWords (map guessVersion deps)]
                            mapM_ (appendExtra (pn++".cabal"))
                                  ["category", "synopsis", "description"]
           installme _ = do io $ createDirectoryIfMissing True destination
                            let inst x =
                                    do case reverse $ dropWhile (/= '/') $ reverse x of
                                         "" -> return ()
                                         xdn -> io $ createDirectoryIfMissing True
                                                $ destination++"/"++xdn
                                       io $ copyFile x (destination++"/"++x)
                                his = filter (endsWith ".hi") $ concatMap buildName mods
                            mapM_ inst (("lib"++pn++".a") : his)
                            pkgflags <- getPkgFlags
                            system "ghc-pkg" $ pkgflags ++ ["update","--auto-ghci-libs",pn++".config"]
       --putS $ "LIBRARY DEPENDS:\n"
       --printBuildableDeep (["lib"++pn++".a"] :< (config:mods) |<- defaultRule)
       --putS "\n\n"
       return $ ["lib"++pn++".a"] :< (config:mods)
                  :<- defaultRule { make = objects_to_a,
                                    install = installme,
                                    clean = \b -> depend : cleanIt b}

commaWords :: [String] -> String
commaWords [] = ""
commaWords [x] = x
commaWords (x:xs) = x++", "++commaWords xs

modName :: Buildable -> [String]
modName (xs :< _ :<- _) = map toMod $ filter (endsWith ".o") xs
    where toMod x = map todots $ take (length x - 2) x
          todots '/' = '.'
          todots x = x
modName _ = error "bug in modName"

parseDeps :: String -> [Buildable]
parseDeps x = builds
    where builds = map makeBuild $ pd $ catMaybes $ map breakdep $ filter notcomm $ lines x
          notcomm ('#':_) = False
          notcomm _ = True
          breakdep (' ':':':' ':r) = Just ("",r)
          breakdep (z:y) = do (a,b) <- breakdep y
                              Just (z:a,b)
          breakdep [] = Nothing
          pd :: [(String,String)] -> [([String],[String])]
          pd [] = []
          pd ((z,y):r) | endsWith ".o" z =
                           ([z], y : map snd ys) :([take (length z-2) z++".hi"], [z]):pd r'
              where (ys,r') = partition ((==z).fst) r
          pd _ = error "bug in parseDeps pd"
          makeBuild :: ([String],[String]) -> Buildable
          makeBuild (xs,xds) = xs <: map (fb builds) xds
          fb _ z | endsWithOneOf [".c",".lhs",".hs"] z = source z
          fb [] z = error $ "Couldn't find build for "++ z
          fb ((xs:<xds:<-h):r) z | z `elem` xs = xs :< xds :<- h
                                 | otherwise = fb r z
          fb _ _ = error "bug in parseDeps fb"

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

objects_to_a :: Dependency -> C ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (endsWith ".o") (concatMap buildName ds))
objects_to_a _ = error "bug in objects_to_a"

tryModule :: String -> C String
tryModule m = do let fn = "Try"++m++".hs"
                 io $ writeFile fn $ unlines $ ["import "++m++" ()",
                                                "main:: IO ()",
                                                "main = undefined"]
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
    do checkMinimumPackages
       e <- tryLib l h func
       case e of
         "" -> putS $ "found library "++l++" without any extra flags."
         _ -> do ldFlags ["-l"++l]
                 e2 <- tryLib l h func
                 case e2 of
                   "" -> putS $ "found library "++l++" with -l"++l
                   _ -> do putV e2
                           fail $ "Couldn't find library "++l

requireModule :: String -> C ()
requireModule m = do haveit <- lookForModule m
                     when (not haveit) $ fail $ "Can't use module "++m

lookForModule :: String -> C Bool
lookForModule m = do x <- seekPackages $ tryModule m
                     case x of
                       [] -> putS $ "found module "++m
                       [_] -> putS $ "found module "++m++" in package "++unwords x
                       _ -> putS $ "found module "++m++" in packages "++unwords x
                     return True
                  `catchC` \e -> do putS e
                                    return False

checkMinimumPackages :: C ()
checkMinimumPackages =
    unlessC (haveExtraData "MINCONFIG") $
    do io $ writeFile "try-min.hs" $ "main :: IO ()\nmain = return ()\n"
       seekPackages (ghc systemErr ["-o","try-min","try-min.hs"])
       mapM_ rm ["try-min","try-min.hs","try-min.hi","try-min.o"]
       addExtraData "MINCONFIG" ""

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
