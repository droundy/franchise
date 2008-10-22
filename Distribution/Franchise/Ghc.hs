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
      requireModule, lookForModule, withModule,
      checkLib, withLib, checkHeader,
      requireModuleExporting, lookForModuleExporting, withModuleExporting,
      findPackagesFor,
      -- defining package properties
      package ) where

import Control.Monad ( when )
import System.Exit ( ExitCode(..) )
import Data.Maybe ( catMaybes, listToMaybe )
import Data.List ( partition, (\\), isSuffixOf )
import System.Directory ( createDirectoryIfMissing, copyFile )

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState

infix 2 <:
(<:) :: [String] -> [Buildable] -> Buildable
[x] <: y | isSuffixOf ".o" x && any (any (endsWithOneOf [".hs",".lhs"]) . buildName) y
             = [x] :< y :<- defaultRule { make = ghc_hs_to_o }
[x] <: [y] | isSuffixOf ".o" x && all (isSuffixOf ".c") (buildName y)
               = [x] :< [y] :<- defaultRule { make = ghc_c }
[x] <: [y] | isSuffixOf ".hi" x && isSuffixOf ".o" yy &&
             drop 3 (reverse x) == drop 2 (reverse yy)
                 = [x] :< [y] :<- defaultRule -- hokey trick
                   where yy = concat $ buildName y
[stubo] <: [y] | isSuffixOf "_stub.o" stubo = [stubo] :< [y] :<- defaultRule -- hokey!
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
       let cleandeps = filter (not . isSuffixOf ".hi") .
                       filter (not . isSuffixOf ".o") .
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
-- not to be installed.  It's also used internally by executable.

privateExecutable :: String -> String -> [String] -> C Buildable
privateExecutable  simpleexname src cfiles =
    do putV $ "finding dependencies of executable "++simpleexname
       requestCleaningFor $
           do isch <- ghcFlagsChanged
              if isch then return $ Just $ endsWithOneOf [".o",".hi",".a"]
                      else return Nothing
       aminwin <- amInWindows
       exname <- if aminwin
                 then do putV $ "calling the executable "++simpleexname++" "++simpleexname++".exe"
                         return (simpleexname++".exe")
                 else return simpleexname
       whenJust (directoryPart src) $ \d -> ghcFlags ["-i"++d, "-I"++d]
       let depend = exname++".depend"
       ghcDeps depend [src] >>= build' CanModifyState
       mods <- parseDeps `fmap` io (readFile depend)
       let objs = filter (isSuffixOf ".o") $ concatMap buildName mods
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

package :: String -> [String] -> [String] -> C Buildable
package pn modules cfiles =
    do putV $ "finding dependencies of package "++pn
       requestCleaningFor $
           do isch <- versionChanged
              if isch then do putV "need to clean because the package version changed..."
                              return $ Just $ endsWithOneOf [".o",".hi",".a",".config",".cabal"]
                      else return Nothing
       requestCleaningFor $
           do isch <- ghcFlagsChanged
              if isch then do putV "need to clean because the ghc flags changed..."
                              return $ Just $ endsWithOneOf [".o",".hi",".a"]
                      else return Nothing
       packageName pn
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
           cobjs = map (\f -> [take (length f - 2) f++".o"] <: [source f]) cfiles
           makeconfig _ =do lic <- getLicense
                            cop <- getCopyright
                            mai <- getMaintainer
                            deps <- packages
                            mkFile (pn++".config") $ unlines
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
                            mkFile (pn++".cabal") $ unlines
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
                                his = filter (isSuffixOf ".hi") $ concatMap buildName mods
                            mapM_ inst (("lib"++pn++".a") : his)
                            pkgflags <- getPkgFlags
                            system "ghc-pkg" $ pkgflags ++ ["update","--auto-ghci-libs",pn++".config"]
       --putS $ "LIBRARY DEPENDS:\n"
       --printBuildableDeep (["lib"++pn++".a"] :< (config:mods) |<- defaultRule)
       --putS "\n\n"
       return $ ["lib"++pn++".a"] :< (config:mods++cobjs)
                  :<- defaultRule { make = objects_to_a,
                                    install = installme,
                                    clean = \b -> depend : cleanIt b}

commaWords :: [String] -> String
commaWords [] = ""
commaWords [x] = x
commaWords (x:xs) = x++", "++commaWords xs

modName :: Buildable -> [String]
modName (xs :< _ :<- _) = map toMod $ filter (isSuffixOf ".o") xs
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
          pd ((z,y):r) | isSuffixOf ".o" z =
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
                                ++ (if any (isSuffixOf ".c") args then cf else packs)
                  case pn of
                    Just p -> sys "ghc" $ opts++["-hide-all-packages","-package-name",p]++packs++args
                    Nothing -> sys "ghc" $ opts++"-hide-all-packages":packs++args

ghc_hs_to_o :: Dependency -> C ()
ghc_hs_to_o (_:<ds) = case filter (endsWithOneOf [".hs",".lhs"]) $ concatMap buildName ds of
                      [d] -> ghc system ["-c",d]
                      [] -> fail "error 1"
                      _ -> fail "error 2"

ghc_c :: Dependency -> C ()
ghc_c (_:<ds) = case filter (isSuffixOf ".c") $ concatMap buildName ds of
                [d] -> ghc system ["-c","-cpp",d]
                [] -> fail "error 4"
                _ -> fail "error 5"

objects_to_a :: Dependency -> C ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (isSuffixOf ".o") (concatMap buildName ds))
objects_to_a _ = error "bug in objects_to_a"

tryModule :: String -> String -> String -> C (ExitCode, String)
tryModule m imports code =
    do let fn = "Try"++m++".hs"
       mkFile fn $ unlines $ ["import "++m++" ("++imports++")",
                              "main:: IO ()",
                              "main = undefined ("++code++")"]
       e <- ghc systemErr ["-c",fn]
       mapM_ rm [fn,"Try"++m++".hi","Try"++m++".o"]
       return e

checkHeader :: String -> C ()
checkHeader h =
    do checkMinimumPackages
       tryHeader
    where tryHeader =
              do mkFile "try-header-ffi.h" $ unlines ["void foo();"]
                 mkFile "try-header-ffi.c" $ unlines ["#include \""++h++"\"",
                                                      "void foo();",
                                                      "void foo() { return; }"]
                 mkFile "try-header.hs" $ unlines
                        ["foreign import ccall unsafe \"try-header-ffi.h foo\" foo :: IO ()",
                         "main :: IO ()",
                         "main = foo"]
                 let rmfiles = mapM_ rm ["try-header-ffi.h", "try-header-ffi.o", "try-header-ffi.c",
                                         "try-header", "try-header.hs"]
                 do ghc systemV ["-c","-cpp","try-header-ffi.c"]
                    ghc systemV ["-fffi","-o","try-header",
                                 "try-header.hs","try-header-ffi.o"]
                  `catchC` \e -> rmfiles >> fail e
                 rmfiles

tryLib :: String -> String -> String -> C ()
tryLib l h func = do let fn = "try-lib"++l++".c"
                         fo = "try-lib"++l++".o"
                         fh = "try-lib"++l++".h"
                         hf = "try-lib.hs"
                     mkFile fh $ unlines ["void foo();"]
                     mkFile hf $ unlines ["foreign import ccall unsafe \""++
                                          fh++" foo\" foo :: IO ()",
                                          "main :: IO ()",
                                          "main = foo"]
                     mkFile fn $ unlines ["#include <stdio.h>",
                                          "#include \""++h++"\"",
                                          "void foo();",
                                          "void foo() {",
                                          "  "++func++";",
                                          "}"]
                     let rmfiles = mapM_ rm [fh,fn,fo,"try-lib"++l++".hi","try-lib","try-lib.o",hf]
                     do ghc systemV ["-c","-cpp",fn]
                        ghc systemV ["-fffi","-o","try-lib",fo,hf]
                       `catchC` (\e -> rmfiles >> fail e)
                     rmfiles

withLib :: String -> String -> String -> C () -> C ()
withLib l h func job = do checkLib l h func
                          job
                       `catchC` \_ -> putS $ "failed to find library "++l

checkLib :: String -> String -> String -> C ()
checkLib l h func =
    do checkMinimumPackages
       do tryLib l h func
          putS $ "found library "++l++" without any extra flags."
          `catchC` \_ ->
              do ldFlags ["-l"++l]
                 tryLib l h func
                 putS $ "found library "++l++" with -l"++l
                 `catchC` \_ -> fail $ "Couldn't find library "++l

requireModule :: String -> C ()
requireModule m = do haveit <- lookForModule m
                     when (not haveit) $ fail $ "Can't use module "++m

withModule :: String -> C () -> C ()
withModule m job = do requireModule m
                      job
                   `catchC` \_ -> putS $ "failed to find module "++m

lookForModule :: String -> C Bool
lookForModule m = lookForModuleExporting m "" ""

lookForModuleExporting :: String -> String -> String -> C Bool
lookForModuleExporting m i c =
    do x <- seekPackages $ tryModule m i c
       case x of
         [] -> putS $ "found module "++m
         [_] -> putS $ "found module "++m++" in package "++unwords x
         _ -> putS $ "found module "++m++" in packages "++unwords x
       return True
    `catchC` \e -> do putV e
                      return False

requireModuleExporting :: String -> String -> String -> C ()
requireModuleExporting m i c = unlessC (lookForModuleExporting m i c) $
                               fail $ "Can't use module "++m

withModuleExporting :: String -> String -> String -> C () -> C ()
withModuleExporting m i c j =
    do requireModuleExporting m i c
       j
    `catchC` \_ -> putS $ "failed to find an adequate module "++m

checkMinimumPackages :: C ()
checkMinimumPackages =
    unlessC (haveExtraData "MINCONFIG") $
    do mkFile "try-min.hs" $ "main :: IO ()\nmain = return ()\n"
       seekPackages (ghc systemErr ["-o","try-min","try-min.hs"])
       mapM_ rm ["try-min","try-min.hs","try-min.hi","try-min.o"]
       addExtraData "MINCONFIG" ""

seekPackages :: C (ExitCode, String) -> C [String]
seekPackages runghcErr = runghcErr >>= lookForPackages
    where lookForPackages (ExitSuccess,_) = return []
          lookForPackages x@(_,e) =
              case catMaybes $ map findOption $ lines e of
              [] -> fail e
              ps -> do addPackages ps
                       x2 <- runghcErr
                       if x2 == x
                          then fail e
                          else do ps2 <- lookForPackages x2
                                  return (ps++ps2)

findOption :: String -> Maybe String
findOption x | take (length foo) x == foo = listToMaybe $
                                            map (takeWhile (/=',')) $
                                            map (takeWhile (/=' ')) $
                                            words $ drop (length foo) x
             where foo = "member of package "
findOption (_:x) = findOption x
findOption [] = Nothing
