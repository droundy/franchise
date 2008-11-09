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
      checkLib, withLib, checkHeader, withHeader, withLibOutput,
      requireModuleExporting, lookForModuleExporting, withModuleExporting,
      findPackagesFor,
      -- defining package properties
      package ) where

import Control.Monad ( when, filterM )
import System.Exit ( ExitCode(..) )
import Data.Maybe ( catMaybes, listToMaybe )
import Data.List ( partition, (\\), isSuffixOf )
import System.Directory ( createDirectoryIfMissing, copyFile )

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.ListUtils

infix 2 <:
(<:) :: [String] -> [String] -> Buildable
[x,h] <: y | isSuffixOf ".o" x && any (endsWithOneOf [".hs",".lhs"]) y
             = [x,h] :< (y++["config.d/ghcFlags","config.d/definitions"])
               :<- defaultRule { make = ghc_hs_to_o }
[x] <: [y] | ".o" `isSuffixOf` x && ".c" `isSuffixOf` y
               = [x] :< [y,"config.d/ghcFlags","config.d/definitions"] :<- defaultRule { make = ghc_c }
[stubo] <: [y] | isSuffixOf "_stub.o" stubo = [stubo] :< [y] :<- defaultRule -- hokey!
xs <: ys = error $ "can't figure out how to build "++ show xs++" from "++ show ys

executable :: String -> String -> [String] -> C String
executable exname src cfiles =
    do exname' <- privateExecutable exname src cfiles
       Just (x :< y :<- b) <- getBuildable exname'
       addTarget $ x :< y :<- b { install = installBin }
       return exname'

findPackagesFor :: String -> C ()
findPackagesFor src = do rm "temp.depend"
                         whenJust (directoryPart src) $ \d -> ghcFlags ["-i"++d,
                                                                        "-I"++d]
                         ghcDeps "temp.depend" [src] $ return ()
                         build' CanModifyState "temp.depend"
                         rm "temp.depend"

ghcDeps :: String -> [String] -> C () -> C ()
ghcDeps dname src announceme =
    do x <- io (readFile dname) `catchC` \_ -> return ""
       let cleandeps = filter (not . isSuffixOf ".hi") .
                       filter (not . isSuffixOf ".o") .
                       filter (/=":") . words . unlines .
                       filter notcomment . lines
           notcomment ('#':_) = False
           notcomment _ = True
       addTarget $ [dname] :< ("config.d/commandLine":cleandeps x)
                  :<- defaultRule { make = builddeps }
  where builddeps _ = do announceme
                         x <- seekPackages (ghc systemErr $ ["-M"
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

privateExecutable :: String -> String -> [String] -> C String
privateExecutable  simpleexname src cfiles0 =
    do checkMinimumPackages
       aminwin <- amInWindows
       exname <- if aminwin
                 then do putV $ "calling the executable "++simpleexname++" "++simpleexname++".exe"
                         return (simpleexname++".exe")
                 else return simpleexname
       whenJust (directoryPart src) $ \d -> ghcFlags ["-i"++d, "-I"++d]
       let depend = exname++".depend"
       ghcDeps depend [src] $ putV $ "finding dependencies of executable "++simpleexname
       build' CanModifyState depend
       (objs,_) <- io (readFile depend) >>= parseDeps []
       let mk _ = do ghc system (objs++ cobjs ++ extraobjs ++ ["-o",exname])
           (cfiles, extraobjs) = partition (".c" `isSuffixOf`) cfiles0
           cobjs = map (\f -> takeAllBut 2 f++".o") cfiles
       mapM_ addTarget $ zipWith (\c o -> [o] <: [c]) cfiles cobjs
       addTarget $ [exname, simpleexname] :< (src:objs++cobjs++extraobjs)
                  :<- defaultRule { make = mk, clean = \b -> depend : cleanIt b }
       return exname

whenJust :: Maybe a -> (a -> C ()) -> C ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

directoryPart :: String -> Maybe String
directoryPart f = case reverse $ drop 1 $ dropWhile (/= '/') $ reverse f of
                  "" -> Nothing
                  d -> Just d

package :: String -> [String] -> [String] -> C String
package pn modules cfiles =
    do packageName pn
       let depend = pn++".depend"
       ghcDeps depend modules $ putV $ "finding dependencies of package "++pn
       build' CanModifyState depend
       checkMinimumPackages -- ensure that we've got at least the prelude...
       (mods,his) <- io (readFile depend) >>= parseDeps [extraData "version"]
       pre <- getLibDir
       ver <- getVersion
       let destination = pre++"/"++pn++"-"++ver++"/"
           guessVersion = takeWhile (/='-') -- crude heuristic for dependencies
           appendExtra f d = do mv <- getExtraData d
                                case mv of
                                  Nothing -> return ()
                                  Just v -> io $ appendFile f $ d++": "++v++"\n"
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
                                           "hidden-modules: "++unwords (map objToModName mods \\ modules),
                                           "hs-libraries: "++pn,
                                           "exposed: True",
                                           "depends: "++commaWords deps]
           makecabal  _ =do lic <- getLicense
                            cop <- getCopyright
                            mai <- getMaintainer
                            deps <- packages
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
                            mapM_ inst (("lib"++pn++".a") : his)
                            pkgflags <- getPkgFlags
                            system "ghc-pkg" $ pkgflags ++ ["update","--auto-ghci-libs",pn++".config"]
       cobjs <- mapM (\f -> do let o = takeAllBut 2 f++".o"
                               addTarget $ [o] <: [f]
                               return o) cfiles
       addTarget $ [pn++".config"] :< [depend, extraData "version"]
                    :<- defaultRule { make = makeconfig }
       addTarget $ [pn++".cabal"] :< [depend, extraData "version"]
                    :<- defaultRule { make = makecabal }
       addTarget $ ["lib"++pn++".a"]
                  :< ((pn++".config"):mods++cobjs)
                  :<- defaultRule { make = objects_to_a,
                                    install = installme,
                                    clean = \b -> depend : cleanIt b}
       return $ "lib"++pn++".a"


objToModName :: String -> String
objToModName = map todots . takeAllBut 2
    where todots '/' = '.'
          todots x = x

takeAllBut :: Int -> [a] -> [a]
takeAllBut n xs = take (length xs - n) xs

commaWords :: [String] -> String
commaWords [] = ""
commaWords [x] = x
commaWords (x:xs) = x++", "++commaWords xs

parseDeps :: [String] -> String -> C ([String], [String])
parseDeps extradeps x = do mapM_ addTarget builds
                           let ohi ((o:h:_):<_:<-_) = (o,h)
                               ohi _ = error "bugggg"
                           return $ unzip $ map ohi builds
    where builds = pd $ catMaybes $ map breakdep $ filter notcomm $ lines x
          notcomm ('#':_) = False
          notcomm _ = True
          breakdep (' ':':':' ':r) = Just ("",r)
          breakdep (z:y) = do (a,b) <- breakdep y
                              Just (z:a,b)
          breakdep [] = Nothing
          pd :: [(String,String)] -> [Buildable]
          pd [] = []
          pd ((z,y):r) | isSuffixOf ".o" z =
                           ([z, takeAllBut 2 z++".hi"] <: y:map snd ys++extradeps) : pd r'
              where (ys,r') = partition ((==z).fst) r
          pd _ = error "bug in parseDeps pd"

ghc :: (String -> [String] -> C a) -> [String] -> C a
ghc sys args = do pn <- getPackageVersion
                  packs <- concatMap (\p -> ["-package",p]) `fmap` packages
                  fl <- getGhcFlags
                  defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v)) `fmap` getDefinitions
                  cf <- (map ("-optc"++) . (++defs)) `fmap` getCFlags
                  ld <- map ("-optl"++) `fmap` getLdFlags
                  let opts = fl ++ defs ++ (if "-c" `elem` args then [] else ld)
                                        ++ (if any (isSuffixOf ".c") args then cf else packs)
                  case pn of
                    Just p -> sys "ghc" $ opts++["-hide-all-packages","-package-name",p]++packs++args
                    Nothing -> sys "ghc" $ opts++"-hide-all-packages":packs++args

ghc_hs_to_o :: Dependency -> C ()
ghc_hs_to_o (_:<ds) = case filter (endsWithOneOf [".hs",".lhs"]) ds of
                      [d] -> ghc system ["-c",d]
                      [] -> fail "error 1"
                      _ -> fail "error 2"

ghc_c :: Dependency -> C ()
ghc_c (_:<ds) = case filter (isSuffixOf ".c") ds of
                [d] -> ghc system ["-c","-cpp",d]
                [] -> fail "error 4"
                _ -> fail "error 5"

objects_to_a :: Dependency -> C ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (isSuffixOf ".o") ds)
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
checkHeader h = do checkMinimumPackages
                   bracketC_ create remove test
    where create = do
            mkFile "try-header-ffi.h" $ unlines ["void foo();"]
            mkFile "try-header-ffi.c" $ unlines [if null h then "" 
                                                           else "#include \""++h++"\"",
                                                 "void foo();",
                                                 "void foo() { return; }"]
            mkFile "try-header.hs" $ unlines [foreign,
                                              "main :: IO ()",
                                              "main = foo"]
          foreign = "foreign import ccall unsafe "++
                    "\"try-header-ffi.h foo\" foo :: IO ()"
          test = do ghc systemV ["-c","-cpp","try-header-ffi.c"]
                    ghc systemV ["-fffi","-o","try-header",
                                 "try-header.hs","try-header-ffi.o"]
          remove = mapM_ rm ["try-header-ffi.h","try-header-ffi.o",
                             "try-header-ffi.c","try-header","try-header.hs"]

withHeader :: String -> C () -> C ()
withHeader h job = (checkHeader h >> job)
                   `catchC` \_ -> putS $ "failed to find header "++h

getLibOutput :: String -> String -> String -> C String
getLibOutput lib h code = do checkMinimumPackages
                             bracketC_ create remove test
    where create = do
            mkFile "get-const-ffi.h" $ unlines ["void foo();"]
            mkFile "get-const-ffi.c" $ unlines [if null h then ""
                                                          else "#include \""++h++"\"",
                                                "void foo();",
                                                "void foo() { "++code++"; }"]
            mkFile "get-const.hs" $ unlines [foreign,
                                             "main :: IO ()",
                                             "main = foo"]
          foreign = "foreign import ccall unsafe "++
                    "\"get-const-ffi.h foo\" foo :: IO ()"
          test = do ghc systemV ["-c","-cpp","get-const-ffi.c"]
                    csum [ghc systemV ["-fffi","-o","get-const",
                                       "get-const.hs","get-const-ffi.o"]
                         ,do ldFlags ["-l"++lib]
                             ghc systemV ["-fffi","-o","get-const",
                                          "get-const.hs","get-const-ffi.o"]]
                    systemOut "./get-const" []
          remove = mapM_ rm ["get-const-ffi.h","get-const-ffi.o",
                             "get-const-ffi.c","get-const","get-const.hs"]

withLibOutput :: String -> String -> String -> (String -> C ()) -> C ()
withLibOutput lib h code job = (getLibOutput lib h code >>= job)
                               `catchC`  \_ -> putS $ "failed to run "++code

tryLib :: String -> String -> String -> C ()
tryLib l h func = do checkMinimumPackages
                     bracketC_ create remove test
    where fn = "try-lib"++l++".c"
          fo = "try-lib"++l++".o"
          fh = "try-lib"++l++".h"
          hf = "try-lib.hs"
          create = do mkFile fh $ unlines ["void foo();"]
                      mkFile hf $ unlines ["foreign import ccall unsafe \""++
                                            fh++" foo\" foo :: IO ()",
                                            "main :: IO ()",
                                            "main = foo"]
                      mkFile fn $ unlines ["#include <stdio.h>",
                                           if null h then ""
                                                     else "#include \""++h++"\"",
                                           "void foo();",
                                           "void foo() { "++func++"; }"]
          test = do ghc systemV ["-c","-cpp",fn]
                    ghc systemV ["-fffi","-o","try-lib",fo,hf]
          remove = mapM_ rm [fh,fn,fo,"try-lib"++l++".hi",
                             "try-lib","try-lib.o",hf]

withLib :: String -> String -> String -> C () -> C ()
withLib l h func job = (checkLib l h func >> job)
                       `catchC` \_ -> putS $ "failed to find library "++l

checkLib :: String -> String -> String -> C ()
checkLib l h func =
    do checkMinimumPackages
       if null l
         then csum [do tryLib "std" h func
                       putS $ "found function "++func++" without any extra flags."
                   ,fail $ "couldn't find function "++func]
         else csum [do tryLib l h func
                       putS $ "found library "++l++" without any extra flags."
                   ,do ldFlags ["-l"++l]
                       tryLib l h func
                       putS $ "found library "++l++" with -l"++l
                   ,fail $ "couldn't find library "++l]

requireModule :: String -> C ()
requireModule m = do haveit <- lookForModule m
                     when (not haveit) $ fail $ "can't use module "++m

withModule :: String -> C () -> C ()
withModule m job = (requireModule m >> job)
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
                               fail $ "can't use module "++m

withModuleExporting :: String -> String -> String -> C () -> C ()
withModuleExporting m i c j =
    (requireModuleExporting m i c >> j)
    `catchC` \_ -> putS $ "failed to find an adequate module "++m

checkMinimumPackages :: C ()
checkMinimumPackages =
    unlessC (haveExtraData "MINCONFIG") $
    do mkFile "try-min.hs" $ "main :: IO ()\nmain = return ()\n"
       seekPackages (ghc systemErr ["-o","try-min","try-min.hs"])
       mapM_ rm ["try-min","try-min.hs","try-min.hi","try-min.o"]
       whenC amInWindows $ mapM_ rm ["try-min.exe", "try-min.exe.manifest"]
       addExtraData "MINCONFIG" ""

seekPackages :: C (ExitCode, String) -> C [String]
seekPackages runghcErr = runghcErr >>= lookForPackages
    where lookForPackages (ExitSuccess,_) = return []
          lookForPackages x@(_,e) =
              csum [case mungePackage e of
                      Nothing -> fail e
                      Just p -> do addPackages [p]
                                   x2 <- runghcErr
                                   if x2 == x
                                      then fail e
                                      else do ps2 <- lookForPackages x2
                                              return (p:ps2)
                   ,case mungeMissingModule e of
                      Nothing -> fail e
                      Just m -> seekPackageForModule m]
          seekPackageForModule m = do putV $ "looking for module "++m
                                      ps <- findPackagesProvidingModule m
                                      tryThesePackages m ps
          tryThesePackages m [] = fail $ "couldn't find package for module "++m++"!"
          tryThesePackages m [p] =
                        do putV $ "looking for module "++m++" in package "++p++"..."
                           addPackages [p]
                           x2 <- runghcErr
                           case x2 of
                             (ExitSuccess,_) -> return [p]
                             (z,e) ->
                                 case mungeMissingModule e of
                                   Just m' | m' /= m -> (p:) `fmap` lookForPackages (z,e)
                                   _ -> fail $ "couldn't use package "++p++" for module "++m++"!"
          tryThesePackages m (p:ps) =
                        do putV $ "looking for module "++m++" in package "++p++"..."
                           addPackages [p]
                           x2 <- runghcErr
                           case x2 of
                             (ExitSuccess,_) -> return [p]
                             (z,e) ->
                                 case mungeMissingModule e of
                                   Just m' | m' /= m -> csum [do ps' <- lookForPackages (z,e)
                                                                 return (p:ps')
                                                             ,tryagain]
                                   _ -> tryagain
                       where tryagain = do removePackages [p]
                                           tryThesePackages m ps

mungeMissingModule :: String -> Maybe String
mungeMissingModule [] = Nothing
mungeMissingModule x@(_:r) = csum [takeWhile (/='\'') `fmap` stripPrefix " `" x,
                                   mungeMissingModule r]

mungePackage :: String -> Maybe String
mungePackage [] = Nothing
mungePackage x@(_:r) = csum [mopt, mungePackage r]
   where mopt = do xxx <- stripPrefix "member of package " x
                   listToMaybe $ map (takeWhile (/=',')) $
                                 map (takeWhile (/=' ')) $ words xxx

findPackagesProvidingModule :: String -> C [String]
findPackagesProvidingModule m =
    csum [(reverse . words) `fmap` systemOut "ghc-pkg" ["find-module","--simple-output",m],
          do allps <- findAllPackages
             filterM (fmap (elem m) . packageModules) allps]

findAllPackages :: C [String]
findAllPackages = (reverse . words) `fmap` systemOut "ghc-pkg" ["list","--simple-output"]

packageModules :: String -> C [String]
packageModules p =
    do mms <- getModulesInPackage p
       case mms of
         Just ms -> return ms
         Nothing -> do ms <- (tail . words) `fmap` systemOut "ghc-pkg" ["field",p,"exposed-modules"]
                       addModulesForPackage p ms
                       return ms

--packageAndDeps :: String -> C [String]
--packageAndDeps p = ((p:) . tail . words) `fmap` systemOut "ghc-pkg" ["field",p,"depends"]
