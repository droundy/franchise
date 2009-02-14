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
      requireLib, lookForLib, withLib, checkHeader, withHeader, withLibOutput,
      requireModuleExporting, lookForModuleExporting, withModuleExporting,
      findPackagesFor,
      -- defining package properties
      package, cabal, installPackageInto ) where

import Control.Monad ( msum, when, filterM )
import System.Exit ( ExitCode(..) )
import Data.Maybe ( catMaybes, listToMaybe, isJust, isNothing )
import Data.List ( delete, partition, (\\), isSuffixOf, isPrefixOf, nub )
import System.Directory ( copyFile, doesFileExist )

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.GhcState ( getGhcFlags, getCFlags, getLdFlags, getPkgFlags,
                                         ghcFlags, ldFlags, setOutputDirectory,
                                         getPackageVersion, getMaintainer, getVersion,
                                         packageName, getLibDir,
                                         packages, addPackages, removePackages,
                                         getDefinitions, needDefinitions
                                       )
import Distribution.Franchise.ListUtils ( stripPrefix )
import Distribution.Franchise.StringSet ( toListS )
import Distribution.Franchise.Env ( setEnv, getEnv )
import Distribution.Franchise.Program ( withProgram )
import Distribution.Franchise.GhcPkg ( readPkgMappings, addToGhcPath )
import Distribution.Franchise.Trie ( Trie, lookupT, alterT )
import Distribution.Franchise.Persistency ( require, requireWithPrereq )

compile_C :: String -> String -> C Buildable
compile_C x y =
    do compileit <- ghc Nothing system ["-c","-cpp",y]
       return $ [x] :< [y,"config.d/ghcFlags","config.d/definitions"]
                :<- defaultRule { make = const compileit }

compile_hs :: Maybe String -> (String,String)
           -> String -> [String] -> C (Maybe Buildable)
compile_hs pn (x,h) y otherdeps =
    do compileit <- ghc pn system ["-c",y]
       ps <- packages
       ppp <- filterM (`objectIsInPackage` x) ps
       case ppp of
         [] -> return $ Just $ [x,h]
                  :< ([y,"config.d/ghcFlags","config.d/definitions"]++otherdeps)
                  :<- defaultRule { make = const compileit }
         _ -> return Nothing

maketixdir :: C ()
maketixdir = whenC (("-fhpc" `elem`) `fmap` getGhcFlags) $
             do -- create directory for coverage output
                tixdir <- (++"/tix") `fmap` pwd
                mkdir tixdir
                setEnv "HPCTIXDIR" tixdir
                addToRule "*clean*" (rm_rf tixdir)

executable :: String -> String -> [String] -> C [String]
executable exname src cfiles =
    do exname' <- privateExecutable exname src cfiles
       Just (x :< y :<- b) <- getBuildable exname
       addTarget $ x :< y :<- b { install = installBin }
       return exname'

findPackagesFor :: String -> C ()
findPackagesFor src = do rm "temp.depend"
                         whenJust (directoryPart src) $ \d -> ghcFlags ["-i"++d,
                                                                        "-I"++d]
                         ghcDeps "temp.depend" [src] $ return ()
                         build' CanModifyState "temp.depend"
                         rm "temp.depend"

ghcRelatedConfig :: [String]
ghcRelatedConfig = [extraData "packages", extraData "definitions"]

ghcDeps :: String -> [String] -> C () -> C ()
ghcDeps dname src announceme =
    do needDefinitions
       x <- io (readFile dname) `catchC` \_ -> return ""
       let cleandeps = filter (not . isSuffixOf ".hi") .
                       filter (not . isSuffixOf ".o") .
                       filter (/=":") . words . unlines .
                       filter notcomment . lines
           notcomment ('#':_) = False
           notcomment _ = True
       addTarget $ [dname] :< (ghcRelatedConfig++cleandeps x)
                  :<- defaultRule { make = builddeps }
  where builddeps _ = do announceme
                         rm dname
                         unlessC (io $ doesFileExist ".package.conf") $
                                 io $ writeFile ".package.conf" "[]"
                         x <- seekPackages (run $ ghc Nothing systemErr $
                                                    ["-M"
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

hiIsInPackage :: [String] -> String -> C (Maybe String)
hiIsInPackage pn hi =
    do mpm <- reallyGetModulePackageMap
       return $ case filter (`elem` pn) `fmap` lookupT (objToModName $ init hi) mpm of
                Just (p:_) -> Just p
                _ -> Nothing

objectIsInPackage :: String -> String -> C Bool
objectIsInPackage pn o =
    do mpm <- reallyGetModulePackageMap
       case lookupT (objToModName o) mpm of
         Nothing -> return False
         Just ps -> return (pn `elem` ps)

privateExecutable :: String -> String -> [String] -> C [String]
privateExecutable  simpleexname src0 cfiles0 =
    do maketixdir
       checkMinimumPackages
       aminwin <- amInWindows
       exname <- if aminwin
                 then do putV $ "calling the executable "++simpleexname++" "++simpleexname++".exe"
                         return (simpleexname++".exe")
                 else return simpleexname
       whenJust (directoryPart src0) $ \d -> ghcFlags ["-i"++d, "-I"++d]
       getHscs -- make sure to run hsc2hs on files we know about first...
       let depend = exname++".depend"
       src <- if ".hsc" `isSuffixOf` src0
              then do addHsc src0; return $ init src0
              else return src0
       ghcDeps depend [src] $ putV $ "finding dependencies of executable "++simpleexname
       build' CanModifyState depend
       pn <- getPackageVersion
       here <- pwd
       addToGhcPath (here++"/.package.conf")
       (objs0,_) <- io (readFile depend) >>=
                    parseDeps (catMaybes [pn]) Nothing [".package.conf"]
       objs <- case pn of
               Just n -> filterM (fmap not . objectIsInPackage n) objs0
               Nothing -> return objs0
       let mk _ = do stubos <- filterM (io . doesFileExist) $ map (stubit "o") objs
                     maybe (return ()) (addPackages . (:[])) pn
                     comp <- ghc Nothing system
                             (objs++ cobjs ++ extraobjs ++ stubos ++ ["-o",exname])
                     maybe (return ()) (removePackages . (:[])) pn
                     comp
           stubit c x = take (length x - 2) x ++ "_stub."++c
           (cfiles, extraobjs) = partition (".c" `isSuffixOf`) cfiles0
           cobjs = map (\f -> takeAllBut 2 f++".o") cfiles
           libname p = "lib"++ reverse (drop 1 $ dropWhile (/='-') $ reverse p)++".a"
       mapM_ (>>= addTarget) $ zipWith compile_C cobjs cfiles
       addTarget $ [exname, phony simpleexname]
                  :< (src:objs++cobjs++extraobjs++maybe [] (\x -> [libname x]) pn)
                  :<- defaultRule { make = mk, clean = \b -> depend : map (stubit "o") objs ++
                                                                      map (stubit "c") objs ++ cleanIt b }
       return [exname, phony simpleexname]

whenJust :: Maybe a -> (a -> C ()) -> C ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

directoryPart :: String -> Maybe String
directoryPart f = case reverse $ drop 1 $ dropWhile (/= '/') $ reverse f of
                  "" -> Nothing
                  d -> Just d

package :: String -> [String] -> [String] -> C [String]
package pn modules cfiles =
    do maketixdir
       checkMinimumPackages -- ensure that we've got at least the prelude...
       packageName pn
       xpn <- getPackageVersion
       getHscs -- to build any hsc files we need to.
       let depend = pn++"-package.depend"
       setOutputDirectory $ "dist/"++pn
       ghcDeps depend modules $ putV $ "finding dependencies of package "++pn
       build' CanModifyState depend
       (mods,his) <- io (readFile depend) >>= parseDeps [] xpn [extraData "version"]
       ver <- getVersion
       libs <- (catMaybes . map (stripPrefix "-l")) `fmap` getLdFlags
       libdirs <- (catMaybes . map (stripPrefix "-L")) `fmap` getLdFlags
       packageProvidesModules (maybe pn id xpn) (map objToModName mods)
       deps <- packages
       let hiddenmodules = map objToModName mods \\ modules
           makeconfig _ =do mai <- getMaintainer
                            mkFile (pn++".config") $ unlines
                                          ["name: "++pn,
                                           "version: "++ver,
                                           "maintainer: "++mai,
                                           "exposed-modules: "++unwords modules,
                                           "hidden-modules: "++unwords hiddenmodules,
                                           "hs-libraries: "++pn,
                                           "extra-libraries: "++unwords libs,
                                           "extra-lib-dirs: "++unwords libdirs,
                                           "exposed: True",
                                           "depends: "++commaWords deps]
       mhaddockdir <- getExtraData "haddock-directory"
       let haddockdir = maybe "haddock" id mhaddockdir
       (preprocsources, coloredfiles) <- preprocessedTargets his haddockdir
       addTarget $ [phony "haddock", haddockdir++"/index.html"] :<
                     (map (".preproc/"++) preprocsources ++ coloredfiles)
           :<- defaultRule { make = const $
                               do -- rm_rf haddockdir
                                  mkdir haddockdir
                                  mcss <- getExtraData "css"
                                  let cssflag = maybe [] (\f -> ["--css",f]) mcss
                                  cd ".preproc"
                                  sourceflags <- withProgram "HsColour" ["hscolour"] $ const $
                                                 return ["--source-module", "%F.html",
                                                         "--source-entity", "%F.html#%{NAME}"]
                                  system "haddock" ("-h":preprocsources++
                                                    concatMap (\hm -> ["--hide",hm]) hiddenmodules++
                                                    cssflag++sourceflags++["-o","../"++haddockdir]) }
       cobjs <- mapM (\f -> do let o = "dist/"++pn++"/"++takeAllBut 2 f++".o"
                               compile_C o f >>= addTarget
                               return o) cfiles
       addTarget $ [pn++".config"] :< [depend, extraData "version"]
                    :<- defaultRule { make = makeconfig }
       libdir <- getLibDir
       addTarget $ ["lib"++pn++".a", phony (pn++"-package")]
                  :< ((pn++".config"):mods++his++cobjs)
                  :<- defaultRule {
                      make = \_ ->
                         do stubos <- filterM (io . doesFileExist) $ map (stubit "o") mods
                            system "ar" ("cqs":("lib"++pn++".a"):mods++cobjs++stubos)
                            here <- pwd
                            setEnv "FRANCHISE_GHC_PACKAGE_CONF" (here++"/.package.conf")
                            installPackageInto pn (here++"/."++pn),
                      install = \_ -> Just $ installPackageInto pn libdir,
                      clean = \b -> ".package.conf" : (pn++".cfg") : depend
                              : map (stubit "o") mods ++
                                map (stubit "c") mods ++ cleanIt b}
       setOutputDirectory "."
       return [phony pn, "lib"++pn++".a"]
    where stubit c x = take (length x - 2) x ++ "_stub."++c

cabal :: String -> [String] -> C [String]
cabal pn modules =
    do checkMinimumPackages -- ensure that we've got at least the prelude...
       packageName pn
       whenC (io $ doesFileExist "LICENSE") $ addExtraData "license-file" "LICENSE"
       getHscs -- to build any hsc files we need to.
       let depend = pn++"-package.depend"
       setOutputDirectory $ "dist/"++pn
       ghcDeps depend modules $ putV $ "finding dependencies of package "++pn
       build' CanModifyState depend
       ver <- getVersion
       libs <- (catMaybes . map (stripPrefix "-l")) `fmap` getLdFlags
       deps <- packages
       let guessVersion = -- crude heuristic for dependencies
                          reverse . drop 1 . dropWhile (/='-') . reverse
           appendExtra f d = do mval <- getExtraData d
                                case mval of
                                  Nothing -> return ()
                                  Just v -> io $ appendFile f $ d++": "++v++"\n"
           makecabal  _ =do mai <- getMaintainer
                            mkFile (pn++".cabal") $ unlines
                                          ["name: "++pn,
                                           "version: "++ver,
                                           "maintainer: "++mai,
                                           "exposed-modules: "++unwords modules,
                                           "extra-libraries: "++unwords libs,
                                           "build-type: Custom",
                                           "build-depends: "++commaWords (map guessVersion deps)]
                            mapM_ (appendExtra (pn++".cabal"))
                                  ["author", "license", "copyright", "homepage", "bug-reports",
                                   "stability", "package-url", "tested-with", "license-file",
                                   "category", "synopsis", "description"]
       addTarget $ [pn++".cabal"] :< [depend, extraData "version"]
                    :<- defaultRule { make = makecabal }
       setOutputDirectory "."
       return [pn++".cabal"]

preprocessedTargets :: [String] -> FilePath -> C ([String],[String])
preprocessedTargets his haddockdir =
    do targets <- catMaybes `fmap` mapM getTarget his
       let sources = nub $ filter (\x -> ".hs" `isSuffixOf` x || ".lhs" `isSuffixOf` x) $
                     concatMap (toListS . dependencies) targets
           preproc s = do let preprocs = ".preproc/"++s
                          compileit <- ghc Nothing system
                                       ["-cpp","-E","-optP-P","-D__HADDOCK__",
                                        s, "-o", preprocs]
                          addTarget $ [preprocs] :< [s] :<-
                            defaultRule { make = const $
                                          do mkdir $ dirname preprocs
                                             compileit}
                          wd <- rememberDirectory
                          scolor <- withProgram "HsColour" ["hscolour"] $ \hscolour ->
                                    do addTarget $ [haddockdir++"/"++s++".html"] :< [s] :<-
                                                 defaultRule { make = const $ wd $
                                                               do mkdir $ dirname (haddockdir++"/"++s)
                                                                  ls $ dirname (haddockdir++"/"++s)
                                                                  system hscolour ["-html","-anchor",s,
                                                                                          "-o"++haddockdir++"/"++s++".html"] }
                                       return [haddockdir++"/"++s++".html"]
                          return (s,scolor)
       (pp,cf) <- unzip `fmap` mapM preproc sources
       return (pp, concat cf)

installPackageInto :: String -> String -> C ()
installPackageInto pn libdir =
    do ver <- getVersion
       let destination = libdir++"/"++pn++"-"++ver++"/"
       config <- cat (pn++".config")
       mkFile (pn++".cfg") $ unlines [config,
                                      "import-dirs: "++ show destination,
                                      "library-dirs: "++ show destination]
       mt <- getTarget ("lib"++pn++".a")
       case mt of
         Nothing -> fail $ "no such package:  "++pn
         Just (Target _ ds _) ->
             do mkdir destination
                let inst x = do putD $ "installing for package "++x
                                let x' = case drop 1 $ dropWhile (/='/') $
                                              drop 1 $ dropWhile (/='/') x of
                                           "" -> x
                                           xx -> xx
                                case dirname x' of
                                  "" -> return ()
                                  xdn -> mkdir $ destination++"/"++xdn
                                putD $ unwords ["copyFile", x, (destination++"/"++x')]
                                io $ copyFile x (destination++"/"++x')
                    his = filter (".hi" `isSuffixOf`) $ toListS ds
                mapM_ inst (("lib"++pn++".a") : his)
                pkgflags <- getPkgFlags
                mpf <- getEnv "FRANCHISE_GHC_PACKAGE_CONF"
                case mpf of
                  Nothing -> system "ghc-pkg" $ pkgflags ++ ["update","--auto-ghci-libs",pn++".cfg"]
                  Just pf -> system "ghc-pkg" $ filter (/="--user") pkgflags ++
                             ["update","--auto-ghci-libs",pn++".cfg", "--package-conf="++pf]

objToModName :: String -> String
objToModName = drop 1 . concatMap ('.':) . dropWhile isntCap . breakDirs . takeAllBut 2
    where isntCap (c:_) = c `notElem` ['A'..'Z']
          isntCap [] = True
          breakDirs xs = case break (`elem` "/\\") xs of
                           ("",'/':r) -> breakDirs r
                           ("",'\\':r) -> breakDirs r
                           ("","") -> []
                           ("",_) -> [xs]
                           (a,b) -> a : breakDirs (drop 1 b)

takeAllBut :: Int -> [a] -> [a]
takeAllBut n xs = take (length xs - n) xs

commaWords :: [String] -> String
commaWords [] = ""
commaWords [x] = x
commaWords (x:xs) = x++", "++commaWords xs

parseDeps :: [String] -> Maybe String -> [String] -> String -> C ([String], [String])
parseDeps ops pn extradeps x =
                        do builds <- catMaybes `fmap` sequence mkbuilds
                           mapM_ addTarget builds
                           let ohi ((o:h:_):<_:<-_) = (o,h)
                               ohi _ = error "bugggg"
                           return $ unzip $ map ohi builds
    where mkbuilds = pd $ catMaybes $ map breakdep $ filter notcomm $ lines x
          notcomm ('#':_) = False
          notcomm _ = True
          breakdep (' ':':':' ':r) = Just ("",r)
          breakdep (z:y) = do (a,b) <- breakdep y
                              Just (z:a,b)
          breakdep [] = Nothing
          pd :: [(String,String)] -> [C (Maybe Buildable)]
          pd [] = []
          pd ((z,y):r) | isSuffixOf ".o" z = mkzy : pd r'
              where (ys,r') = partition ((==z).fst) r
                    mkzy = do ys' <- filterM (fmap isNothing . hiIsInPackage ops) $
                                     map snd ys
                              ps <- (nub . catMaybes)
                                    `fmap` mapM (hiIsInPackage ops) (map snd ys)
                              alreadyKnown <- isJust `fmap` hiIsInPackage ops y
                              if alreadyKnown
                                 then return Nothing
                                 else do addPackages ps
                                         mk <- compile_hs pn (z, takeAllBut 2 z++".hi")
                                               y (map libname ps++ys'++extradeps)
                                         removePackages ps
                                         return mk
          pd _ = error "bug in parseDeps pd"
          libname p = "lib"++ reverse (drop 1 $ dropWhile (/='-') $ reverse p)++".a"

hsc2hs :: (String -> [String] -> C a) -> [String] -> C a
hsc2hs sys args = do fl <- filter ("-I" `isPrefixOf`) `fmap` getGhcFlags
                     defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v)) `fmap` getDefinitions
                     cf <- map ("--cflag="++) `fmap` getCFlags
                     ld <- map ("--lflag="++) `fmap` getLdFlags
                     let opts = fl ++ defs ++ ld ++ cf
                     sys "hsc2hs" $ "--cc=ghc":opts++args

ghc :: Maybe String -> (String -> [String] -> C a) -> [String] -> C (C a)
ghc = ghcWithoutFlags []

ghcWithoutFlags :: [String] -> Maybe String
                -> (String -> [String] -> C a) -> [String] -> C (C a)
ghcWithoutFlags nonflags pn sys args = do
  packs <- concatMap (\p -> ["-package",p]) `fmap` packages
  fl <- getGhcFlags
  defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v)) `fmap` getDefinitions
  cf <- (map ("-optc"++) . (++defs)) `fmap` getCFlags
  ld <- map ("-optl"++) `fmap` getLdFlags
  let opts = fl ++ defs ++ (if "-c" `elem` args then [] else ld)
                        ++ (if any (isSuffixOf ".c") args then cf else packs)
  case pn of
    Just p -> return $ sys "ghc" $ filter (`notElem` nonflags) $
              opts++["-hide-all-packages","-package-name",p,"-fforce-recomp"]++packs++args
    Nothing -> return $ sys "ghc" $ filter (`notElem` nonflags) $
               opts++"-hide-all-packages":"-fforce-recomp":packs++args

tryModule :: String -> String -> String -> C (ExitCode, String)
tryModule m imports code =
    do let fn = "Try"++m++".hs"
       mkFile fn $ unlines $ ["import "++m++" ("++imports++")",
                              "main:: IO ()",
                              "main = undefined ("++code++")"]
       e <- run $ ghcWithoutFlags ["-Werror"] Nothing systemErr ["-c",fn]
       mapM_ rm [fn,"Try"++m++".hi","Try"++m++".o"]
       return e

checkHeader :: String -> C ()
checkHeader h = require ("for header file "++h) $
                do checkMinimumPackages
                   bracketC_ create remove test
    where create = do
            mkFile "try-header-ffi.h" $ unlines ["void foo();"]
            mkFile "try-header-ffi.c" $ unlines [if null h then "" 
                                                           else "#include \""++h++"\"",
                                                 "void foo();",
                                                 "void foo() { return; }"]
            mkFile "try-header.hs" $ unlines [foreignf,
                                              "main :: IO ()",
                                              "main = foo"]
          foreignf = "foreign import ccall unsafe "++
                     "\"try-header-ffi.h foo\" foo :: IO ()"
          test = do run $ ghcWithoutFlags ["-Werror"] Nothing
                                    systemV ["-c","-cpp","try-header-ffi.c"]
                    run $ ghcWithoutFlags ["-Werror"] Nothing
                                    systemV ["-fffi","-o","try-header",
                                             "try-header.hs","try-header-ffi.o"]
          remove = mapM_ rm ["try-header-ffi.h","try-header-ffi.o",
                             "try-header-ffi.c","try-header","try-header.hs"]

withHeader :: String -> C () -> C ()
withHeader h job = (checkHeader h >> job)
                   `catchC` \_ -> return ()

getLibOutput :: String -> String -> String -> C String
getLibOutput lib h code = do checkMinimumPackages
                             bracketC_ create remove test
    where create = do
            mkFile "get-const-ffi.h" $ unlines ["void foo();"]
            mkFile "get-const-ffi.c" $ unlines [if null h then ""
                                                          else "#include \""++h++"\"",
                                                "void foo();",
                                                "void foo() { "++code++"; }"]
            mkFile "get-const.hs" $ unlines [foreignf,
                                             "main :: IO ()",
                                             "main = foo"]
          foreignf = "foreign import ccall unsafe "++
                     "\"get-const-ffi.h foo\" foo :: IO ()"
          test = do ghcWithoutFlags ["-Werror"] Nothing
                                    systemV ["-c","-cpp","get-const-ffi.c"]
                    csum [ghcWithoutFlags ["-Werror"] Nothing
                                          systemV ["-fffi","-o","get-const",
                                                   "get-const.hs","get-const-ffi.o"]
                         ,do ldFlags ["-l"++lib]
                             ghcWithoutFlags ["-Werror"] Nothing
                                             systemV ["-fffi","-o","get-const",
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
          test = do run $ ghcWithoutFlags ["-Werror"] Nothing
                        systemV ["-c","-cpp",fn]
                    run $ ghcWithoutFlags ["-Werror"] Nothing
                        systemV ["-fffi","-o","try-lib",fo,hf]
          remove = mapM_ rm [fh,fn,fo,"try-lib"++l++".hi",
                             "try-lib","try-lib.o",hf]

withLib :: String -> String -> String -> C () -> C ()
withLib l h func job = whenC (lookForLib l h func) $ job

lookForLib :: String -> String -> String -> C Bool
lookForLib l h func = (requireLib l h func >> return True)
                      `catchC` \_ -> return False

requireLib :: String -> String -> String -> C ()
requireLib l h func =
    requireWithPrereq ("for library "++l)
                      ("for library "++l++" exporting "++func++" using header "++h)
                      getLdFlags $
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
requireModule m = requireWithPrereq ("for module "++m) ("for module "++m) packages $
                  do haveit <- lookForModule m
                     when (not haveit) $ fail $ "can't use module "++m

withModule :: String -> C () -> C ()
withModule m job = (requireModule m >> job)
                   `catchC` \_ -> return ()

lookForModule :: String -> C Bool
lookForModule m = lookForModuleExporting m "" ""

lookForModuleExporting :: String -> String -> String -> C Bool
lookForModuleExporting m i c =
    do putSnoln $ "checking module "++m++" ... "
       x <- seekPackages $ tryModule m i c
       case x of
         [] -> putS $ "found"
         [_] -> putS $ "found in package "++unwords x
         _ -> putS $ "found in packages "++unwords x
       return True
    `catchC` \e -> do putV e
                      return False

requireModuleExporting :: String -> String -> String -> C ()
requireModuleExporting m i c = requireWithPrereq ("module "++m++" exporting "++i)
                                                 ("module "++m++" exporting "++i++" with code "++c)
                                                 packages $
                               unlessC (lookForModuleExporting m i c) $
                                   fail $ "can't use module "++m :: C ()

withModuleExporting :: String -> String -> String -> C () -> C ()
withModuleExporting m i c j =
    (requireModuleExporting m i c >> j)
    `catchC` \_ -> return ()

run :: C (C a) -> C a
run j = j >>= id

checkMinimumPackages :: C ()
checkMinimumPackages = require "the compiler works" $
    do mkFile "try-min.hs" $ "main :: IO ()\nmain = return ()\n"
       seekPackages (run $ ghcWithoutFlags ["-Werror"] Nothing
                     systemErr ["-o","try-min","try-min.hs"])
       mapM_ rm ["try-min","try-min.hs","try-min.hi","try-min.o"]
       whenC amInWindows $ mapM_ rm ["try-min.exe", "try-min.exe.manifest"]

seekPackages :: C (ExitCode, String) -> C [String]
seekPackages runghcErr = runghcErr >>= lookForPackages
    where lookForPackages (ExitSuccess,_) = return []
          lookForPackages x@(_,e) =
              csum [case mungeMissingModule e of
                      Nothing -> fail e
                      Just m -> do mrule <- findRuleForModule m
                                   case mrule of
                                     Just rulename -> do putV $ "found "++rulename
                                                         runghcErr >>= lookForPackages
                                     Nothing -> fail $ "we don't seem to have any source for "++m,
                    case mungePackage e of
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
mungeMissingModule x@(_:r) = msum [takeWhile (/='\'') `fmap` stripPrefix "load interface for `" x,
                                   takeWhile (/='\'') `fmap` stripPrefix "not find module `" x,
                                   mungeMissingModule r]

mungePackage :: String -> Maybe String
mungePackage [] = Nothing
mungePackage x@(_:r) = msum [mopt, mungePackage r]
   where mopt = do xxx <- stripPrefix "member of package " x
                   listToMaybe $
#if __GLASGOW_HASKELL__ >= 610
                                 filter ('-' `elem`) $
#endif
                                 map (takeWhile (/=',')) $
                                 map (takeWhile (/=' ')) $ words xxx

ghcPaths :: C [String]
ghcPaths = ((".":) . map (drop 2) . filter ("-i" `isPrefixOf`)) `fmap` getGhcFlags

findRuleForModule :: String -> C (Maybe String)
findRuleForModule m =
    do ps <- ghcPaths
       let hscname = '/':map topath m++".hsc"
           hsname = '/':map topath m++".hs"
           lhsname = '/':map topath m++".lhs"
           topath '.' = '/'
           topath x = x
           comb "." ('/':x) = x
           comb d x = d ++ x
       haverule <- filterM (fmap isJust . getTarget) $ concatMap (\p -> [comb p hsname,comb p lhsname]) ps
       x <- filterM (io . doesFileExist) $ map (++hscname) ps
       case haverule of
         (hs:_) -> do build' CanModifyState hs
                      return $ Just $ "rule for file "++hs
         [] ->case x of
                [] -> return Nothing
                (hsc:_) -> do addHsc hsc; return $ Just $ "hsc file "++hsc

addHsc :: String -> C ()
addHsc hsc = do hscs <- getHscs
                putExtra "hsc2hs" $ nub (hsc:hscs)
                let hscit = hsc2hs system [hsc]
                addTarget $ [init hsc] :< [hsc] :<- defaultRule { make = const hscit }
                hscit

getHscs :: C [String]
getHscs = do hscs <- getExtra "hsc2hs"
             mapM_ (\hsc -> addTarget $ [init hsc] :< [hsc]
                            :<- defaultRule { make = const $ hsc2hs system [hsc] }) hscs
             return hscs

packageProvidesModules :: String -> [String] -> C ()
packageProvidesModules p ms =
    do mpm <- reallyGetModulePackageMap
       setModulePackageMap $ addfoo ms mpm
    where addfoo [] x = x
          addfoo (y:ys) x = addfoo ys $ alterT y addp x
          addp (Just ps) = Just (p : delete p ps)
          addp Nothing = Just [p]

findPackagesProvidingModule :: String -> C [String]
findPackagesProvidingModule m = do mpm <- reallyGetModulePackageMap
                                   return $ maybe [] id $ lookupT m mpm

reallyGetModulePackageMap :: C (Trie [String])
reallyGetModulePackageMap = do mpm <- getModulePackageMap
                               case mpm of
                                 Just x -> return x
                                 Nothing -> do x <- readPkgMappings
                                               setModulePackageMap x
                                               return x
