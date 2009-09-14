{-# LANGUAGE CPP #-}
{- Copyright (c) 2009 David Roundy

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

module Distribution.Franchise.Jhc
    ( privateExecutable, package, cabal,
    --  findPackagesFor, installPackageInto,
    --  checkHeader, getLibOutput, tryLib,
      checkMinimumPackages,
      lookForModuleExporting
    ) where

import System.Exit ( ExitCode(..) )
import Data.List ( isPrefixOf )
import Data.Maybe ( catMaybes )
import System.Directory ( doesFileExist )

import Distribution.Franchise.ConfigureState
    ( C, io, catchC, whenC, (<<=), amInWindows,
      putExtra, getExtra, getExtraData,
      putS, putV, putSnoln )
import Distribution.Franchise.Buildable
    ( rule, addDependencies, phony, rm, extraData )
import Distribution.Franchise.GhcState
    ( packages, jhcFlags, addPackages, removePackages,
      getDefinitions, needDefinitions,
      getVersion, getPackageVersion, getMaintainer,
      getCFlags, getJhcFlags, getLdFlags )
import Distribution.Franchise.Util
    ( system, systemErr, systemOut , mkFile, nubs )
import Distribution.Franchise.ListUtils ( stripPrefix, commaWords )
import Distribution.Franchise.YAML
    ( readYAML, getMapping, getMappingValues, getScalar, getSequence )
import Distribution.Franchise.Trie ( toListT )

jhc :: (String -> [String] -> C a) -> [String] -> C (C a)
jhc sys args =
    do packs <- concatMap (\p -> ["-p",p]) `fmap` packages
       fl <- getJhcFlags
       defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v))
               `fmap` getDefinitions
       cf <- (map ("-optc"++) . (++defs)) `fmap` getCFlags
       ld <- getLdFlags
       return (sys "jhc" $ fl ++ defs ++ cf ++ ld ++ packs++args)

jhcRelatedConfig :: [String]
jhcRelatedConfig = [extraData "packages", extraData "definitions"]

-- | Build a Haskell executable, but do not install it when running
-- @.\/Setup.hs install@.

privateExecutable :: String -> String -> [String] -> C String
privateExecutable  _ _ (_:_) =
    fail "I don't yet know how to compile C source for jhc."
privateExecutable  simpleexname src [] =
    do aminwin <- amInWindows
       exname <- if aminwin
                 then do putV $ unwords $ "calling the executable":
                                          simpleexname:simpleexname:[".exe"]
                         return (simpleexname++".exe")
                 else return simpleexname
       whenJust (directoryPart src) $ \d -> jhcFlags ["-i"++d, "-I"++d]
       let targets = if exname == simpleexname
                     then [exname, depend]
                     else [exname, depend, phony simpleexname]
           depend = exname++".jhc-deps"
       needDefinitions
       hscs <- getHscs
       othersrc <- (lines `fmap` io (readFile depend))
                   `catchC` \_ -> return [src]
       buildit <- jhc system [src, "-o", exname]
       -- buildit <- jhc system [src, "--dependencies", depend, "-o", exname]
       rule (depend:targets)
            (nubs $ src:othersrc++map init hscs++jhcRelatedConfig) $ buildit
       addDependencies (phony "build") [exname]
       return exname

whenJust :: Maybe a -> (a -> C ()) -> C ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

directoryPart :: String -> Maybe String
directoryPart f = case reverse $ dropWhile (=='/') $
                       dropWhile (/= '/') $ reverse f of
                  "" -> Nothing
                  d -> Just d

package :: String -- ^ name of package to be generated
        -> [String] -- ^ list of modules to be exported
        -> [String] -- ^ list of C source files to be included
        -> C ()
package pn modules [] =
    do xpn <- maybe pn id `fmap` getPackageVersion
       ver <- takeWhile (`elem` ('.':['0'..'9']))  `fmap` getVersion
       let hiddenmodules = [] -- need tracking!
           depend = pn++".jhc-deps"
       src <- (lines `fmap` io (readFile depend))
              `catchC` \_ -> return []
       needDefinitions
       hscs <- getHscs
       buildit <- jhc system ["--dependencies", depend,
                              "--build-hl",pn++".config"]
       rule [xpn++".hl", phony (pn++"-package"), pn++".config"]
            (src++map init hscs++jhcRelatedConfig) $
            do mkFile (pn++".config") $ unlines
                  ["name: "++pn,
                   "version: "++ver,
                   "exposed-modules: "++unwords modules,
                   "hidden-modules: "++unwords hiddenmodules]
               buildit
       addDependencies (phony "build") [phony (pn++"-package"), "lib"++pn++".a"]
package _ _ _ = fail "Can't handle C files with jhc."

cabal :: String -> [String] -> C ()
cabal pn modules =
    do whenC (io $ doesFileExist "LICENSE") $ "license-file" <<= "LICENSE"
       getHscs -- to build any hsc files we need to.
       ver <- getVersion
       extralibs <- (catMaybes . map (stripPrefix "-l")) `fmap` getLdFlags
       deps <- packages
       let guessVersion = -- crude heuristic for dependencies
                          reverse . drop 1 . dropWhile (/='-') . reverse
           appendExtra f d = do mval <- getExtraData d
                                case mval of
                                  Nothing -> return ()
                                  Just v -> io $ appendFile f $ d++": "++v++"\n"
           makecabal = do mai <- getMaintainer
                          mkFile (pn++".cabal") $ unlines
                                 ["name: "++pn,
                                  "version: "++ver,
                                  "maintainer: "++mai,
                                  "exposed-modules: "++unwords modules,
                                  "extra-libraries: "++unwords extralibs,
                                  "build-type: Custom",
                                  "build-depends: "++
                                       commaWords (map guessVersion deps)]
                          mapM_ (appendExtra (pn++".cabal"))
                                ["author", "license", "copyright", "homepage",
                                 "bug-reports",
                                 "stability", "package-url", "tested-with",
                                 "license-file",
                                 "category", "synopsis", "description"]
       rule [pn++".cabal"] [extraData "version"] makecabal

checkMinimumPackages :: C ()
checkMinimumPackages = return ()

lookForModuleExporting :: String -> String -> String -> C Bool
lookForModuleExporting m i c =
    do putSnoln $ "checking module "++m++" ... "
       x <- tryModule m i c
       case x of
         (ExitSuccess, _) -> do putS "found"
                                return True
         (_, e) -> do putV e
                      ps <- seekModuleInLibraries m
                      if null ps
                        then do putS "no package found"
                                return False
                        else do putV $ unwords ("looking in packages":ps)
                                tryPackages ps
    where tryPackages [] = return False
          tryPackages (p:ps) =
              do addPackages [p]
                 putSnoln $ "looking in package "++p++" ... "
                 x <- tryModule m i c
                 case x of
                   (ExitSuccess, _) -> do putS "found"
                                          return True
                   (_, e) -> do putV e
                                removePackages [p]
                                tryPackages ps

seekModuleInLibraries :: String -> C [String]
seekModuleInLibraries m =
    do x <- jhc systemOut ["--list-libraries","-v"] >>= id
       case readYAML x >>= getMappingValues of
         Nothing -> fail "bad yaml?"
         Just vs -> return $ map name $ filter hasmod vs
             where name n = case getMapping "BaseName" n >>= getScalar of
                              Just nm -> nm
                              _ -> error "bad library has no name!!!"
                   hasmod n = case getMapping "Exported-Modules" n >>=
                                   getSequence of
                                Just ms ->
                                    m `elem` catMaybes (map getScalar ms)
                                _ -> False

tryModule :: String -> String -> String -> C (ExitCode, String)
tryModule m imports code =
    do let fn = "Try"++m++".hs"
       mkFile fn $ unlines $ ["module Try"++m++" where",
                              "import "++m++" ("++imports++")",
                              "foo :: ()",
                              "foo = undefined ("++code++")"]
       e <- jhc systemErr ["-c",fn] >>= id
       mapM_ rm [fn]
       return e

-- | 'addHsc' will be used when we add module tracking, so that we can
-- easily add rules to build source from hsc.

addHsc :: String -> C ()
addHsc hsc = do hscs <- getHscs
                putExtra "hsc2hs" $ nubs (hsc:hscs)
                let hscit = hsc2hs system [hsc]
                rule [init hsc] [hsc] hscit
                hscit

getHscs :: C [String]
getHscs = do hscs <- getExtra "hsc2hs"
             mapM_ (\hsc -> rule [dropdotslash $ init hsc] [dropdotslash hsc]
                                 $ hsc2hs system [hsc]) hscs
             return hscs
    where dropdotslash ('.':'/':r) = dropdotslash $ dropWhile (=='/') r
          dropdotslash x = x

hsc2hs :: (String -> [String] -> C a) -> [String] -> C a
hsc2hs sys args =
    do fl <- filter ("-I" `isPrefixOf`) `fmap` getJhcFlags
       defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v))
               `fmap` getDefinitions
       cf <- map ("--cflag="++) `fmap` getCFlags
       ld <- map ("--lflag="++) `fmap` getLdFlags
       hscopts <- words `fmap` (jhc systemOut ["--print-hsc-options"] >>= id)
       let opts = fl ++ defs ++ ld ++ cf
       sys "hsc2hs" (hscopts++opts++args)

