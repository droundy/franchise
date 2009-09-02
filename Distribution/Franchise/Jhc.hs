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
    ( privateExecutable, package,
    --  cabal,
    --  findPackagesFor, installPackageInto,
    --  checkHeader, getLibOutput, tryLib,
      checkMinimumPackages,
    --  lookForModuleExporting
    ) where

import Distribution.Franchise.ConfigureState ( C, amInWindows, putV )
import Distribution.Franchise.Buildable
    ( rule, addDependencies, phony )
import Distribution.Franchise.GhcState ( packages, jhcFlags, getDefinitions,
                                         getVersion, getPackageVersion,
                                         getCFlags, getJhcFlags, getLdFlags )
import Distribution.Franchise.Util ( system, mkFile )

jhc :: (String -> [String] -> C a) -> [String] -> C (C a)
jhc sys args =
    do packs <- concatMap (\p -> ["-p",p]) `fmap` packages
       fl <- getJhcFlags
       defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v))
               `fmap` getDefinitions
       cf <- (map ("-optc"++) . (++defs)) `fmap` getCFlags
       ld <- getLdFlags
       return (sys "jhc" $ fl ++ defs ++ cf ++ ld ++ packs++args)

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
                     then [exname]
                     else [exname, phony simpleexname]
       buildit <- jhc system [src, "-o", exname]
       rule targets [src] $ buildit
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
       buildit <- jhc system ["--build-hl",pn++".config","-o",xpn++".hl"]
       rule [xpn++".hl", phony (pn++"-package"), pn++".config"] [] $
            do mkFile (pn++".config") $ unlines
                  ["name: "++pn,
                   "version: "++ver,
                   "exposed-modules: "++unwords modules,
                   "hidden-modules: "++unwords hiddenmodules]
               buildit
       addDependencies (phony "build") [phony (pn++"-package"), "lib"++pn++".a"]
package _ _ _ = fail "Can't handle C files with jhc."

checkMinimumPackages :: C ()
checkMinimumPackages = return ()
