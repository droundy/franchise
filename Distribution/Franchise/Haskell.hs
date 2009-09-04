{-# LANGUAGE CPP #-}
{- Copyright (c) 2008-2009 David Roundy

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

module Distribution.Franchise.Haskell
    ( executable, privateExecutable,
      -- Handy module-searching
      requireModule, withModule,
      requireLib, lookForLib, withLib, checkHeader, withHeader, withLibOutput,
      requireModuleExporting, withModuleExporting,
      findPackagesFor,
      -- defining package properties
      package, cabal, installPackageInto ) where

import Control.Monad ( when )
import Data.List ( isSuffixOf, isPrefixOf )
import System.Directory ( doesFileExist )
import Data.Monoid ( Monoid, mempty )

import qualified Distribution.Franchise.Ghc as Ghc
    ( package, cabal, privateExecutable,
      findPackagesFor, installPackageInto,
      checkHeader, getLibOutput, tryLib,
      checkMinimumPackages, lookForModuleExporting )
import qualified Distribution.Franchise.Jhc as Jhc
    ( privateExecutable, package, cabal,
      checkMinimumPackages, lookForModuleExporting )
import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.GhcState
    ( getGhcFlags, getJhcFlags, getCFlags, getLdFlags, ldFlags,
      packageName, packages, getDefinitions )
import Distribution.Franchise.Env ( setEnv )
import Distribution.Franchise.Persistency ( requireWithPrereq )

data HC a = HC { ghc :: C a,
                 jhc :: C a }

hc :: String -> HC a
hc op = HC { ghc = fail (op++": operation not supported for ghc."),
             jhc = fail (op++": operation not supported for jhc.") }

withHc :: HC a -> C a
withHc hC = do x <- getExtraData "hc"
               case x of
                 Just "jhc" -> jhc hC
                 Just "ghc" -> ghc hC
                 Just c -> fail $ "Misunderstood compiler: "++c
                 Nothing -> ghc hC

maketixdir :: C ()
maketixdir = whenC (("-fhpc" `elem`) `fmap` getGhcFlags) $
             do -- create directory for coverage output
                tixdir <- (++"/tix") `fmap` pwd
                mkdir tixdir
                setEnv "HPCTIXDIR" tixdir
                clean [tixdir]

-- | The 'executable' function creates a haskell executable target.
--
-- For a tutorial in the use of 'executables', see
-- <../01-simple-executable.html>.

executable :: String -- ^ name of executable to be generated
           -> String -- ^ name of main file
           -> [String] -- ^ list of C source files to be included
           -> C ()
executable exname src cfiles =
    do exname' <- privateExecutable exname src cfiles
       bin exname' -- install the binary

findPackagesFor :: String -> C ()
findPackagesFor modul = withHc $ (hc "findPackagesFor") {
                          ghc = Ghc.findPackagesFor modul }

-- | Build a Haskell executable, but do not install it when running
-- @.\/Setup.hs install@.

privateExecutable :: String -> String -> [String] -> C String
privateExecutable  simpleexname src0 cfiles =
    do maketixdir
       checkMinimumPackages
       src <- if ".hsc" `isSuffixOf` src0
              then do addHsc src0; return $ init src0
              else return src0
       build' CanModifyState src
       withHc $ (hc "privateExecutable") {
          ghc = Ghc.privateExecutable simpleexname src cfiles,
          jhc = Jhc.privateExecutable simpleexname src cfiles
       }

-- | Inform franchise that you wish to build a haskell package
-- (i.e. library).  Some of the less important package properties,
-- such as synopsis or description, must first be defined using the
-- '<<=' operator.
--
-- Note that the 'package' function also introduces a \"haddock\"
-- target providing documentation of the exposed API.  You may control
-- where this documentation is built by defining \"haddock-directory\"
-- with '<<='.
--
-- @
--    \"haddock-directory\" <<= \"doc\/manual\/haddock\"
-- @

package :: String -- ^ name of package to be generated
        -> [String] -- ^ list of modules to be exported
        -> [String] -- ^ list of C source files to be included
        -> C ()
package pn modules cfiles =
    do maketixdir
       checkMinimumPackages -- ensure that we've got at least the prelude...
       packageName pn
       withHc $ (hc "package") { ghc = Ghc.package pn modules cfiles,
                                 jhc = Jhc.package pn modules cfiles }

-- | Generate a cabal file describing a package.  The arguments are
-- the same as those to 'package'.  The package properties which are
-- least important in 'package' (such as synopsis or description) are
-- important parts of the cabal file, since its contents are normally
-- exposed to the user, and should be defined using the '<<='
-- operator.

cabal :: String -> [String] -> C ()
cabal pn modules =
    do checkMinimumPackages -- ensure that we've got at least the prelude...
       packageName pn
       whenC (io $ doesFileExist "LICENSE") $ "license-file" <<= "LICENSE"
       getHscs -- to build any hsc files we need to.
       withHc $ (hc "cabal") { ghc = Ghc.cabal pn modules,
                               jhc = Jhc.cabal pn modules }

-- | Install the specified package into a given ghc config file.  This
-- is essentially only useful for test suites, when you want to test
-- a package by installing it, but without /really/ installing it.

installPackageInto :: String -> String -> C ()
installPackageInto pn libdir =
    withHc $ (hc "cabal") { ghc = Ghc.installPackageInto pn libdir }

getHcFlags :: C [String]
getHcFlags = withHc $ (hc "getHcFlags") { ghc = getGhcFlags,
                                          jhc = getJhcFlags }

hsc2hs :: (String -> [String] -> C a) -> [String] -> C a
hsc2hs sys args =
    do fl <- filter ("-I" `isPrefixOf`) `fmap` getHcFlags
       defs <- map (\(k,v)->"-D"++k++(if null v then "" else "="++v))
               `fmap` getDefinitions
       cf <- map ("--cflag="++) `fmap` getCFlags
       ld <- map ("--lflag="++) `fmap` getLdFlags
       let opts = fl ++ defs ++ ld ++ cf
       sys "hsc2hs" $ "--cc=ghc":opts++args

checkHeader :: String -> C ()
checkHeader h = withHc $ (hc "checkHeader") { ghc = Ghc.checkHeader h }

withHeader :: String -> C () -> C ()
withHeader h job = (checkHeader h >> job)
                   `catchC` \_ -> return ()

getLibOutput :: String -> String -> String -> C String
getLibOutput lib h code =
    withHc $ (hc "getLibOutput") { ghc = Ghc.getLibOutput lib h code }

withLibOutput :: Monoid a => String -> String -> String
              -> (String -> C a) -> C a
withLibOutput lib h code job = (getLibOutput lib h code >>= job)
                               `catchC`  \_ -> do putS $ "failed to run "++code
                                                  return mempty

tryLib :: String -> String -> String -> C ()
tryLib lib h code = withHc $ (hc "tryLib") { ghc = Ghc.tryLib lib h code }

-- | If specified library is available, do something.
withLib :: Monoid a => String -- ^ library name (i.e. what goes in -lfoo)
        -> String -- ^ header file to use
        -> String -- ^ check for the presence of this function
        -> C a -- ^ job to do
        -> C a
withLib l h func job = whenC (lookForLib l h func) job

lookForLib :: String -> String -> String -> C Bool
lookForLib l h func = (requireLib l h func >> return True)
                      `catchC` \_ -> return False

-- | Fail if specified library isn't available.
requireLib :: String -- ^ library name (i.e. what goes in -lfoo)
           -> String -- ^ header file to use
           -> String -- ^ check for the presence of this function
           -> C ()
requireLib l h func =
    requireWithPrereq ("for library "++l)
                      (unlines ["for library",l,"exporting",
                                func,"using header",h])
                      getLdFlags $
    do checkMinimumPackages
       if null l
         then csum [do tryLib "std" h func
                       putS $ "found function "++func++
                                " without any extra flags."
                   ,fail $ "couldn't find function "++func]
         else csum [do tryLib l h func
                       putS $ "found library "++l++" without any extra flags."
                   ,do ldFlags ["-l"++l]
                       tryLib l h func
                       putS $ "found library "++l++" with -l"++l
                   ,fail $ "couldn't find library "++l]

-- | Fail if the specified module isn't available.

requireModule :: String -> C ()
requireModule m = requireWithPrereq ("for module "++m)
                                    ("for module "++m) packages $
                  do haveit <- lookForModule m
                     when (not haveit) $ fail $ "can't use module "++m

-- | If the specified module is available, do something.

withModule :: Monoid a => String -> C a -> C a
withModule m job = (requireModule m >> job)
                   `catchC` \_ -> return mempty

lookForModule :: String -> C Bool
lookForModule m = lookForModuleExporting m "" ""

lookForModuleExporting :: String -> String -> String -> C Bool
lookForModuleExporting m i c =
    withHc $ (hc "lookForModuleExporting") {
                 ghc = Ghc.lookForModuleExporting m i c,
                 jhc = Jhc.lookForModuleExporting m i c }

-- | Fail if there isn't available a module that provides the
-- specified exports.  Ideally, the \'code\' argument should use all
-- of the functions you're checking to see exported.  See also
-- <../99-requireExporting.html>

requireModuleExporting
    :: String -- ^ module name, e.g. System.Exit
    -> String -- ^ export list, e.g. ExitCode, exitWith
    -> String -- ^ code using the exports, e.g. exitWith :: ExitCode -> IO ()
    -> C ()
requireModuleExporting m i c =
    requireWithPrereq ("module "++m++" exporting "++i)
                      ("module "++m++" exporting "++i++" with code "++c)
                      packages $
                      unlessC (lookForModuleExporting m i c) $
                               fail $ "can't use module "++m :: C ()

-- | If the specified module is available (and exports the right
-- stuff), do something.

withModuleExporting :: Monoid a =>
       String -- ^ module name, e.g. System.Exit
    -> String -- ^ export list, e.g. ExitCode, exitWith
    -> String -- ^ code using the exports, e.g. exitWith :: ExitCode -> IO ()
    -> C a -- ^ action to do if the module is available
    -> C a
withModuleExporting m i c j =
    (requireModuleExporting m i c >> j)
    `catchC` \_ -> return mempty

checkMinimumPackages :: C ()
checkMinimumPackages =
    withHc $ (hc "checkMinimumPackages") { ghc = Ghc.checkMinimumPackages,
                                           jhc = Jhc.checkMinimumPackages }

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
