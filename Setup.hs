#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do copyright "David Roundy"
               license "BSD3"
               addExtraData "category" "Distribution"
               addExtraData "synopsis"
                   "A package for configuring and building Haskell software"
               addExtraData "description" $ unlines
                   ["",
                    "        Franchise is an easy-to use package for building Haskell",
                    "        software.  Unlike Cabal, you aren't required to track every",
                    "        possible dependency in every possible build condition.  In",
                    "        addition, you are not required to use an external tool such as",
                    "        autoconf in order to configure the build based on which",
                    "        packages, libraries and tools are present.",
                    "",
                    "        Note: the cabal dependencies are autogenerated, and approximate."]
               ghcFlags ["-threaded","-O2","-Wall"]

main = build [] configure $ do -- versionFromDarcs doesn't go in configure
                               -- because we want to rerun it with each
                               -- build rather than waiting for the user to
                               -- run Setup.hs configure again.
                               versionFromDarcs
                               package "franchise" ["Distribution.Franchise"] []

