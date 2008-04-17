#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do copyright "David Roundy"
               license "BSD3"
               findPackagesFor "Distribution.Franchise"
               addEnv "GHC_FLAGS" "-threaded"
               version "0.0"

buildable = do p <- package "franchise" ["Distribution.Franchise"]
               e <- privateExecutable "sample-setup" "Setup.hs" []
               --return (p .& e)
               return p

main = build configure buildable

