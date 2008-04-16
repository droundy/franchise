#!/usr/bin/runhaskell
import Distribution.Franchise

configure = do copyright "David Roundy"
               license "BSD3"
               version "0.0"
               addEnv "GHC_FLAGS" "-threaded"
               requireModule "System.Posix.Env ( setEnv )"

buildable = do p <- package "franchise" ["Distribution.Franchise"]
               e <- privateExecutable "sample-setup" "Setup.hs" []
               return (p .& e)

main = build configure buildable

