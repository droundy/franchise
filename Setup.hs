#!/usr/bin/runhaskell

import Distribution.Franchise

main = do copyright "David Roundy"
          license "BSD3"
          version "0.0"
          requireModule "Prelude"
          requireModule "System.Posix.Files"
          requireModule "System.Posix.Env ( setEnv )"
          p <- package "franchise" ["Distribution.Franchise"]
          e <- executable "sample-setup" "Setup.hs"
          --build (p .& e)
          build p
