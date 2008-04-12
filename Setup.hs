#!/usr/bin/runhaskell

import Distribution.Franchise

main = do copyright "David Roundy"
          license "BSD3"
          version "0.0"
          requireModule "System.Posix.Files"
          package "franchise" ["Distribution.Franchise"] >>= build
