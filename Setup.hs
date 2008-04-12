#!/usr/bin/runhaskell

import Distribution.Franchise

main = package "franchise" ["Distribution.Franchise"] >>= build
