#!/usr/bin/runhaskell

import Distribution.Franchise

main = build $ package "franchise" ["Distribution.Franchise"]
