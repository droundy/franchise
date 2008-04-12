#!/usr/bin/runhaskell

import Distribution.Franchise

distributionFranchiseO = ["Distribution/Franchise.o","Distribution/Franchise.hi"]
                         <: [source "Distribution/Franchise.hs"]

lib = ["libfranchise.a"] <: [distributionFranchiseO]

obj = ["franchise.o"] <: [lib]

main = build $ ["/home/droundy/lib/franchise/"] <:
               [lib,obj,source "franchise.cabal",distributionFranchiseO]
