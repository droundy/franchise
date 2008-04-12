#!/usr/bin/runhaskell

import Distribution.Franchise

distributionFranchiseO = DependsOn ["Distribution/Franchise.o","Distribution/Franchise.hi"]
                         [source "Distribution/Franchise.hs"] ghc_hs_to_o

lib = DependsOn ["libfranchise.a"] [distributionFranchiseO] objects_to_a

obj = DependsOn ["franchise.o"] [lib] a_to_o

main = build $ DependsOn ["/home/droundy/lib/franchise"]
                         [lib,obj,source "franchise.cabal",distributionFranchiseO] install
