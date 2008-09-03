{- Copyright (c) 2008 David Roundy

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

module Distribution.Franchise ( build, executable, privateExecutable,
                                installBin,
                                replace, createFile, define, io, catchC,
                                -- The constructors are exported so users
                                -- can construct arbitrarily complex build
                                -- systems, hopefully.
                                Dependency(..), Buildable, (|<-), BuildRule(..),
                                -- Handy module-searching
                                requireModule, lookForModule,
                                checkLib, findPackagesFor,
                                -- defining package properties
                                package, copyright, license, version,
                                -- setting compile parameters
                                ghcFlags, ldFlags,
                                -- utility for running external code
                                systemOut,
                                -- useful for user-oriented messages.
                                putS,
                                -- simplification of getopt data types
                                flag,
                                -- semi-automatic rule generation
                                (<:), source, (.&) )
    where

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.Ghc
import Distribution.Franchise.ConfigureState
