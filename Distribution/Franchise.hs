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

module Distribution.Franchise ( build, buildWithArgs, nothing,
                                executable, privateExecutable,
                                installBin,
                                replace, replaceStr, createFile,
                                define, defineAs, isDefined,
                                io, catchC, unlessC, whenC,
                                -- The constructors are exported so users
                                -- can construct arbitrarily complex build
                                -- systems, hopefully.
                                Dependency(..), Buildable, (|<-), BuildRule(..),
                                defaultRule, build', CanModifyState(..),
                                -- Handy module-searching
                                requireModule, lookForModule, withModule,
                                requireModuleExporting, lookForModuleExporting, withModuleExporting,
                                -- Searching for an executable
                                findAnExecutable,
                                checkLib, withLib, checkHeader, withHeader,
                                findPackagesFor,
                                getConstant, withConstant,
                                -- defining package properties
                                package, copyright, license, version,
                                addExtraData, haveExtraData,
                                -- Some common platform tests
                                amInWindows, amLittleEndian,
                                -- Various utilities for interfacing with darcs
                                inDarcs, darcsDist, dist,
                                darcsPatchLevel, versionFromDarcs, patchVersionFromDarcs,
                                -- utilities for interfacing with git
                                inGit, gitPatchLevel, getTag,
                                -- setting compile parameters
                                ghcFlags, ldFlags, cFlags,
                                rmGhcFlags,
                                -- utility for running external code
                                system, systemOut,
                                cd, pwd, ls, cat,
                                -- useful for user-oriented messages.
                                putS, putV, putSV,
                                -- for handy preprocessing...
                                splitFile, mapDirectory,
                                -- simplification of getopt data types
                                flag, unlessFlag,
                                configureFlag, configureUnlessFlag,
                                -- combining buildables
                                source, extraData, combineBuildables )
    where

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.Ghc
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Endian
import Distribution.Franchise.Darcs
import Distribution.Franchise.Git
import Distribution.Franchise.SplitFile
