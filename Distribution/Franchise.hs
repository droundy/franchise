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

module Distribution.Franchise ( -- * The recommended API is defined in the module Distribution.Franchise_1.
                                module Distribution.Franchise.V1,
                                -- * The rest of the API defined below is either deprecated or experimental!
                                buildWithArgs,
                                buildTarget, privateExecutable,
                                installBin, addTarget,
                                csum,
                                -- | test suite helpers
                                testOutput,
                                testResultsFile,
                                addToRule,
                                -- | The constructors are exported so users
                                -- can construct arbitrarily complex build
                                -- systems, hopefully.
                                Dependency(..), Buildable, (|<-), BuildRule(..),
                                defaultRule,
                                -- | some handy utilities for writing checks
                                cacheifC, require, requireWithFeedback, requireWithPrereq, setOnce, checkOnce,
                                -- | Handy module-searching
                                lookForModule, lookForModuleExporting,
                                -- | Searching for an executable
                                lookForLib, checkHeader, withHeader,
                                findPackagesFor,
                                withLibOutput,
                                -- | defining package properties
                                haveExtraData, installPackageInto,
                                -- | Various utilities for interfacing with darcs
                                darcsDist,
                                -- | generalized version control support
                                inDarcs, inGit,
                                releaseDescription, releaseName,
                                -- | utilities for autoheader files
                                autoHeader,
                                -- | setting compile parameters
                                rmGhcFlags, setOutputDirectory,
                                -- | utility for running external code
                                systemInOut,
                                systemOutErrToFile,
                                mkFile,
                                rememberDirectory,
                                -- | string-processing...
                                stripPrefix,
                                -- | environment-handling functions
                                addToGhcPath,
                                -- | for handy preprocessing...
                                splitFile, mapDirectory,
                                extraData )
    where

import Distribution.Franchise.V1
import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.Ghc
import Distribution.Franchise.GhcPkg ( addToGhcPath )
import Distribution.Franchise.GhcState
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.VersionControl
import Distribution.Franchise.Darcs ( inDarcs, darcsDist )
import Distribution.Franchise.Git ( inGit )
import Distribution.Franchise.AutoHeader
import Distribution.Franchise.SplitFile
import Distribution.Franchise.Test
import Distribution.Franchise.Program
import Distribution.Franchise.ListUtils ( stripPrefix )
import Distribution.Franchise.Replace ( replace, replaceLiteral, createFile )
import Distribution.Franchise.Flags ( FranchiseFlag, flag, unlessFlag )
import Distribution.Franchise.Persistency ( cacheifC, require, requireWithFeedback, requireWithPrereq,
                                            setOnce, checkOnce )
