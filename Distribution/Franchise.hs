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

module Distribution.Franchise ( C, FranchiseFlag, build, buildWithArgs,
                                buildTarget,
                                executable, privateExecutable,
                                installBin, addTarget,
                                replace, replaceLiteral, createFile,
                                define, defineAs, isDefined,
                                io, catchC, unlessC, whenC,
                                -- test suite helpers
                                test, testC, testOne, testOutput,
                                beginTestWith, prepareForTest,
                                addToRule,
                                -- The constructors are exported so users
                                -- can construct arbitrarily complex build
                                -- systems, hopefully.
                                Dependency(..), Buildable, (|<-), BuildRule(..),
                                defaultRule,
                                -- some handy utilities for writing checks
                                cacheifC, require, requireWithFeedback, requireWithPrereq, setOnce, checkOnce,
                                -- Handy module-searching
                                requireModule, lookForModule, withModule,
                                requireModuleExporting, lookForModuleExporting, withModuleExporting,
                                -- Searching for an executable
                                findProgram, withProgram,
                                configurableProgram, configuredProgram, withConfiguredProgram,
                                requireLib, lookForLib, withLib, checkHeader, withHeader,
                                findPackagesFor,
                                withLibOutput,
                                -- defining package properties
                                package, copyright, license, version,
                                addExtraData, haveExtraData, installPackageInto,
                                -- Some common platform tests
                                amInWindows, amLittleEndian,
                                -- Various utilities for interfacing with darcs
                                darcsDist,
                                -- generalized version control support
                                inDarcs, inGit,
                                ReleaseType(..),
                                autoVersion, autoPatchVersion,
                                releaseDescription, releaseName,
                                -- utilities for processing markdown files
                                splitMarkdown, markdownToHtml, markdownStringToHtmlString,
                                -- utilities for autoheader files
                                autoHeader,
                                -- setting compile parameters
                                ghcFlags, ldFlags, cFlags, pkgFlags,
                                rmGhcFlags,
                                -- utility for running external code
                                system, systemOut, systemInOut,
                                systemOutErrToFile,
                                cd, mkdir, pwd, ls, cat, rm_rf, mv,
                                mkFile,
                                withDirectory, rememberDirectory, splitPath,
                                -- string-processing...
                                stripPrefix,
                                -- environment-handling functions
                                setEnv, getEnv, addToPath,
                                -- useful for user-oriented messages.
                                putS, putV, putSV,
                                -- for handy preprocessing...
                                splitFile, mapDirectory,
                                -- simplification of getopt data types
                                flag, unlessFlag,
                                extraData )
    where

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.Ghc
import Distribution.Franchise.GhcState
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Endian
import Distribution.Franchise.ReleaseType ( ReleaseType(..) )
import Distribution.Franchise.VersionControl
import Distribution.Franchise.Darcs ( inDarcs, darcsDist )
import Distribution.Franchise.Git ( inGit )
import Distribution.Franchise.AutoHeader
import Distribution.Franchise.SplitFile
import Distribution.Franchise.Test
import Distribution.Franchise.Program
import Distribution.Franchise.Env
import Distribution.Franchise.ListUtils
import Distribution.Franchise.Markdown ( splitMarkdown, markdownToHtml,
                                         markdownStringToHtmlString )
import Distribution.Franchise.Replace ( replace, replaceLiteral, createFile )
import Distribution.Franchise.Flags ( FranchiseFlag, flag, unlessFlag )
import Distribution.Franchise.Persistency ( cacheifC, require, requireWithFeedback, requireWithPrereq,
                                            setOnce, checkOnce )
