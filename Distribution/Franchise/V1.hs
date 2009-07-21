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

-- | The module "Distribution.Franchise.V1" is the version 1 API of
-- franchise, which will be supported for as long as franchise itself
-- is supported.
--
-- Note that although this haddock documentation is comprehensive, the
-- \'user-friendly\' manual is at <../index.html>.

module Distribution.Franchise.V1 ( -- ** Core stuff that almost everyone needs to use
                                   build, rule, addDependencies,
                                   executable, package, version,
                                   -- ** A handy configuration monad
                                   C, (<<=),
                                   io, catchC,
                                   whenC, unlessC,
                                   putS, putV, putSV,
                                   -- ** C preprocessor helpers
                                   -- | For a tutorial in the use of the
                                   -- C preprocessor, see
                                   -- <../02-cpp.html>.
                                   define, defineAs, isDefined,
                                   -- ** Generation of source files
                                   -- | For a tutorial on generating
                                   -- source files, see
                                   -- <../08-replace.html>.
                                   createFile, replace, replaceLiteral,
                                   -- ** Handy module searching
                                   -- | Note that you don't need to
                                   -- use these functions for most
                                   -- modules you use, since franchise
                                   -- will automatically require any
                                   -- modules that you actually use.
                                   -- It's only when you want to do
                                   -- something tricky that you're
                                   -- likely to need them.
                                   requireModule, withModule,
                                   requireModuleExporting, withModuleExporting,
                                   -- ** Searching for an executable
                                   findProgram, withProgram,
                                   configurableProgram, configuredProgram, withConfiguredProgram,
                                   -- ** Using C libraries
                                   requireLib, withLib,
                                   -- ** Creating cabal files
                                   cabal, copyright, license,
                                   -- ** Some common platform tests
                                   amInWindows, amLittleEndian,
                                   -- ** Generalized version control support
                                   ReleaseType(..),
                                   autoVersion, autoPatchVersion, autoDist,
                                   -- ** Setting compile parameters
                                   ghcFlags, ldFlags, cFlags, pkgFlags,
                                   -- ** Utilities for running executables
                                   system, systemOut,
                                   -- ** Utilities for processing markdown failes
                                   splitMarkdown, markdownToHtml, markdownStringToHtmlString,
                                   -- ** Filesystem-handling utilities
                                   -- | Franchise has a slew of
                                   -- filesystem-handling utilities,
                                   -- with the goal that your build
                                   -- rules can look a lot like
                                   -- makefile rules.  Using these
                                   -- utilities is greatly prefered,
                                   -- as the true 'current working
                                   -- directory' is not a useful
                                   -- concept when running parallel
                                   -- builds.
                                   cd, mkdir, pwd, ls, cat, rm_rf, mv,
                                   basename, dirname,
                                   withDirectory,
                                   -- ** Environment-handling functions
                                   setEnv, getEnv, addToPath,
                                   -- ** Simplification of getopt data types
                                   FranchiseFlag, flag, unlessFlag,
                                   -- ** Test suite helpers
                                   -- | For an example of the use of
                                   -- the test suite code, see
                                   -- <../07-test-suite.html>.
                                   test, testOne, testC,
                                   setupTestEnvironment,
                                   -- ** Enforce coding style
                                   enforceAllPrivacy, enforceModulePrivacy )
    where

import Distribution.Franchise.Util
import Distribution.Franchise.Buildable
import Distribution.Franchise.Ghc
import Distribution.Franchise.GhcState
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Endian ( amLittleEndian )
import Distribution.Franchise.ReleaseType ( ReleaseType(..) )
import Distribution.Franchise.VersionControl
import Distribution.Franchise.Program
import Distribution.Franchise.Env
import Distribution.Franchise.Replace ( replace, replaceLiteral, createFile )
import Distribution.Franchise.Flags ( FranchiseFlag, flag, unlessFlag )
import Distribution.Franchise.ModulePrivacy ( enforceAllPrivacy,
                                              enforceModulePrivacy )
import Distribution.Franchise.Test ( test, testOne, testC,
                                     setupTestEnvironment )
import Distribution.Franchise.Markdown ( splitMarkdown, markdownToHtml,
                                         markdownStringToHtmlString )
