{- Copyright (c) 2008 Austin Seipp

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

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module Distribution.Franchise.Git ( inGit, gitPatchLevel, gitRelease ) where

import System.Directory ( doesDirectoryExist )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.ReleaseType

inGit :: C Bool
inGit = io $ doesDirectoryExist ".git"

gitPatchLevel :: ReleaseType -> C Int
gitPatchLevel t = do ver <- gitRelease t
                     (length . lines) `fmap` systemOut "git" ["log",ver++"..","--pretty=oneline"]

gitTags :: C [String]
gitTags = (reverse . lines) `fmap` systemOut "git" ["tag"]

gitRelease :: ReleaseType -> C String
gitRelease t = do tags <- gitTags
                  case filter (releasePredicate t) tags of
                    [] -> fail "no such git tag!"
                    (v:_) -> return v
