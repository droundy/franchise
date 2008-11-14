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

module Distribution.Franchise.ReleaseType
    ( ReleaseType(..), releaseRegexp, releaseName, releasePredicate ) where

data ReleaseType = Numbered | NumberedPreRc | AnyTag

releaseRegexp :: ReleaseType -> String
releaseRegexp Numbered = "^[0-9\\.]+$"
releaseRegexp NumberedPreRc = "^[0-9\\.]+-?(rc[0-9]*|pre[0-9]*)?$"
releaseRegexp AnyTag = "."

releaseName :: ReleaseType -> String
releaseName Numbered = ".releaseVersion"
releaseName NumberedPreRc = ".latestRelease"
releaseName AnyTag = ".lastTag"

releasePredicate :: ReleaseType -> String -> Bool
releasePredicate Numbered = all (`elem` numbersDot)
releasePredicate NumberedPreRc = isprerc . dropWhile (`elem` numbersDot)
    where isprerc "" = True
          isprerc ('-':r) = isprerc r
          isprerc ('p':'r':'e':'-':x) = all (`elem` ['0'..'9']) x
          isprerc ('p':'r':'e':x) = all (`elem` ['0'..'9']) x
          isprerc ('r':'c':'-':x) = all (`elem` ['0'..'9']) x
          isprerc ('r':'c':x) = all (`elem` ['0'..'9']) x
          isprerc _ = False
releasePredicate AnyTag = const True

numbersDot :: String
numbersDot = '.':['0'..'9']
