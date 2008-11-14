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

module Distribution.Franchise.VersionControl
    ( getRelease, patchLevel, autoVersion, autoPatchVersion, releaseDescription )
    where

import Control.Monad ( when )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.ReleaseType ( ReleaseType(..),
                                            releaseName, releaseUnknown )
import Distribution.Franchise.Darcs ( inDarcs, darcsRelease, darcsPatchLevel )
import Distribution.Franchise.Git ( inGit, gitRelease, gitPatchLevel )

data VC a = VC { darcs :: C a,
                 git :: C a,
                 none :: C a,
                 panic :: a }

inVC :: VC a -> C a
inVC j = do ind <- inDarcs
            if ind then darcs j `catchC` \_ -> donone
                   else do ing <- inGit
                           if ing then git j `catchC` \_ -> donone
                                  else donone
    where donone = none j `catchC` \_ -> return $ panic j

getRelease :: ReleaseType -> C String
getRelease t = withRootdir $
               do x <- inVC $ VC (darcsRelease t) (gitRelease t) readV (releaseUnknown t)
                  when (x /= releaseUnknown t) $
                       writeF (releaseName t) x `catchC` \_ -> return ()
                  return x
    where readV = do x:_ <- words `fmap` cat (releaseName t)
                     return x

patchLevel :: ReleaseType -> C Int
patchLevel t = withRootdir $
               do level <- inVC $ VC (darcsPatchLevel t) (gitPatchLevel t) readL (-1)
                  writeF dotfile (show level) `catchC` \_ -> return ()
                  return level
     where dotfile = releaseName t++"PatchLevel"
           readL = do [(i,"")] <- reads `fmap` cat dotfile
                      return i

autoVersion :: ReleaseType -> C ()
autoVersion t = do vers <- getRelease t
                   oldversion <- getVersion
                   when (oldversion /= vers) $
                        do version vers
                           putS $ "version is now "++vers

autoPatchVersion :: ReleaseType -> C ()
autoPatchVersion t = do r <- getRelease t
                        p <- patchLevel t
                        let vers = if p == 0 || p == -1
                                   then r
                                   else r++'.':show p
                        oldversion <- getVersion
                        when (oldversion /= vers) $
                             do version vers
                                putS $ "version is now "++vers

releaseDescription :: ReleaseType -> C String
releaseDescription t = do r <- getRelease t
                          l <- patchLevel t
                          return $ case l of
                                   0 -> r
                                   1 -> r++" + one patch"
                                   _ -> r++" + "++show l++" patches"
