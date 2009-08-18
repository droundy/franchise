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

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module Distribution.Franchise.VersionControl
    ( patchLevel, autoVersion, autoPatchVersion, autoDist,
      releaseDescription, releaseName )
    where

import Control.Monad ( when )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.Buildable ( simpleTarget, distclean )
import Distribution.Franchise.ReleaseType ( ReleaseType(..),
                                            releaseFile, releaseUnknown )
import Distribution.Franchise.Darcs ( inDarcs, darcsRelease,
                                      darcsPatchLevel, darcsDist )
import Distribution.Franchise.Git ( inGit, gitRelease, gitPatchLevel )
import Distribution.Franchise.GhcState ( version, getVersion )

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

-- | Return the version name that would be set by 'autoVersion'.
-- Useful if you want to use 'defineAs' to create the output of
-- --version.

releaseName :: ReleaseType -> C String
releaseName t = withRootdir $ inVC $
                VC (writeit darcsRelease) (writeit gitRelease)
                   readV (releaseUnknown t)
    where readV = do x:_ <- words `fmap` cat (releaseFile t)
                     return x
          writeit rel = do x <- rel t
                           distclean [releaseFile t]
                           writeF (releaseFile t) x
                           return x

patchLevel :: ReleaseType -> C Int
patchLevel t = withRootdir $ inVC $
               VC (writeit darcsPatchLevel) (writeit gitPatchLevel) readL (-1)
     where writeit rel = do x <- rel t
                            distclean [releaseFile t++"PatchLevel"]
                            writeF (releaseFile t++"PatchLevel") (show x)
                            return x
           readL = do [(i,"")] <- reads `fmap`
                                  cat (releaseFile t++"PatchLevel")
                      return i

releaseTargets :: C ()
releaseTargets = inVC $ VC inv inv inn ()
    where inv = do distclean [releaseFile Numbered,
                              releaseFile NumberedPreRc,
                              releaseFile AnyTag,
                              releaseFile Numbered++"PatchLevel",
                              releaseFile NumberedPreRc++"PatchLevel",
                              releaseFile AnyTag++"PatchLevel"]
                   simpleTarget (releaseFile Numbered) $ releaseName Numbered
                   simpleTarget (releaseFile NumberedPreRc) $
                                releaseName NumberedPreRc
                   simpleTarget (releaseFile AnyTag) $ releaseName AnyTag
                   simpleTarget (releaseFile Numbered++"PatchLevel") $
                                patchLevel Numbered
                   simpleTarget (releaseFile NumberedPreRc++"PatchLevel") $
                                patchLevel NumberedPreRc
                   simpleTarget (releaseFile AnyTag++"PatchLevel") $
                                patchLevel AnyTag
          inn = do nobrainer $ releaseFile Numbered
                   nobrainer $ releaseFile NumberedPreRc
                   nobrainer $ releaseFile AnyTag
                   nobrainer $ releaseFile Numbered ++ "PatchLevel"
                   nobrainer $ releaseFile NumberedPreRc ++ "PatchLevel"
                   nobrainer $ releaseFile AnyTag ++ "PatchLevel"
          nobrainer f = whenC (isFile f) $ simpleTarget f $ return ()

-- | Determine the version based on a reversion control system.
-- Currently only @git@ and @darcs@ are supported.  The 'ReleaseType'
-- argument determines how the version is determined.

autoVersion :: ReleaseType -> C String
autoVersion t = do releaseTargets
                   vers <- releaseName t
                   oldversion <- getVersion
                   when (oldversion /= vers) $
                        do version vers
                           putS $ "version is now "++vers
                   return vers

-- | This is like 'autoVersion', but the number of patches since the
-- latest release is then added to the version, so if there have been
-- 10 patches since the 1.0.0 release, this would give a version of
-- 1.0.0.10.

autoPatchVersion :: ReleaseType -> C String
autoPatchVersion t = do releaseTargets
                        r <- releaseName t
                        p <- patchLevel t
                        let vers = if p == 0 || p == -1
                                   then r
                                   else r++'.':show p
                        oldversion <- getVersion
                        when (oldversion /= vers) $
                             do version vers
                                putS $ "version is now "++vers
                        return vers

-- | Return a user-friendly name of the release, similar to the output
-- of 'autoPatchVersion', but with the number of patches since release
-- written in English instead of as a number.  Its use is similar to
-- 'releaseName'.

releaseDescription :: ReleaseType -> C String
releaseDescription t = do r <- releaseName t
                          l <- patchLevel t
                          return $ case l of
                                   0 -> r
                                   1 -> r++" + one patch"
                                   _ -> r++" + "++show l++" patches"

-- | Create a 'distribution tarball'.  This currently only works in
-- darcs repositories, but could be extended to also work under git or
-- other revision control systems.  The tarball directory is
-- originally created as a repository, then the specified targets are
-- built (e.g. this could be documentation that tarball users
-- shouldn't have to build), and then the \"distclean\" target is
-- built, which should remove anything that you don't want in the
-- tarball (and automatically includes the \"clean\" target).

autoDist :: String -- ^ base name of tarball (e.g. \"franchise\")
         -> [String] -- ^ list of targets to be built inside tarball
         -> C String -- ^ returns name of tarball.
autoDist dn tocopy =
    withRootdir $ inVC $ VC (darcsDist dn tocopy)
                            (fail "autoDist doesn't work in git.")
                            (fail "autoDist requires version control.")
                            (fail "autoDist requires darcs.")
