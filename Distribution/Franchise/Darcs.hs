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
module Distribution.Franchise.Darcs
    ( inDarcs, darcsDist, darcsRelease, darcsPatchLevel )
        where

import System.Directory ( doesDirectoryExist )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.GhcState ( getVersion )
import Distribution.Franchise.ReleaseType ( ReleaseType(..), releaseRegexp )
import Distribution.Franchise.Permissions ( setExecutable )

inDarcs :: C Bool
inDarcs = io $ doesDirectoryExist "_darcs"

darcsPatchLevel :: ReleaseType -> C Int
darcsPatchLevel t =
           do patches' <- systemOut "darcs" ["changes","--from-tag",
                                             releaseRegexp t,"--count"]
              ((patches'',_):_) <- return $ reads patches'
              return $  max 0 (patches'' - 1)

darcsRelease :: ReleaseType -> C String
darcsRelease t =
    do xxx <- systemOut "darcs" ["annotate","-t",releaseRegexp t,"-s"]
       ((_:zzz):_) <- return $ filter ("tagged" `elem`) $
                        map words $ reverse $ lines xxx
       return $ unwords zzz

darcsDist :: String -> [String] -> C String
darcsDist dn tocopy = withRootdir $
    do v <- getVersion
       let distname = dn++"-"++v
           tarname = distname++".tar.gz"
           mkdist = do putS $ "making tarball as "++tarname
                       rm_rf distname
                       system "darcs" ["get","-t",v,".",distname]
                       withDirectory distname $
                         do setExecutable "Setup.hs" `catchC` \_ -> return ()
                            system "./Setup.hs"
                                       (".releaseVersion":".latestRelease":
                                        ".lastTag":".lastTagPatchLevel":
                                        ".releaseVersionPatchLevel":
                                        ".latestReleasePatchLevel":"distclean":
                                        tocopy)
                            rm_rf "_darcs"
                            rm_rf ".arcs-prefs"
                       system "tar" ["zcf",tarname,distname]
                       rm_rf distname
       rule ["sdist",tarname] tocopy mkdist
       return tarname
