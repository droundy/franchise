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

module Distribution.Franchise.Darcs ( ReleaseType(..), inDarcs, darcsDist,
                                      getRelease,
                                      versionFromDarcs, patchVersionFromDarcs,
                                      tagStringFromDarcs )
    where

import System.Directory ( doesDirectoryExist, copyFile, createDirectory,
                          getDirectoryContents )
import Control.Monad ( msum, when )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util

inDarcs :: C Bool
inDarcs = io $ doesDirectoryExist "_darcs"

darcsPatchLevel :: ReleaseType -> C Int
darcsPatchLevel t = withRootdir $
           do True <- inDarcs
              patches' <- systemOut "darcs" ["changes","--from-tag",releaseRegexp t,"--count"]
              ((patches'',_):_) <- return $ reads patches'
              let level = max 0 (patches'' - 1)
              writeF dotfile (show level) `catchC` \_ -> return ()
              return level
             `catchC` \_ -> do [(i,"")] <- reads `fmap` cat dotfile
                               return i
                               `catchC` \_ -> return 0
    where dotfile = releaseName t++"PatchLevel"

getRelease :: ReleaseType -> C String
getRelease t = withRootdir $
    do v <- msum [do True <- inDarcs
                     xxx <- systemOut "darcs" ["changes","-t",releaseRegexp t,"--reverse"]
                     ((_:zzz:_):_) <- return $ filter ("tagged" `elem`) $
                                      map words $ reverse $ lines xxx
                     return zzz,
                  do x:_ <- words `fmap` cat (releaseName t)
                     return x,
                  return "0.0"]
       writeF (releaseName t) v `catchC` \_ -> return ()
       return v

data ReleaseType = Numbered | NumberedPreRc | AnyTag

releaseRegexp :: ReleaseType -> String
releaseRegexp Numbered = "^[0-9\\.]+$"
releaseRegexp NumberedPreRc = "^[0-9\\.]+-?(rc[0-9]*|pre[0-9]*)?$"
releaseRegexp AnyTag = "."

releaseName :: ReleaseType -> String
releaseName Numbered = ".releaseVersion"
releaseName NumberedPreRc = ".latestRelease"
releaseName AnyTag = ".lastTag"

versionFromDarcs :: ReleaseType -> C ()
versionFromDarcs t = do vers <- getRelease t
                        oldversion <- getVersion
                        when (oldversion /= vers) $
                             do version vers
                                putS $ "version is now "++vers

patchVersionFromDarcs :: ReleaseType -> C ()
patchVersionFromDarcs t = do r <- getRelease t
                             p <- darcsPatchLevel t
                             let vers = if p == 0 then r else r++'.':show p
                             oldversion <- getVersion
                             when (oldversion /= vers) $
                                  do version vers
                                     putS $ "version is now "++vers

tagStringFromDarcs :: C String
tagStringFromDarcs = do r <- getRelease AnyTag
                        t <- darcsPatchLevel AnyTag
                        return $ case t of
                                 0 -> r
                                 1 -> r++" + one patch"
                                 _ -> r++" + "++show t++" patches"

darcsDist :: String -> [String] -> C String
darcsDist dn tocopy = withRootdir $
    do v <- getVersion
       -- let's ensure that .releaseVersion, .latestRelease and .lastTag
       -- exist to make it easier to track where this tarball came from...
       getRelease NumberedPreRc
       getRelease Numbered
       getRelease AnyTag
       darcsPatchLevel Numbered
       darcsPatchLevel NumberedPreRc
       darcsPatchLevel AnyTag
       let distname = dn++"-"++v
           tarname = distname++".tar.gz"
           mkdist = do putS $ "making tarball as "++tarname
                       system "darcs" ["dist","--dist-name",distname]
                       rm_rf distname
                       system "tar" ["zxf",tarname]
                       withDirectory distname $ do dist ".releaseVersion"
                                                   dist ".latestRelease"
                                                   dist ".lastTag"
                                                   dist ".releaseVersionPatchLevel"
                                                   dist ".latestReleasePatchLevel"
                                                   dist ".lastTagPatchLevel"
                                                   mapM_ dist tocopy
                       system "tar" ["zcf",tarname,distname]
                       rm_rf distname
       addTarget $ ["sdist",tarname] :< tocopy
               |<- defaultRule { make = const mkdist }
       return tarname

-- | Copy specified file from the build directory to the tarball.
-- This is intended to be used in your darcsDist job.
dist :: String -> C ()
dist fn = do fn' <- processFilePath fn
             cp_recursive fn fn'
          `catchC` \_ -> putV $ "unable to include "++fn++" in the tarball"
    where cp_recursive :: FilePath -> FilePath -> C ()
          cp_recursive o n =
              do putD $ "cp_recursive "++o++" "++n
                 isd <- io $ doesDirectoryExist o
                 if not isd
                   then io (copyFile o n) `catchC` \_ -> return ()
                   else do io $ createDirectory n
                           fs <- io $ filter (not . (`elem` [".",".."]))
                                 `fmap` getDirectoryContents o
                           putD $ "foobar "++unwords fs
                           mapM_ (\f -> cp_recursive (o++"/"++f) (n++"/"++f)) fs
