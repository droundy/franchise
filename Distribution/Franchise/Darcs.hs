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

module Distribution.Franchise.Darcs ( inDarcs, darcsDist, darcsRelease, darcsPatchLevel )
    where

import System.Directory ( doesDirectoryExist, copyFile, createDirectory,
                          getDirectoryContents )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.ReleaseType ( ReleaseType(..), releaseRegexp )
import Distribution.Franchise.Permissions ( setExecutable )

{-# NOINLINE inDarcs #-}
inDarcs :: C Bool
inDarcs = io $ doesDirectoryExist "_darcs"

{-# NOINLINE darcsPatchLevel #-}
darcsPatchLevel :: ReleaseType -> C Int
darcsPatchLevel t =
           do patches' <- systemOut "darcs" ["changes","--from-tag",releaseRegexp t,"--count"]
              ((patches'',_):_) <- return $ reads patches'
              return $  max 0 (patches'' - 1)

{-# NOINLINE darcsRelease #-}
darcsRelease :: ReleaseType -> C String
darcsRelease t =
    do xxx <- systemOut "darcs" ["changes","-t",releaseRegexp t,"--reverse"]
       ((_:zzz:_):_) <- return $ filter ("tagged" `elem`) $
                        map words $ reverse $ lines xxx
       return zzz

{-# NOINLINE darcsDist #-}
darcsDist :: String -> [String] -> C String
darcsDist dn tocopy = withRootdir $
    do v <- getVersion
       whenC inDarcs $
             do -- let's ensure that .releaseVersion, .latestRelease and .lastTag
                -- exist to make it easier to track where this tarball came from...
                darcsPatchLevel Numbered
                darcsPatchLevel NumberedPreRc
                darcsPatchLevel AnyTag
                darcsRelease NumberedPreRc
                darcsRelease Numbered
                darcsRelease AnyTag
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
                                                   setExecutable "Setup.hs" `catchC` \_ -> return ()
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
