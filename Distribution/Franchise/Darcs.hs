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

module Distribution.Franchise.Darcs ( inDarcs, patchLevel,
                                      versionFromDarcs, patchVersionFromDarcs )
    where

import System.Directory ( doesDirectoryExist )
import Distribution.Franchise.ConfigureState

import Distribution.Franchise.Util ( systemOut, cat )

inDarcs :: C Bool
inDarcs = io $ doesDirectoryExist "_darcs"

data Literal = Literal String
instance Show Literal where
    showsPrec _ (Literal x) = showString x

patchLevel :: String -> C Int
patchLevel true_v =
           do True <- inDarcs
              patches' <- systemOut "darcs" ["changes","--from-tag",true_v,"--count"]
              ((patches'',_):_) <- return $ reads patches'
              let level = max 0 (patches'' - 1)
              io (writeFile ".patchLevel" $ show level) `catchC` \_ -> return ()
              return level
             `catchC` \_ -> do [(i,"")] <- reads `fmap` cat ".patchLevel"
                               return i
                               `catchC` \_ -> return 0

getRelease :: C String
getRelease =
    do v <- do True <- inDarcs
               xxx <- systemOut "darcs" ["changes","-t","^[0-9\\.]+(rc[0-9]*|pre[0-9]*)?$", "--reverse"]
               ((_:zzz:_):_) <- return $ map words $ reverse $ lines xxx
               return zzz
           `catchC` \_ -> do (x:_) <- words `fmap` cat ".releaseVersion"
                             return x
                             `catchC` \_ -> return "0.0"
       io (writeFile ".releaseVersion" v) `catchC` \_ -> return ()
       return v

versionFromDarcs :: C ()
versionFromDarcs = do r <- getRelease
                      version r
                      whenC versionChanged $ putS $ "version is now "++r

patchVersionFromDarcs :: C ()
patchVersionFromDarcs = do r <- getRelease
                           p <- patchLevel r
                           let vers = if p == 0 then r else r++'.':show p
                           version vers
                           whenC versionChanged $ putS $ "version is now "++vers

