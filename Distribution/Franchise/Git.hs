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

module Distribution.Franchise.Git ( inGit, gitPatchLevel, getTag ) where
import System.Directory ( doesFileExist, doesDirectoryExist )
import Control.Monad ( unless, msum )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util

inGit :: C Bool
inGit = io $ doesDirectoryExist ".git"

gitPatchLevel :: String -> C Int
gitPatchLevel ver = withRootdir $ do v <- inGit
                                     unless v $ fail "Not in a git repository!"
                                     b <- io $ doesFileExist ".patchLevel"
                                     if b then returnPatchLvl else getPatchLvl
    where returnPatchLvl = do [(i,"")] <- reads `fmap` cat ".patchLevel"
                              return i
                             `catchC` \_ -> return 0
          getPatchLvl = do patches <- systemOut "git" ["log",ver++"..","--pretty=oneline"]
                           let patchlvl = max 0 $ length (lines patches)
                           writeF ".patchLevel" (show patchlvl) `catchC` \_ -> return ()
                           return patchlvl
                          `catchC` \_ -> return 0

getTag :: C String
getTag = withRootdir $
         do b <- inGit
            unless b $ fail "Not in a git repository!"
            v <- msum [ do xxx <- systemOut "git" ["tag"]
                           let v = (reverse . lines) xxx
                           if (null v) then return "0.0"
                            else return $ head v
                      , do x:_ <- words `fmap` cat ".releaseVersion"
                           return x
                      , return "0.0"]
            writeF ".releaseVersion" v `catchC` \_ -> return ()
            return v
