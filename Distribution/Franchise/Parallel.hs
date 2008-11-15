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
module Distribution.Franchise.Parallel ( mapC, sequenceC )
    where

import Control.Concurrent ( newChan, readChan, writeChan, getChanContents, writeList2Chan )

import Distribution.Franchise.ConfigureState

mapC :: (a -> C b) -> [a] -> C [b]
mapC f xs = sequenceC $ map f xs

sequenceC :: [C b] -> C [b]
sequenceC js0 = do njobs <- getNumJobs
                   if njobs < 1
                      then sequence js0
                      else do done <- io $ newChan
                              avail <- io $ newChan
                              forkC CannotModifyState $ io $ writeList2Chan avail js0
                              let startworker = do j <- io $ readChan avail
                                                   j >>= io . writeChan done
                                                   startworker
                              sequence_ $ take njobs $ repeat $ forkC CannotModifyState startworker
                              take (length js0) `fmap` io (getChanContents done)
