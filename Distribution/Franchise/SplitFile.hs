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
module Distribution.Franchise.SplitFile
    ( splitFile, mapDirectory, mapDirectory_ )
    where

import Data.List ( isSuffixOf )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util

-- | Create a build target for test suites.

splitFile :: String -> (String -> [(FilePath, String)]) -> C [FilePath]
splitFile fn _ | ".splits" `isSuffixOf` fn = return []
splitFile fn _ | "~" `isSuffixOf` fn = return []
splitFile fn splitfun =
    whenC (isFile fn) $
    do withd <- rememberDirectory
       rule [fnsplits] [fn] $ withd splitf
       processFilePath fnsplits >>= build' CannotModifyState
       ss <- readSplits
       case ss of
         [] -> return ()
         (s:_) -> do addTarget $ ss :< [fn,fnsplits]
                         |<- defaultRule { make = const $ withd $ splitf }
                     processFilePath s >>= build' CannotModifyState
       return ss
    where fnsplits = fn++".splits"
          splitf = do x <- cat fn
                      let splits = splitfun x
                      mapM (uncurry writeF) splits
                      -- we use writeFile rather than writeF because
                      -- we want the timestamp always to change, so we
                      -- won't have to re-run this next time.
                      fnsplits' <- processFilePath fnsplits
                      io $ writeFile fnsplits' $ show $ map fst splits
          readSplits = do x <- cat fnsplits
                          case reads x of
                            [(y,"")] -> return y
                            _ -> fail $ "unable to read "++fnsplits

-- | Run the provided action on each file in the specified directory.

mapDirectory :: (FilePath -> C a) -> FilePath -> C [a]
mapDirectory f d = withDirectory d $ do xs <- ls "."
                                        mapM f xs

mapDirectory_ :: (FilePath -> C a) -> FilePath -> C ()
mapDirectory_ f d = withDirectory d $ do xs <- ls "."
                                         mapM_ f xs
