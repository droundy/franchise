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

module Distribution.Franchise.Env ( setEnv, getEnv, addToPath,
                                    getEnvironment, getPrivateEnvironment ) where

import Data.Maybe ( catMaybes )
import qualified System.Environment as E ( getEnv, getEnvironment )

import Distribution.Franchise.ConfigureState
    ( C, getAllExtraData, getExtraData, addExtraData, io, catchC, amInWindows )
import Distribution.Franchise.ListUtils ( stripPrefix )

getEnv :: String -> C (Maybe String)
getEnv e =
    do mv <- getExtraData ("env-"++e)
       case mv of
         Just v -> return (Just v)
         Nothing -> fmap Just (io (E.getEnv e)) `catchC` \_ -> return Nothing

setEnv :: String -> String -> C ()
setEnv e v = addExtraData ("env-"++e) v

getPrivateEnvironment :: C [(String, String)]
getPrivateEnvironment = (catMaybes . map cleanEnv) `fmap` getAllExtraData
    where cleanEnv (e,v) = do e' <- stripPrefix "env-" e
                              return (e',v)

getEnvironment :: C [(String, String)]
getEnvironment = do pe <- getPrivateEnvironment
                    e <- io E.getEnvironment
                    return (pe ++ filter ((`notElem` (map fst pe)) . fst) e)

-- WARNING: on Windows, addToPath affects only the path used by programs we
-- call, not the path we use to find programs we call!

addToPath :: FilePath -> C ()
addToPath d = do amw <- amInWindows
                 oldpath <- maybe "" id `fmap` getEnv "PATH"
                 setEnv "PATH" $ if amw then d++';':oldpath
                                        else d++':':oldpath
