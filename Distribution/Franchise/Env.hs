{-# LANGUAGE CPP #-}
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
module Distribution.Franchise.Env ( setEnv, getEnv, unsetEnv, addToPath, extraPath,
                                    getEnvironment, getPrivateEnvironment ) where

import Data.Maybe ( catMaybes )
import Control.Monad ( msum )
import qualified System.Environment as E ( getEnv,
#ifdef GETENVIRONMENTWORKS
                                           getEnvironment
#endif
                                         )

import Distribution.Franchise.ConfigureState
    ( C, getAllExtraData, getExtraData, addExtraData, addExtra, getExtra, rmExtra,
      io, catchC, amInWindows )
import Distribution.Franchise.ListUtils ( stripPrefix )

getEnv :: String -> C (Maybe String)
getEnv e =
    do mv <- getExtraData ("env-"++e)
       case mv of
         Just v -> return (Just v)
         Nothing -> fmap Just (io (E.getEnv e)) `catchC` \_ -> return Nothing

setEnv :: String -> String -> C ()
setEnv e v = addExtraData ("env-"++e) v

unsetEnv :: String -> C ()
unsetEnv e = rmExtra ("env-"++e)

getPrivateEnvironment :: C [(String, String)]
getPrivateEnvironment = (catMaybes . map cleanEnv) `fmap` getAllExtraData
    where cleanEnv (e,v) = do e' <- stripPrefix "env-" e
                              return (e',v)

getEnvironment :: C [(String, String)]
getEnvironment = do pe <- getPrivateEnvironment
#ifdef GETENVIRONMENTWORKS
                    e <- io E.getEnvironment
#else
                    let gete x = do v <- getEnv x
                                    return $ fmap (\y -> (x,y)) v
                    e <- catMaybes `fmap`
                         mapM gete ["HOME","PATH","PWD","PREFIX",
                                    "GHC_PACKAGE_PATH",
                                    "FRANCHISE_GHC_PACKAGE_CONF",
                                    "ALLUSERSPROFILE", "APPDATA", "ComSpec",
                                    "DISPLAY", "EDITOR", "NUMBER_OF_PROCESSORS",
                                    "OS", "USER", "USERNAME", "USERPROFILE",
                                    "windir", "winsysdir", "TEMP"]
#endif
                    return (pe ++ filter ((`notElem` (map fst pe)) . fst) e)

addToPath :: FilePath -> C ()
addToPath d = do amw <- amInWindows
                 if amw
                     then do -- environment variables are case-insensitive on windows
                             Just oldpath <- msum `fmap` sequence [getEnv "PATH",
                                                                   getEnv "Path",
                                                                   return $ Just ""]
                             rmExtra "env-Path"
                             setEnv "PATH" $ d++';':oldpath
                     else do oldpath <- maybe "" id `fmap` getEnv "PATH"
                             setEnv "PATH" $ d++':':oldpath
                 ps <- extraPath
                 addExtra "extra-path" (d:ps)

extraPath :: C [FilePath]
extraPath = getExtra "extra-path"
