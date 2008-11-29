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
module Distribution.Franchise.Program ( findProgram, withProgram,
                                        configurableProgram, withConfiguredProgram,
                                        configuredProgram )
    where

import System.Directory ( findExecutable )
import Data.Monoid ( Monoid, mempty )
import Control.Monad ( when )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Flags ( FranchiseFlag, configureFlagWithDefault )
import Distribution.Franchise.Persistency ( requireWithPrereqOutput )

-- throw exception on failure to find something
findProgram :: String -> [String] -> C String
findProgram e xs = fe (e:xs)
    where fe [] = fail $ "Couldn't find executable "++e
          fe (y:ys) = do me <- io $ findExecutable y
                         case me of
                           Just _ -> return y
                           Nothing -> fe ys

withProgram :: Monoid a => String -> [String] -> (String -> C a) -> C a
withProgram pname alts j = (findProgram pname alts >>= j)
                           `catchC` \_ -> return mempty

configurableProgram :: String -> String -> [String] -> C FranchiseFlag
configurableProgram humanName defaultProg options =
    configureFlagWithDefault ("with-"++humanName) "COMMAND" ("use command as "++humanName)
                             (requireWithPrereqOutput ("for "++ humanName) humanName
                                                      (return $ defaultProg:options) $
                              do rn <- getExtra requestedname
                                 case rn of
                                   "Manually" -> getExtra extraname
                                   _ -> do p <- findProgram defaultProg options
                                           addExtraData extraname p
                                           persistExtra extraname
                                           return p)
                             (\p -> do addExtraData extraname p
                                       cl <- getExtra "commandLine"
                                       when ("configure" `elem` cl) $ do
                                         requireWithPrereqOutput ("for "++humanName) humanName
                                                                 (return $ defaultProg:options)
                                                                 (return p)
                                         persistExtra extraname
                                         addExtra requestedname "Manually"
                                         persistExtra requestedname)
    where requestedname = "program-"++humanName++"-requested"
          extraname = "program-"++humanName

configuredProgram :: String -> C String
configuredProgram humanName = withConfiguredProgram humanName return

withConfiguredProgram :: String -> (String -> C a) -> C a
withConfiguredProgram humanName j =
    do mp <- getExtraData ("program-"++humanName)
       case mp of
         Just p -> j p
         Nothing -> fail $ "No "++ humanName ++" program"
