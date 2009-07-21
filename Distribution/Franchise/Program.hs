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

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Flags ( FranchiseFlag, configureFlagWithDefault )
import Distribution.Franchise.Persistency ( requireWithPrereqWithFeedback )

-- | Look for the program.  On failure, throw an exception.
findProgram :: String -- ^ program name (e.g. diff)
            -> [String] -- ^ other possible names (e.g. gdiff)
            -> C String -- ^ returns the found name of the program
findProgram e xs = fe (e:xs)
    where fe [] = fail $ "Couldn't find executable "++e
          fe (y:ys) = do me <- io $ findExecutable y
                         case me of
                           Just _ -> return y
                           Nothing -> fe ys

-- | Look for a program, and do something with it if it is found.

withProgram :: Monoid a => String -> [String] -> (String -> C a) -> C a
withProgram pname alts j = (findProgram pname alts >>= j)
                           `catchC` \_ -> return mempty

-- | Define a program that may be configured by the user.  Best
-- demonstrated with an example (see also
-- <../17-configured-program.html>).
--
-- @
--     main = 'build' ['configurableProgram' \"shell\" \"bash\" [\"dash\", \"shsh\",\"sh\"]] $
--            do sh <- 'configuredProgram' \"shell\"
--               'system' sh [\"-c\", \"echo\", \"hello world\"]
-- @

configurableProgram :: String -> String -> [String] -> C FranchiseFlag
configurableProgram humanName defaultProg options =
    configureFlagWithDefault
        ("with-"++humanName) "COMMAND"
        ("use command as "++humanName)
        (do putExtra ("program-default-"++humanName) defaultProg
            putExtra ("program-options-"++humanName) options)
        (\p -> do putExtra ("program-default-"++humanName) p
                  putExtra ("program-options-"++humanName) ([] :: [String]))

-- | Find the value of a program that may have been configured by the
-- user via 'configurableProgram'.

configuredProgram :: String -> C String
configuredProgram humanName = withConfiguredProgram humanName return

-- | Use the result of a configured program, defined via
-- 'configurableProgram'.

withConfiguredProgram :: String -> (String -> C a) -> C a
withConfiguredProgram humanName j =
    do let extraname = "program-"++humanName
       def <- getExtra ("program-default-"++humanName)
       opts <- getExtra ("program-options-"++humanName)
       requireWithPrereqWithFeedback ("for "++humanName) humanName
                                     (return $ def:opts) $
         do p <- findProgram def opts
            extraname <<= p
            persistExtra extraname
            return (p, ())
       mp <- getExtraData extraname
       case mp of
         Just p -> j p
         Nothing -> fail $ "No "++ humanName ++" program"
