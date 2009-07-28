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
module Distribution.Franchise.Persistency
    ( cacheifC, setOnce, checkOnce, require,
      requireWithFeedback, requireWithPrereqWithFeedback,
      requireWithPrereq )
        where

import Distribution.Franchise.ConfigureState
import Data.Monoid ( mempty, Monoid )
import Data.Maybe ( isJust, fromJust )

cacheifC :: String -> C Bool -> C () -> C () -> C ()
cacheifC name check ifok iffail =
    do let checkname = "check-"++name
       amok <- getExtra checkname
       case amok of
         (True:_) -> ifok
         (False:_) -> iffail
         [] -> (do putSnoln $ "checking "++name++" ... "
                   isok <- quietly check
                   if isok
                      then do putS "yes"
                              ifok
                              putExtra checkname [True]
                      else do putS "no"
                              iffail
                              putExtra checkname [False]
                   persistExtra checkname)
               `catchC` \_ -> (do putS "no"
                                  iffail
                                  putExtra checkname [False]
                                  persistExtra checkname)

setOnce :: (Read a, Show a, Monoid a) => String -> C a -> C a
setOnce name j = requireWithPrereqActionWithFeedback "setting" name name
                 (return ["SET"]) $ do x <- j
                                       return ("done",x)

-- | Perform the provided test once.  The string provided is a unique
-- human-visible name.  The test must store its own result.  Upon
-- failure (due to an exception or an explicit call to 'fail'),
-- franchise will report the test as failed (i.e. it will write
-- \"no\").  The result is cached, and the test is not performed
-- again.

checkOnce :: (Read a, Show a, Monoid a) => String -- ^ name of test
          -> C a -- ^ action to perform
          -> C a
checkOnce name check = require name check `catchC` \_ -> return mempty

requireWithFeedback :: String -> C String -> C ()
requireWithFeedback name check =
    requireWithPrereqWithFeedback name name (return ["TEST"]) $ do x <- check
                                                                   return (x,())

require :: (Read a, Show a, Monoid a) => String -> C a -> C a
require name check = requireWithPrereq name name (return ["TEST"]) check

requireWithPrereq :: (Read a, Show a, Monoid a) => String -> String
                  -> C [String] -> C a -> C a
requireWithPrereq name longname prereq check =
    requireWithPrereqWithFeedback name longname prereq $ do x <- check
                                                            return ("yes",x)

requireWithPrereqWithFeedback :: (Read a, Show a, Monoid a) => String -> String
                              -> C [String] -> C (String,a) -> C a
requireWithPrereqWithFeedback name longname prereq check =
    requireWithPrereqActionWithFeedback "checking" name longname prereq check

requireWithPrereqActionWithFeedback :: (Read a, Show a, Monoid a) =>
                                       String -> String -> String
                                    -> C [String] -> C (String,a) -> C a
requireWithPrereqActionWithFeedback action name longname prereq check =
    do let checkname = cleanName $ action++"-"++longname
       v <- prereq
       checkval <- getExtra checkname
       case checkval of
         ("FAIL":e:v') | v' == v -> fail e
         (('P':'A':'S':'S':rest): v')
             | v' == v && isJust aval -> return (fromJust aval)
             where aval = case reads $ dropWhile (`elem` "\r\n\t ") rest of
                            ((r,_):_) -> Just r
                            _ -> Nothing
         z -> do putD $ "found confirmation "++ show z ++ " on "++ name
                 putSnoln $ action++" "++name++" ... "
                 (out,x) <- quietly check
                 putS out
                 satisfyWithPrereq x checkname prereq
                 -- persistExtra checkname
                 return x
             `catchC` \e -> do putS "no"
                               failWithPrereq checkname prereq e

satisfyWithPrereq :: Show a => a -> String -> C [String] -> C ()
satisfyWithPrereq a checkname prereq =
    ((("PASS\n"++show a):) `fmap` prereq) >>= putExtra checkname

failWithPrereq :: String -> C [String] -> String -> C a
failWithPrereq checkname prereq e = do v <- prereq
                                       putExtra checkname $ "FAIL":e:v
                                       persistExtra checkname
                                       fail e

cleanName :: String -> String
cleanName "" = ""
cleanName ('(':r) = cleanName $ drop 1 $ dropWhile (/= ')') r
cleanName ('\'':r) = cleanName r
cleanName ('"':r) = cleanName r
cleanName (c:r) | c `elem` "/\\\n\t \r><," =
                    '-' : dropWhile (=='-') (cleanName r)
                | otherwise = c : cleanName r
