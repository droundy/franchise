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
      requireOutput, requireWithPrereqOutput,
      requireWithPrereq )
        where

import Distribution.Franchise.ConfigureState

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

setOnce :: String -> C () -> C ()
setOnce name j = requireWithPrereqActionOutput "setting" name name
                 (return ["SET"]) (j >> return "done")

checkOnce :: String -> C () -> C ()
checkOnce name check = require name check `catchC` \_ -> return ()

requireOutput :: String -> C String -> C ()
requireOutput name check = requireWithPrereqOutput name name (return ["TEST"]) check

require :: String -> C () -> C ()
require name check = requireWithPrereq name name (return ["TEST"]) check

requireWithPrereq :: String -> String -> C [String] -> C () -> C ()
requireWithPrereq name longname prereq check =
    requireWithPrereqOutput name longname prereq (check >> return "yes")

requireWithPrereqOutput :: String -> String -> C [String] -> C String -> C ()
requireWithPrereqOutput name longname prereq check =
    requireWithPrereqActionOutput "checking" name longname prereq check

requireWithPrereqActionOutput :: String -> String -> String -> C [String] -> C String -> C ()
requireWithPrereqActionOutput action name longname prereq check =
    do let checkname = cleanName $ action++"-"++longname
       v <- prereq
       checkval <- getExtra checkname
       case checkval of
         ("FAIL":e:v') | v' == v -> fail e
         ("PASS":v') | v' == v -> return ()
         z -> do putD $ "found confirmation "++ show z ++ " on "++ name
                 putSnoln $ action++" "++name++" ... "
                 out <- quietly check
                 putS out
                 satisfyWithPrereq checkname prereq
                 persistExtra checkname
             `catchC` \e -> do putS "no"
                               failWithPrereq checkname prereq e

satisfyWithPrereq :: String -> C [String] -> C ()
satisfyWithPrereq checkname prereq = (("PASS":) `fmap` prereq) >>= putExtra checkname

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
cleanName (c:r) | c `elem` "/\\\n\t \r" = '-' : cleanName r
                | otherwise = c : cleanName r
