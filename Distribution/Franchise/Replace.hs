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
module Distribution.Franchise.Replace ( replace, replaceLiteral, createFile ) where

import Data.List ( isPrefixOf )

import Distribution.Franchise.ConfigureState ( C, getExtra, addExtra, writeF )
import Distribution.Franchise.Util ( cat )
import Distribution.Franchise.Buildable ( addTarget,
                                          Buildable(..), Dependency(..), BuildRule(..), defaultRule )

replace :: Show a => String -> a -> C ()
replace a = replaceLiteral a . show

replaceLiteral :: String -> String -> C ()
replaceLiteral a b = do r <- getExtra "replacements" :: C [(String, String)]
                        if a `elem` map fst r
                          then return ()
                          else addExtra "replacements" [(a,b)]

createFile :: String -> C ()
createFile fn = addTarget $ [fn] :< [fn++".in"] :<-
                defaultRule { make = \_ ->  do x <- cat (fn++".in")
                                               r <- getExtra "replacements"
                                               writeF fn $ repl r x }
    where repl [] x = x
          repl ((a,b):rs) x = repl rs $ r1 a b x
          r1 a b x@(x1:xs) | a `isPrefixOf` x = b ++ r1 a b (drop (length a) x)
                           | otherwise = x1 : r1 a b xs
          r1 _ _ "" = ""
