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

-- | The 'replace' function allows you to make replacement based on
-- actual Haskell values, and works great if you are generating
-- Haskell source code.
--
-- @
--      'replace' \"FOOBAR\" $ Just \"Hello world\"
--      'replace' \"BAZBAR\" True
-- @
--
-- which will work nicely with the haskell source code
--
-- @
-- putStrLn $ if BAZBAR then maybe \"Goodbye world\" id FOOBAR
--                      else \"Yucky world\"
-- @

replace :: Show a => String -> a -> C ()
replace a = replaceLiteral a . show

-- | 'replaceLiteral' is more general than 'replace', although
-- sometimes less convenient.  The example from 'replace' could
-- instead have been achieved using 'replaceLiteral' as follows.
--
-- @
--      'replaceLiteral' \"FOOBAR\" \"Just \\\"Hello world\\\"\"
--      'replaceLiteral' \"BAZBAR\" \"True\"
-- @
--
-- You can judge for yourself which you would prefer.  But, of course,
-- 'replaceLiteral' is needed when you want to substitute a string
-- that is not a valid Haskell datum, as in
--
-- @
--      'replaceLiteral' \"PRAGMAS\" \"{-\# GHC_OPTIONS -fglasgow-exts \#-}\"
--      'replaceLiteral' \"-- DEFS\" \"#undef CONFIG_H\"
-- @
replaceLiteral :: String -> String -> C ()
replaceLiteral a b = do r <- getExtra "replacements" :: C [(String, String)]
                        if a `elem` map fst r
                          then return ()
                          else addExtra "replacements" [(a,b)]

-- | You can generate source files (or any other files you wish) using
-- 'createFile'.  This function accepts a single string @name@, which
-- is the name of the file to be created.  It is generated from a file
-- called @name.in@, and replaces any strings you've asked to have
-- replaced.  It allows work flows similar to those that autoconf's
-- configure scripts typically use.
-- 
-- Note that the relative order of calls to 'createFile' and 'replace'
-- does not matter, and that files are not modified except when they
-- are changed (i.e. so you don't get excess rebuilds, as would happen
-- if you ran a naive perl script to do the same).

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
