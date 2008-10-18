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

module Distribution.Franchise.StringSet ( StringSet, emptyS, elemS,
                                          addS, addsS, delS, delsS, lengthS ) where

import Data.Maybe ( catMaybes )

newtype StringSet = SS [(Maybe Char, StringSet)]

instance Show StringSet where
    showsPrec x ss = showsPrec x (toList ss)

toList :: StringSet -> [String]
toList (SS []) = []
toList (SS ls) = concatMap toL ls
    where toL (Nothing,_) = [""]
          toL (Just c,ss) = map (c:) $ toList ss

lengthS :: StringSet -> Int
lengthS (SS []) = 0
lengthS (SS ls) = sum $ map l ls
    where l (Nothing,_) = 1
          l (_, x) = lengthS x

emptyS :: StringSet
emptyS = SS []

elemS :: String -> StringSet -> Bool
elemS "" (SS ls) = case lookup Nothing ls of
                   Nothing -> False
                   Just _ -> True
elemS (c:cs) (SS ls) = case lookup (Just c) ls of
                       Nothing -> False
                       Just ls' -> elemS cs ls'

addS :: String -> StringSet -> StringSet
addS "" (SS ls) = case lookup Nothing ls of
                  Nothing -> SS $ (Nothing, emptyS):ls
                  Just _ -> SS ls
addS (c:cs) (SS ls) = SS $ repl ls
    where repl ((Just c', ss):r) | c == c' = (Just c', addS cs ss) : r
          repl (x:r) = x : repl r
          repl [] = [(Just c, addS cs emptyS)]

delS :: String -> StringSet -> StringSet
delS "" (SS ls) = SS $ filter d ls
    where d (Nothing, _) = False
          d _ = True
delS (c:cs) (SS ls) = SS $ catMaybes $ map d ls
    where d (Just c', x) | c == c' = case delS cs x of
                                     SS [] -> Nothing
                                     x' -> Just (Just c', x')
          d x = Just x
                      
addsS :: [String] -> StringSet -> StringSet
addsS [] x = x
addsS (s:ss) x = addsS ss $ addS s x

delsS :: [String] -> StringSet -> StringSet                      
delsS [] x = x
delsS (s:ss) x = delsS ss $ delS s x
