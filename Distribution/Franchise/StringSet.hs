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

module Distribution.Franchise.StringSet ( StringSet(..), nullS, emptyS, elemS,
                                          fromListS, toListS, foreachS,
                                          unionS, unionallS,
                                          addS, addsS, delS, delsS,
                                          lengthS ) where

import Data.Maybe ( catMaybes )

data StringSet = SS {-# UNPACK #-} !Bool [(Char,StringSet)]

instance Show StringSet where
    showsPrec x ss = showsPrec x (toListS ss)

instance Eq StringSet where
    SS a _ == SS b _ | a /= b = False
    SS _ [] == SS _ [] = True
    SS _ [] == _ = False
    _ == SS _ [] = False
    SS _ (x:xs) == SS _ ys = case takeOne x ys of
                         Just ys' -> SS False xs == SS False ys'
                         Nothing -> False

takeOne :: Eq a => a -> [a] -> Maybe [a]
takeOne x (y:ys) | x == y = Just ys
                 | otherwise = (y:) `fmap` takeOne x ys
takeOne _ [] = Nothing

toListS :: StringSet -> [String]
toListS (SS b ls) = (if b then [""] else []) ++ concatMap toL ls
    where toL (c,ss) = map (c:) $ toListS ss

fromListS :: [String] -> StringSet
fromListS x = addsS x emptyS

lengthS :: StringSet -> Int
lengthS (SS b ls) = sum (map (lengthS . snd) ls) + (if b then 1 else 0)

emptyS :: StringSet
emptyS = SS False []

nullS :: StringSet -> Bool
nullS (SS False []) = True
nullS _ = False

elemS :: String -> StringSet -> Bool
elemS "" (SS b _) = b
elemS (c:cs) (SS _ ls) = case lookup c ls of
                         Nothing -> False
                         Just ls' -> elemS cs ls'

addS :: String -> StringSet -> StringSet
addS "" (SS _ ls) = SS True ls
addS (c:cs) (SS b ls) = SS b $ repl ls
    where repl ((c', ss):r) | c == c' = (c', addS cs ss) : r
          repl (x:r) = x : repl r
          repl [] = [(c, addS cs emptyS)]

delS :: String -> StringSet -> StringSet
delS "" (SS _ ls) = SS False ls
delS (c:cs) (SS b ls) = SS b $ catMaybes $ map d ls
    where d (c', x) | c == c' = case delS cs x of
                                SS False [] -> Nothing
                                x' -> Just (c', x')
          d x = Just x

unionS :: StringSet -> StringSet -> StringSet
unionS a b = addsS (toListS a) b

unionallS :: [StringSet] -> StringSet
unionallS [] = emptyS
unionallS (x:xs) = addsS (concatMap toListS xs) x

addsS :: [String] -> StringSet -> StringSet
addsS [] x = x
addsS (s:ss) x = addsS ss $ addS s x

delsS :: [String] -> StringSet -> StringSet
delsS [] x = x
delsS (s:ss) x = delsS ss $ delS s x

foreachS :: Monad m => (String -> m a) -> StringSet -> m [a]
foreachS f s = mapM f $ toListS s
