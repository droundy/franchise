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

module Distribution.Franchise.Trie ( Trie, emptyT, lookupT, fromListT, toListT,
                                     sloppyLookupKey,
                                     insertT, adjustT, insertSeveralT, filterT, keysT,
                                     delT, delSeveralT, lengthT ) where

import Distribution.Franchise.StringSet
import Data.Maybe ( catMaybes, isJust )

data Trie a = Trie {-# UNPACK #-} !(Maybe a) [(Char,Trie a)]

instance Show a => Show (Trie a) where
    showsPrec x ss = showsPrec x (toListT ss)

instance Eq a => Eq (Trie a) where
    Trie a _ == Trie b _ | a /= b = False
    Trie _ [] == Trie _ [] = True
    Trie _ [] == _ = False
    _ == Trie _ [] = False
    Trie _ (x:xs) == Trie _ ys = case takeOne x ys of
                                 Just ys' -> Trie Nothing xs == Trie Nothing ys'
                                 Nothing -> False

mapSnd :: (a -> b) -> [(Char, a)] -> [(Char, b)]
mapSnd f = map (\(c, a) -> (c, f a))

keysT :: Trie a -> StringSet
keysT (Trie x ts) = SS (isJust x) $ mapSnd keysT ts

takeOne :: Eq a => a -> [a] -> Maybe [a]
takeOne x (y:ys) | x == y = Just ys
                 | otherwise = (y:) `fmap` takeOne x ys
takeOne _ [] = Nothing

toListT :: Trie a -> [(String, a)]
toListT (Trie b ls) = (case b of Just a -> [("",a)]; _ -> []) ++ concatMap toL ls
    where toL (c,ss) = map (\(s,a) -> (c:s,a)) $ toListT ss

fromListT :: [(String,a)] -> Trie a
fromListT x = insertSeveralT x emptyT

lengthT :: Trie a -> Int
lengthT (Trie b ls) = sum (map (lengthT . snd) ls) + (case b of Just _ -> 1; _ -> 0)

emptyT :: Trie a
emptyT = Trie Nothing []

lookupT :: String -> Trie a -> Maybe a
lookupT "" (Trie ma _) = ma
lookupT (c:cs) (Trie _ ls) = do ls' <- lookup c ls
                                lookupT cs ls'

sloppyLookupKey :: String -> Trie a -> [String]
sloppyLookupKey "" (Trie (Just _) _) = [""]
sloppyLookupKey "" t = map fst $ toListT t
sloppyLookupKey (c:cs) (Trie _ ls) = case lookup c ls of
                                     Nothing -> []
                                     Just ls' -> map (c:) $ sloppyLookupKey cs ls'

adjustT :: String -> (a -> a) -> Trie a -> Trie a
adjustT "" f (Trie (Just v) ls) = fv `seq` Trie (Just fv) ls
    where fv = f v -- make this strict to avoid stack overflow!
adjustT "" _ (Trie Nothing ls) = Trie Nothing ls
adjustT (c:cs) f (Trie b ls) = Trie b $ adj ls
    where adj ((c', ss):r) | c == c' = (c', adjustT cs f ss) : r
          adj (x:r) = x : adj r
          adj [] = []

insertT :: String -> a -> Trie a -> Trie a
insertT "" a (Trie _ ls) = Trie (Just a) ls
insertT (c:cs) a (Trie b ls) = Trie b $ repl ls
    where repl ((c', ss):r) | c == c' = (c', insertT cs a ss) : r
          repl (x:r) = x : repl r
          repl [] = [(c, insertT cs a emptyT)]

filterT :: (a -> Bool) -> Trie a -> Trie a
filterT f (Trie ma ts) =
    case f `fmap` ma of
    Just True -> Trie ma ts'
    _ -> Trie Nothing ts'
    where ts' = catMaybes $ map (\(c,t) -> case filterT f t of
                                           Trie Nothing [] -> Nothing
                                           t' -> Just (c,t')) ts

delT :: String -> Trie a -> Trie a
delT "" (Trie _ ls) = Trie Nothing ls
delT (c:cs) (Trie b ls) = Trie b $ catMaybes $ map d ls
    where d (c', x) | c == c' = case delT cs x of
                                Trie Nothing [] -> Nothing
                                x' -> Just (c', x')
          d x = Just x

insertSeveralT :: [(String, a)] -> Trie a -> Trie a
insertSeveralT [] x = x
insertSeveralT ((s,a):ss) x = insertSeveralT ss $ insertT s a x

delSeveralT :: [String] -> Trie a -> Trie a
delSeveralT [] x = x
delSeveralT (s:ss) x = delSeveralT ss $ delT s x
