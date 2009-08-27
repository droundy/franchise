{- Copyright (c) 2008-2009 David Roundy

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

module Distribution.Franchise.CharAssocList
    ( CharAssocList, emptyC, nullC, lookupC, fromListC, toListC,
      unionC, insertC, singleC,
      adjustC, alterC, alterDelC, alterAddC, mapDelC,
      filterC, sumC,
      delC, delSeveralC, lengthC ) where

import Data.List ( sort )

data CharAssoc a = CA {-# UNPACK #-} !Char {-# UNPACK #-} !a
                   deriving ( Eq )
instance Eq a => Ord (CharAssoc a) where
    compare (CA c1 _) (CA c2 _) = compare c1 c2
instance Functor CharAssoc where
    fmap f (CA c a) = CA c (f a)
toPair :: CharAssoc a -> (Char, a)
toPair (CA c a) = (c,a)
fromPair :: (Char, a) -> CharAssoc a
fromPair (c,a) = CA c a
newtype CharAssocList a = CAL [CharAssoc a]
    deriving ( Eq, Ord )
instance Functor CharAssocList where
    fmap f (CAL xs) = CAL (map (fmap f) xs)

instance Show a => Show (CharAssocList a) where
    showsPrec x ss = showsPrec x (toListC ss)

{-# RULES "fromListC . toListC"
  forall x. fromListC (toListC x) = x #-}

toListC :: CharAssocList a -> [(Char, a)]
toListC (CAL ls) = map toPair ls

fromListC :: Eq a => [(Char,a)] -> CharAssocList a
fromListC = CAL . sort . map fromPair

lengthC :: CharAssocList a -> Int
lengthC (CAL ls) = length ls

emptyC :: CharAssocList a
emptyC = CAL []

nullC :: CharAssocList a -> Bool
nullC (CAL []) = True
nullC _ = False

sumC :: (a -> Int) -> CharAssocList a -> Int
sumC f (CAL xs) = sum $ map f' xs
    where f' (CA _ x) = f x

lookupC :: Char -> CharAssocList a -> Maybe a
lookupC c (CAL (CA c' a':r))
    | c == c' = Just a'
    | otherwise = lookupC c (CAL r)
lookupC _ (CAL []) = Nothing

alterC :: Char -> (Maybe a -> Maybe a) -> CharAssocList a -> CharAssocList a
alterC c f (CAL xxs) = CAL $ alt xxs
    where alt (CA c1 a1:r)
              | c1 == c = case f (Just a1) of Nothing -> r
                                              Just a' -> CA c a':r
              | c1 > c = case f Nothing of
                           Nothing -> CA c1 a1 : r
                           Just a -> CA c a : CA c1 a1 : r
              | otherwise = CA c1 a1 : alt r
          alt [] = case f Nothing of Nothing -> []
                                     Just a -> [CA c a]

adjustC :: Char -> (a -> a) -> CharAssocList a -> CharAssocList a
adjustC c f (CAL xxs) = CAL $ alt xxs
    where alt (CA c' a:r) | c' == c = CA c (f a):r
                          | c' > c = CA c' a : r
                          | otherwise = CA c' a : alt r
          alt [] = []

alterAddC :: Char -> (Maybe a -> a) -> CharAssocList a -> CharAssocList a
alterAddC c f (CAL xxs) = CAL $ alt xxs
    where alt (CA c1 a1:r) | c1 == c = CA c1 (f $ Just a1):r
                           | c1 > c = CA c (f Nothing):CA c1 a1:r
                           | otherwise = CA c1 a1 : alt r
          alt [] = [CA c $ f Nothing]

alterDelC :: Char -> (a -> Maybe a) -> CharAssocList a -> CharAssocList a
alterDelC c f (CAL xxs) = CAL $ alt xxs
    where alt (CA c1 a1:r) | c1 == c = case f a1 of
                                       Nothing -> r
                                       Just a' -> CA c a' : r
                           | c1 > c = CA c1 a1 : r
                           | otherwise = CA c1 a1 : alt r
          alt [] = []

mapDelC :: (a -> Maybe a) -> CharAssocList a -> CharAssocList a
mapDelC f (CAL xxs) = CAL $ alt xxs
    where alt (CA c a:r) = case f a of
                             Nothing -> alt r
                             Just a' -> CA c a' : alt r
          alt [] = []

singleC :: Char -> a -> CharAssocList a
singleC c a = CAL [CA c a]

insertC :: Char -> a -> CharAssocList a -> CharAssocList a
insertC c a (CAL ls) = CAL $ alt ls
    where alt (CA c1 a1:r)
              | c1 == c = CA c a:r
              | c1 > c = CA c a : CA c1 a1 : r
              | otherwise = CA c1 a1 : alt r
          alt [] = [CA c a]

filterC :: (a -> Bool) -> CharAssocList a -> CharAssocList a
filterC f (CAL ls) = CAL $ filter f' ls
    where f' (CA _ a) = f a

delC :: Char -> CharAssocList a -> CharAssocList a
delC c (CAL ls) = CAL $ filter f ls
    where f (CA c' _) = c /= c'

unionC :: CharAssocList a -> CharAssocList a -> CharAssocList a
unionC (CAL x) (CAL y) = CAL $ un x y
    where un [] b = b
          un a [] = a
          un aaa@(CA ca a:aa) bbb@(CA cb b:bb)
              | ca < cb = CA ca a : un aa bbb
              | cb < ca = CA cb b : un aaa bb
              | otherwise = CA ca a : un aa bb

delSeveralC :: String -> CharAssocList a -> CharAssocList a
delSeveralC s (CAL ls) = CAL $ filter f ls
    where f (CA c _) = c `notElem` s
