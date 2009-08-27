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

module Distribution.Franchise.Trie ( Trie, emptyT, lookupT, fromListT, toListT,
                                     sloppyLookupKey, unionT,
                                     insertT, adjustT, alterT,
                                     insertSeveralT, filterT, keysT,
                                     delT, delSeveralT, lengthT ) where

import Distribution.Franchise.StringSet
import Distribution.Franchise.CharAssocList

data Trie a = Trie {-# UNPACK #-} !(Maybe a)
                   {-# UNPACK #-} !(CharAssocList (Trie a))
              deriving ( Eq )

instance Show a => Show (Trie a) where
    showsPrec x ss = showsPrec x (toListT ss)
instance Functor Trie where
    fmap f (Trie x y) = Trie (fmap f x) (fmap (fmap f) y)

keysT :: Trie a -> StringSet
keysT (Trie (Just _) ts) = SS True $ fmap keysT ts
keysT (Trie Nothing ts) = SS False $ fmap keysT ts

{-# RULES "fromListT . toListT"
  forall x. fromListT (toListT x) = x #-}

toListT :: Trie a -> [(String, a)]
toListT (Trie b ls) = (case b of Just a -> (("",a):); _ -> id) $
                      concatMap toL (toListC ls)
    where toL (c,ss) = map (\(s,a) -> (c:s,a)) $ toListT ss

fromListT :: [(String,a)] -> Trie a
fromListT x = insertSeveralT x emptyT

lengthT :: Trie a -> Int
lengthT (Trie b ls) =  case b of Just _ -> rest + 1
                                 Nothing -> rest
    where rest = sumC lengthT ls

emptyT :: Trie a
emptyT = Trie Nothing emptyC

nullT :: Trie a -> Bool
nullT (Trie Nothing x) = nullC x
nullT _ = False

lookupT :: String -> Trie a -> Maybe a
lookupT "" (Trie ma _) = ma
lookupT (c:cs) (Trie _ ls) = do ls' <- lookupC c ls
                                lookupT cs ls'

sloppyLookupKey :: String -> Trie a -> [String]
sloppyLookupKey "" (Trie (Just _) _) = [""]
sloppyLookupKey "" t = map fst $ toListT t
sloppyLookupKey (c:cs) (Trie _ ls) =
    case lookupC c ls of
    Nothing -> []
    Just ls' -> map (c:) $ sloppyLookupKey cs ls'

alterT :: String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
alterT "" f (Trie mv ls) = Trie (f mv) ls
alterT (c:cs) f (Trie b ls) = Trie b $ alterC c alt ls
    where alt (Just t) = case alterT cs f t of
                           t' | nullT t' -> Nothing
                              | otherwise -> Just t'
          alt Nothing = case f Nothing of
                          Nothing -> Nothing
                          Just a -> Just $ singleT cs a

adjustT :: String -> (a -> a) -> Trie a -> Trie a
adjustT "" f (Trie (Just v) ls) = fv `seq` Trie (Just fv) ls
    where fv = f v -- make this strict to avoid stack overflow!
adjustT "" _ (Trie Nothing ls) = Trie Nothing ls
adjustT (c:cs) f (Trie b ls) = Trie b $ adjustC c (adjustT cs f) ls

insertT :: String -> a -> Trie a -> Trie a
-- The following code causes a major slowdown!
-- insertT "" a (Trie _ ls) = a `seq` Trie (Just a) ls
insertT "" a (Trie _ ls) = Trie (Just a) ls
insertT (c:cs) a (Trie b ls) = Trie b $ alterAddC c ins ls
    where ins Nothing = singleT cs a
          ins (Just t) = insertT cs a t

singleT :: String -> a -> Trie a
singleT "" a = Trie (Just a) emptyC
singleT (c:cs) a = Trie Nothing $ singleC c $ singleT cs a

filterT :: (a -> Bool) -> Trie a -> Trie a
filterT f (Trie ma ts) = Trie ma' $ mapDelC fil ts
    where ma' = do a <- ma
                   if f a then Just a
                          else Nothing
          fil t = case filterT f t of
                     t' | nullT t' -> Nothing
                        | otherwise -> Just t'

delT :: String -> Trie a -> Trie a
delT "" (Trie _ ls) = Trie Nothing ls
delT (c:cs) (Trie b ls) = Trie b $ alterC c del ls
    where del (Just x) = case delT cs x of
                           x' | nullT x' -> Nothing
                              | otherwise -> Just x'
          del Nothing = Nothing

unionT :: Trie a -> Trie a -> Trie a
unionT x y = insertSeveralT (toListT x) y

insertSeveralT :: [(String, a)] -> Trie a -> Trie a
insertSeveralT [] x = x
insertSeveralT ((s,a):ss) x = insertSeveralT ss $ insertT s a x

delSeveralT :: [String] -> Trie a -> Trie a
delSeveralT [] x = x
delSeveralT (s:ss) x = delSeveralT ss $ delT s x
