module Distribution.Franchise.StringSet ( StringSet, emptyS, elemS, anyElemS,
                                          addS, addsS, delS, delsS, lengthS ) where

import Data.Maybe ( catMaybes )

newtype StringSet = SS [(Maybe Char, StringSet)]

lengthS (SS []) = 0
lengthS (SS ls) = sum $ map l ls
    where l (Nothing,_) = 1
          l (_, x) = lengthS x

emptyS = SS []

elemS "" (SS ls) = case lookup Nothing ls of
                   Nothing -> False
                   Just _ -> True
elemS (c:cs) (SS ls) = case lookup (Just c) ls of
                       Nothing -> False
                       Just ls' -> elemS cs ls'

anyElemS ss set = any (`elemS` set) ss

addS "" (SS ls) = case lookup Nothing ls of
                  Nothing -> SS $ (Nothing, emptyS):ls
                  Just _ -> SS ls
addS (c:cs) (SS ls) = SS $ repl ls
    where repl ((Just c', ss):r) | c == c' = (Just c', addS cs ss) : r
          repl (x:r) = x : repl r
          repl [] = [(Just c, addS cs emptyS)]

delS "" (SS ls) = SS $ filter d ls
    where d (Nothing, _) = False
          d _ = True
delS (c:cs) (SS ls) = SS $ catMaybes $ map d ls
    where d (Just c', x) | c == c' = case delS cs x of
                                     SS [] -> Nothing
                                     x' -> Just (Just c', x')
          d x = Just x
                      
                      
addsS [] x = x
addsS (s:ss) x = addsS ss $ addS s x
                      
delsS [] x = x
delsS (s:ss) x = delsS ss $ delS s x
