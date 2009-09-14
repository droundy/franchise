module Distribution.Franchise.YAML ( Node(..), YAML(..),
                                     getScalar, getMapping, getMappingValues,
                                     getSequence,
                                     showYAML, readYAML ) where

import Data.Maybe ( catMaybes )
import Data.Char
import Distribution.Franchise.StringSet ( StringSet, toListS, fromListS )
import Distribution.Franchise.Trie
import Distribution.Franchise.ListUtils ( stripPrefix )

data Node = Leaf String | List [Node] | Map (Trie Node) | Null
            deriving ( Eq )

class YAML a where
    toNode :: a -> Node
    fromNode :: Node -> Maybe a
    toString :: [a] -> Maybe String
    toString _ = Nothing
    fromString :: String -> [a]
    fromString _ = []

instance YAML Node where
    toNode x = x
    fromNode x = Just x

instance YAML Char where
    toNode c = Leaf [c]
    fromNode (Leaf [c]) = Just c
    fromNode _ = Nothing
    toString = Just
    fromString s = s

instance YAML a => YAML [a] where
    toNode ns = maybe (List $ map toNode ns) Leaf $ toString ns
    fromNode (List ns) = Just $ catMaybes $ map fromNode ns
    fromNode (Leaf x) = Just $ fromString x
    fromNode _ = Just []

instance YAML a => YAML (Trie a) where
    toNode ns = Map $ toNode `fmap` ns
    fromNode (Map t) = Just $ catMaybesT $ fmap fromNode t
    fromNode _ = Just emptyT

instance YAML StringSet where
    toNode = toNode . toListS
    fromNode s = fromListS `fmap` fromNode s

instance YAML a => YAML (Maybe a) where
    toNode Nothing = Null
    toNode (Just x) = toNode x
    fromNode Null = Just Nothing
    fromNode n = case fromNode n of
                   Just x -> Just (Just x)
                   Nothing -> Nothing

instance (YAML a, YAML b) => YAML (Either a b) where
    toNode (Left  x) = Map $ singleT "Left" $ toNode x
    toNode (Right x) = Map $ singleT "Right" $ toNode x
    fromNode (Map t) = case toListT t of
                         [("Left",n)] -> Left `fmap` fromNode n
                         [("Right",n)] -> Right `fmap` fromNode n
                         _ -> Nothing
    fromNode _ = Nothing

instance (YAML a, YAML b) => YAML (a,b) where
    toNode (a,b) = List [toNode a, toNode b]
    fromNode n = do [a,b] <- fromNode n
                    aa <- fromNode a
                    bb <- fromNode b
                    Just (aa,bb)

instance YAML Bool where
    toNode True  = Leaf "true"
    toNode False = Leaf "false"
    fromNode (Leaf "true") = Just True
    fromNode (Leaf "false") = Just False
    fromNode _ = Nothing

instance YAML () where
    toNode () = Null
    fromNode Null = Just ()
    fromNode _ = Nothing

dumpNode :: Node -> String
dumpNode node0 = f False 0 node0 "\n" where
    f nn _ Null     = foo nn . showString "null"
    f nn _ (Leaf x) = foo nn . showString' x
    f nn i (List ns) =
        nl nn [ g i . showString "-" . f True (i + 1) n | n <- ns ]
    f nn i (Map ns) =
        nl nn [ g i . showString x . showString ":" . f True (i + 1) y
                    | (x,y) <- toListT ns ]
    g i = showString $ replicate i ' '
    nl _ [] = id
    nl nn xs = (if nn then ('\n':) else id) .
               foldr1 (\x y -> x . showChar '\n' . y ) xs
    foo True  = showChar ' '
    foo False = id

showYAML :: YAML a => a -> String
showYAML n = dumpNode (toNode n)

showString' :: String -> String -> String
showString' a b = if all isGood a then a ++ b else '"':f a b where
    f [] y = '"':y
    f (x:xs) ys |  isQuoteGood x = x:f xs ys
                | otherwise  = '\\':x:f xs ys
    isQuoteGood x = isGood x || isSpace x || x `elem` "!@#$%^&*(){}/"

isGood :: Char -> Bool
isGood x = isAlphaNum x || x `elem` "_-.@/"

readYAML :: YAML a => String -> Maybe a
readYAML s = fromNode $ readYAML' s

readYAML' :: String -> Node
readYAML' "null\n" = Null
readYAML' string0 = rn $ lines string0
    where rn [] = Null
          rn ("-":r) = List $ nlist ("-":r)
          rn (('-':' ':v):r) = List $ nlist (('-':' ':v):r)
          rn [l] | all isGood l = Leaf l
          rn ['"':r] = Leaf $ unquote $ init r
              where unquote ('\\':x:xs) = x : unquote xs
                    unquote (x:xs) = x : unquote xs
                    unquote "" = ""
          rn (x:xs) | ':' `elem` x = Map $ fromListT $ nmap (x:xs)
          rn ls = error $ unlines $ "nasty yaml:\n" : ls
          nlist ("-":r) = case splitByIndentation r of
                            (a,b) -> rn a : nlist b
          nlist (('-':' ':v):r) = case splitByIndentation (v':r) of
                                    (a,b) -> rn a : nlist b
              where v' = ' ':' ':v
          nlist (x:_) = error ("bad yaml list element: "++x)
          nlist [] = []
          nmap (x:xs) = case break (==':') x of
                          (k,':':' ':v) ->
                              case splitByIndentation ((' ':' ':v):xs) of
                                (a,b) -> (k, rn a) : nmap b
                          (k,":") -> case splitByIndentation xs of
                                       (a,b) -> (k, rn a) : nmap b
                          (a,b) -> error $ unlines ["bad yaml!",a,b]
          nmap [] = []

splitByIndentation :: [String] -> ([String], [String])
splitByIndentation [] = ([],[])
splitByIndentation (x:xs) = splitByPrefix (takeWhile (==' ') x) (x:xs)

splitByPrefix :: String -> [String] -> ([String], [String])
splitByPrefix x (l:ls) =
    case stripPrefix x l of
      Just l' -> case splitByPrefix x ls of
                   (a,b) -> (l':a,b)
      Nothing -> ([],l:ls)
splitByPrefix _ [] = ([],[])

-- | read a node that we expect to be a scalar value.  This supports
-- the \"tag:yaml.org,2002:value\" type, which is just a convention
-- that a key of \"=\" indicates a scalar value.  It's a trick that
-- allows us to switch a scalar to a mapping without breaking
-- applications that already know how to read this format.

getScalar :: Node -> Maybe String
getScalar (Leaf s) = Just s
getScalar (Map t) = lookupT "=" t >>= getScalar
getScalar _ = Nothing

-- | find an element from a mapping.

getMapping :: String -> Node -> Maybe Node
getMapping k (Map t) = lookupT k t
getMapping "=" (Leaf x) = Just $ Leaf x
getMapping _ _ = Nothing

-- | read a mapping into a list.

getMappingValues :: Node -> Maybe [Node]
getMappingValues (Map t) = Just $ map snd $ toListT t
getMappingValues _ = Nothing

-- | read a sequence.

getSequence :: Node -> Maybe [Node]
getSequence (List ns) = Just ns
getSequence _ = Nothing
