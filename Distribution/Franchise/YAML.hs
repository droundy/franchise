module Distribution.Franchise.YAML ( Node(..), ToNode(..),
                                     showYAML, readYAML ) where

import Data.Char
import Distribution.Franchise.Trie
import Distribution.Franchise.ListUtils ( stripPrefix )

data Node = Leaf String | List [Node] | Map (Trie Node) | Null
            deriving ( Eq )

class ToNode a where
    toNode :: a -> Node
    toString :: [a] -> Maybe String
    toString _ = Nothing

instance ToNode Node where
    toNode x = x

instance ToNode Char where
    toNode c = Leaf [c]
    toString = Just

instance ToNode a => ToNode [a] where
    toNode ns = maybe (List $ map toNode ns) Leaf $ toString ns

instance ToNode a => ToNode (Trie a) where
    toNode ns = Map $ toNode `fmap` ns

instance ToNode a => ToNode (Maybe a) where
    toNode Nothing = Null
    toNode (Just x) = toNode x

instance (ToNode a,ToNode b) => ToNode (Either a b) where
    toNode (Left  x) = toNode x
    toNode (Right x) = toNode x

instance ToNode Bool where
    toNode True  = Leaf "true"
    toNode False = Leaf "false"

instance ToNode () where
    toNode () = Null

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

showYAML :: ToNode a => a -> String
showYAML n = dumpNode (toNode n)

showString' :: String -> String -> String
showString' a b = if all isGood a then a ++ b else '"':f a b where
    f [] y = '"':y
    f (x:xs) ys |  isQuoteGood x = x:f xs ys
                | otherwise  = '\\':x:f xs ys
    isQuoteGood x = isGood x || isSpace x || x `elem` "!@#$%^&*(){}/"

isGood :: Char -> Bool
isGood x = isAlphaNum x || x `elem` "_-.@/"

readYAML :: String -> Node
readYAML "null\n" = Null
readYAML string0 = rn $ lines string0
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
          nlist ("-":r) = case splitByPrefix " " r of
                            (a,b) -> rn a : nlist b
          nlist (('-':' ':v):r) = rn [v] : nlist r
          nlist (x:_) = error ("bad yaml list element: "++x)
          nlist [] = []
          nmap (x:xs) = case break (==':') x of
                          (k,':':' ':v) -> (k, rn [v]) : nmap xs
                          (k,":") -> case splitByPrefix " " xs of
                                       (a,b) -> (k, rn a) : nmap b
                          (a,b) -> error $ unlines ["bad yaml!",a,b]
          nmap [] = []

splitByPrefix :: String -> [String] -> ([String], [String])
splitByPrefix x (l:ls) =
    case stripPrefix x l of
      Just l' -> case splitByPrefix x ls of
                   (a,b) -> (l':a,b)
      Nothing -> ([],l:ls)
splitByPrefix _ [] = ([],[])
