module Main where

import Data.Maybe ( isJust, isNothing, catMaybes )
import Data.Char ( toLower )
import Data.Monoid ( Monoid, mempty )

import Distribution.Franchise

main :: IO ()
main = build [] $
       do [cabalfile] <- filter ((==".cabal") . dropWhile (/='.')) `fmap` ls "."
          putS $ "cabal file is:  "++ cabalfile
          xxx <- cat cabalfile
          putS $ unlines $ "it parses to:" : map show (parseCabal $ lines xxx)
          let cf = parseCabal $ lines xxx
              lookupField :: String -> Maybe [String]
              lookupField d = lu cf
                  where lu [] = Nothing
                        lu ((d',v):r) = if map toLower d' == map toLower d
                                        then case lu r of
                                             Just v' -> Just (v++v')
                                             Nothing -> Just v
                                        else lu r
              withToken :: Monoid a => String -> (String -> C a) -> C a
              withToken d j = case lookupField d of
                              Nothing -> do putS $ "Couldn't find "++d
                                            return mempty
                              Just v -> case reads $ unlines v of
                                        (x,_):_ -> j x
                                        _ -> j $ takeWhile (`notElem` "\r\n") $ unlines v
              withTokens :: Monoid a => String -> ([String] -> C a) -> C a
              withTokens d j =
                  case lookupField d of
                  Nothing -> do putS $ "Couldn't find "++d
                                return mempty
                  Just v -> j $ readTokens $ unlines v
              withField :: Monoid a => String -> ([String] -> C a) -> C a
              withField d j = maybe (return mempty) j $ lookupField d
          withToken "version" version
          withTokens "hs-source-dirs" $ \ds -> ghcFlags $ map ("-i"++) ds
          withTokens "extensions" $ \es -> ghcFlags $ map ("-X"++) es
          withTokens "cpp-options" ghcFlags
          withTokens "ghc-options" ghcFlags
          withField "exposed-modules" $ \ds ->
              do pn <- withToken "name" return
                 withField "description" $ addExtraData "description" . unlines
                 putS $ "found package "++pn++" exporting modules "++
                          (unwords $ words $ unlines ds)
                 package pn (words $ unlines ds) []
          withToken "main-is" $ \m ->
              executable (reverse $ drop 1 $ dropWhile (/='.') $ reverse m) m []
          return []

readTokens :: String -> [String]
readTokens "" = []
readTokens (' ':r) = readTokens r
readTokens ('\n':r) = readTokens r
readTokens ('\r':r) = readTokens r
readTokens x@('"':_) = case reads x of
                       (t,r):_ -> t : readTokens r
                       [] -> readTokens $ drop 1 x
readTokens x = takeWhile (`notElem` " \t\n\r") x :
               readTokens (dropWhile (`notElem` " \t\n\r") x)

parseCabal :: [String] -> [(String, [String])]
parseCabal = parseCabal' . filter (not . isComment)

parseCabal' :: [String] -> [(String, [String])]
parseCabal' [] = []
parseCabal' (l:lls) =
    case break (==':') $ dropWhile (==' ') l of
    (fn,':':val) ->
        case takeMore (takeWhile (==' ') l) lls of
        (vals, rest) -> (fn,dropWhile (==' ') val:vals) : parseCabal' rest
    _ -> parseCabal' lls
    where takeMore _ [] = ([],[])
          takeMore indent (x:xs) =
              case stripPrefix indent x of
              Nothing -> ([],x:xs)
              Just x' -> case break (/=' ') x' of
                         ("",_) -> ([],x:xs)
                         (indent',v1) -> case keepGoing (indent++indent') xs of
                                         (vs,r) -> (v1:vs, r)
          keepGoing indent xs =
              case break (isNothing . stripPrefix indent) xs of
              (pvs, r) -> (catMaybes $ map (stripPrefix indent) pvs, r)

isComment :: String -> Bool
isComment = isJust . stripPrefix "--" . dropWhile (==' ')
