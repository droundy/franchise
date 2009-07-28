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

module Distribution.Franchise.HaskellPolicy
    ( enforceLineLength, enforceNoTabs,
      enforceAllPrivacy, enforceModulePrivacy ) where

import Data.Maybe ( catMaybes )
import Data.List ( isPrefixOf, isSuffixOf )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Trie ( keysT, toListT )
import Distribution.Franchise.Buildable ( getTarget )
import Distribution.Franchise.StringSet ( toListS, unionallS )
import Distribution.Franchise.Util ( cat )


-- | Enforce a line-length policy.

enforceLineLength :: Int -> C ()
enforceLineLength l =
    do badfs <- haskellSource >>= mapM checkLength
       case concat badfs of
         [] -> return ()
         [f] -> fail ("Long lines found in "++f)
         fs -> fail ("Long lines found in files "++unwords fs)
    where checkLength :: FilePath -> C [String]
          checkLength f =
              do x <- cat f `catchC` \_ -> return []
                 if any (> l) $ map length $ lines x
                   then return [f]
                   else return []

sources :: C [FilePath]
sources = (toListS . unionallS . map dependencies . map snd . toListT)
          `fmap` getTargets

haskellSource :: C [FilePath]
haskellSource = filter isHaskell `fmap` sources

isHaskell :: FilePath -> Bool
isHaskell x | ".preproc/" `isPrefixOf` x = False
isHaskell x = "hs" `isSuffixOf` x || ".hsc" `isSuffixOf` x ||
              "hs.in" `isSuffixOf` x


-- | Enforce a policy of no-tabs-in-haskell-source.  I highly
-- recommend this.  Of course, you may only wish to check this in your
-- test suite, to reduce the overhead on each compile.

enforceNoTabs :: C ()
enforceNoTabs =
    do badfs <- haskellSource >>= mapM checkTabs
       case concat badfs of
         [] -> return ()
         [f] -> fail ("Tabs found in "++f)
         fs -> fail ("Tabs found in files "++unwords fs)
    where checkTabs :: FilePath -> C [String]
          checkTabs f = do x <- cat f `catchC` \_ -> return ""
                           if '\t' `elem` x
                              then return [f]
                              else return []

-- | Enforce module privacy.  For details, see
--   <../13-enforcePrivacy.html>

enforceAllPrivacy :: C ()
enforceAllPrivacy =
    do ts <- getTargets
       let ms = catMaybes $ map sourceToModule $ toListS $ keysT ts
       enforceModulePrivacy ms

-- | Enforce the privacy of some modules.  For details, see
--   <../13-enforcePrivacy.html>

enforceModulePrivacy :: [String] -> C ()
enforceModulePrivacy modules =
    do ts <- getTargets
       mapM_ emp $ toListS $ keysT ts
    where emp :: String -> C ()
          emp x = do yy <- getTarget x
                     let ds = maybe [] (toListS . dependencies) yy
                     case sourceToModule x of
                       Just m -> checkM m modules
                                 (catMaybes $ map sourceToModule ds)
                       Nothing -> return ()
          checkM :: String -> [String] -> [String] -> C ()
          checkM x [] _ = putV $ "No problem with "++x
          checkM x (m:ms) ds
              | not ((m++".") `isPrefixOf` x) && not (m==x) =
                  case filter (isPrefixOf (m++".")) ds of
                    [] -> do --putS $ m ++ " is not a prefix of "++unwords ds
                             checkM x ms ds
                    bad -> fail ("Module "++x++" illegally imports "++
                                 unwords bad)
              | otherwise = do --putS $ m ++ " is prefix of "++x
                               checkM x ms ds

sourceToModule :: FilePath -> Maybe String
sourceToModule "" = Nothing
sourceToModule (x:xs)
    | x `elem` ['A'..'Z'] =
        case reverse xs of
          'i':'h':'.':rxs -> Just $ x:map todots (reverse rxs)
          _ -> Nothing
    | x `elem` ['a'..'z'] = sourceToModule $ drop 1 $ dropWhile (/= '/') xs
    | otherwise = Nothing
    where todots '/' = '.'
          todots c = c
