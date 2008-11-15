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
module Distribution.Franchise.Markdown
    ( splitMarkdown, markdownToHtml, markdownStringToHtmlString )
        where

import Data.List ( isPrefixOf )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.Program ( withProgram )
import Distribution.Franchise.SplitFile ( splitFile )
import Distribution.Franchise.ListUtils ( stripPrefix )

-- | splitMarkdown reads its first argument, which is presumed to be
-- marked-up markdown, and generates any files indicated in the contents.
-- It also outputs a cleaned version of the file that is standard markdown
-- and can be processed by markdown (or pandoc).
--
-- If the output filename is an empty string, then the output filename is
-- generated from the input filename.

splitMarkdown :: String -- ^ input filename
              -> String -- ^ output filename
              -> C [String] -- ^ returns list of files generated
splitMarkdown fin fout =
    splitFile fin (\x -> (fout, unlines $ concatMap purge $ lines x):splitf (lines x))
    where purge l | "...." `isPrefixOf` l = []
                  | otherwise = case stripPrefix "file: " l of
                                Just fn -> ['*':fn++":*",""] -- need blank line to get code mode
                                Nothing -> [l]
          splitf (x:r) =
            case stripPrefix "file: " x of
              Nothing -> splitf r
              Just fn -> case break (\l -> not $ "    " `isPrefixOf` l
                                              || "...." `isPrefixOf` l) r of
                           (fc, rest) -> (fn, unlines $ map (drop 4) fc) : splitf rest
          splitf [] = []

-- | markdownToHtml defines a rule for converting a markdown file into an
-- html file.

markdownToHtml :: String -> String -> String -> C [String]
markdownToHtml cssfile fin fout =
    withProgram "markdown" ["hsmarkdown"] $ \markdown ->
    do withd <- rememberDirectory
       x <- cat fin
       let makehtml = withd $ do putS $ "["++markdown++"] "++fin
                                 html <- systemOut markdown [fin]
                                 mkFile htmlname $
                                        unlines [htmlHead cssfile x,html,htmlTail]
           htmlname = case fout of
                      "" -> if '.' `elem` fin
                            then reverse (dropWhile (/='.') $ reverse fin) ++ "html"
                            else fin++".html"
                      _ -> fout
       addTarget $ [htmlname] :< [fin]
           |<- defaultRule { make = const makehtml }
       return [htmlname]

-- | markdownStringToHtml accepts the actual markdown content as a string
-- intput, rather than a filename, and it returns the html contents, rather
-- than creating a file, but is otherwise quite similar to markdownToHtml.

markdownStringToHtmlString :: String -> String -> C String
markdownStringToHtmlString cssfile mkdn =
    withProgram "markdown" ["hsmarkdown"] $ \markdown ->
    do html <- systemInOut markdown [] mkdn
       return $ unlines [htmlHead cssfile mkdn,html,htmlTail]

htmlHead :: String -> String -> String
htmlHead css x = unlines ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"",
                          " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n",
                          "<html xml:lang=\"en-US\" lang=\"en-US\">",
                          "<head>",
                          unwords ["<title>",head $ filter (not . null) $ lines x,"</title>"],
                          "<link rel=\"stylesheet\" type=\"text/css\" href=\""++css++"\" />",
                          "</head>",
                          "<body>"]

htmlTail :: String
htmlTail = unlines ["</body>",
                    "</html>"]
