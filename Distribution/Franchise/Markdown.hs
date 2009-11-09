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

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.Program ( withProgram )
import Distribution.Franchise.SplitFile ( splitFile )
import Distribution.Franchise.ListUtils ( stripSuffix )

-- | see <../16-markdown.html> for an explanation of how and when to
-- use 'splitMarkdown'.  Briefly, its purpose is to generate an html
-- file and possibly some other files, as described by a single
-- extended markdown file.

splitMarkdown :: String -- ^ input filename
              -> String -- ^ name of output HTML file (or empty string)
              -> C [String] -- ^ returns list of files generated
splitMarkdown fin fout0 =
    splitFile fin (\x -> (fout, unlines $ cleanMarkdown $ lines x)
                         : splitf (lines x))
    where fout = mkName fin fout0 "txt"
          splitf (x:r) =
            case tildesfn x of
              Nothing -> splitf r
              Just ("",_) -> splitf r
              Just (fn,n) ->
                  case break (n `atleasttildes`) r of
                  (fc, rest) -> (fn, unlines fc) : splitf (drop 1 rest)
          splitf [] = []
          cleanMarkdown (x:xs) =
              case tildesfn x of
                Just (fn,n) -> ("#### "++fn) : indentnext xs
                    where indentnext (z:zs) =
                              if n `atleasttildes` z
                              then cleanMarkdown zs
                              else ("    "++z) : indentnext zs
                          indentnext [] = []
                Nothing -> x : cleanMarkdown xs
          cleanMarkdown [] = []
          tildelen x = length $ takeWhile (=='~') x
          tildesfn x = do let fn = dropWhiteAndBraces $ dropWhile (=='~') x
                              n = tildelen x
                          if n >= 4 then Just (fn, n) else Nothing
          n `atleasttildes` x = tildelen x >= n
          dropWhiteAndBraces (' ':x) = dropWhiteAndBraces x
          dropWhiteAndBraces ('\t':x) = dropWhiteAndBraces x
          dropWhiteAndBraces ('{':x) = dropWhiteAndBraces $
                                       drop 1 $ dropWhile (/='}') x
          dropWhiteAndBraces x = x

mkName :: String -> String -> String -> String
mkName old "" suff =
    case stripSuffix ".in" old of
    Just new -> new
    Nothing -> if '.' `elem` old
               then reverse (dropWhile (/='.') $ reverse old)++suff
               else old++'.':suff
mkName _ new _ = new

-- | markdownToHtml defines a rule for converting a markdown file into an
-- html file.

markdownToHtml :: String -- ^ name of CSS file
               -> String -- ^ name of markdown file
               -> String -- ^ name of HTML output file (or empty string)
               -> C String -- ^ returns name of output file
markdownToHtml cssfile fin fout =
    withProgram "markdown" ["hsmarkdown"] $ \markdown ->
    do withd <- rememberDirectory
       let makehtml = withd $ do x <- cat fin
                                 putS $ "["++markdown++"] "++fin
                                 html <- systemOut markdown [fin]
                                 mkFile htmlname $
                                        unlines [htmlHead cssfile x,
                                                 html,htmlTail]
           htmlname = mkName fin fout "html"
       addTarget $ [htmlname] :< [fin,cssfile]
           |<- defaultRule { make = const makehtml }
       return htmlname

-- | markdownStringToHtml accepts the actual markdown content as a string
-- intput, rather than a filename, and it returns the html contents, rather
-- than creating a file, but is otherwise quite similar to markdownToHtml.

markdownStringToHtmlString :: String -- ^ name of CSS file
                           -> String -- ^ markdown content
                           -> C String -- ^ HTML content
markdownStringToHtmlString cssfile mkdn =
    withProgram "markdown" ["hsmarkdown"] $ \markdown ->
    do html <- systemInOut markdown [] mkdn
       return $ unlines [htmlHead cssfile mkdn,html,htmlTail]

htmlHead :: String -> String -> String
htmlHead css x =
    unlines ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"",
             " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n",
             "<html xml:lang=\"en-US\" lang=\"en-US\">",
             "<head>",
             unwords ["<title>",
                      concat $ take 1 $ filter (not . null) $ lines x,
                      "</title>"],
             "<link rel=\"stylesheet\" type=\"text/css\" href=\""++css++"\" />",
             "</head>",
             "<body>"]

htmlTail :: String
htmlTail = unlines ["</body>",
                    "</html>"]
