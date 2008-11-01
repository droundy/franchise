{-# LANGUAGE PatternGuards #-}

module Distribution.Franchise.AutoHeader ( autoHeader ) where

import Distribution.Franchise.SplitFile
import Distribution.Franchise.ConfigureState

-- |This is the split function for @splitFile@.  We need to provide the output filename
-- explicitly since it's not in the file, and we also need to provide the list of all
-- definitions since it's a pure function.
ahSplit :: FilePath -> [(String,String)] -> String -> [(FilePath,String)]
ahSplit outfile subs content = [(outfile,unlines $ map repl $ lines content)]
    where repl s | ("#undef ",ss) <- splitAt 7 s
                 , Just v <- lookup ss subs = "#define " ++ ss ++ if null v then ""
                                                                            else " " ++ v
                 | otherwise = s

autoHeader :: FilePath -> C ()
autoHeader target = do defs <- getDefinitions
                       splitFile (target++".in") (ahSplit target defs)
                       return ()
