{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module Distribution.Franchise.AutoHeader ( autoHeader ) where

import Distribution.Franchise.SplitFile
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.GhcState ( getDefinitions )
import Distribution.Franchise.ListUtils ( stripPrefix )

-- |This is the split function for @splitFile@.  We need to provide
-- the output filename explicitly since it's not in the file, and we
-- also need to provide the list of all definitions since it's a pure
-- function.
ahSplit :: FilePath -> [(String,String)] -> String -> [(FilePath,String)]
ahSplit outfile subs content = [(outfile,unlines $ map repl $ lines content)]
    where repl s = maybe s id $
                   do ss <- stripPrefix "#undef " s
                      v <- lookup ss subs
                      Just $ "#define " ++ ss ++ if null v
                                                 then ""
                                                 else " " ++ v

autoHeader :: FilePath -> C ()
autoHeader target = do defs <- getDefinitions
                       splitFile (target++".in") (ahSplit target defs)
                       return ()
