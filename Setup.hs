#!/usr/bin/runhaskell
import Distribution.Franchise
import Data.List ( isSuffixOf, isPrefixOf )

configure = do copyright "David Roundy"
               license "BSD3"
               addExtraData "category" "Distribution"
               addExtraData "synopsis"
                   "A package for configuring and building Haskell software"
               addExtraData "description" $ unlines
                   ["",
                    "        Franchise is an easy-to use package for building Haskell",
                    "        software.  Unlike Cabal, you aren't required to track every",
                    "        possible dependency in every possible build condition.  In",
                    "        addition, you are not required to use an external tool such as",
                    "        autoconf in order to configure the build based on which",
                    "        packages, libraries and tools are present.",
                    "",
                    "        Note: the cabal dependencies are autogenerated, and approximate."]
               ghcFlags ["-threaded","-O2","-Wall"]

main = build [] configure $ do -- versionFromDarcs doesn't go in configure
                               -- because we want to rerun it with each
                               -- build rather than waiting for the user to
                               -- run Setup.hs configure again.
                               versionFromDarcs
                               buildDoc
                               darcsDist "franchise" ["franchise.cabal"]
                               package "franchise" ["Distribution.Franchise"] []

buildDoc = do alltests <- mapDirectory buildOneDoc "doc"
              test $ concat alltests
    where buildOneDoc f = do tests0 <- splitFile f (splitf f)
                             let tests = map (drop 6) $
                                         filter (".sh" `isSuffixOf`) $
                                         filter ("tests/" `isPrefixOf`) tests0
                             withDirectory "tests" $
                                           mapM (testOne "bash") tests
          splitf f x = case splitOn "\\begin{file}{" x of
                       Nothing -> []
                       Just (_,after) ->
                         case splitOn "}\n" after of
                         Nothing -> [(f++".error", "Parse failure on:\n"++after)]
                         Just (fn,after2) ->
                           case splitOn "\\end{file}" after2 of
                           Nothing -> [(f++".error", "Parse failure on:\n"++after2)]
                           Just (contents,after3) -> (fn,contents): splitf f after3
          splitOn x (c:cs) = case stripPrefix x (c:cs) of
                             Just cs' -> Just ("",cs')
                             Nothing -> do (cs1,cs2) <- splitOn x cs
                                           Just (c:cs1,cs2)
          splitOn _ "" = Nothing
          stripPrefix [] ys = Just ys
          stripPrefix (x:xs) (y:ys) | x == y = stripPrefix xs ys
          stripPrefix _ _ = Nothing
