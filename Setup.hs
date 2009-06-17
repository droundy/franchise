#!/usr/bin/runhaskell
import Distribution.Franchise
import Data.List ( sort, isSuffixOf, isPrefixOf )
import System.Exit ( ExitCode(..) )

configure = do copyright "Copyright 2008 David Roundy"
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
               checkOnce "getEnvironment works" $
                   do mkFile "testenv.hs" $
                             unlines ["import System.Environment",
                                      "main = do getEnvironment",
                                      "          putStrLn \"System.Environment.getEnvironment works\""]
                      system "runghc" ["testenv.hs"]
                      define "GETENVIRONMENTWORKS"
               rm_rf "testenv.hs"
               setOnce "default ghc flags" $ ghcFlags ["-threaded","-O2","-Wall"]

main = build [configurableProgram "shell" "bash" ["shsh","sh"]] $
       do configure
          autoVersion Numbered >>= (defineAs "VERSION" . show)
          releaseDescription Numbered >>= (defineAs "FRANCHISE_VERSION" . show)
          buildDoc
          let exported = ["Distribution.Franchise", "Distribution.Franchise.V1"]
          p <- package "franchise" exported []
          cabal "franchise" exported
          darcsDist "franchise" ["franchise.cabal"]
          e <- executable "enfranchise" "enfranchise.hs" []
          enforceAllPrivacy
          return (p++e)

buildDoc = do rm_rf "doc/tests"
              addExtraData "haddock-directory" "doc/manual/haddock"
              addTarget $ ["*webpage*"] :< ["*manual*","index.html"] |<- defaultRule
              markdownToHtml "doc/doc.css" "doc/home.txt" "index.html"
              alltests <- mapDirectory buildOneDoc "doc"
              here <- pwd
              beginTestWith $ -- make a local install of franchise for test
                      do setEnv "HOME" (here++"/doc/tests")
                         setEnv "PREFIX" (here++"/doc/tests/local")
                         let pfile = here++"/doc/tests/local/ghc-package.conf"
                         addToGhcPath pfile
                         setEnv "FRANCHISE_GHC_PACKAGE_CONF" pfile
                         installPackageInto "franchise" (here++"/doc/tests/local/lib")
              test $ concatMap snd alltests
              withDirectory "doc" $ do buildIndex (concatMap fst alltests)
                                       htmls <- concat `fmap` mapM (\i -> markdownToHtml "../doc.css" i "")
                                                                   (concatMap fst alltests)
                                       addTarget $ ["*manual*","*html*"] :<
                                                     ("*haddock*":"manual/index.html":htmls) |<- defaultRule
    where buildOneDoc f | not (".txt.in" `isSuffixOf` f) = return ([],[])
          buildOneDoc f = do tests0@(txtf:_) <- splitMarkdown f ("manual/"++take (length f-3) f)
                             let tests = map splitPath $
                                         filter (".sh" `isSuffixOf`) $
                                         filter ("tests/" `isPrefixOf`) tests0
                             let mktest (d,t) =
                                     do withDirectory d $ testC t $
                                            do sh <- configuredProgram "shell"
                                               ec <- systemOutErrToFile sh [t] (t++".out")
                                               out <- cat (t++".out")
                                               case ec of
                                                 ExitSuccess ->
                                                     putV $ unlines $ map (\l->('|':' ':l)) $ lines out
                                                 _ -> fail $ show ec++"\n"++out
                                        return t
                             ts <- mapM mktest tests
                             return ([txtf],ts)
          buildIndex inps =
                  do withd <- rememberDirectory
                     let mklink mkdnf = do title <- (head . filter (not . null) . lines) `fmap` cat mkdnf
                                           return $ '[':title++"]("++
                                                  drop 7 (take (length mkdnf-4) mkdnf)++".html)\n"
                         makeindex _ = withd $
                                       do indhead <- cat "manual.txt"
                                          links <- mapM mklink $ sort inps
                                          html <- markdownStringToHtmlString "../doc.css" $
                                                  indhead ++ "\n\n"++unlines links
                                          mkFile "manual/index.html" html
                     addTarget $ ["manual/index.html"] :< ("manual.txt":inps)
                         |<- defaultRule { make = makeindex }
