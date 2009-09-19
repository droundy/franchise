#!/usr/bin/runhaskell
import Distribution.Franchise.V1
import Data.List ( sort, isSuffixOf, isPrefixOf )
import System.Exit ( ExitCode(..) )

main = build [configurableProgram "shell" "bash" ["shsh","sh"]] $
  do "copyright" <<= "Copyright 2008 David Roundy"
     "license" <<= "BSD3"
     "category" <<= "Distribution"
     "synopsis" <<= "A package for configuring and building Haskell software"
     "description" <<= (unlines
         ["",
          "        Franchise is an easy-to use package for building Haskell",
          "        software.  Unlike Cabal, you aren't required to track every",
          "        possible dependency in every possible build condition.  In",
          "        addition, you are not required to use tools such as",
          "        autoconf in order to configure the build based on which",
          "        packages, libraries and tools are present."])
     checkOnce "getEnvironment works" $ -- workaround broken getEnvironment
         do mkFile "testenv.hs" $ unlines
                ["import System.Environment",
                 "main = do getEnvironment",
                 "          putStrLn \"getEnvironment works\""]
            system "runghc" ["testenv.hs"]
            define "GETENVIRONMENTWORKS"
     rm_rf "testenv.hs"
     ghcFlags ["-threaded","-Wall"]
     autoVersion Numbered >>= (defineAs "VERSION" . show)
     releaseDescription Numbered >>= (defineAs "FRANCHISE_VERSION" . show)
     let exported = ["Distribution.Franchise", "Distribution.Franchise.V1"]
     "haddock-directory" <<= "doc/manual/haddock"
     p <- package "franchise" exported []
     cabal "franchise" exported
     buildDoc
     autoDist "franchise" ["franchise.cabal"
                          -- ,"manual"
                          ]
     executable "enfranchise" "enfranchise.hs" []
     enforceAllPrivacy
     enforceNoTabs
     enforceLineLength 80

buildDoc =
   do rm_rf "doc/tests"
      markdownToHtml "doc/doc.css" "doc/home.txt" "index.html"
      alltests <- mapDirectory buildOneDoc "doc"
      here <- pwd
      setupTestEnvironment $ -- make a local install of franchise for test
              do setEnv "HOME" (here++"/doc/tests")
                 setEnv "PREFIX" (here++"/doc/tests/local")
                 let pfile = here++"/doc/tests/local/ghc-package.conf"
                 addToGhcPath pfile
                 setEnv "FRANCHISE_GHC_PACKAGE_CONF" pfile
                 installPackageInto "franchise" (here++"/doc/tests/local/lib")
      linkcheck <- withProgram "linklint" [] $ const $ return ["check-links"]
      testSuite "test" $ linkcheck ++ concatMap snd alltests
      withDirectory "doc" $
          do buildIndex (concatMap fst alltests)
             test "check-links" $
                 do x <- systemOut "linklint" ["-xref", "-error", "/@"]
                    if "ERROR" `elem` words x
                        then fail x
                        else putS "no broken links"
             htmls <- mapM (\i -> markdownToHtml "../doc.css" i "")
                           (concatMap fst alltests)
             hadd <- withProgram "haddock" [] $ const $ return ["haddock"]
             addDependencies "html" ("manual/index.html":hadd++htmls)
      addDependencies "manual" ["html"]
      -- The following is a hack to extract part of the test suite
      -- into the documentation:
      withDirectory "doc/manual" $
          do cp "../tests/markdown/markdown-format.txt.in"
                 "markdown-format.txt.in"
             docs <- splitMarkdown "markdown-format.txt.in" ""
             hs <- mapM (\x -> markdownToHtml "../doc.css" x "") docs
             addDependencies "manual" hs
      addDependencies "webpage" ["manual","index.html"]
      withProgram "markdown" ["hsmarkdown"] $ const $
                  addDependencies "build" ["webpage"]
  where buildOneDoc f | not (".txt.in" `isSuffixOf` f) = return ([],[])
        buildOneDoc f =
            do tests0@(txtf:_)
                   <- splitMarkdown f ("manual/"++take (length f-3) f)
               let tests = filter (".sh" `isSuffixOf`) $
                           filter ("tests/" `isPrefixOf`) tests0
                   mktest dt =
                       do let d = dirname dt
                              t = basename dt
                          withDirectory d $ testScript t "shell" t
                          return t
               ts <- mapM mktest tests
               return ([txtf],ts)
        buildIndex inps =
            do let mklink mkdnf =
                       do title <- (head . filter (not . null) . lines)
                                   `fmap` cat mkdnf
                          let htmlname =
                                  drop 7 (take (length mkdnf-4) mkdnf)++".html"
                          return ('[':title++"]("++htmlname++")", htmlname)
               rule ["manual/index.html"] ("manual.txt":inps) $
                   do indhead <- cat "manual.txt"
                      links <- mapM mklink $ sort inps
                      let inps1 = filter ((`elem` words indhead) . snd) links
                          inps2 = filter ((`notElem` words indhead) . snd) links
                          fixhead [] x = x
                          fixhead ((a,b):r) x = fixhead r $ repl a b x
                          repl a b x | b `isPrefixOf` x =
                                         a ++ drop (length b) x
                          repl a b (x:xs) = x : repl a b xs
                          repl _ _ "" = ""
                      html <- markdownStringToHtmlString "../doc.css" $
                              fixhead inps1 indhead ++ "\n\n"++
                              unlines (map (++"\n") $ map fst inps2)
                      mkFile "manual/index.html" html
