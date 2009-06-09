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
module Distribution.Franchise.Test ( phonyRule, testy, test, testC, testOne, testOutput,
                                     testResultsFile,
                                     prepareForTest, beginTestWith )
    where

import System.Exit ( ExitCode(..) )
import Data.List ( isPrefixOf )
import Data.Maybe ( isJust )
import Control.Monad ( when )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.StringSet ( elemS )
import Distribution.Franchise.Parallel ( mapC )

-- | Create a phony build target, perhaps with dependencies.

phonyRule :: String -- ^ name of target
          -> [String] -- ^ list of dependencies
          -> C () -- ^ rule to build this (phony) target
          -> C ()
phonyRule n deps j =
    addTarget $ [phony n] :< deps |<- defaultRule { make = const j }

testC :: String -> C () -> C ()
testC n j = phonyRule n [phony "build", phony "prepare-for-test"] runtest
    where runtest =
              do begin <- maybe (return ()) rule `fmap` getTarget "begin-test"
                 (do begin
                     j
                     if "fail" `isPrefixOf` n
                       then putS $ pad ("testing "++n)++" unexpectedly passed!"
                       else putS $ pad ("testing "++n)++" ok")
                   `catchC` \e ->
                       do putS $ pad ("testing "++n)++" FAILED"
                          putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                          fail e

testOutput :: String -> String -> C String -> C ()
testOutput n o j = testC n runtest
    where runtest = do out <- j
                       --let nice = show
                       let nice = unlines . map (\l->('|':' ':l)) . lines
                       if out == o
                         then putV $ nice out
                         else fail $ unlines [nice out,"differs from",nice o]

testOne :: String -> String -> String -> C ()
testOne n r f = testC n runtest
    where runtest = do ec <- silently $ systemOutErrToFile r [f] (n++".out")
                       out <- cat (n++".out")
                       case ec of
                         ExitSuccess ->
                             putV $ unlines $ map (\l->('|':' ':l)) $ lines out
                         _ -> fail $ show ec++"\n"++out

pad :: String -> String
pad x0 = if length x < 65
         then x++take (65 - length x) (repeat '.')
         else x
    where x = case x0 of
              ('*':r) -> case reverse r of
                         ('*':rr) -> reverse rr
                         _ -> x0
              _ -> x0

data TestResult = Passed | Failed | Surprise | Expected deriving ( Eq )

prepareForTest :: C () -> C ()
prepareForTest initialize =
    addTarget $ [phony "prepare-for-test"] :< []
        |<- defaultRule { make = const (clearTestResults >> initialize) }

beginTestWith :: C () -> C ()
beginTestWith initialize =
    addTarget $ [phony "begin-test"] :< []
        |<- defaultRule { make = const $ unlessC (haveExtraData "began-test") $
                                         do putV "beginning test..."
                                            initialize
                                            addExtraData "began-test" "" }

test :: [String] -> C ()
test = testy "test"

testy :: String -> [String] -> C ()
testy tname ts0 =
    do begin <- maybe (return ()) rule `fmap` getTarget "begin-test"
       unlessC (isJust `fmap` getTarget "prepare-for-test") $
               addTarget $ [phony "prepare-for-test"] :< []
                   |<- defaultRule { make = const clearTestResults }
       addTarget $ [phony tname] :< [phony "is-testy", phony "build", phony "prepare-for-test"]
           |<- defaultRule { make = const $ do begin
                                               results <- mapC runSingleTest ts0
                                               announceResults tname
                                                               (length $ filter (==Passed) results)
                                                               (length $ filter (==Surprise) results)
                                                               (length $ filter (==Expected) results)
                                                               (length $ filter (==Failed) results)
                                               when (tname == "test") $
                                                    summarizeTestsIfMoreThan (length results)
                           }
    where runSingleTest t =
              do istest <- istesty t
                 if istest then build' CannotModifyState t
                           else do whenC oneJob $ putSnoln $ pad t
                                   silently $ build' CannotModifyState t
                 if "fail" `isPrefixOf` t || "*fail" `isPrefixOf` t
                   then do unlessC (istesty t) $ announceResult " unexpectedly succeeded!"
                           saveResult "pass-unexpected" t
                           return Surprise
                   else do unlessC (istesty t) $ announceResult " ok"
                           saveResult "passed" t
                           return Passed
             `catchC` \e ->
                 do if "fail" `isPrefixOf` t || "*fail" `isPrefixOf` t
                      then do unlessC (istesty t) $ announceResult " failed as expected."
                              saveResult "failure-expected" t
                              putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                              return Expected
                      else do unlessC (istesty t) $ announceResult " FAILED!"
                              saveResult "failed" t
                              putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                              return Failed
            where announceResult r = do whenC oneJob $ putS r
                                        unlessC oneJob $ putS $ pad t++r
          istesty t = maybe False (elemS "*is-testy*" . dependencies) `fmap` getTarget t

announceResults :: String -> Int -> Int -> Int -> Int -> C ()
announceResults tname npassed 0 0 0 = putAll npassed tname "passed!"
announceResults tname npassed oddpass expectedfail 0 =
    do putNonZero expectedfail tname "failed as expected."
       putNonZero oddpass tname " unexpectedly passed!"
       putCountable npassed tname "passed."
announceResults tname 0 0 0 nfailed = do putAll nfailed tname "FAILED!"
                                         fail "tests failed!"
announceResults tname npassed oddpass expectedfail nfailed =
    do putS $ show nfailed++"/"++show (npassed+nfailed)++" tests FAILED!"
       putNonZero expectedfail tname "failed as expected."
       putNonZero oddpass tname "unexpectedly passed!"
       fail "tests failed!"

summarizeTestsIfMoreThan :: Int -> C ()
summarizeTestsIfMoreThan n = do f <- maybe ".tests" id `fmap` getExtra "test-results-file"
                                failures <- lines `fmap` cat (f++"-failed")
                                unepected <- lines `fmap` cat (f++"-pass-unexpected")
                                passes <- lines `fmap` cat (f++"-passed")
                                expected <- lines `fmap` cat (f++"-failure-expected")
                                when (length failures+length passes+length expected+length unepected>n) $
                                  do putS "\nIn summary:\n"
                                     announceResults "test" (length passes) (length unepected)
                                                            (length expected) (length failures)

saveResult :: String -> String -> C ()
saveResult r t = do f <- maybe ".tests" id `fmap` getExtra "test-results-file"
                    io $ appendFile (f++'-':r) (t++"\n")

testResultsFile :: FilePath -> C ()
testResultsFile f = do putExtra "test-results-file" (Just f)
                       clearTestResults

clearTestResults :: C ()
clearTestResults = do f <- maybe ".tests" id `fmap` getExtra "test-results-file"
                      io $ writeFile (f++"-failed") ""
                      io $ writeFile (f++"-pass-unexpected") ""
                      io $ writeFile (f++"-passed") ""
                      io $ writeFile (f++"-failure-expected") ""

putAll :: Int -> String -> String -> C ()
putAll 0 noun verb = putS $ "There were no "++noun++"s... but they all "++verb
putAll 1 noun verb = putS $ "One "++noun++" "++verb
putAll 2 noun verb = putS $ "Both "++noun++"s "++verb
putAll n noun verb = putS $ "All "++show n++" "++noun++"s "++verb

putNonZero :: Int -> String -> String -> C ()
putNonZero 0 _ _ = return ()
putNonZero n noun verb = putCountable n noun verb

putCountable :: Int -> String -> String -> C ()
putCountable 0 noun verb = putS $ "No "++noun++"s "++verb
putCountable 1 noun verb = putS $ "1 "++noun++" "++verb
putCountable n noun verb = putS $ show n++" "++noun++"s "++verb
