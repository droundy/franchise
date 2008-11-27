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
module Distribution.Franchise.Test ( test, testC, testOne, testOutput,
                                     prepareForTest, beginTestWith )
    where

import System.Exit ( ExitCode(..) )
import Data.List ( isPrefixOf )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.Parallel ( mapC )

-- | Create a build target for test suites.

testC :: String -> C () -> C ()
testC n j = do withcwd <- rememberDirectory
               addTarget $ [phony n] :< [phony "build", phony "prepare-for-test"]
                   |<- defaultRule { make = const $ withcwd runtest }
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
                         _ -> fail out

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
    addTarget $ [phony "prepare-for-test"] :< [] |<- defaultRule { make = const initialize }

beginTestWith :: C () -> C ()
beginTestWith initialize =
    addTarget $ [phony "begin-test"] :< []
        |<- defaultRule { make = const $ unlessC (haveExtraData "began-test") $
                                         do putV "beginning test..."
                                            initialize
                                            addExtraData "began-test" "" }

test :: [String] -> C ()
test ts0 =
    do begin <- maybe (return ()) rule `fmap` getTarget "begin-test"
       addTarget $ [phony "test"] :< [phony "build", phony "prepare-for-test"]
           |<- defaultRule { make = const $ do begin
                                               results <- mapC runSingleTest ts0
                                               announceResults (length $ filter (==Passed) results)
                                                               (length $ filter (==Surprise) results)
                                                               (length $ filter (==Expected) results)
                                                               (length $ filter (==Failed) results) }
    where announceResults npassed 0 0 0 = putAll npassed "test" "passed!"
          announceResults npassed oddpass expectedfail 0 =
              do putNonZero expectedfail "test" "failed as expected."
                 putNonZero oddpass "test" " unexpectedly passed!"
                 putCountable npassed "test" "passed."
          announceResults 0 0 0 nfailed = do putAll nfailed "test" "FAILED!"
                                             fail "tests failed!"
          announceResults npassed oddpass expectedfail nfailed =
              do putS $ show nfailed++"/"++show (npassed+nfailed)++" tests FAILED!"
                 putNonZero expectedfail "test" "failed as expected."
                 putNonZero oddpass "test" "unexpectedly passed!"
                 fail "tests failed!"
          runSingleTest t =
              do whenC oneJob $ putSnoln $ pad t
                 silently $ build' CannotModifyState t
                 if "fail" `isPrefixOf` t || "*fail" `isPrefixOf` t
                   then do announceResult " unexpectedly succeeded!"
                           return Surprise
                   else do announceResult " ok"
                           return Passed
             `catchC` \e ->
                 do if "fail" `isPrefixOf` t || "*fail" `isPrefixOf` t
                      then do announceResult " failed as expected."
                              putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                              return Expected
                      else do announceResult " FAILED!"
                              putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                              return Failed
            where announceResult r = do whenC oneJob $ putS r
                                        unlessC oneJob $ putS $ pad t++r

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
