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
module Distribution.Franchise.Test ( test, testOne, prepareForTest, beginTestWith )
    where

import System.Exit ( ExitCode(..) )
import Data.List ( isPrefixOf )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util
import Distribution.Franchise.ListUtils ( stripPrefix )

-- | Create a build target for test suites.

testOne :: String -> String -> C String
testOne r f = do withcwd <- rememberDirectory
                 let testname = "*"++tn f++"*"
                     tn x = maybe x tn $ stripPrefix "../" x
                 addTarget $ [testname] :< [phony "build", phony "prepare-for-test"]
                           -- no dependencies, so it'll get automatically run
                           |<- defaultRule { make = const $ runtest withcwd }
                 return $ testname
    where runtest withcwd =
              do begin <- maybe (return ()) rule `fmap` getTarget "begin-test"
                 (ec,out) <- withcwd $ do begin
                                          (ec,out) <- quietly $ withcwd $ systemOutErr r [f]
                                          writeF (f++".output") out
                                          return (ec,out)
                 putV $ unlines $ map (\l->('|':' ':l)) $ lines out
                 case ec of
                   ExitSuccess -> if "fail" `isPrefixOf` f
                                  then putS $ pad ("testing "++f)++" unexpectedly passed!"
                                  else putS $ pad ("testing "++f)++" ok"
                   _ -> do putS $ pad ("testing "++f)++" FAILED"
                           fail out

pad :: String -> String
pad x0 = if length x < 65
         then x++take (65 - length x) (repeat '.')
         else x
    where x = case x0 of
              ('*':r) -> case reverse r of
                         ('*':rr) -> reverse rr
                         _ -> x0
              _ -> x0

data TestResult = Passed | Failed | Surprise | Expected

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
                                               runtests 0 0 0 0 ts0 }
    where 
          runtests :: Int -> Int -> Int -> Int -> [String] -> C ()
          runtests npassed 0 0 0 [] = putAll npassed "test" "passed!"
          runtests npassed oddpass expectedfail 0 [] =
              do putNonZero expectedfail "test" "failed as expected."
                 putNonZero oddpass "test" " unexpectedly passed!"
                 putCountable npassed "test" "passed."
          runtests 0 0 0 nfailed [] = do putAll nfailed "test" "FAILED!"
                                         fail "tests failed!"
          runtests npassed oddpass expectedfail nfailed [] =
              do putS $ show nfailed++"/"++show (npassed+nfailed)++" tests FAILED!"
                 putNonZero expectedfail "test" "failed as expected."
                 putNonZero oddpass "test" "unexpectedly passed!"
                 fail "tests failed!"
          runtests np oddpasses expectedfail nfailed (t:ts) =
              do tOK <- runSingleTest t
                 case tOK of
                   Passed -> runtests (np+1) oddpasses expectedfail nfailed ts
                   Failed -> runtests np oddpasses expectedfail (nfailed+1) ts
                   Surprise -> runtests np (oddpasses+1) expectedfail nfailed ts
                   Expected -> runtests np oddpasses (expectedfail+1) nfailed ts
          runSingleTest t =
              do quietly $ build' CannotModifyState t
                 if "fail" `isPrefixOf` t || "*fail" `isPrefixOf` t
                   then do putS $ pad t++" unexpectedly succeeded!"
                           return Surprise
                   else do putS $ pad t++" ok"
                           return Passed
             `catchC` \e ->
                 do if "fail" `isPrefixOf` t || "*fail" `isPrefixOf` t
                      then do putS $ pad t++" failed as expected."
                              putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                              return Expected
                      else do putS $ pad t++" FAILED!"
                              putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                              return Failed

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
