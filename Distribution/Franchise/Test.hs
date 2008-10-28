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

module Distribution.Franchise.Test ( test, testOne )
    where

import System.Exit ( ExitCode(..) )

import Distribution.Franchise.Buildable
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Util

-- | Create a build target for test suites.

testOne :: String -> String -> C String
testOne r f = do putV $ "defining test "++r++" "++f
                 addTarget $ [f++".output"] :< []
                           -- no dependencies, so it'll get automatically run
                           |<- defaultRule { make = const runtest }
                 return $ f++".output"
    where runtest = do (ec,out) <- quietly $ systemOutErr r [f]
                       writeF (f++".output") out
                       putV $ unlines $ map (\l->('|':' ':l)) $ lines out
                       case ec of
                         ExitSuccess -> putS $ pad ("testing "++f)++" ok"
                         _ -> do putS $ pad ("testing "++f)++" FAILED"
                                 fail out

pad :: String -> String
pad x = if length x < 65
        then x++take (65 - length x) (repeat '.')
        else x

test :: [String] -> C ()
test ts0 = addTarget $ ["test"] :< [] |<- defaultRule { make = \_ -> runtests 0 0 ts0 }
    where runtests :: Int -> Int -> [String] -> C ()
          runtests npassed 0 [] = putS $ "All "++show npassed++" tests passed!"
          runtests 0 nfailed [] = do putS $ "All "++show nfailed++" tests FAILED!"
                                     fail "tests failed!"
          runtests npassed nfailed [] = do putS $ show nfailed++"/"++
                                                show (npassed+nfailed)++" tests FAILED!"
                                           fail "tests failed!"
          runtests np nfailed (t:ts) =
             (do quietly $ build' CannotModifyState t
                 putS $ pad t++" ok"
                 runtests (np+1) nfailed ts)
             `catchC` (\e -> do putS $ pad t++" FAILED!"
                                putV $ unlines $ map (\l->('|':' ':l)) $ lines e
                                runtests np (nfailed+1) ts)
