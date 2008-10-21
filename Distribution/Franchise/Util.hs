{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Distribution.Franchise.Util ( system, systemV, systemOut, systemErr,
                                     cd, cat, endsWithOneOf )
    where

import System.Directory ( setCurrentDirectory )
import System.Exit ( ExitCode(..) )
import System.Process ( ProcessHandle, runInteractiveProcess,
                        waitForProcess, getProcessExitCode )
import Control.Concurrent ( threadDelay, rtsSupportsBoundThreads )
import System.IO ( hGetContents )
import Control.Concurrent ( forkIO )
import Data.List ( isSuffixOf )

import Distribution.Franchise.ConfigureState

-- | A version of waitForProcess that is non-blocking even when linked with
-- the non-threaded runtime.

--
-- waitForProcess uses a very hokey heuristic to try to avoid burning too
-- much CPU time in a busy wait, while also not adding too much extra
-- latency.

waitForProcessNonBlocking :: ProcessHandle -> IO ExitCode
waitForProcessNonBlocking = if rtsSupportsBoundThreads
                            then waitForProcess
                            else wfp 0
    where wfp n pid = do mec <- getProcessExitCode pid
                         case mec of
                           Just ec -> return ec
                           Nothing -> do threadDelay n
                                         putStrLn $ "Waiting for process... " ++ show n
                                         wfp (min 100000 (n+1+n`div`4)) pid

-- | Checks if a string ends with any given suffix
endsWithOneOf :: [String] -- ^ List of strings to check
              -> String   -- ^ String to check against
              -> Bool
endsWithOneOf xs y = any (\x -> x `isSuffixOf` y) xs

-- | Run a command
system :: String   -- ^ Command
       -> [String] -- ^ Arguments
       -> C ()
system c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                   out <- io $ hGetContents o
                   err <- io $ hGetContents e
                   -- now we ensure that out and err are consumed, so that
                   -- the code we're running won't hang waiting for its
                   -- output (or error) to be consumed.
                   io $ forkIO $ seq (length out) $ return ()
                   io $ forkIO $ seq (length err) $ return ()
                   let cl = unwords (('[':c++"]"):drop (length args-1) args)
                       clv = unwords (c:args)
                   putSV (cl++'\n':out++err) (clv++'\n':out++err)
                   ec <- io $ waitForProcessNonBlocking pid
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure 127 -> fail $ c ++ ": command not found"
                     ExitFailure ecode -> fail $ c ++ " failed with exit code "++show ecode

-- | Run a command silently, unless we're verbose
systemV :: String   -- ^ Command
        -> [String] -- ^ Arguments
        -> C ()
systemV c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                    out <- io $ hGetContents o
                    err <- io $ hGetContents e
                    -- now we ensure that out and err are consumed, so that
                    -- the code we're running won't hang waiting for its
                    -- output (or error) to be consumed.
                    io $ forkIO $ seq (length out) $ return ()
                    io $ forkIO $ seq (length err) $ return ()
                    putV $ unwords (c:args)++'\n':out++err
                    ec <- io $ waitForProcessNonBlocking pid
                    case ec of
                      ExitSuccess -> return ()
                      ExitFailure 127 -> fail $ c ++ ": command not found"
                      ExitFailure ecode -> fail $ c ++ " failed with exit code "++show ecode

-- | Run a process with a list of arguments and return anything from /stderr/
systemErr :: String -> [String] -> C (ExitCode, String)
systemErr c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                      out <- io $ hGetContents o
                      err <- io $ hGetContents e
                      io $ forkIO $ seq (length out) $ return ()
                      io $ forkIO $ seq (length err) $ return ()
                      putV $ unwords (c:args)++'\n':out++err
                      ec <- io $ waitForProcessNonBlocking pid
                      case ec of
                        ExitFailure 127 -> fail $ c ++ ": command not found"
                        _ -> return (ec, err)

-- | Run a process with a list of arguments and get the resulting output from stdout.
systemOut :: String   -- ^ Program name
          -> [String] -- ^ Arguments
          -> C String -- ^ Output
systemOut c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                      out <- io $ hGetContents o
                      err <- io $ hGetContents e
                      io $ forkIO $ seq (length out) $ return ()
                      io $ forkIO $ seq (length err) $ return ()
                      putV $ unwords (c:args)++'\n':out++err
                      ec <- io $ waitForProcessNonBlocking pid
                      case ec of
                        ExitSuccess -> return out
                        ExitFailure 127 -> fail $ c ++ ": command not found"
                        ExitFailure ecode -> fail $ c ++ " failed with exit code "++show ecode

-- | Change current directory
cd :: String -> C ()
cd = io . setCurrentDirectory

-- | cat is just a strict readFile.
cat :: String -> C String
cat fn = do x <- io $ readFile fn
            length x `seq` return x
