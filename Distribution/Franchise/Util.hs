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

module Distribution.Franchise.Util ( system, systemOut, systemErr, cd, cat,
                                     inDarcs,
                                     beginsWith, endsWith, endsWithOneOf )
    where

import System.Directory ( setCurrentDirectory, doesDirectoryExist )
import System.Exit ( ExitCode(..) )
import System.Process ( runInteractiveProcess, waitForProcess )
import System.IO ( hGetContents )
import Control.Concurrent ( forkIO )

import Distribution.Franchise.ConfigureState

-- | Checks if a string begins with a given string
beginsWith :: String -- ^ Prefix that might be there
           -> String -- ^ String to check against
           -> Bool   
beginsWith x y = take (length x) y == x

-- | Checks if a string ends with a given string
endsWith :: String -- ^ Suffix that might be at the end of a string
         -> String -- ^ String to check against
         -> Bool
endsWith x y = drop (length y - length x) y == x

-- | Checks if a string ends with any given suffix
endsWithOneOf :: [String] -- ^ List of strings to check
              -> String   -- ^ String to check against
              -> Bool
endsWithOneOf xs y = any (\x -> endsWith x y) xs

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
                   ec <- io $ waitForProcess pid
                   let cl = unwords (('[':c++"]"):drop (length args-1) args)
                       clv = unwords (c:args)
                   putSV (cl++'\n':out++err) (clv++'\n':out++err)
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure 127 -> fail $ c ++ ": command not found"
                     ExitFailure ecode -> fail $ c ++ " failed with exit code "++show ecode

-- | Run a process with a list of arguments and return anything from /stderr/
systemErr :: String   -- ^ Name
          -> [String] -- ^ Arguments
          -> C String -- ^ Output
systemErr c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                      out <- io $ hGetContents o
                      err <- io $ hGetContents e
                      io $ forkIO $ seq (length out) $ return ()
                      io $ forkIO $ seq (length err) $ return ()
                      ec <- io $ waitForProcess pid
                      putV $ unwords (c:args)++'\n':out++err
                      case ec of
                        ExitFailure 127 -> fail $ c ++ ": command not found"
                        _ -> return ()
                      return err

-- | Run a process with a list of arguments and get the resulting output from stdout.
systemOut :: String   -- ^ Program name
          -> [String] -- ^ Arguments
          -> C String -- ^ Output
systemOut c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                      out <- io $ hGetContents o
                      err <- io $ hGetContents e
                      io $ forkIO $ seq (length out) $ return ()
                      case err of
                        "" -> return ()
                        _ -> putV $ unwords (c:args) ++ '\n':err
                      ec <- io $ waitForProcess pid
                      putV $ unwords (c:args)++'\n':out++err
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

inDarcs :: C Bool
inDarcs = io $ doesDirectoryExist "_darcs"

