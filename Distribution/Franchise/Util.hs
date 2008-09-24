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

module Distribution.Franchise.Util ( system, systemOut, systemErr, cd,
                                     beginsWith, endsWith, endsWithOneOf )
    where

import System.Directory ( setCurrentDirectory )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv )
import System.Process ( runInteractiveProcess, waitForProcess )
import System.IO ( hFlush, stdout, hGetContents )
import Control.Concurrent ( forkIO )

import Distribution.Franchise.ConfigureState

beginsWith :: String -> String -> Bool
beginsWith x y = take (length x) y == x

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

endsWithOneOf :: [String] -> String -> Bool
endsWithOneOf xs y = any (\x -> endsWith x y) xs

amVerbose :: C Bool
amVerbose = io $ do v <- getEnv "VERBOSE"
                    return (v /= "" && v /= "0")
                 `catch` \_ -> return False

system :: String -> [String] -> C ()
system c args = do (_,o,e,pid) <- io $ runInteractiveProcess c args Nothing Nothing
                   out <- io $ hGetContents o
                   err <- io $ hGetContents e
                   -- now we ensure that out and err are consumed, so that
                   -- the code we're running won't hang waiting for its
                   -- output (or error) to be consumed.
                   io $ forkIO $ seq (length out) $ return ()
                   io $ forkIO $ seq (length err) $ return ()
                   ec <- io $ waitForProcess pid
                   v <- amVerbose
                   let cl = if v then unwords (c:args)
                                 else unwords (c:"...":drop (length args-1) args)
                   io $ putStrLn cl
                   --io $ putStrLn (out++err)
                   io $ hFlush stdout
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure _ -> fail $ indent 4 $ c ++ " failed with:" ++indent 2 (out++err)

indent :: Int -> String -> String
indent n = unlines . map ((replicate n ' ')++) . lines

systemErr :: String -> [String] -> C String
systemErr c args = io $
                   do (_,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
                      out <- hGetContents o
                      err <- hGetContents e
                      forkIO $ seq (length out) $ return ()
                      forkIO $ seq (length err) $ return ()
                      waitForProcess pid
                      return err

systemOut :: String -> [String] -> C String
systemOut c args = io $
                   do (_,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
                      out <- hGetContents o
                      err <- hGetContents e
                      forkIO $ seq (length out) $ return ()
                      case err of
                        [] -> return ()
                        _ -> putStr $ unwords (c:args) ++ '\n':err
                      waitForProcess pid
                      return out

cd :: String -> C ()
cd = io . setCurrentDirectory
