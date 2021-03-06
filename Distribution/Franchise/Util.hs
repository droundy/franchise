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
module Distribution.Franchise.Util ( system, systemV, systemOut, systemErr,
                                     saveAsScript,
                                     systemInOut,
                                     systemOutErrToFile,
                                     nubs,
                                     mkFile, cat, pwd, ls, mv, cp,
                                     isFile, isDirectory, takeExtension,
                                     bracketC, csum, finallyC, bracketC_ )
    where

import System.Exit ( ExitCode(..) )
import System.Directory ( getCurrentDirectory, getDirectoryContents,
                          doesFileExist, doesDirectoryExist, renameFile,
                          copyFile )
import System.Process ( ProcessHandle, runInteractiveProcess, runProcess,
                        waitForProcess, getProcessExitCode )
import Control.Concurrent ( threadDelay, rtsSupportsBoundThreads, forkIO )
import System.IO ( hGetContents, openFile, IOMode(..), hPutStr, hClose )

import Distribution.Franchise.ConfigureState
import Distribution.Franchise.Env ( getEnvironment, extraPath )
import Distribution.Franchise.Permissions ( isExecutable, setExecutable )
import Distribution.Franchise.StringSet ( toListS, fromListS )

-- | A version of waitForProcess that is non-blocking even when linked with
-- the non-threaded runtime.

--
-- waitForProcess uses a very hokey heuristic to try to avoid burning too
-- much CPU time in a busy wait, while also not adding too much extra
-- latency.

waitForProcessNonBlocking :: ProcessHandle -> C ExitCode
waitForProcessNonBlocking = if rtsSupportsBoundThreads
                            then io . waitForProcess
                            else wfp 0
    where wfp n pid = do mec <- io $ getProcessExitCode pid
                         case mec of
                           Just ec -> return ec
                           Nothing ->
                               do io $ threadDelay n
                                  putD $ "Waiting for process... " ++ show n
                                  wfp (min 100000 (n+1+n`div`4)) pid

findCommandInExtraPath :: String -> C String
findCommandInExtraPath c = do ds <- extraPath
                              csum $ map fcip ds++[return c]
    where fcip d = do amw <- amInWindows
                      if amw
                         then do ise <- isExecutable (d++"/"++c)
                                 if ise
                                   then return (d++"/"++c)
                                   else do ise2 <- isExecutable
                                                   (d++"/"++c++".exe")
                                           if ise2
                                             then return (d++"/"++c++".exe")
                                             else fail $ "not "++(d++"/"++c)
                         else do ise <- isExecutable (d++"/"++c)
                                 if ise then return (d++"/"++c)
                                        else fail $ "not "++(d++"/"++c)

assertNotCreatingScript :: String -> C ()
assertNotCreatingScript err =
    do x <- getExtraData "savescript"
       case x of
         Nothing -> return ()
         Just _ -> fail $ err++" doesn't work when creating script."

saveAsScript :: String -> C () -> C ()
saveAsScript scriptname job =
    do x <- getExtraData "savescript"
       case x of
         Just s -> fail $ "Already saving as a script: "++s
         Nothing -> return ()
       "savescript" <<= scriptname
       unlessC amInWindows $
               do writeF scriptname "#!/bin/sh\n"
                  setExecutable scriptname
       whenC amInWindows $ writeF scriptname "" -- empty script is okay
       job
       rmExtra "savescript"

inscript :: String -> C () -> C ()
inscript inscr j = do x <- getExtraData "savescript"
                      case x of
                        Nothing -> j
                        Just s -> io $ appendFile s (inscr++"\n")

mkscript :: String -> [String] -> Maybe String
         -> [(String,String)] -> String
mkscript c args subdir _env =
    dropWhile (==' ') $ unwords $
                  mycd: -- map setenv env++
                  showSh c:map showSh args
    where -- setenv (x,y) = x++"="++showSh y
          mycd = case subdir of
                   Nothing -> ""
                   Just x -> "cd "++showSh x++";"

showSh :: String -> String
showSh x | any (`elem` x) "\'\t\n" = '"':concatMap doublequote x++"\""
         | any (`elem` x) "\" ;{}()*?$@" = '\'':x++"\""
         | otherwise = x
    where doublequote '"' = "\\\""
          doublequote '\t' = "\\t"
          doublequote '\n' = "\\n"
          doublequote '$' = "\\$"
          doublequote '@' = "\\@"
          doublequote c = [c]

-- | Run a command
system :: String   -- ^ Command
       -> [String] -- ^ Arguments
       -> C ()
system g args =
    do sd <- getCurrentSubdir
       c <- findCommandInExtraPath g
       env <- Just `fmap` getEnvironment
       let cl = unwords (('[':c++"]"):drop (length args-1) args)
           clv = unwords (c:args)
       inscript (mkscript c args sd (maybe [] id env)) $
                do whenC oneJob $ putSV cl clv
                   (i,o,e,pid) <- io $ runInteractiveProcess c args sd env
                   io $ hClose i
                   out <- io $ hGetContents o
                   err <- io $ hGetContents e
                   -- now we ensure that out and err are consumed, so that
                   -- the code we're running won't hang waiting for its
                   -- output (or error) to be consumed.
                   io $ forkIO $ seq (length out) $ return ()
                   io $ forkIO $ seq (length err) $ return ()
                   whenC oneJob $ putS $ out++err
                   unlessC oneJob $
                           putSV (cl++'\n':out++err) (clv++'\n':out++err)
                   ec <- waitForProcessNonBlocking pid
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure 127 -> fail $ c ++ ": command not found"
                     ExitFailure ecode -> fail $ c ++
                                          " failed with exit code "++show ecode

-- | Run a command silently, unless we're verbose
systemV :: String   -- ^ Command
        -> [String] -- ^ Arguments
        -> C ()
systemV g args =
    do sd <- getCurrentSubdir
       c <- findCommandInExtraPath g
       env <- Just `fmap` getEnvironment
       inscript (mkscript c args sd (maybe [] id env)) $
                 do whenC oneJob $ putV $ unwords (c:args)
                    (i,o,e,pid) <- io $ runInteractiveProcess c args sd env
                    io $ hClose i
                    out <- io $ hGetContents o
                    err <- io $ hGetContents e
                    -- now we ensure that out and err are consumed, so that
                    -- the code we're running won't hang waiting for its
                    -- output (or error) to be consumed.
                    io $ forkIO $ seq (length out) $ return ()
                    io $ forkIO $ seq (length err) $ return ()
                    whenC oneJob $ putV $ out++err
                    unlessC oneJob $ putV $ unwords (c:args)++'\n':out++err
                    ec <- waitForProcessNonBlocking pid
                    case ec of
                      ExitSuccess -> return ()
                      ExitFailure 127 -> fail $ c ++ ": command not found"
                      ExitFailure ecode -> fail $ c ++
                                           " failed with exit code "++show ecode

-- | Run a process with a list of arguments and return anything from /stderr/
systemErr :: String -> [String] -> C (ExitCode, String)
systemErr g args = do sd <- getCurrentSubdir
                      c <- findCommandInExtraPath g
                      env <- Just `fmap` getEnvironment
                      whenC oneJob $ putV $ unwords (c:args)
                      (i,o,e,pid) <- io $ runInteractiveProcess c args sd env
                      io $ hClose i
                      out <- io $ hGetContents o
                      err <- io $ hGetContents e
                      io $ forkIO $ seq (length out) $ return ()
                      io $ forkIO $ seq (length err) $ return ()
                      whenC oneJob $ putV $ out++err
                      unlessC oneJob $ putV $ unwords (c:args)++'\n':out++err
                      ec <- waitForProcessNonBlocking pid
                      case ec of
                        ExitFailure 127 -> fail $ c ++ ": command not found"
                        _ -> return (ec, err)

-- | Run a process with a list of arguments and send anything from
-- /stderr/ or /stdout/ to a file
systemOutErrToFile :: String -> [String] -> String -> C ExitCode
systemOutErrToFile g args outf0 =
    do assertNotCreatingScript "systemInOut"
       sd <- getCurrentSubdir
       c <- findCommandInExtraPath g
       let long = unwords (c:args)
           short = if length args < 2 then long
                                      else '[':c++"]"
       putSV short long
       env <- Just `fmap` getEnvironment
       outf <- processFilePath outf0
       h <- io $ openFile outf WriteMode
       pid <- io $ runProcess c args sd env Nothing (Just h) (Just h)
       io $ waitForProcess pid

-- | Run a process with a list of arguments and get the resulting
-- output from stdout.
systemOut :: String   -- ^ Program name
          -> [String] -- ^ Arguments
          -> C String -- ^ Output
systemOut g args = do assertNotCreatingScript "systemInOut"
                      sd <- getCurrentSubdir
                      c <- findCommandInExtraPath g
                      env <- Just `fmap` getEnvironment
                      whenC oneJob $ putV $ unwords (c:args)
                      (i,o,e,pid) <- io $ runInteractiveProcess c args sd env
                      io $ hClose i
                      out <- io $ hGetContents o
                      err <- io $ hGetContents e
                      io $ forkIO $ seq (length out) $ return ()
                      io $ forkIO $ seq (length err) $ return ()
                      whenC oneJob $ putV $ indent "\t" (out++err)
                      unlessC oneJob $ putV $ unwords (c:args)++
                                              '\n':indent "\t" (out++err)
                      ec <- waitForProcessNonBlocking pid
                      case ec of
                        ExitSuccess -> return out
                        ExitFailure 127 -> fail $ c ++ ": command not found"
                        ExitFailure ecode ->
                            fail $ c ++ " failed with exit code "++
                                 show ecode++"\n"++indent "\t*" err
    where indent ind s = ind ++ indent' s
              where indent' ('\n':r) = '\n':ind++ indent' r
                    indent' (x:xs) = x : indent' xs
                    indent' "" = ""

-- | Run a process with a list of arguments and a string as input and get
-- the resulting output from stdout.
systemInOut :: String   -- ^ Program name
            -> [String] -- ^ Arguments
            -> String   -- ^ stdin
            -> C String -- ^ output
systemInOut g args inp = do assertNotCreatingScript "systemInOut"
                            sd <- getCurrentSubdir
                            c <- findCommandInExtraPath g
                            env <- Just `fmap` getEnvironment
                            whenC oneJob $ putV $ unwords (c:args)
                            (i,o,e,pid) <-
                                io $ runInteractiveProcess c args sd env
                            out <- io $ hGetContents o
                            err <- io $ hGetContents e
                            io $ forkIO $ do hPutStr i inp
                                             hClose i
                            io $ forkIO $ seq (length out) $ return ()
                            io $ forkIO $ seq (length err) $ return ()
                            whenC oneJob $ putV $ indent "\t" (out++err)
                            unlessC oneJob $ putV $ unwords (c:args)++
                                                    '\n': indent "\t" (out++err)
                            ec <- waitForProcessNonBlocking pid
                            case ec of
                              ExitSuccess -> return out
                              ExitFailure 127 -> fail $ c++": command not found"
                              ExitFailure ecode ->
                                 fail $ c++" failed with exit code "++show ecode
    where indent ind s = ind ++ indent' s
              where indent' ('\n':r) = '\n':ind++ indent' r
                    indent' (x:xs) = x : indent' xs
                    indent' "" = ""

-- | Create a file.  This also logs the contents of the file to
-- @franchise.log@, so it's helpful if you want to make debugging
-- easier (but also leads to lots of verbosity.

mkFile :: FilePath -> String -> C ()
mkFile f s = inscript ("cat > "++showSh f++" <<EOF\n"++s'++"EOF\n") $
             do f' <- processFilePath f
                io $ writeFile f' s
                putL $ "wrote file "++f++":\n"
                putL $ unlines $ map (\l->('|':' ':l)) $ lines s
    where s' = concatMap quote s
          quote '$' = "\\$"
          quote '@' = "\\@"
          quote c = [c]

-- | cat is a strict readFile that handles the franchise working
-- directory properly.
cat :: String -> C String
cat fn = do assertNotCreatingScript "cat"
            fn' <- processFilePath fn
            x <- io $ readFile fn'
            length x `seq` return x

-- | Return the current franchise working directory.  This might not
-- be the same thing as the true working directory, as franchise
-- maintains its own internal concept of a working directory, so as to
-- allow separate build threads to have separate working directories
-- in order to build in parallel.

pwd :: C String
pwd = do assertNotCreatingScript "pwd"
         x <- io $ getCurrentDirectory
         sd <- getCurrentSubdir
         return $ case sd of Nothing -> x
                             Just d -> x++"/"++d

-- | 'ls' lists the files in the franchise working directory.  Unlike
-- the unix @ls@, it omits the directories @.@ and @..@, which aren't
-- often useful in build rules.

ls :: String -> C [String]
ls d = do assertNotCreatingScript "ls"
          d' <- processFilePath d
          io $ filter (not . (`elem` [".",".."])) `fmap` getDirectoryContents d'

-- | Rename a file.

mv :: String -> String -> C ()
mv a b = inscript ("mv "++showSh a++" "++showSh b) $
         do a' <- processFilePath a
            b' <- processFilePath b
            io $ renameFile a' b'

-- | Copy a file or directory.

cp :: String -> String -> C ()
cp a b = inscript ("cp -a "++showSh a++" "++showSh b) $
         do whenC (isFile a) $
                  do a' <- processFilePath a
                     b' <- processFilePath b
                     io $ copyFile a' b'
            whenC (isDirectory a) $
                  do mkdir b
                     withDirectory a $ do xs <- ls "."
                                          mapM_ (`cp` b) xs

isFile :: String -> C Bool
isFile f = do assertNotCreatingScript "isFile"
              f' <- processFilePath f
              io $ doesFileExist f'

isDirectory :: String -> C Bool
isDirectory f = do assertNotCreatingScript "isDirectory"
                   f' <- processFilePath f
                   io $ doesDirectoryExist f'

takeExtension :: String -> String
takeExtension "" = ""
takeExtension ('.':s) = case dropWhile (/='.') s of
                          "" -> s
                          s' -> takeExtension s'
takeExtension s = takeExtension $ dropWhile (/='.') s

-- | Just like 'Control.Exception.bracket', except we're in the C monad.
bracketC :: C a         -- ^ computation to run first (\"make files\")
         -> (a -> C b)  -- ^ computation to run last (\"delete files\")
         -> (a -> C c)  -- ^ computation to run in-between
         -> C c         -- returns the value from the in-between computation
bracketC before after thing = do a <- before
                                 r <- (thing a) `catchC`
                                        (\e -> do { after a; fail e })
                                 after a
                                 return r

-- | A specialised variant of 'bracketC' with just a computation to run
-- afterward.
finallyC :: C a         -- ^ computation to run first
         -> C b         -- ^ computation to run afterward (even if an exception
                        -- was raised)
         -> C a         -- returns the value from the first computation
a `finallyC` sequel = do r <- a `catchC` (\e -> do { sequel; fail e })
                         sequel
                         return r

-- | A variant of 'bracketC' where the return value from the first computation
-- is not required.
bracketC_ :: C a -> C b -> C c -> C c
bracketC_ before after thing = bracketC before (const after) (const thing)

-- | csum is a variant of msum that preserves the last error output.
csum :: [C a] -> C a
csum [] = fail "csum given an empty list"
csum [f] = f
csum (f1:fs) = f1 `catchC` \_ -> csum fs

nubs :: [String] -> [String]
nubs = toListS . fromListS
