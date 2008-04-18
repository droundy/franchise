module Distribution.Franchise.Util ( system, systemOut, systemErr,
                                     getDir, parseArgs,
                                     endsWith, endsWithOneOf )
    where

import System.Exit ( ExitCode(..) )
import System.Posix.Env ( getEnv, setEnv )
import System.Process ( runInteractiveProcess, waitForProcess )
import System.IO ( hFlush, stdout, hGetContents )
import Control.Monad ( msum )
import Control.Concurrent ( forkIO )

beginsWith :: String -> String -> Bool
beginsWith x y = take (length x) y == x

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

endsWithOneOf :: [String] -> String -> Bool
endsWithOneOf xs y = any (\x -> endsWith x y) xs

system :: String -> [String] -> IO ()
system c args = do (_,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
                   out <- hGetContents o
                   err <- hGetContents e
                   -- now we ensure that out and err are consumed, so that
                   -- the code we're running won't hang waiting for its
                   -- output (or error) to be consumed.
                   forkIO $ seq (length out) $ return ()
                   forkIO $ seq (length err) $ return ()
                   ec <- waitForProcess pid
                   v <- getEnv "VERBOSE"
                   let cl = case v of
                            Nothing -> unwords (c:"...":drop (length args-1) args)
                            Just _ -> unwords (c:args)
                   putStr (cl++'\n':out++err)
                   hFlush stdout
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure x -> fail $ c ++ " failed with: " ++ show (out++err)

systemErr :: String -> [String] -> IO String
systemErr c args = do (_,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
                      out <- hGetContents o
                      err <- hGetContents e
                      forkIO $ seq (length out) $ return ()
                      forkIO $ seq (length err) $ return ()
                      waitForProcess pid
                      return err

systemOut :: String -> [String] -> IO String
systemOut c args = do (_,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
                      out <- hGetContents o
                      err <- hGetContents e
                      forkIO $ seq (length out) $ return ()
                      case err of
                        [] -> return ()
                        _ -> putStr $ unwords (c:args) ++ '\n':err
                      waitForProcess pid
                      return out

withArg :: String -> [String] -> (String -> IO ()) -> IO ()
withArg _ [] _ = return ()
withArg x (y:z:_) f | y == x = f z
                    | y == x++"=" = f z
withArg x (y:z) f | beginsWith (x++"=") y = f $ drop (1+length x) y
                  | otherwise = withArg x z f

withArgEnv :: String -> String -> [String] -> IO ()
withArgEnv p e args = withArg p args $ \v -> setEnv e v True

getDir :: String -> String -> IO String
getDir e d = do bindir <- getEnv e
                pre <- getEnv "PREFIX"
                hom <- getEnv "HOME"
                return $ maybe ("/usr/local/"++d) id $ msum $
                       bindir : map (fmap (++("/"++d))) [pre,hom]

parseArgs :: [String] -> IO ()
parseArgs args = do withArgEnv "--prefix" "PREFIX" args
                    withArgEnv "--bindir" "BINDIR" args
                    withArgEnv "--libdir" "LIBDIR" args
                    withArgEnv "--libsubdir" "LIBSUBDIR" args
