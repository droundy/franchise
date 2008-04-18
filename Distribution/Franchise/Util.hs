module Distribution.Franchise.Util ( system, systemOut, systemErr,
                                     getDir, parseArgs,
                                     endsWith, endsWithOneOf )
    where

import System.Directory ( removeFile )
import System.Exit ( ExitCode(..) )
import System.Posix.Env ( getEnv, setEnv )
import System.Process ( runProcess, waitForProcess )
import System.IO ( IOMode(..), openBinaryFile )
import Control.Monad ( msum )

beginsWith :: String -> String -> Bool
beginsWith x y = take (length x) y == x

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

endsWithOneOf :: [String] -> String -> Bool
endsWithOneOf xs y = any (\x -> endsWith x y) xs

system :: String -> [String] -> IO ()
system c args = do v <- getEnv "VERBOSE"
                   case v of
                     Nothing -> putStrLn $ "  " ++ unwords (c:"...":drop (length args-1) args)
                     Just _ -> putStrLn $ "  " ++ unwords (c:args)
                   pid <- runProcess c args Nothing Nothing Nothing Nothing Nothing
                   ec <- waitForProcess pid
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure x -> fail $ c ++ " failed with: " ++ show x

systemErr :: String -> [String] -> IO String
systemErr c args = do h <- openBinaryFile "systemErr" WriteMode
                      pid <- runProcess c args Nothing Nothing Nothing Nothing (Just h)
                      waitForProcess pid
                      x <- readFile "systemErr"
                      removeFile "systemErr" `catch` \_ -> return ()
                      return x

systemOut :: String -> [String] -> IO String
systemOut c args = do h <- openBinaryFile "systemOut" WriteMode
                      pid <- runProcess c args Nothing Nothing Nothing (Just h) Nothing
                      ec <- waitForProcess pid
                      case ec of
                        ExitSuccess -> return ()
                        ExitFailure x -> fail $ c ++ " failed with: " ++ show x
                      x <- readFile "systemOut"
                      removeFile "systemOut" `catch` \_ -> return ()
                      return x

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
