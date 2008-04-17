module Distribution.Franchise.Util ( system, systemOut, systemErr )
    where

import System.Directory ( removeFile )
import System.Exit ( ExitCode(..) )
import System.Process ( runProcess, waitForProcess )
import System.IO ( IOMode(..), openBinaryFile )

system :: String -> [String] -> IO ()
system c args = do putStrLn $ "  " ++ unwords (c:args)
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
