module Distribution.Franchise.Util ( system )
    where

import System.Exit ( ExitCode(..) )
import System.Process ( runProcess, waitForProcess )

system :: String -> [String] -> IO ()
system c args = do putStrLn $ "  " ++ unwords (c:args)
                   pid <- runProcess c args Nothing Nothing Nothing Nothing Nothing
                   ec <- waitForProcess pid
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure x -> fail $ c ++ " failed with: " ++ show x
