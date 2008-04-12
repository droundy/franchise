module Distribution.Franchise where

import Control.Monad ( when )
import Data.List ( partition )
import System.Exit ( ExitCode(..) )
import System.Process ( runProcess, waitForProcess )
import System.Directory ( doesFileExist )

{-
import Distribution.InstalledPackageInfo ( InstalledPackageInfo,
                                           emptyInstalledPackageInfo,
                                           parseInstalledPackageInfo, 
                                           showInstalledPackageInfo )
-}

data Dependency = DependsOn [String] [Dependency]
                  ([String] -> [Dependency] -> IO ())

source :: String -> Dependency
source x = DependsOn [x] [] (\_ _ -> do e <- doesFileExist x
                                        when (not e) $ fail $ "Source file "++x++" does not exist!")

depName :: Dependency -> [String]
depName (DependsOn n _ _) = n

-- data Target = Package InstalledPackageInfo

build :: Dependency -> IO ()
build (DependsOn x ds how) = do mapM_ build ds
                                how x ds

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

ghc_hs_to_o :: [String] -> [Dependency] -> IO ()
ghc_hs_to_o _ ds = case filter (endsWith ".hs") $ concatMap depName ds of
                   [d] -> system "ghc" ["-c","-package-name","franchise-0.0",d]
                   [] -> fail "error 1"
                   _ -> fail "error 2"

objects_to_a :: [String] -> [Dependency] -> IO ()
objects_to_a [outname] ds = system "ar" ("cqs":outname:filter (endsWith ".o") (concatMap depName ds))

a_to_o :: [String] -> [Dependency] -> IO ()
a_to_o [outname] ds = system "ld" ("-r":"--whole-archive":"-o":outname:
                                   filter (endsWith ".a") (concatMap depName ds))

install :: [String] -> [Dependency] -> IO ()
install [prefix] ds = do system "mkdir" ["-p",prefix]
                         let inst x = system "cp" ["--parents",x,prefix]
                             (cabal,others) = partition (endsWith ".cabal") $ concatMap depName ds
                         mapM_ inst others
                         system "ghc-pkg" ("--user":"update":cabal)

system :: String -> [String] -> IO ()
system c args = do putStrLn $ "  " ++ unwords (c:args)
                   pid <- runProcess c args Nothing Nothing Nothing Nothing Nothing
                   ec <- waitForProcess pid
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure x -> fail $ c ++ " failed with: " ++ show x
