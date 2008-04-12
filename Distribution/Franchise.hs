module Distribution.Franchise ( build, Dependency(..), Buildable(..),
                                (<:), source )
    where

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

data Dependency = [String] :< [Buildable]
data Buildable = Dependency :<- (Dependency -> IO ())

infix 2 :<
infix 1 :<-

infix 2 <:
(<:) :: [String] -> [Buildable] -> Buildable
[x] <: y | endsWith ".a" x = [x] :< y :<- objects_to_a
[x] <: y | endsWith ".o" x && all (all (endsWith ".a") . buildName) y = [x] :< y :<- a_to_o
x <: y | all (\z -> endsWith ".o" z || endsWith ".hi" z) x &&
         all (\z -> all (endsWith ".hs") (buildName z) ||
                    all (endsWith ".lhs") (buildName z)) y
             = x :< y :<- ghc_hs_to_o
[x] <: y | endsWith "/" x = [x] :< y :<- install

source :: String -> Buildable
source x = ([x]:<[]) :<- (const $ do e <- doesFileExist x
                                     when (not e) $ fail $ "Source file "++x++" does not exist!")

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d

build :: Buildable -> IO ()
build ((x :< ds) :<- how) = do mapM_ build ds
                               how (x :< ds)

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

ghc_hs_to_o :: Dependency -> IO ()
ghc_hs_to_o (_:<ds) = case filter (endsWith ".hs") $ concatMap buildName ds of
                      [d] -> system "ghc" ["-c","-package-name","franchise-0.0",d]
                      [] -> fail "error 1"
                      _ -> fail "error 2"

objects_to_a :: Dependency -> IO ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (endsWith ".o") (concatMap buildName ds))

a_to_o :: Dependency -> IO ()
a_to_o ([outname]:<ds) = system "ld" ("-r":"--whole-archive":"-o":outname:
                                   filter (endsWith ".a") (concatMap buildName ds))

install :: Dependency -> IO ()
install ([prefix]:<ds) =
    do system "mkdir" ["-p",prefix]
       let inst x = system "cp" ["--parents",x,prefix]
           (cabal,others) = partition (endsWith ".cabal") $ concatMap buildName ds
       mapM_ inst others
       system "ghc-pkg" ("--user":"update":cabal)

system :: String -> [String] -> IO ()
system c args = do putStrLn $ "  " ++ unwords (c:args)
                   pid <- runProcess c args Nothing Nothing Nothing Nothing Nothing
                   ec <- waitForProcess pid
                   case ec of
                     ExitSuccess -> return ()
                     ExitFailure x -> fail $ c ++ " failed with: " ++ show x
