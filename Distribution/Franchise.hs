module Distribution.Franchise ( build, Dependency(..), Buildable(..),
                                copyright, license, version,
                                (<:), source, package, clean )
    where

import Control.Monad ( when, mplus )
import Data.Maybe ( catMaybes )
import Data.List ( nub, partition, delete, (\\) )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist, removeFile )
import System.Posix.Files ( getFileStatus, modificationTime )
import System.Posix.Env ( setEnv, getEnv )

import Distribution.Franchise.Util ( system )

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
x <: y | all (endsWithOneOf [".o",".hi"]) x &&
         any (any (endsWithOneOf [".hs",".lhs"]) . buildName) y
             = x :< y :<- ghc_hs_to_o
[x] <: y | endsWith "/" x = [x] :< y :<- const (return ())
xs <: ys = error $ "Can't figure out how to build "++ show xs++" from "++ show (map buildName ys)

source :: String -> Buildable
source x = ([x]:<[]) :<- (const $ do e <- doesFileExist x
                                     when (not e) $ fail $ "Source file "++x++" does not exist!")

clean :: Buildable -> Buildable
clean b = [] :< [] :<- const (mapM_ rm $ listOutputs b)
    where rm f | endsWith "/" f = return ()
               | otherwise = do putStrLn $ "  removing " ++ f
                                removeFile f `catch` \_ -> return ()

listOutputs :: Buildable -> [String]
listOutputs (_ :< [] :<- _) = [] -- things that have no input (i.e. source!) aren't output
listOutputs (xs :< ys :<- _) = nub (xs ++ concatMap listOutputs ys)

copyright, license, version :: String -> IO ()
copyright x = setEnv "FRANCHISE_COPYRIGHT" x True
license x = setEnv "FRANCHISE_LICENSE" x True
version x = setEnv "FRANCHISE_VERSION" x True

package :: String -> [String] -> IO Buildable
package packageName modules =
    do setEnv "FRANCHISE_PACKAGE" packageName True
       system "ghc" ("-M":"-optdep-f":"-optdep.depend":modules)
       mods <- parseDeps `fmap` readFile ".depend"
       pre <- getPrefix
       let lib = ["lib"++packageName++".a"] <: mods
           obj = [packageName++".o"] <: [lib]
           cabal = [packageName++".cabal"] :< [source ".depend"] :<- makecabal
           destination = pre++"/"++packageName++"/"
           makecabal _ = do ver <- getVersion
                            lic <- getEnv "FRANCHISE_LICENSE"
                            cop <- getEnv "FRANCHISE_COPYRIGHT"
                            mai <- getEnv "FRANCHISE_MAINTAINER"
                            ema <- getEnv "EMAIL"
                            writeFile (packageName++".cabal") $ unlines
                                          ["name: "++packageName,
                                           "version: "++ver,
                                           "license: "++maybe "OtherLicense" id lic,
                                           "copyright: "++maybe "" id cop,
                                           "maintainer: "++maybe "" id (mai `mplus` ema),
                                           "import-dirs: "++ destination,
                                           "library-dirs: "++ destination,
                                           "exposed-modules: "++unwords modules,
                                           "hidden-modules: "++unwords (concatMap modName mods \\ modules),
                                           "hs-libraries: "++packageName,
                                           "exposed: True",
                                           "depends: base, unix"]
       return $ [destination] <: (lib:obj:cabal:mods)

modName :: Buildable -> [String]
modName (xs :< _ :<- _) = map toMod $ filter (endsWith ".o") xs
    where toMod x = map todots $ take (length x - 2) x
          todots '/' = '.'
          todots x = x

getPackageVersion :: IO (Maybe String)
getPackageVersion = do ver <- getVersion
                       pn <- getEnv "FRANCHISE_PACKAGE"
                       return $ fmap (++("-"++ver)) pn

getVersion :: IO String
getVersion = do ver <- getEnv "FRANCHISE_VERSION"
                return $ maybe "0.0" id ver

getPrefix :: IO String
getPrefix = do pre <- getEnv "FRANCHISE_PREFIX"
               hom <- getEnv "HOME"
               return $ maybe "./lib" id $ pre `mplus` fmap (++"/lib") hom

parseDeps :: String -> [Buildable]
parseDeps x = builds
    where builds = map makeBuild $ pd $ catMaybes $ map breakdep $ filter notcomm $ lines x
          notcomm ('#':_) = False
          notcomm x = True
          breakdep (' ':':':' ':r) = Just ("",r)
          breakdep (x:y) = do (a,b) <- breakdep y
                              Just (x:a,b)
          breakdep [] = Nothing
          pd :: [(String,String)] -> [([String],[String])]
          pd [] = []
          pd ((x,y):r) | endsWith ".o" x = 
                           ([x,take (length x-2) x++".hi"], y : map snd ys) : pd r'
              where (ys,r') = partition ((==x).fst) r
          makeBuild :: ([String],[String]) -> Buildable
          makeBuild (xs,xds) = xs <: map (fb builds) xds
          fb _ x | endsWithOneOf [".lhs",".hs"] x = source x
          fb [] x = error $ "Couldn't find build for "++ x
          fb ((xs:<xds:<-h):r) x | x `elem` xs = (x:delete x xs) :< xds :<- h
                                 | otherwise = fb r x

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d

build :: Buildable -> IO ()
build b = do args <- getArgs
             case args of
               ["clean"] -> build' $ clean b
               ["build"] -> build' b
               [] -> build' b
               ["install"] -> build' $ install b
               x -> fail $ "I don't understand arguments " ++ unwords x

build' :: Buildable -> IO ()
build' ((x :< ds) :<- how) = do mapM_ build' ds
                                nw <- needsWork (x:<ds)
                                when nw $ how (x :< ds)

needsWork :: Dependency -> IO Bool
needsWork ([]:<_) = return True
needsWork ((x:_) :< ds) =
    do fe <- doesFileExist x
       if not fe
         then return True
         else do s <- getFileStatus x
                 let mt = modificationTime s
                     latertime y = do ye <- doesFileExist y
                                      if not ye
                                        then return True
                                        else do sy <- getFileStatus y
                                                return (modificationTime sy > mt)
                     anyM _ [] = return False
                     anyM f (z:zs) = do b <- f z
                                        if b then return True
                                             else anyM f zs
                 anyM latertime $ concatMap buildName ds

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

endsWithOneOf :: [String] -> String -> Bool
endsWithOneOf xs y = any (\x -> endsWith x y) xs

ghc_hs_to_o :: Dependency -> IO ()
ghc_hs_to_o (_:<ds) = case filter (endsWithOneOf [".hs",".lhs"]) $ concatMap buildName ds of
                      [d] -> do pn <- getPackageVersion
                                case pn of
                                  Just p -> system "ghc" ["-c","-package-name",p,d]
                                  Nothing -> system "ghc" ["-c",d]
                      [] -> fail "error 1"
                      _ -> fail "error 2"

objects_to_a :: Dependency -> IO ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (endsWith ".o") (concatMap buildName ds))

a_to_o :: Dependency -> IO ()
a_to_o ([outname]:<ds) = system "ld" ("-r":"--whole-archive":"-o":outname:
                                   filter (endsWith ".a") (concatMap buildName ds))

install :: Buildable -> Buildable
install ([prefix]:<ds:<-h) | endsWith "/" prefix = [prefix]:<ds:<-h'
  where h' b = do h b
                  system "mkdir" ["-p",prefix]
                  let inst x = system "cp" ["--parents",x,prefix]
                      (cabal,others) = partition (endsWith ".cabal") $ concatMap buildName ds
                  mapM_ inst others
                  system "ghc-pkg" ("--user":"update":cabal)
