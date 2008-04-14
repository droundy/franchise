module Distribution.Franchise ( build, executable, privateExecutable,
                                installBin,
                                -- The constructors are exported so users
                                -- can construct arbitrarily complex build
                                -- systems, hopefully.
                                Dependency(..), Buildable, (|<-), BuildRule(..),
                                -- Handy module-searching
                                requireModule, searchForModule,
                                -- defining package properties
                                package, copyright, license, version,
                                -- semi-automatic rule generation
                                (<:), source, (.&) )
    where

import Control.Monad ( when, mplus, msum )
import Data.Maybe ( catMaybes, listToMaybe )
import Data.List ( nub, partition, delete, (\\) )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist, removeFile )
import System.Posix.Files ( getFileStatus, modificationTime )
import System.Posix.Env ( setEnv, getEnv )
import Control.Concurrent ( forkOS, readChan, writeChan, newChan )

import Distribution.Franchise.Util ( system, systemErr )

{-
import Distribution.InstalledPackageInfo ( InstalledPackageInfo,
                                           emptyInstalledPackageInfo,
                                           parseInstalledPackageInfo, 
                                           showInstalledPackageInfo )
-}

data Dependency = [String] :< [Buildable]
data Buildable = Dependency :<- BuildRule
               | Unknown String
(|<-) :: Dependency -> BuildRule -> Buildable
(|<-) = (:<-)

data BuildRule = BuildRule { make :: Dependency -> IO (),
                             install :: Dependency -> IO (),
                             clean :: Dependency -> IO () }

defaultRule = BuildRule (const $ return ()) (const $ return ()) cleanIt

infix 2 :<
infix 1 :<-, |<-

infix 2 <:
(<:) :: [String] -> [Buildable] -> Buildable
[x] <: y | endsWith ".a" x = [x] :< y :<- defaultRule { make = objects_to_a }
[x] <: y | endsWith ".o" x && all (all (endsWith ".a") . buildName) y
             = [x] :< y :<- defaultRule { make = a_to_o }
x <: y | all (endsWithOneOf [".o",".hi"]) x &&
         any (any (endsWithOneOf [".hs",".lhs"]) . buildName) y
             = x :< y :<- defaultRule { make = ghc_hs_to_o }
xs <: ys = error $ "Can't figure out how to build "++ show xs++" from "++ show (map buildName ys)

source :: String -> Buildable
source = Unknown

(.&) :: Buildable -> Buildable -> Buildable
infixr 3 .&
a .& b = [] :< [a',b'] :<- defaultRule
    where a' = fixbuild b a
          b' = fixbuild a b
          fixbuild x (Unknown y) = maybe (Unknown y) id $ lookupB y x
          fixbuild x (xs:<xds:<-h) = xs :< map (fixbuild x) xds :<- h

lookupB :: String -> Buildable -> Maybe Buildable
lookupB f (Unknown _) = Nothing
lookupB f (xs:<xds:<-h) | f `elem` xs = Just (xs:<xds:<-h)
                        | otherwise = msum (map (lookupB f) xds)

cleanIt (_:<[]) = return ()
cleanIt (xs:<_) = mapM_ rm xs

copyright, license, version :: String -> IO ()
copyright x = setEnv "FRANCHISE_COPYRIGHT" x True
license x = setEnv "FRANCHISE_LICENSE" x True
version x = setEnv "FRANCHISE_VERSION" x True

-- WARNING: If you want to create a package and an executable, you must
-- define the package before building the executable!

executable :: String -> String -> IO Buildable
executable exname src =
    do x :< y :<- b <- privateExecutable exname src
       return $ x :< y :<- b { install = installBin }

-- privateExecutable is used for executables used by the build system but
-- not to be installed.

privateExecutable :: String -> String -> IO Buildable
privateExecutable  exname src =
    do rm ".depend"
       ghc system ["-M","-optdep-f","-optdep.depend",src]
       mods <- parseDeps `fmap` readFile ".depend"
       let objs = filter (endsWith ".o") $ concatMap buildName mods
           mk _ = do ghc system (objs ++ ["-o",exname])
       return $ [exname] :< (source src:mods)
                  :<- defaultRule { make = mk, clean = \b -> rm ".depend" >> cleanIt b }

package :: String -> [String] -> IO Buildable
package packageName modules =
    do setEnv "FRANCHISE_PACKAGE" packageName True
       rm ".depend"
       ghc system ("-M":"-optdep-f":"-optdep.depend":modules)
       mods <- parseDeps `fmap` readFile ".depend"
       pre <- getHSLibPrefix
       ver <- getVersion
       let lib = ["lib"++packageName++".a"] <: mods
           obj = [packageName++".o"] <: [lib]
           cabal = [packageName++".cabal"] :< [source ".depend"] :<- defaultRule { make = makecabal }
           destination = pre++"/"++packageName++"-"++ver++"/"
           makecabal _ = do lic <- getEnv "FRANCHISE_LICENSE"
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
           installme _ = do system "mkdir" ["-p",destination]
                            let inst x = system "cp" ["--parents",x,destination]
                                his = filter (endsWith ".hi") $ concatMap buildName mods
                            mapM_ inst ["lib"++packageName++".a",packageName++".o"]
                            mapM_ inst his
                            system "ghc-pkg" ["--user","update",packageName++".cabal"]
       return $ [destination] :< (lib:obj:cabal:mods)
                  :<- defaultRule { install = installme,
                                    clean = \b -> rm ".depend" >> cleanIt b}

rm :: String -> IO ()
rm f | endsWith "/" f = return ()
rm f = removeFile f `catch` \_ -> return ()

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

getHSLibPrefix :: IO String
getHSLibPrefix = do pre <- getEnv "FRANCHISE_PREFIX"
                    hom <- getEnv "HOME"
                    return $ maybe "./lib" id $ pre `mplus` fmap (++"/lib") hom

getBinPrefix :: IO String
getBinPrefix = do hom <- getEnv "HOME"
                  return $ maybe "/usr/bin" (++"/bin") hom

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
          fb ((xs:<xds:<-h):r) x | x `elem` xs = xs :< xds :<- h
                                 | otherwise = fb r x

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d
buildName (Unknown d) = [d]

build :: Buildable -> IO ()
build b = do args <- getArgs
             case args of
               ["clean"] -> clean' b
               ["build"] -> buildPar b
               [] -> buildPar b
               ["install"] -> do buildPar b
                                 install' b
               x -> fail $ "I don't understand arguments " ++ unwords x

install' :: Buildable -> IO ()
install' ((x :< ds) :<- how) = do mapM_ install' ds
                                  install how (x :< ds)
install' (Unknown _) = return ()

clean' :: Buildable -> IO ()
clean' ((x :< ds) :<- how) = do mapM_ clean' ds
                                clean how (x :< ds)
clean' (Unknown _) = return ()

build' :: Buildable -> IO ()
build' ((x :< ds) :<- how) = do mapM_ build' ds
                                nw <- needsWork (x:<ds)
                                when nw $ make how (x :< ds)
build' (Unknown f) =  do e <- doesFileExist f
                         when (not e) $ fail $ "Source file "++f++" does not exist!"

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
                                                return (modificationTime sy >= mt)
                     anyM _ [] = return False
                     anyM f (z:zs) = do b <- f z
                                        if b then return True
                                             else anyM f zs
                 anyM latertime $ concatMap buildName ds

buildPar :: Buildable -> IO ()
buildPar (Unknown f) = do e <- doesFileExist f
                          when (not e) $ fail $ "Source file "++f++" does not exist!"
buildPar b = do --w <- (mynub . reverse) `fmap` findWork b
                w1 <- reverse `fmap` findWork b
                let w = mynub w1
                chan <- newChan
                buildthem chan [] w
    where mynub (b:bs) = b : mynub (filter (/= b) bs)
          mynub [] = []
          buildthem _ [] [] = return ()
--          buildthem chan inprogress [] =
--              do done <- readChan chan
--                 buildthem chan (delB done inprogress) []
          buildthem chan inprogress w =
              do let (canb',depb') = partition (canBuildNow (inprogress++w)) w
                     jobs = max 0 (4 - length inprogress)
                     canb = take jobs canb'
                     depb = drop jobs canb' ++ depb'
                     buildone (d:<-how) = forkOS $ do make how d
                                                           `catch` \_ -> writeChan chan Nothing
                                                      writeChan chan (Just (d:<-how))
                 mapM_ buildone canb
                 md <- readChan chan
                 case md of
                   Nothing -> fail "Ooops..."
                   Just d -> buildthem chan (delB d (inprogress++canb)) depb
          delB done = filter (/= done)

showBuild :: Buildable -> String
showBuild (xs:<ds:<-_) = unwords (xs++ [":"]++nub (concatMap buildName ds))

instance Eq Buildable where
    Unknown x == Unknown y = x == y
    Unknown x == (ys:<_:<-_) = x `elem` ys
    (ys:<_:<-_) == Unknown x = x `elem` ys
    (xs:<_:<-_) == (ys:<_:<-_) = eqset xs ys
        where eqset [] [] = True
              eqset [] _ = False
              eqset _ [] = False
              eqset (x:xs) ys = x `elem` ys && xs `eqset` (delete x ys)

canBuildNow :: [Buildable] -> Buildable -> Bool
canBuildNow needwork b = all (\b' -> not (b `dependsUpon` b')) needwork

dependsUpon :: Buildable -> Buildable -> Bool
dependsUpon (Unknown _) _ = False
dependsUpon (_:<ds:<-_) x = or (elem x ds : map (`dependsUpon` x) ds)

findWork :: Buildable -> IO [Buildable]
findWork (Unknown _) = return []
findWork b@(xs:<ds:<-_) = do dw <- concat `fmap` mapM findWork ds
                             ineedwork <- case dw of [] -> needsWork (xs:<ds)
                                                     _ -> return True
                             return $ if ineedwork then b:dw else []

endsWith :: String -> String -> Bool
endsWith x y = drop (length y - length x) y == x

endsWithOneOf :: [String] -> String -> Bool
endsWithOneOf xs y = any (\x -> endsWith x y) xs

packages :: IO [String]
packages = do x <- getEnv "FRANCHISE_PACKAGES"
              case x of Nothing -> return []
                        Just y -> return $ words y

ghc :: (String -> [String] -> IO a) -> [String] -> IO a
ghc sys args = do pn <- getPackageVersion
                  packs <- concatMap (\p -> ["-package",p]) `fmap` packages
                  case pn of
                    Just p -> sys "ghc" $ packs++["-hide-all-packages","-package-name",p]++args
                    Nothing -> sys "ghc" $ "-hide-all-packages":packs++args

ghc_hs_to_o :: Dependency -> IO ()
ghc_hs_to_o (_:<ds) = case filter (endsWithOneOf [".hs",".lhs"]) $ concatMap buildName ds of
                      [d] -> ghc system ["-c",d]
                      [] -> fail "error 1"
                      _ -> fail "error 2"

installBin :: Dependency -> IO ()
installBin (xs:<_) = do pref <- getBinPrefix
                        let inst x = system "cp" [x,pref++"/"]
                        mapM_ inst xs

objects_to_a :: Dependency -> IO ()
objects_to_a ([outname]:<ds) =
    system "ar" ("cqs":outname:filter (endsWith ".o") (concatMap buildName ds))

a_to_o :: Dependency -> IO ()
a_to_o ([outname]:<ds) = system "ld" ("-r":"--whole-archive":"-o":outname:
                                   filter (endsWith ".a") (concatMap buildName ds))

requireModule :: String -> IO ()
requireModule m = needModule m Nothing

searchForModule :: String -> [String] -> IO ()
searchForModule m ps = needModule m (Just []) `catch` \_ -> needModule m (Just ps)

needModule :: String -> Maybe [String] -> IO ()
needModule m Nothing = do let fn = "Try"++m++".hs"
                          writeFile fn ("import "++m++"\nmain = undefined")
                          e <- ghc systemErr ["-c",fn]
                          cleanModuleTest m
                          case e of
                            "" -> return ()
                            _ -> case catMaybes $ map findOption $ lines e of
                                   [] -> do putStrLn e
                                            fail $ "Can't use module "++m
                                   ps -> do -- putStrLn e -- for debugging!
                                            needModule m (Just ps)
needModule m (Just (p:ps)) =
    do let fn = "Try"++m++".hs"
       writeFile fn ("import "++m++"\nmain = undefined")
       e <- ghc systemErr ["-package",p,"-c",fn]
       cleanModuleTest m
       case e of
         "" -> do otherPacks <- packages
                  setEnv "FRANCHISE_PACKAGES" (unwords $ p:otherPacks) True
                  putStrLn $ "Found "++ m ++" in package "++ p
         _ -> do putStrLn $ "Couldn't find " ++ m ++ " in package "++p
                 needModule m (Just ps)
needModule m (Just []) =
    do let fn = "Try"++m++".hs"
       writeFile fn ("import "++m++"\nmain = undefined")
       e <- ghc systemErr ["-c",fn]
       cleanModuleTest m
       case e of
         "" -> putStrLn $ "Found "++ m
         _ -> fail $ "Couldn't find module " ++ m

cleanModuleTest :: String -> IO ()
cleanModuleTest m = do let fns = ["Try"++m++".hs","Try"++m++".hi","Try"++m++".o"]
                       mapM_ rm fns

findOption :: String -> Maybe String
findOption x | take (length foo) x == foo = listToMaybe $
                                            map (takeWhile (/=',')) $
                                            map (takeWhile (/='-')) $
                                            words $ drop (length foo) x
             where foo = "member of package "
findOption (_:x) = findOption x
findOption [] = Nothing
