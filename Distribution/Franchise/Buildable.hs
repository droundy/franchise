{-# LANGUAGE CPP #-}
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

module Distribution.Franchise.Buildable
    ( Buildable(..), BuildRule(..), Dependency(..),
      build, buildWithArgs, buildTarget,
      bin, install,
      defaultRule, buildName, build', rm,
      clean, distclean,
      addToRule, addDependencies, addTarget,
      rule, simpleTarget, getBuildable, (|<-),
      getTarget, Target(..),
      phony, extraData )
    where

import Data.List ( sort, isSuffixOf, (\\) )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist, removeFile, getModificationTime )
import Control.Concurrent ( readChan, writeChan, newChan )
import Control.Monad ( when, mplus )

import Distribution.Franchise.Util
import Distribution.Franchise.ConfigureState
import Distribution.Franchise.StringSet
import Distribution.Franchise.Trie
import Distribution.Franchise.GhcState ( getBinDir )
import Distribution.Franchise.Flags ( FranchiseFlag, handleArgs )

data Dependency = [String] :< [String]

infix 2 :<
data BuildRule = BuildRule { make :: Dependency -> C (),
                             postinst :: Dependency -> Maybe (C ()),
                             clean0 :: Dependency -> [String] }

data Buildable = Dependency :<- BuildRule

instance Eq Buildable where
    (xs:<_:<-_) == (ys:<_:<-_) = fromListS xs == fromListS ys

(|<-) :: Dependency -> BuildRule -> Buildable
(|<-) = (:<-)

infix 1 :<-
infix 1 |<-

defaultRule :: BuildRule
defaultRule = BuildRule (const $ return ()) (const $ Just $ return ())
              cleanIt

extraData :: String -> String
extraData x = "config.d/"++x

cleanIt :: Dependency -> [String]
cleanIt (_:<[]) = []
cleanIt (xs:<_) = filter (not . isPhony) xs

isPhony :: String -> Bool
isPhony ('*':r) = case reverse r of ('*':_) -> True
                                    _ -> False
isPhony _ = False

phony :: String -> String
phony x | isPhony x = x
phony x = '*':x++"*"

unphony :: String -> String
unphony x = if isPhony x then init $ tail x else x

rm :: String -> C ()
rm f | "/" `isSuffixOf` f = return ()
rm f = do noRm <- getNoRemove
          f' <- processFilePath f
          if not $ null noRm
                  then putV $ "#rm "++f
                  else do io $ removeFile f'
                          putV $ "rm "++f
                         `catchC` \_ -> return ()

depName :: Dependency -> [String]
depName (n :< _) = n

buildName :: Buildable -> [String]
buildName (d:<-_) = depName d

-- | 'build' is at the heart of any Setup.hs build script, and drives
-- the entire build process.  Its first argument is a list of flags
-- you want the Setup.hs script to accept, and the second argument is
-- the actual function that configures and build, which itself returns
-- a list of targets that are built as part of the default build
-- target.

build :: [C FranchiseFlag] -> C () -> IO ()
build opts mkbuild =
    do args <- getArgs
       buildWithArgs args opts mkbuild

#ifndef FRANCHISE_VERSION
#define FRANCHISE_VERSION "franchise (unknown)"
#endif

addPaths :: String -> [FilePath] -> C ()
addPaths v ps = do ps' <- mapM processFilePath ps
                   addExtraUnique v ps'

clean :: [FilePath] -> C ()
clean = addPaths "to-clean"

distclean :: [FilePath] -> C ()
distclean = addPaths "to-distclean"

(</>) :: FilePath -> FilePath -> FilePath
"" </> x = x
x </> ('/':y) = x </> y
x </> y = reverse (dropWhile (=='/') $ reverse x) ++ '/':y

-- | add the specified install to the install target.  This function
-- respects the DESTDIR environment variable, and should therefore be
-- used for installing.

install :: FilePath -- ^ file or directory to be installed
        -> FilePath -- ^ install location as an absolute path
        -> C ()
install x y = do x' <- processFilePath x
                 addDependencies (phony "build") [x]
                 destdir <- maybe "" id `fmap` getExtraData "destdir"
                 addExtraUnique "to-install" [(x',destdir</>y)]

-- | request that the given file be installed in the bindir.

bin :: FilePath -> C ()
bin x = do bind <- getBinDir
           install x (bind++"/"++x)

buildWithArgs :: [String] -> [C FranchiseFlag] -> C () -> IO ()
buildWithArgs args opts mkbuild = runC $
       do putV $ "compiled with franchise version "++FRANCHISE_VERSION
          rule [phony "clean"] [] $ do toclean <- getExtra "to-clean"
                                       mapM_ rm_rf toclean
          rule [phony "distclean"] [] $ do toclean <- getExtra "to-clean"
                                           dist <- getExtra "to-distclean"
                                           mapM_ rm_rf (toclean++dist)
          -- clear out old install/clean policies
          putExtra "to-clean" ([] :: [String])
          putExtra "to-distclean" ["config.d", "franchise.log"]
          putExtra "to-install" ([] :: [String])
          rule [phony "copy"] [phony "build"] $
               do is <- getExtra "to-install"
                  let inst (x, y) = do mkdir (dirname y)
                                       cp x y
                  mapM_ inst is
          rule [phony "uncopy"] [] $
               do is <- getExtra "to-install"
                  mapM_ (rm_rf . snd) (is :: [(String,String)])
          rule [phony "register"] [] $ return ()
          whenC amInWindows $
            do rule ["register.bat", phony "register-script"] [] $
                  saveAsScript "register.bat" $
                  build' CannotModifyState (phony "register")
               rule ["unregister.bat", phony "unregister-script"] [] $
                  saveAsScript "unregister.bat" $
                  build' CannotModifyState (phony "unregister")
          unlessC amInWindows $
            do rule ["register.sh", phony "register-script"] [] $
                  saveAsScript "register.sh" $
                  build' CannotModifyState (phony "register")
               rule ["unregister.sh", phony "unregister-script"] [] $
                  saveAsScript "unregister.sh" $
                  build' CannotModifyState (phony "unregister")
          rule [phony "unregister"] [] $ return ()
          rule [phony "install"] [phony "build"] $
               do build' CannotModifyState (phony "copy")
                  build' CannotModifyState (phony "register")
          rule [phony "uninstall"] [] $
               do build' CannotModifyState (phony "unregister")
                  build' CannotModifyState (phony "uncopy")
          if "configure" `elem` args
              then return ()
              else (do readConfigureState "config.d"
                       putV "reusing old configuration")
                   `catchC` \e -> do putD e
                                     putV "Couldn't read old config.d"
                                     rm_rf "config.d"
          putExtra "commandLine" args
          targets <- handleArgs opts
          runHooks
          mkbuild
          writeConfigureState "config.d"
          putD "Done writinf to config.d"
          when ("configure" `elem` args) $ putS "configure successful!"
          mapM_ buildtarget targets
    where buildtarget t =
              do mt <- sloppyTarget t
                 case mt of
                   [] -> fail $ "No such target: "++t
                   ["*register*"] ->
                       do putS "{register}"
                          x <- getExtraData "gen-script"
                          let realtarg = maybe "*register*"
                                         (const "*register-script*") x
                          build' CannotModifyState realtarg
                   ["*unregister*"] ->
                       do putS "{unregister}"
                          x <- getExtraData "gen-script"
                          let realtarg = maybe "*unregister*"
                                         (const "*unregister-script*") x
                          build' CannotModifyState realtarg
                   ["*clean*"] -> do putS "{clean}"
                                     build' CannotModifyState "*clean*"
                                     clearAllBuilt
                   ["*distclean*"] -> do putS "{distclean}"
                                         build' CannotModifyState "*distclean*"
                                         clearAllBuilt
                   [tt] -> do putS $ "["++unphony tt++"]"
                              build' CannotModifyState tt
                   ts -> fail $ unlines ["No such target: "++t,
                                         "Perhaps you meant one of "++
                                         unwords (map unphony ts)++"?"]

needsWork :: String -> C Bool
needsWork t =
 do
  isb <- isBuilt t
  if isb
   then do putD $ t++" is already built!"
           return False
   else
    do mtt <- getTarget t
       case mtt of
         Nothing -> do putD $ "marking "++t++" as built since it has no rule"
                       setBuilt t
                       return False
         Just (Target _ ds _)
             | nullS ds ->
                 return True -- no dependencies means it always needs work!
         Just (Target ts ds _) ->
           do mmt <- (Just `fmap` io (mapM getModificationTime $
                                      filter (not . isPhony) $ t:toListS ts))
                     `catchC` \_ -> return Nothing
              case mmt of
                Nothing -> do putD $ "need work because "++t++
                                       " doesn't exist (or a friend)"
                              return True
                Just [] ->
                    do putD $ "need work because "++ t ++ " is a phony target."
                       return True
                Just mt -> do anylater <- anyM (latertime mt) $ toListS ds
                              if anylater
                                then return ()
                                else do putD $ unwords $
                                               "Marking":t:
                                               "as built since it's older than":
                                               toListS ds
                                        setBuilt t
                              return anylater
 where latertime mt y =
           do ye <- io $ doesFileExist y
              if not ye
                  then do putD $ "Need work cuz "++y++" don't exist"
                          return True
                  else do mty <- io $ getModificationTime y
                          if mty > maximum mt
                              then putD $ "I need work since "++y++
                                          " is newer than " ++ t
                              else return ()
                          return (mty > maximum mt)
       anyM _ [] = return False
       anyM f (z:zs) = do b <- f z
                          if b then return True else anyM f zs

buildTarget :: String -> C ()
buildTarget = build' CannotModifyState

build' :: CanModifyState -> String -> C ()
build' cms b = unlessC (isBuilt b) $ -- short circuit if we're already built!
  do --put $S unwords ("I'm thinking of recompiling...": buildName b)
     origw <- toListS `fmap` findWork b
     case origw of
       [] -> putD "I see nothing here to recompile"
       _ -> putD $ "I want to recompile all of "++ unwords origw
     case length origw of
       0 -> putD $ "Nothing to recompile for "++b++"."
       l -> putD $ unwords $ ["Need to recompile ",show l,"for"]++b:["."]
     chan <- io $ newChan
     tis <- targetImportances
     let buildthem _ [] = return ()
         buildthem inprogress w =
             do putD $ unwords ("I am now wanting to compile":w)
                loadavgstr <- cat "/proc/loadavg" `catchC` \_ -> return ""
                let loadavg = case reads loadavgstr of
                                ((n,_):_) -> max 0.0 (n :: Double)
                                _ -> 0.0
                    fixNumJobs nj =
                        if nj > 1 && loadavg >= 0.5+fromIntegral nj
                        then do putV $ "Throttling jobs with load "++
                                     show loadavg
                                return 1
                        else return nj
                njobs <- getNumJobs >>= fixNumJobs
                (canb'',depb') <-
                    partitionM (canBuildNow (w `addsS` inprogress)) w
                canb' <- if njobs > 1
                         then filterDupTargets $ map snd $ sort $
                              map (\t -> (maybe 0 negate$lookupT t tis,t))
                                  canb''
                         else filterDupTargets canb''
                putD $ unwords $ "I can now build: ": canb'
                let jobs = max 0 (njobs - lengthS inprogress)
                    canb = take jobs canb'
                    depb = drop jobs canb' ++ (canb'' \\ canb') ++ depb'
                    buildone ttt =
                        forkC cms $
                          do Just (Target ts xs0 makettt) <- getTarget ttt
                             stillneedswork <-
                                 if any (`elemS` ts) $ toListS inprogress
                                 then do putD "Already in progress..."
                                         return False
                                 else needsWork ttt
                             if stillneedswork
                               then do putD $ unlines
                                                ["I am making "++ ttt,
                                                 "  This depends on "++
                                                 unwords (toListS xs0)]
                                       makettt `catchC`
                                         \e -> do putV $ errorBuilding e ttt
                                                  io $ writeChan chan $ Left e
                                       io $ writeChan chan $ Right (ttt, ts)
                               else do putD $ "I get to skip one! " ++ ttt
                                       io $ writeChan chan $ Right (ttt, ts)
                case filter (".o" `isSuffixOf`) canb of
                  [] -> return ()
                  [_] -> return ()
                  tb -> putD $ "I can now build "++ unwords tb
                mapM_ buildone canb
                md <- io $ readChan chan
                case md of
                  Left e -> do putV $ errorBuilding e b
                               fail $ errorBuilding e b
                  Right (d,ts) -> do putD $ "Done building "++ show d
                                     mapM_ setBuilt $ d : toListS ts
                                     buildthem
                                         (delS d (addsS canb $ inprogress))
                                         (depb \\ toListS ts)
         errorBuilding e "config.d/commandLine" = "configure failed:\n"++e
         errorBuilding e f | ".depend" `isSuffixOf` f = e
         errorBuilding e bn = "Error building "++unphony bn++'\n':e
         filterDupTargets [] = return []
         filterDupTargets (t:ts) =
             do Just (Target xs _ _) <- getTarget t
                ts' <- filterDupTargets $ filter (not . (`elemS` xs)) ts
                return (t:ts')
     buildthem emptyS origw

targetImportances :: C (Trie Int)
targetImportances = do ts <- getTargets
                       let invertedDeps = foldl invertDep emptyT $ toListT ts
                           invertDep ids (t,Target _ dd _) =inv (toListS dd) ids
                               where inv [] x = x
                                     inv (d:ds) x = inv ds $ alterT d addit x
                                     addit (Just ss) = Just $ addS t ss
                                     addit Nothing = Just $ addS t emptyS
                           gti x [] = x
                           gti ti (t:rest) =
                               case lookupT t ti of
                               Just _ -> gti ti rest
                               _ -> case toListS `fmap`
                                         lookupT t invertedDeps of
                                    Nothing -> gti (insertT t 0 ti) rest
                                    Just ds ->
                                        case mapM (`lookupT` ti) ds of
                                        Just dsv ->
                                            gti (insertT t
                                                 (1+maximum (0:dsv)) ti) rest
                                        Nothing -> gti ti (ds++t:rest)
                       return $ gti emptyT $ toListS $ keysT ts

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ [] = return ([],[])
partitionM f (x:xs) = do amok <- f x
                         (ok,notok) <- partitionM f xs
                         return $ if amok then (x:ok,notok) else (ok,x:notok)

canBuildNow :: StringSet -> String -> C Bool
canBuildNow needwork t = do mt <- getTarget t
                            case dependencies `fmap` mt of
                              Just d -> return $ not $
                                        any (`elemS` needwork) $ toListS d
                              _ -> return True

getBuildable :: String -> C (Maybe Buildable)
getBuildable t = do allts <- getTargets
                    case lookupT t allts of
                      Just (Target ts ds how) ->
                          return $ Just (t:toListS ts :< toListS ds
                                         :<- defaultRule { make = const how })
                      Nothing ->
                          case lookupT (phony t) allts of
                          Nothing -> return Nothing
                          Just (Target ts ds how) ->
                              return $ Just (phony t:toListS ts :< toListS ds
                                           :<- defaultRule { make = const how })

getTarget :: String -> C (Maybe Target)
getTarget t = do allts <- getTargets
                 return $ lookupT t allts `mplus` lookupT (phony t) allts

sloppyTarget :: String -> C [String]
sloppyTarget "configure" = return []
sloppyTarget t =
    do allts <- getTargets
       return $
          if t `elemS` keysT allts
          then [t]
          else if phony t `elemS` keysT allts
               then [phony t]
               else sloppyLookupKey t allts ++ sloppyLookupKey ('*':t) allts

findWork :: String -> C StringSet
findWork zzz = do putD $ "findWork called on "++zzz
                  fw emptyS zzz
    where fw :: StringSet -> String -> C StringSet
          fw nw t | t `elemS` nw = return nw
          fw nw t =
              do amb <- isBuilt t
                 if amb
                  then return nw
                  else
                    do mt <- getTarget t
                       case mt of
                         Nothing -> return nw
                         Just (Target _ ds _)
                             | nullS ds ->
                                 -- no dependencies means it always needs work!
                                 return $ addS t nw
                         Just (Target _ ds00 _) ->
                             do let ds0 = toListS ds00
                                nwds <- lookAtDeps nw ds0
                                if any (`elemS` nwds) ds0
                                   then return $ addS t nwds
                                   else do tooold <- needsWork t
                                           if tooold then return $ addS t nwds
                                                     else return nwds
                             where lookAtDeps nw' [] = return nw'
                                   lookAtDeps nw' (d:ds) = do nw2 <- fw nw' d
                                                              lookAtDeps nw2 ds

simpleTarget :: String -> C a -> C ()
simpleTarget outname myrule =
    addTarget $ [outname] :< []
                  :<- defaultRule { make = const (myrule >> return ()) }

addTarget :: Buildable -> C ()
addTarget (ts :< ds :<- r) =
    do withd <- rememberDirectory
       mapM_ clearBuilt ts
       ts' <- mapM processFilePathOrTarget ts
       ds' <- fromListS `fmap` mapM processFilePathOrTarget ds
       let fixt t = (t, delS t allts)
           allts = fromListS ts'
           ts'' = map fixt ts'
           addt (t,otherTs) = modifyTargets $
                              insertT t (Target otherTs ds' $
                                                withd $ make r (ts:<ds))
       clean $ clean0 r (ts:<ds)
       case postinst r (ts:<ds) of
         Just inst -> addToRule (phony "register") $ withd inst
         Nothing -> return ()
       mapM_ addt ts''

-- | If you want to create a new target, you can do this using 'rule',
-- which creates a build target with certain dependencies and a rule
-- to do any extra actual building.

rule :: [String] -- ^ list of targets simultaneously built by this rule
     -> [String] -- ^ list of dependencies
     -> C () -- ^ rule to build this target
     -> C ()
rule n deps j =
    addTarget $ n :< deps |<- defaultRule { make = const j }

-- | Add a bit more work to be done when building target.  This will
-- be done /before/ the function that is already present, and thus may
-- be used to prepare the environment in some way.

{-# NOINLINE addToRule #-}
addToRule :: String -> C () -> C ()
addToRule t j | unphony t == "install" = addToRule (phony "register") j
addToRule targ j = do withd <- rememberDirectory
                      modifyTargets $ adjustT' targ $
                            \ (Target a b c) -> Target a b (withd j >> c)
    where adjustT' t f m = case lookupT t m of
                           Just _ -> adjustT t f m
                           Nothing -> adjustT (phony t) f m

-- | Add some dependencies to the specified target.  If the target
-- does not exist, it is created as a phony target.  This is pretty
-- simple, but you may care to see also the regression tests described
-- in <../27-addDependencies.html> for examples.

addDependencies :: String -- ^ target
                -> [String] -- ^ new dependencies
                -> C ()
addDependencies t ds =
    do ots <- maybe [] (toListS . fellowTargets) `fmap` getTarget t
       ds0 <- maybe emptyS dependencies `fmap` getTarget t
       rul <- maybe (return ()) buildrule `fmap` getTarget t
       t' <- maybe (phony t) (const t) `fmap` getTarget t
       ds' <- fromListS `fmap` mapM processFilePathOrTarget ds
       let ds'' = ds0 `unionS` ds'
           fixt tar = (tar, delS tar allts)
           allts = fromListS (t':ots)
           ts'' = map fixt (t':ots)
           addt (thisT,otherTs) = modifyTargets $
                                  insertT thisT (Target otherTs ds'' rul)
       mapM_ clearBuilt (t':ots)
       mapM_ addt ts''
