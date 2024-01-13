{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module that does the analysis using a state monad
-- keywords:
-- decl(s) => declaration(s)
-- dep(s) => dependencie(s)
module Analysis where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alg
import Data.Tree (Forest, Tree (..), drawForest, foldTree)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), execStateT, get, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Foldable ( traverse_ )

import GHC.Unit.Module
import GHC.Unit.Types

import Core
    ( DependencyGraph,
      Declaration(declUnitId, declModuleName, Declaration, declOccName),
      getDependenciesFromCoreBinds,
      mkDecl )

import GhcSession
    ( ModuleName, mkModuleNameFS, Ghc, runGhcWithEnv, getCoreBind )

import Dependency
    ( ModuleFS(..),
      Declaration(declUnitId, declModuleName),
      DependencyGraph )

import Options ( Options(rootModules) )
import Control.Monad ( unless, forM_ )
import Data.List ( intercalate )
import GHC.Data.FastString (FastString(FastString))
import Data.Maybe (mapMaybe)

data Analysis = Analysis
        { getDependacyGraph :: DependencyGraph
        , getMissingModules :: Set ModuleFS
        , getUnknownDecls :: Set Declaration
        , getKnownModules :: Map ModuleFS DependencyGraph
        , getKnownDecls :: Set Declaration
        }

getAnalysisWithSelector :: (Monad m) => (Analysis -> a) -> StateT Analysis m a
getAnalysisWithSelector selector = selector <$> get

emptyAnalysis :: Analysis
emptyAnalysis = Analysis mempty mempty mempty mempty mempty

runAnalysis :: (Monad m) => StateT Analysis m () -> m Analysis
runAnalysis = flip execStateT emptyAnalysis

addKnownModules :: (Monad m) => ModuleFS -> DependencyGraph -> StateT Analysis m ()
addKnownModules key value = get >>= \s -> put s{getKnownModules = Map.insert key value s.getKnownModules}

addUnknownDecl :: (Monad m) => Declaration -> StateT Analysis m ()
addUnknownDecl decl = get >>= \s -> put s{getUnknownDecls = Set.insert decl s.getUnknownDecls}

addKnownDecl :: (Monad m) => Declaration -> StateT Analysis m ()
addKnownDecl decl = get >>= \s -> put s{getKnownDecls = Set.insert decl s.getKnownDecls}

addMissingModule :: (Monad m) => ModuleFS -> StateT Analysis m ()
addMissingModule mi = get >>= \s -> put s{getMissingModules = Set.insert mi s.getMissingModules}

addDeclaration :: (Monad m) => Declaration -> [Declaration] -> StateT Analysis m ()
addDeclaration decl deps = get >>= \s -> put s{getDependacyGraph = AdjMap.overlay s.getDependacyGraph $ AdjMap.star decl deps }

setRootDeclarations :: (Monad m) => DependencyGraph -> StateT Analysis m ()
setRootDeclarations decls = get >>= \s -> put s{getDependacyGraph = decls}

analyze :: [ModuleName] -> IO Analysis
analyze rootModules = runGhcWithEnv $ runAnalysis $ do
  -- Load root modules
  let mkModuleInfo moduleName = ModuleFS moduleName Nothing
  (rootDependencies :: DependencyGraph) <- AdjMap.overlays <$> mapM (lookupOrLoadModule . mkModuleInfo) rootModules
  setRootDeclarations rootDependencies
  let dependancyList = AdjMap.adjacencyList rootDependencies
  -- Load dependencies
  traverse_ go (concatMap snd dependancyList)
    where
      -- goes trough all the declarations one by one 
      -- and loads them from their respective module and package
      go :: Declaration -> StateT Analysis Ghc ()
      go decl = do
        depGraph <- getAnalysisWithSelector getDependacyGraph
        knownDecls <- getAnalysisWithSelector getKnownDecls

        if Set.member decl knownDecls
          then -- already processed
            pure ()

          else do -- decl not yet processed
            -- find all declarations inside the module this decl is supposedly in
            let modInfo = ModuleFS (mkModuleNameFS decl.declModuleName) (Just decl.declUnitId)
            moduleDecls <- lookupOrLoadModule modInfo
            declDeps <- if AdjMap.hasVertex decl moduleDecls
              then
                -- were able to load module and find this decl inside it
                pure $ AdjMap.postSet decl $ AdjMap.transitiveClosure moduleDecls
              else do
                -- decl is not found in module it says it's in
                addUnknownDecl decl
                pure mempty

            -- store result in analysis
            addKnownDecl decl
            addDeclaration decl $ Set.toList declDeps

            -- collect the dependencies of the dependencies
            traverse_ go declDeps

-- looks up module from the analysis
-- if not there we load it using the Ghc Api
lookupOrLoadModule :: ModuleFS -> StateT Analysis Ghc DependencyGraph
lookupOrLoadModule moduleInfo = do
    knownModules <- getAnalysisWithSelector getKnownModules

    case Map.lookup moduleInfo knownModules of
        Just deps -> pure deps -- already loaded
        Nothing -> do          -- not yet loaded
            deps <- lift loadModule >>= \case
                Just deps -> pure deps -- loaded
                Nothing -> do  -- could not be loaded
                    -- store missing module in analysis
                    addMissingModule moduleInfo
                    pure AdjMap.empty
            -- store module in analysis
            addKnownModules moduleInfo deps
            pure deps
    where
      loadModule :: Ghc (Maybe DependencyGraph)
      loadModule = getCoreBind moduleInfo.moduleName moduleInfo.moduleUnit >>= \case
          Nothing -> pure Nothing
          Just (genModule, coreBinds) -> do
              let deps :: DependencyGraph
                  deps = getDependenciesFromCoreBinds genModule coreBinds
              pure (Just deps)

showAnalysis :: Options -> Analysis -> IO ()
showAnalysis opts analysis = do
    putStrLn "Printing the analysis:"

    -- unless (Set.null analysis.getMissingModules) $ do
    --     print $ "Unknown modules: " <> intercalate ", " (show <$> Set.toList analysis.getMissingModules)
    -- unless (Set.null analysis.getUnknownDecls) $ do
    --     print $ "Unknown declaration: " <> intercalate ", " (show <$> Set.toList analysis.getUnknownDecls)

    --print $ AdjMap.adjacencyList analysis.getDependacyGraph
    let depGraph = analysis.getDependacyGraph 
        isTargetModule m = m `elem` opts.rootModules
        blacklist = [Declaration {declUnitId = "base", declModuleName = "GHC.Base", declOccName = "id"}]
        
        -- the decls that are the root decls in the target modules with non empty dependencies
        targetDecls = filter (\decl -> 
          isTargetModule (mkModuleNameFS decl.declModuleName) && 
          isRootDeclInModule decl depGraph &&
          not (isLeaf decl depGraph)) 
            (AdjMap.vertexList depGraph)
        trimmedDepGraph = trimDepGraphWithBlacklist blacklist depGraph

        forest        = Alg.bfsForest trimmedDepGraph targetDecls
        trimmedForest = mapMaybe (trimTreeWithBlackList blacklist) forest

    print targetDecls

    putStrLn "Drawing Forest:"
    
    --print forest
    printForest trimmedForest
    
    -- dump for all root modules
    --dumpTargetDeclDeps targetDecls analysis.getDependacyGraph

    -- case opts.targetDecls of
    --     [] -> dumpDependencies (AdjMap.adjacencyList analysis.callGraph)
    --     xs -> checkTarget (AdjMap.adjacencyList analysis.callGraph) xs
    
    
    putStrLn "Analysis done!"
  where
    dumpTargetDeclDeps :: [Declaration] -> DependencyGraph -> IO ()
    dumpTargetDeclDeps targetDecls callGraph = forM_ targetDecls printTargetDecl
      where
        printTargetDecl :: Declaration -> IO ()
        printTargetDecl decl = do
            let deps = Alg.reachable callGraph decl--AdjMap.postSet decl $ AdjMap.transitiveClosure callGraph

                onlyLeaves = filter (null . (`AdjMap.postSet` callGraph)) deps

            unless (null deps) $ putStrLn $ show decl <> ": " <> intercalate ", " (show <$> onlyLeaves)

    isRootDeclInModule :: Declaration -> DependencyGraph ->  Bool
    isRootDeclInModule decl callGraph = not $ any
      (\parent -> parent.declModuleName == decl.declModuleName)
      (AdjMap.preSet decl callGraph)

    isLeaf :: Declaration -> DependencyGraph -> Bool 
    isLeaf decl depGraph = let deps = Alg.reachable depGraph decl
      in null deps

    trimTreeWithBlackList :: [Declaration] -> Tree Declaration -> Maybe (Tree Declaration)
    trimTreeWithBlackList blacklist tree = let (tree', b) = foldTree trimTree tree
      in if b then Just tree' else Nothing
      where
        -- folds through each subtree and removes any branches that raise no concern
        trimTree :: Declaration -> [(Tree Declaration, Bool)] -> (Tree Declaration, Bool)
        trimTree decl trees = let subTrees = [t | (t,b) <- trees, b]
          in (Node decl subTrees, not (null subTrees) || isBlacklisted decl)

        isBlacklisted :: Declaration -> Bool
        isBlacklisted decl = decl `elem` blacklist
 
    printForest :: Show a => Forest a -> IO ()
    printForest forest = putStrLn $ drawForest $ (show <$>) <$> forest

    -- removes any vertex that doesn't depend on a blacklisted declaration.
    -- aka removes unnecessary leaves and branches
    trimDepGraphWithBlacklist :: [Declaration] -> DependencyGraph -> DependencyGraph
    trimDepGraphWithBlacklist blacklist depGraph = 
      let deps              = Set.fromList . Alg.reachable depGraph
          blacklistSet      = Set.fromList blacklist 
          hasBlacklistedDep = not . null . Set.intersection blacklistSet . deps
      in
        AdjMap.induce hasBlacklistedDep depGraph


