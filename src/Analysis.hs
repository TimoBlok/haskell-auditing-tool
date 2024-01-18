{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | module that does the analysis using a state monad
-- keywords:
-- decl(s) => declaration(s)
-- dep(s) => dependencie(s)
module Analysis where

import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alg

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), execStateT, get, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Foldable ( traverse_ )

import Core
    ( getDependenciesFromCoreBinds )

import GhcSession
    ( ModuleName, mkModuleNameFS, Ghc, runGhcWithEnv, getCoreBind )

import Dependency
    ( ModuleFS(..),
      Declaration(..),
      DependencyGraph )

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
                pure $ Alg.reachable moduleDecls decl --AdjMap.postSet decl $ AdjMap.transitiveClosure moduleDecls
              else do
                -- decl is not found in module it says it's in
                addUnknownDecl decl
                pure mempty

            -- store result in analysis
            addKnownDecl decl
            addDeclaration decl declDeps

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

