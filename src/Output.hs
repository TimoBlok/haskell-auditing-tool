{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Output where

import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alg
import Data.Tree (Forest, Tree (..), drawForest, foldTree)
import Data.Set (Set)
import Data.Set qualified as Set

import Control.Monad ( unless, forM_, when )
import Data.List ( intercalate, nub )
import Data.List.Split
import Data.Char ( isAlphaNum )
import Data.Maybe ( mapMaybe )
import GHC.Unit.Module
import Dependency
import Options
import Data.Foldable (foldl')

type TargetDecls = [Declaration]

outputAnalysis :: TargetDecls -> Options -> DependencyGraph -> IO ()
outputAnalysis targetDecls options depGraph' = do
    putStrLn "Printing the dependency graph:"

    print $ AdjMap.adjacencyList depGraph'

    -- unless (Set.null analysis.getMissingModules) $ do
    --     print $ "Unknown modules: " <> intercalate ", " (show <$> Set.toList analysis.getMissingModules)
    -- unless (Set.null analysis.getUnknownDecls) $ do
    --     print $ "Unknown declaration: " <> intercalate ", " (show <$> Set.toList analysis.getUnknownDecls)

    --print $ AdjMap.adjacencyList analysis.getDependacyGraph
    let rootModules = options.rootModules
        depGraph = removeSelfLoops depGraph'
        isRootModule m = m `elem` rootModules
        --example = Declaration {declUnitId = "base", declModuleName = "System.IO", declOccName = "print"}


        -- the decls that are the root decls in the target modules with non empty dependencies
        rootDecls = filter (\decl ->
          isRootModule (mkModuleName decl.declModuleName) &&
          --isRootDeclInModule decl depGraph &&
          not (isLeaf decl depGraph))
            (AdjMap.vertexList depGraph)
        trimmedDepGraph = trimDepGraphWithBlacklist targetDecls depGraph

        forest        = Alg.bfsForest trimmedDepGraph rootDecls
        trimmedForest = mapMaybe (trimTreeWithBlackList targetDecls) forest

    -- print rootModules
    -- print rootDecls
    dumpRootDeclDeps rootDecls depGraph
    -- print $ AdjMap.adjacencyList depGraph
    -- print $ AdjMap.adjacencyList trimmedDepGraph

    putStrLn "Drawing Forest:\n"

    printForest trimmedForest

    putStrLn "Analysis done!"


    when options.useGraphViz $ do
      let basePath = options.graphVizFile ++ intercalate "-" (map (last . splitOn "." . moduleNameString) options.rootModules)
          graphPath = basePath ++ ".dot"
          treePath = basePath ++ "-trimmed.dot"

      putStrLn "Outputting graph to: "
      putStrLn graphPath
      dumpGraphViz graphPath trimmedDepGraph

      putStrLn "and the trimmed version to: "
      putStrLn treePath
      dumpGraphViz treePath $ AdjMap.forest trimmedForest

removeSelfLoops :: DependencyGraph -> DependencyGraph
removeSelfLoops = induceEdges hasSelfLoop
  where hasSelfLoop = uncurry (==)

induceEdges :: ((Declaration, Declaration) -> Bool) -> DependencyGraph -> DependencyGraph
induceEdges p depGraph = foldl' (removeEdgeIf p) depGraph $ AdjMap.edgeList depGraph
  where
    removeEdgeIf :: ((Declaration, Declaration) -> Bool) -> DependencyGraph -> (Declaration, Declaration) -> DependencyGraph
    removeEdgeIf p depGraph' edge | p edge    = uncurry AdjMap.removeEdge edge depGraph'
                                  | otherwise = depGraph'

dumpRootDeclDeps :: [Declaration] -> DependencyGraph -> IO ()
dumpRootDeclDeps rootDecls callGraph = forM_ rootDecls printTargetDecl
  where
    printTargetDecl :: Declaration -> IO ()
    printTargetDecl decl = do
        let deps = Alg.reachable callGraph decl
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


dumpGraphViz :: FilePath -> DependencyGraph -> IO ()
dumpGraphViz fp depGraph = do
  let
  writeFile fp $
    "digraph {\n" <>
    (concat . nub . map showGraphVizEdge $ AdjMap.edgeList depGraph) <>
    "}"
  where
    showGraphVizEdge :: (Declaration, Declaration) -> String
    showGraphVizEdge (v1, v2) = concat ["\"", v1.declOccName, "\" -> \"", v2.declOccName, "\";\n"]
