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
import Data.List.Split ( splitOn )
import Data.Char ( isAlphaNum )
import Data.Maybe ( mapMaybe, catMaybes )
import GHC.Unit.Module ( mkModuleName, moduleNameString )
import Dependency
    ( Declaration(..), DependencyGraph )
import Options ( Options(..) )
import Data.Foldable (foldl')

type TargetDecls = [Declaration]

outputAnalysis :: TargetDecls -> Options -> DependencyGraph -> IO ()
outputAnalysis targetDecls options depGraph' = do
    putStrLn "Printing the dependency graph:"

    --print $ AdjMap.adjacencyList analysis.getDependacyGraph
    let rootModules = options.rootModules
        depGraph = removeSelfLoops depGraph'
        isRootModule m = m `elem` rootModules
        blacklist = Set.fromList targetDecls

        -- the decls that are the root decls with non empty dependencies
        rootDecls = filter (\decl ->
          isRootModule (mkModuleName decl.declModuleName) &&
          not (isLeaf decl depGraph))
            (AdjMap.vertexList depGraph)
        --trimmedDepGraph = trimDepGraphWithBlacklist blacklist depGraph

        forest        = Alg.bfsForest depGraph rootDecls
        trimmedForest = mapMaybe (trimTreeWithBlackList blacklist) forest

        whitelist = AdjMap.vertexSet $ AdjMap.forest trimmedForest
        trimmedDepGraph = trimDepGraphWithWhitelist whitelist depGraph

    -- print rootModules
    -- print rootDecls
    -- dumpRootDeclDeps rootDecls depGraph
    -- print $ AdjMap.adjacencyList depGraph
    -- print $ AdjMap.adjacencyList trimmedDepGraph

    putStrLn "Drawing Forest:\n"

    --printForest forest
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

trimTreeWithBlackList :: Set Declaration -> Tree Declaration -> Maybe (Tree Declaration)
trimTreeWithBlackList blacklist = foldTree trimTree
  where
    -- folds through each subtree and removes any branches that raise no concern
    trimTree :: Declaration -> [Maybe (Tree Declaration)] -> Maybe (Tree Declaration)
    trimTree decl trees = let subTrees = catMaybes trees
      in if not (null subTrees) || isBlacklisted decl 
        then Just (Node decl subTrees) 
        else Nothing

    isBlacklisted :: Declaration -> Bool
    isBlacklisted decl = decl `Set.member` blacklist

printForest :: Show a => Forest a -> IO ()
printForest forest = putStrLn $ drawForest $ (show <$>) <$> forest

-- removes any vertex that doesn't depend on a blacklisted declaration.
-- aka removes unnecessary leaves and branches
trimDepGraphWithBlacklist :: Set Declaration -> DependencyGraph -> DependencyGraph
trimDepGraphWithBlacklist blacklist depGraph =
  let 
    hasBlacklistedDep = not . null . Set.intersection blacklist . Set.fromList . Alg.reachable depGraph
  in
    AdjMap.induce hasBlacklistedDep depGraph

trimDepGraphWithWhitelist :: Set Declaration -> DependencyGraph -> DependencyGraph
trimDepGraphWithWhitelist whitelist depGraph = 
  let
    isWhitelisted = flip Set.member whitelist
  in
    AdjMap.induce isWhitelisted depGraph

  

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
