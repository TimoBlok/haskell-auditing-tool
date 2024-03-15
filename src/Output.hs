{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Output where

import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alg
import Data.Tree (Forest, Tree (..), drawForest, foldTree)
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Exception (onException)
import Control.Monad ( unless, forM_, when )
import Data.List ( intercalate, nub )
import Data.List.Split ( splitOn )
import Data.Char ( isAlphaNum )
import Data.Maybe ( mapMaybe, catMaybes )
import System.FilePath ( takeBaseName )

import Dependency
    ( Declaration(..), DependencyGraph )
import Options ( Options(..) )
import Data.Foldable (foldl')

type TargetDecls = [Declaration]
type Blacklist = Set Declaration
type RelevantNodes = Set Declaration

outputAnalysis :: TargetDecls -> Options -> DependencyGraph -> IO ()
outputAnalysis targetDecls options depGraph = do
    putStrLn "Printing the dependency graph:"

    let isRootModule m = m `elem` options.rootModules

        -- the decls that are the root decls with non empty dependencies
        rootDecls = filter (\decl ->
          isRootModule decl.declModuleName &&
          not (isLeaf decl depGraph))
            (AdjMap.vertexList depGraph)

        blacklist       = Set.fromList targetDecls
        trimmedDepGraph = keepRelevantNodes blacklist $ removeSelfLoops depGraph
        forest          = Alg.bfsForest trimmedDepGraph rootDecls
        whitelist       = AdjMap.vertexSet $ AdjMap.forest forest
        finalDepGraph   = trimDepGraphWithWhitelist whitelist trimmedDepGraph

    -- print blacklist
    -- print $ Set.intersection blacklist $ AdjMap.vertexSet depGraph

    putStrLn "Drawing Forest:\n"

    printForest forest

    putStrLn "Analysis done!"

    handleGraphViz options finalDepGraph


--------------------
-- Graph cleanup
--------------------

removeSelfLoops :: DependencyGraph -> DependencyGraph
removeSelfLoops = induceEdges hasSelfLoop
  where hasSelfLoop = uncurry (==)

-- | like induce but on edges
induceEdges :: ((Declaration, Declaration) -> Bool) -> DependencyGraph -> DependencyGraph
induceEdges p depGraph = foldl' (removeEdgeIf p) depGraph $ AdjMap.edgeList depGraph
  where
    removeEdgeIf :: ((Declaration, Declaration) -> Bool) -> DependencyGraph -> (Declaration, Declaration) -> DependencyGraph
    removeEdgeIf p depGraph' edge | p edge    = uncurry AdjMap.removeEdge edge depGraph'
                                  | otherwise = depGraph'

isLeaf :: Declaration -> DependencyGraph -> Bool
isLeaf decl = null . AdjMap.postSet decl


-- Induces the graph based on a whitelist
trimDepGraphWithWhitelist :: Set Declaration -> DependencyGraph -> DependencyGraph
trimDepGraphWithWhitelist whitelist depGraph = 
  let
    isWhitelisted = flip Set.member whitelist
  in
    AdjMap.induce isWhitelisted depGraph

-- | This function filters out nodes that do not depend on any blacklisted declaration
keepRelevantNodes :: Blacklist -> DependencyGraph -> DependencyGraph
keepRelevantNodes blacklist depGraph = 
  let
    relevantNodes = findRelevantNodes blacklist depGraph
    isRelevant = flip Set.member relevantNodes
  in
    AdjMap.induce isRelevant depGraph
  where 
    findRelevantNodes :: Blacklist -> DependencyGraph -> RelevantNodes
    findRelevantNodes blacklist depGraph = 
      let 
        startSet = Set.intersection blacklist $ AdjMap.vertexSet depGraph
      in 
        searchRelevantNodes Set.empty startSet $ AdjMap.transpose depGraph

    searchRelevantNodes :: RelevantNodes -> Set Declaration -> DependencyGraph -> RelevantNodes
    searchRelevantNodes visited nonvisited depGraph 
      | null nonvisited         = visited 
      | Set.member decl visited = searchRelevantNodes visited tailOfNonvisited depGraph
      | otherwise               = searchRelevantNodes newVisited newNonvisited depGraph
      where 
        decl             = Set.elemAt 0 nonvisited
        postList         = AdjMap.postSet decl depGraph
        tailOfNonvisited = Set.delete decl nonvisited
        newNonvisited    = Set.union postList tailOfNonvisited
        newVisited       = Set.insert decl visited 


--------------------
-- Output
--------------------

printForest :: Show a => Forest a -> IO ()
printForest forest = putStrLn $ drawForest $ (show <$>) <$> forest

handleGraphViz :: Options -> DependencyGraph -> IO ()
handleGraphViz options depGraph = do 
  when options.useGraphViz $ do
    let basePath = options.graphVizFile ++ intercalate "-" (map (last . splitOn ".") options.rootModules)
        usedTargetDecls = takeBaseName options.targetDecls 
        graphPath = basePath ++ "." ++ usedTargetDecls ++ ".dot"

    putStrLn "Outputting graph to: "
    putStrLn graphPath
    dumpGraphViz graphPath depGraph

-- | Writes to a .dot file in the dot language
dumpGraphViz :: FilePath -> DependencyGraph -> IO ()
dumpGraphViz fp depGraph = do
  writeFile fp $
    "digraph {\n" 
    ++
    (concat . nub . map showGraphVizEdge $ AdjMap.edgeList depGraph) 
    ++
    "}"
  where
    showGraphVizEdge :: (Declaration, Declaration) -> String
    showGraphVizEdge (v1, v2) = concat ["\"", v1.declOccName, "\" -> \"", v2.declOccName, "\";\n"]

--------------------
-- debugging
--------------------

dumpRootDeclDeps :: [Declaration] -> DependencyGraph -> IO ()
dumpRootDeclDeps rootDecls callGraph = forM_ rootDecls printTargetDecl
  where
    printTargetDecl :: Declaration -> IO ()
    printTargetDecl decl = do
        let deps = Alg.reachable callGraph decl
            onlyLeaves = filter (null . (`AdjMap.postSet` callGraph)) deps

        unless (null deps) $ putStrLn $ show decl <> ": " <> intercalate ", " (show <$> onlyLeaves)