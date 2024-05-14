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

    let blacklist       = Set.filter (\d -> declIsIO d && isTargetUnit d.declUnitId) $ AdjMap.vertexSet depGraph --Set.fromList targetDecls
        trimmedDepGraph = keepRelevantNodes blacklist {- $ removeSomeLoops -} depGraph
        forest          = Alg.bfsForest trimmedDepGraph rootDecls
        whitelist       = AdjMap.vertexSet $ AdjMap.forest forest
        finalDepGraph   = trimDepGraphWithWhitelist whitelist trimmedDepGraph

    -- print blacklist
    -- print $ Set.intersection blacklist $ AdjMap.vertexSet depGraph

    putStrLn "Size of graph:"
    print $ Set.size $ AdjMap.vertexSet trimmedDepGraph

    -- printForest forest

    putStrLn "Analysis done!"
    handleGraphViz options trimmedDepGraph
    handleCypher options trimmedDepGraph
  where
    -- the decls that are the root decls with non empty dependencies
    rootDecls :: [Declaration]
    rootDecls = filter (\decl ->
      isRootModule decl.declModuleName &&
      not (isLeaf decl depGraph))
        (AdjMap.vertexList depGraph)

    isRootModule :: String -> Bool
    isRootModule m = m `elem` options.rootModules

    isTargetUnit :: String -> Bool
    isTargetUnit u = u `elem` options.rootModules


--------------------
-- Graph cleanup
--------------------
-- removes the loops of 1 node and loops of 2 nodes
removeSomeLoops :: DependencyGraph -> DependencyGraph
removeSomeLoops depGraph = induceEdges hasLoop1or2 depGraph
  where
    hasLoop1or2 edge = hasLoopSize2 edge || hasSelfLoop edge
    hasLoopSize2 (x,_) = AdjMap.preSet x depGraph == AdjMap.postSet x depGraph
    hasSelfLoop  = uncurry (==)

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
        -- invert the graph as postset is more efficient than preset
        inverseGraph = AdjMap.transpose depGraph
      in
        searchRelevantNodes Set.empty blacklist inverseGraph

    -- searching backwards from the blacklisted nodes
    -- using a set instead of a stack or queue, because order doesn't matter
    searchRelevantNodes :: RelevantNodes -> Set Declaration -> DependencyGraph -> RelevantNodes
    searchRelevantNodes visited nonvisited depGraph
      -- done searching
      | null nonvisited         = visited
      -- already been here
      | Set.member decl visited = searchRelevantNodes visited tailOfNonvisited depGraph
      -- found new unvisited node
      | otherwise               = searchRelevantNodes newVisited newNonvisited depGraph
      where
        decl             = Set.elemAt 0 nonvisited              -- grab new node to search next
        postList         = AdjMap.postSet decl depGraph         -- neighbours we also want to search at some point
        tailOfNonvisited = Set.delete decl nonvisited           -- remove searched node from search set
        newNonvisited    = Set.union postList tailOfNonvisited  -- add new neigbours to search set
        newVisited       = Set.insert decl visited              -- add searched node to visited nodes


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

handleCypher :: Options -> DependencyGraph -> IO ()
handleCypher options depGraph = do
  when options.useCypher $ do
    let basePath = options.cypherFile ++ intercalate "-" (map (last . splitOn ".") options.rootModules)
        usedTargetDecls = takeBaseName options.targetDecls
        graphPath = basePath ++ "." ++ usedTargetDecls ++ ".cypher"

    putStrLn "Outputting graph to: "
    putStrLn graphPath
    dumpCypher graphPath depGraph

-- | Writes to a .dot file in the dot language
dumpCypher :: FilePath -> DependencyGraph -> IO ()
dumpCypher fp depGraph = do
  writeFile fp $
    concatMap showCypherNode (AdjMap.vertexList depGraph)
    ++
    concatMap showCypherEdge (AdjMap.edgeList depGraph)
  where
    showCypherNode :: Declaration -> String
    showCypherNode decl = "Create (" ++ mkCypherVariable decl ++ ":Declaration {" ++ showCypherProps decl ++ "})\n"

    -- note how the values are inside quotes already
    showCypherProps :: Declaration -> String
    showCypherProps decl = "name:"     ++ quote (map dotToUnderscore decl.declOccName)
                        ++ ", module:" ++ quote (map dotToUnderscore decl.declModuleName)
                        ++ ", unit:"   ++ quote (map dotToUnderscore decl.declUnitId)

    quote :: String -> String
    quote s | '\'' `elem` s = "\"" ++ s ++ "\""
            | otherwise     = "\'" ++ s ++ "\'"

    showCypherEdge :: (Declaration, Declaration) -> String
    showCypherEdge (v1, v2) = "Create (" ++ mkCypherVariable v1 ++ ")-[:DEPENDS_ON]->" ++ "(" ++ mkCypherVariable v2 ++ ")\n"

    -- Names with non alphanumeric characters will have to escaped with backticks
    -- (Though underscore is allowed).
    -- Neo4j recommends doing this only if necessary.
    -- Hence, we are not escaping everything and we check instead.
    maybeQuotes :: String -> String
    maybeQuotes name | all (\c -> isAlphaNum c || c=='_') name = name
                     | otherwise = "`" ++ name ++ "`"

    -- variable can only be alphanum and underscores
    mkCypherVariable :: Declaration -> String
    mkCypherVariable = maybeQuotes . map dotToUnderscore . show

    dotToUnderscore :: Char -> Char
    dotToUnderscore '.' = '_'
    dotToUnderscore '\"' = '\''
    dotToUnderscore c   = c

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