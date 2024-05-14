{-# LANGUAGE OverloadedRecordDot #-}
module TrimGraph (
  trimGraph
) where 

import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alg
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.List (foldl')

import Dependency
import Options

type Blacklist = Set Declaration
type RelevantNodes = Set Declaration

trimGraph :: Options -> DependencyGraph -> DependencyGraph
trimGraph options = removeSomeLoops . trimFromRoots . trimFromTargets 
  where 
    trimFromTargets :: DependencyGraph -> DependencyGraph
    trimFromTargets depGraph' = 
      let 
        blacklist = Set.filter (\d -> declIsIO d && isTargetDecl d) $ AdjMap.vertexSet depGraph'
      in 
        keepRelevantNodes blacklist depGraph'

    trimFromRoots :: DependencyGraph -> DependencyGraph
    trimFromRoots depGraph' = 
      let 
        rootDecls = filter isRootDecl $ AdjMap.vertexList depGraph'
        forest    = Alg.bfsForest depGraph' rootDecls
        whitelist = AdjMap.vertexSet $ AdjMap.forest forest
      in 
        trimDepGraphWithWhitelist whitelist depGraph'

    -- decl is part either part of specified module or unit, or nothing was specified
    isRootDecl :: Declaration -> Bool
    isRootDecl decl = 
      decl.declUnitId     `elem` options.rootUnits   || 
      decl.declModuleName `elem` options.rootModules ||
      show decl           `elem` options.query       ||
      (null options.rootUnits   && 
       null options.rootModules &&
       null options.query)

    -- decl is part either part of specified module or unit, or nothing was specified
    isTargetDecl :: Declaration -> Bool
    isTargetDecl decl = 
      decl.declUnitId     `elem` options.targetUnits   || 
      decl.declModuleName `elem` options.targetModules || 
      (null options.targetUnits && null options.targetUnits)

-- removes the loops of 1 node and loops of 2 nodes
removeSomeLoops :: DependencyGraph -> DependencyGraph
removeSomeLoops depGraph = induceEdges hasLoop1or2 depGraph
  where
    hasLoop1or2 edge = hasLoopSize2 edge || hasSelfLoop edge
    hasLoopSize2 (x,_) = AdjMap.preSet x depGraph == AdjMap.postSet x depGraph
    hasSelfLoop = uncurry (==)

-- | like induce but on edges
induceEdges :: ((Declaration, Declaration) -> Bool) -> DependencyGraph -> DependencyGraph
induceEdges p depGraph = foldl' (removeEdgeIf p) depGraph $ AdjMap.edgeList depGraph
  where
    removeEdgeIf :: ((Declaration, Declaration) -> Bool) -> DependencyGraph -> (Declaration, Declaration) -> DependencyGraph
    removeEdgeIf p depGraph' edge | p edge    = uncurry AdjMap.removeEdge edge depGraph'
                                  | otherwise = depGraph'

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

