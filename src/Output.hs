{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Output where

import qualified Algebra.Graph.AdjacencyMap as AdjMap
import Control.Monad ( unless, forM_, when )
import Data.List ( intercalate, nub, dropWhileEnd )
import Data.List.Split ( splitOn )
import Data.Char ( isAlphaNum )

import Dependency
    ( Declaration(..), DependencyGraph )
import Options ( Options(..) )

outputAnalysis :: Options -> DependencyGraph -> IO ()
outputAnalysis options depGraph = do
    handleGraphViz options depGraph
    handleCypher options depGraph
  where

handleGraphViz :: Options -> DependencyGraph -> IO ()
handleGraphViz options depGraph = do
  when options.useGraphViz $ do    
    let graphPath = options.graphVizFile ++ personalisedFileName options ++ ".dot"

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
    let graphPath =  options.cypherFile ++ personalisedFileName options ++ ".cypher"

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

-- generates a file path based on the arguments given to the porgram
-- /Graph_RootModule1.RootModule2_root-unit1.root_unit2_TargetModule1.TargetModule2_target-unit1.target-unit2
personalisedFileName :: Options -> FilePath 
personalisedFileName options = 
  let  
    usedRootModules   = intercalate "." $ map (last . splitOn ".") options.rootModules
    usedRootUnits     = intercalate "."  options.rootUnits
    usedTargetModules = intercalate "." $ map (last . splitOn ".") options.targetModules
    usedTargetUnits   = intercalate "."  options.targetUnits
    usedMiddleUnits   = intercalate "."  options.middleUnits
    usedInput         = "_" ++ intercalate "_" [usedRootModules, usedRootUnits, usedTargetModules, usedTargetUnits, usedMiddleUnits]
    final             = dropWhileEnd (== '_') usedInput
  in
    "Graph" ++ final

--------------------
-- debugging
--------------------

-- dumpRootDeclDeps :: [Declaration] -> DependencyGraph -> IO ()
-- dumpRootDeclDeps rootDecls callGraph = forM_ rootDecls printTargetDecl
--   where
--     printTargetDecl :: Declaration -> IO ()
--     printTargetDecl decl = do
--         let deps = Alg.reachable callGraph decl
--             onlyLeaves = filter (null . (`AdjMap.postSet` callGraph)) deps
--         unless (null deps) $ putStrLn $ show decl <> ": " <> intercalate ", " (show <$> onlyLeaves)