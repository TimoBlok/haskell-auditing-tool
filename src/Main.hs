{-# LANGUAGE OverloadedRecordDot #-}

-- | Script to analyse function dependency
module Main where

import Algebra.Graph.AdjacencyMap (vertexCount, edgeCount)

import Options ( Options(..), getOpts )
import Output ( outputAnalysis ) 
import Dependency (Declaration(Declaration), mkDeclaration, Dependencies(..))
import Json (collectDependencies)
import TrimGraph 

import GHC.IO.Encoding

main :: IO ()
main = do
    setLocaleEncoding utf8

    opts <- getOpts

    putStrLn "Collecting json files..."

    deps <- collectDependencies opts.pathToJsonFiles

    putStrLn "Done!"

    putStrLn $ "Found " ++ show (vertexCount deps.graph) ++ " nodes."
    putStrLn $ "Found " ++ show (edgeCount deps.graph) ++ " edges."

    let newGraph :: Dependencies
        newGraph = deps {graph = trimGraph opts deps.graph}

    putStrLn $ "Filtered down to " ++ show (vertexCount newGraph.graph) 
      ++ " nodes and " ++ show (edgeCount newGraph.graph) ++ " edges."

    outputAnalysis opts newGraph

    return ()