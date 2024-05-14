{-# LANGUAGE OverloadedRecordDot #-}

-- | Script to analyse function dependency
module Main where

import Options ( Options(..), getOpts )
import Output ( outputAnalysis ) 
import Dependency (Declaration(Declaration), mkDeclaration)
import Json (collectDependencies)
import TrimGraph 

import GHC.IO.Encoding

main :: IO ()
main = do
    setLocaleEncoding utf8

    opts <- getOpts

    putStrLn "Collecting json files..."

    depGraph <- collectDependencies opts.pathToJsonFiles

    putStrLn "Done!"

    let newGraph = trimGraph opts depGraph

    outputAnalysis opts newGraph

    return ()