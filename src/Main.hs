{-# LANGUAGE OverloadedRecordDot #-}

-- | Script to analyse function dependency
module Main where

import Options ( Options(..), getOpts )
import Output ( TargetDecls, outputAnalysis ) 
import Dependency (Declaration(Declaration), mkDeclaration)
import Collect (collectDependencies)

import GHC.IO.Encoding

main :: IO ()
main = do
    setLocaleEncoding utf8

    opts <- getOpts

    targetDecls <- getTargetDecls opts.targetDecls

    putStrLn "Collecting json files..."

    depGraph <- collectDependencies opts.pathToJsonFiles

    putStrLn "Done!"

    outputAnalysis targetDecls opts depGraph
    return ()

getTargetDecls :: FilePath -> IO TargetDecls
getTargetDecls fp = do
    map parseTargetDecls . lines <$> readFile fp 
    where 
        parseTargetDecls :: String -> Declaration
        parseTargetDecls line = case words line of 
            (unit:mod:occName:xs) -> mkDeclaration unit mod occName False
            _ -> error "target declaration is in the wrong format! Expecting:\n\"unitName moduleName occName --text after fourth space is ignored"
