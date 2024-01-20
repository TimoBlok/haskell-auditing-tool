{-# LANGUAGE OverloadedRecordDot #-}

-- | Script to analyse function dependency
module Main where

import Analysis ( analyze )
import Options ( Options(..), getOpts )
import Output ( TargetDecls, outputAnalysis ) 
import Dependency (Declaration(Declaration), mkDeclaration)

main :: IO ()
main = do
    opts <- getOpts

    targetDecls <- getTargetDecls opts.targetDecls

    putStrLn "starting analysis..."

    analysis <- analyze opts.rootModules

    putStrLn "analysis done."

    outputAnalysis targetDecls opts analysis
    return ()

getTargetDecls :: FilePath -> IO TargetDecls
getTargetDecls fp = do
    map parseTargetDecls . lines <$> readFile fp 
    where 
        parseTargetDecls :: String -> Declaration
        parseTargetDecls line = case words line of 
            (unit:mod:occName:xs) -> mkDeclaration unit mod occName
            _ -> error "target declaration is in the wrong format! Expecting:\n\"unitName moduleName occName --text after fourth space is ignored"
