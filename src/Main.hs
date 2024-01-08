{-# LANGUAGE OverloadedRecordDot #-}
module Main where
-- Script to analyse function dependency

import Control.Monad (forM_, when, unless)
import Prelude hiding (id)
import System.IO (hFlush, stdout)
import Analysis
import Data.List (intercalate)
import Options


main :: IO ()
main = do
    opts <- getOpts

    putStrLn "starting analysis..."

    analysis <- analyze opts.rootModules

    putStrLn "analysis done."

    showAnalysis opts analysis
    return ()
