module Main where
-- Script to analyse function dependency

-- N.B. build this with cabal or nix.
--  stack is not supported since (as of 26 Oct 2023) no stackage revision
--  is compatible with the hiedb dependency


import Control.Monad (forM_, when)
import Options.Applicative
import Prelude hiding (id)
import System.IO (hFlush, stdout)

import GhcSession
import Analysis

-- Command-line options
parseOpts :: Parser Options
parseOpts = Options
    <$> strOption
        (   long "ghc-environment"
        <>  metavar "ENVIRONMENT"
        <>  short 'e'
        <>  help "FilePath to the env file that came from e.g. running your cabal project with the flag --write-ghc-environment-files=always"
        <>  showDefault
        <>  value "."
        )
    <*> strOption
        (   long "blacklist"
        <>  metavar "BLACKLIST"
        <>  short 'b'
        <>  help "Txt file with the blacklisted functions"
        <>  showDefault
        <>  value "input/Blacklist"
        )
        
data Options = Options {envFile :: FilePath, blacklistFile :: FilePath}

main :: IO ()
main = do
    opts <- execParser ((parseOpts <**> helper) `info` fullDesc)
    return ()
