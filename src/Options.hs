module Options (
    getOpts,
    Options (..),
    RootModules
    ) where 

import Options.Applicative
import GHC.Unit.Module (ModuleName, mkModuleName)

data Options = Options {
    envFile :: FilePath, 
    targetDecls :: FilePath, 
    useGraphViz :: Bool,
    graphVizFile :: FilePath,
    rootModules :: RootModules}

type RootModules = [ModuleName]

getOpts :: IO Options
getOpts = execParser ((parseOpts <**> helper) `info` fullDesc)

-- Command-line options
parseOpts :: Parser Options
parseOpts = Options
    <$> strOption
        (   long "ghc-environment"
        <>  metavar "GHCENVIRONMENT"
        <>  short 'e'
        <>  help "FilePath to the ghc environment file that came from e.g. running your cabal project with the flag --write-ghc-environment-files=always"
        <>  showDefault
        <>  value "."
        )
    <*> strOption
        (   long "target-decls"
        <>  metavar "TARGETDECLS"
        <>  short 't'
        <>  help "Txt file with the target function declerations to check whether anything depends on one of them"
        <>  showDefault
        <>  value "target-decls.txt"
        )
    <*> switch
        (   long "use-graphviz"
        <>  short 'g'
        <>  help "Whether graphviz dot file is written. Specify filePath with graphviz-dot-path"
        <>  showDefault
        )
    <*> strOption
        (   long "graphviz-dot-path"
        <>  metavar "DOTPATH"
        <>  short 'd'
        <>  help "The prefered path of where the .dot file will be stored"
        <>  showDefault
        <>  value ""
        )
    <*> some (mkModuleName <$> strOption
        (   long "rootmodules"
        <>  metavar "ROOTMODULES"
        <>  short 'r'
        <>  help "the modules that will be analized"
        ))
