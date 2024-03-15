module Options (
    getOpts,
    Options (..),
    RootModules
    ) where 

import Options.Applicative

data Options = Options {
    pathToJsonFiles :: FilePath, 
    targetDecls :: FilePath, 
    useGraphViz :: Bool,
    graphVizFile :: FilePath,
    rootModules :: RootModules}

type RootModules = [String]

getOpts :: IO Options
getOpts = execParser ((parseOpts <**> helper) `info` fullDesc)

-- Command-line options
parseOpts :: Parser Options
parseOpts = Options
    <$> strOption
        (   long "json"
        <>  long "json-files"
        <>  metavar "JSONFILES"
        <>  help "Filepath to directory with json files containing subgraphs"
        <>  showDefault
        <>  value "."
        )
    <*> strOption
        (   long "tds"
        <>  long "target-decls"
        <>  metavar "TARGETDECLS"
        <>  help "Txt file with the target function declerations to check whether anything depends on one of them"
        <>  showDefault
        <>  value "target-decls.txt"
        )
    <*> switch
        (   long "use-graphviz"
        <>  short 'g'
        <>  help "Whether graphviz dot file is written. Specify filePath with --graphviz-dot-path"
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
    <*> some (strArgument
        (   metavar "ROOTMODULES"
        <>  help "the modules that will be analized"
        ))
