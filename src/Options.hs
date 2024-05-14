module Options (
    getOpts,
    Options (..),
    ) where 

import Dependency (Declaration (..))

import Options.Applicative

data Options = Options {
    pathToJsonFiles :: FilePath, 
    useGraphViz :: Bool,
    graphVizFile :: FilePath,
    useCypher :: Bool,
    cypherFile :: FilePath,
    rootModules :: [String],
    rootUnits :: [String],
    targetModules :: [String],
    targetUnits :: [String],
    query :: [String]}

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
    <*> switch
        (   long "use-graphviz"
        <>  help "Whether graphviz dot file is written. Specify filePath with --graphviz-dot-path"
        <>  showDefault
        )
    <*> strOption
        (   long "graphviz-dot-path"
        <>  metavar "DOTPATH"
        <>  help "The prefered path of where the .dot file will be stored"
        <>  showDefault
        <>  value ""
        )
    <*> switch
        (   long "cypher"
        <>  long "neo4j"
        <>  help "Whether cypher file is written, which can be loaded into neo4j. Specify filePath with --cypher-path"
        <>  showDefault
        )
    <*> strOption
        (   long "cypher-path"
        <>  metavar "CYPHERPATH"
        <>  help "The prefered path of where the .cypher file will be stored"
        <>  showDefault
        <>  value ""
        )
    <*> option auto
        (   long "rm's"
        <>  metavar "ROOTMODULES"
        <>  help "output will only consider the part of the graph that start at one of these modules"
        <>  showDefault
        <>  value []
        ) 
    <*> option auto
        (   long "ru's"
        <>  metavar "ROOTUNITS"
        <>  help "output will only consider the part of the graph that start at one of these units"
        <>  showDefault
        <>  value []
        ) 
    <*> option auto
        (   long "tm's"
        <>  metavar "TARGETMODULES"
        <>  help "output will only consider the part of the graph that end at one of these modules"
        <>  showDefault
        <>  value []
        ) 
    <*> option auto
        (   long "tu's"
        <>  metavar "TARGETUNITS"
        <>  help "output will only consider the part of the graph that end at one of these units"
        <>  showDefault
        <>  value []
        ) 
    <*> option auto
        (   long "query"
        <>  metavar "QUERY"
        <>  help "get estimated permissions required by given declarations"
        <>  showDefault
        <>  value []
        ) 
