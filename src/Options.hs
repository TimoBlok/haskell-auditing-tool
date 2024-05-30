module Options (
    getOpts,
    Options (..),
    ) where 

import Dependency (Declaration (..))

import Options.Applicative

data Options = Options {
    pathToJsonFiles :: FilePath, 
    useGraphViz     :: Bool,
    graphVizFile    :: FilePath,
    useCypher       :: Bool,
    cypherFile      :: FilePath,
    trim            :: Bool,
    filterIO        :: Bool,
    rootModules     :: [String],
    rootUnits       :: [String],
    targetModules   :: [String],
    targetUnits     :: [String],
    middleUnits     :: [String],
    queries         :: [String]}

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
    <*> switch
        (   long "trim"
        <>  help "Whether the graph is trimmed based on specified roots, targets, or queries"
        <>  showDefault
        )
    <*> switch
        (   long "filter-io"
        <>  help "Whether the graph is trimmed based on whether the path ends in IO (including ffi)"
        <>  showDefault
        )
    <*> many (strOption 
        (   long "rm"
        <>  metavar "ROOTMODULES"
        <>  help "output will only consider the part of the graph that start at one of these modules"
        )) 
    <*> many (strOption 
        (   long "ru"
        <>  metavar "ROOTUNITS"
        <>  help "output will only consider the part of the graph that start at one of these units"
        )) 
    <*> many (strOption 
        (   long "tm"
        <>  metavar "TARGETMODULES"
        <>  help "output will only consider the part of the graph that end at one of these modules"
        )) 
    <*> many (strOption 
        (   long "tu"
        <>  metavar "TARGETUNITS"
        <>  help "output will only consider the part of the graph that end at one of these units"
        )) 
    <*> many (strOption 
        (   long "mu"
        <>  metavar "MIDDLEUNITS"
        <>  help "output will only consider the part of the graph that go through one of these units"
        )) 
    <*> many (strOption 
        (   long "query"
        <>  metavar "QUERY"
        <>  help "Get estimated permissions required by given declarations. In the form of: example-unit:Module1.Module2:occname"
        )) 
