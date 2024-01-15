module Options (
    getOpts,
    Options (..),
    RootModules
    ) where 

import Options.Applicative
    ( Alternative(some),
      (<**>),
      fullDesc,
      help,
      info,
      long,
      metavar,
      short,
      showDefault,
      strOption,
      value,
      execParser,
      helper,
      Parser )
import GHC.Unit.Module (ModuleName, mkModuleName)

data Options = Options {
    envFile :: FilePath, 
    targetDecls :: FilePath, 
    rootModules :: RootModules}

type RootModules = [ModuleName]

getOpts :: IO Options
getOpts = execParser ((parseOpts <**> helper) `info` fullDesc)

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
        (   long "target-decls"
        <>  metavar "TARGETDECLS"
        <>  short 'd'
        <>  help "Txt file with the target function declerations to check whether anything depends on one of them"
        <>  showDefault
        <>  value "input/target-decls"
        )
    <*> some (mkModuleName <$> strOption
        (   long "rootmodules"
        <>  metavar "ROOTMODULES"
        <>  short 'r'
        <>  help "the modules that will be analized"
        ))
