module Options where 

import Options.Applicative
import GHC.Unit.Module (ModuleName, mkModuleName)

data Options = Options {envFile :: FilePath, blacklistFile :: FilePath, rootModules :: [ModuleName]}

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
        (   long "blacklist"
        <>  metavar "BLACKLIST"
        <>  short 'b'
        <>  help "Txt file with the blacklisted functions"
        <>  showDefault
        <>  value "input/Blacklist"
        )
    <*> some (mkModuleName <$> strOption
        (   long "rootmodules"
        <>  metavar "ROOTMODULES"
        <>  short 'r'
        <>  help "the modules that will be analized"
        ))
