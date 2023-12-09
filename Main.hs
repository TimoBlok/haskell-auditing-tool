
-- Script to analyse cross-module dependency cycles

-- N.B. build this with cabal or nix.
--  stack is not supported since (as of 26 Oct 2023) no stackage revision
--  is compatible with the hiedb dependency

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

import Algebra.Graph.AdjacencyMap (AdjacencyMap, edgeList, vertexList)
import qualified Algebra.Graph.AdjacencyMap as AdjacencyMap
import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import Control.Monad (forM_, when)
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.FileEmbed (embedStringFile,makeRelativeToProject)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
import Text.InterpolatedString.Perl6
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import HieDb
import qualified HieDb
import Options.Applicative
import Prelude hiding (id)
import System.IO (hFlush, stdout)


-- Command-line options
parseOpts :: Parser Options
parseOpts = Options
    <$> strArgument
        (   metavar "HIEDIR"
        <>  help "File path to look for .hie files in"
        <>  showDefault
        <>  value "default-input/hie-files/"
        )
    <*> strOption
        (   long "cache"
        <>  short 'c'
        <>  help "Database file to cache intermediate data in"
        <>  metavar "DBFILE"
        <>  showDefault
        <>  value "default-input/deps.sqlite"
        )
    <*> strOption
        (   long "blacklist"
        <>  short 'b'
        <>  help "Txt file with the blacklisted functions"
        <>  metavar "BLACKLIST"
        <>  showDefault
        <>  value "default-input/"
        )
data Options = Options {hieDir :: FilePath, dbFile :: FilePath, outDir :: FilePath}

main :: IO ()
main = do
    opts <- execParser ((parseOpts <**> helper) `info` fullDesc)
    withHieDb (dbFile opts) $ \db -> do
        -- Get the definition-dependency graph
        g <- getOrCreateGraph (hieDir opts) db
        print $ vertexList g
        return ()

-- Getter functions for HieDb.Vertex
v2ident, v2module, v2fqident :: HieDb.Vertex -> String
v2ident (_,_,ident,_,_,_,_) = tail ident
v2module (mod,_,_,_,_,_,_) = mod
v2fqident v = concat $
    [v2module v, ".", v2ident v , "@[", show $ v2sl v, ",", show $ v2el v,"]"]
v2sl (_,_,_,sl,_,_,_) = sl
v2el (_,_,_,_,_,el,_) = el

-- Load HieDb from file (creating file if does not exist)
getOrCreateGraph :: FilePath -> HieDb -> IO (AdjacencyMap HieDb.Vertex)
getOrCreateGraph hieDir db = do 
    initConn db
    g0 <- getGraph db
    if g0 /= AdjacencyMap.empty then return g0 else do
        putStrLn $ "Graph empty, loading hiefiles from " ++ show hieDir
        hiefiles <- getHieFilesIn hieDir
        when (hiefiles == []) $ putStrLn "No hiefiles found, run `make type-check-no-deps`"
        ncr <- newIORef =<< makeNc
        runDbM ncr $ addRefsFrom db `mapM_` hiefiles
        getGraph db
