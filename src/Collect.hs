module Collect where

import qualified Algebra.Graph.AdjacencyMap as AdjMap

import System.Directory ( listDirectory )
import System.FilePath ( takeExtension, hasExtension)
import Data.Char ( toLower )
import Control.Monad ( forM, when )

import Dependency ( DependencyGraph )
import Json ( eitherDecodeFileStrict  )


collectDependencies :: FilePath -> IO DependencyGraph
collectDependencies fp = do
  files <- listDirectory fp

  when (null files) $ error "no json files found"

  

  putStrLn $ "trying just: " ++ files !! 2
  print =<< (eitherDecodeFileStrict (fp ++ "/" ++ head files) :: IO (Either String DependencyGraph))

  let jsonFiles = filter isJsonFile files
      jsonPaths = map ((fp ++) . ("/" ++)) jsonFiles

  eitherSubgraphs <- sequenceA <$> forM jsonPaths eitherDecodeFileStrict
  case eitherSubgraphs of
    Left e -> error $ show e
    Right subGraphs -> do
      print subGraphs
      let depGraph = foldr AdjMap.overlay mempty subGraphs
      return depGraph
  where
    isJsonFile fp = hasExtension fp && ((== ".json") . map toLower . takeExtension) fp


