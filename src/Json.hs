module Json (
  encodeFile,
  eitherDecodeFileStrict,
  collectDependencies
) where

import Data.Aeson
    ( encodeFile,
      eitherDecodeFileStrict ,
      defaultOptions,
      genericToEncoding,
      FromJSON,
      FromJSONKey,
      ToJSON(toEncoding),
      ToJSONKey )

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AdjMap

import System.Directory ( listDirectory )
import System.FilePath ( takeExtension, hasExtension)
import Data.Char ( toLower )
import Control.Monad ( forM, when )

import Dependency ( Declaration , DependencyGraph)

instance ToJSON (AdjacencyMap Declaration) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (AdjacencyMap Declaration)

instance ToJSON Declaration where
    toEncoding = genericToEncoding defaultOptions
instance ToJSONKey Declaration
instance FromJSON Declaration
instance FromJSONKey Declaration


collectDependencies :: FilePath -> IO DependencyGraph
collectDependencies fp = do
  files <- listDirectory fp

  when (null files) $ error "no json files found"

  let jsonFiles = filter isJsonFile files
      jsonPaths = map ((fp ++) . ("/" ++)) jsonFiles

  eitherSubgraphs <- sequenceA <$> forM jsonPaths eitherDecodeFileStrict
  case eitherSubgraphs of
    Left e -> error $ show e
    Right subGraphs -> do
      --print subGraphs
      let depGraph = foldr AdjMap.overlay mempty subGraphs
      return depGraph
  where
    isJsonFile fp = hasExtension fp && ((== ".json") . map toLower . takeExtension) fp
