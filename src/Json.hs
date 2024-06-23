
-- | Responsible for dealing with JSON
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
import Data.Foldable (fold)

import Dependency ( Declaration , DependencyGraph, Dependencies(..))

instance ToJSON Dependencies where 
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Dependencies

instance ToJSON (AdjacencyMap Declaration) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (AdjacencyMap Declaration)

instance ToJSON Declaration where
    toEncoding = genericToEncoding defaultOptions
instance ToJSONKey Declaration
instance FromJSON Declaration
instance FromJSONKey Declaration


collectDependencies :: FilePath -> IO Dependencies
collectDependencies fp = do
  files <- listDirectory fp

  let isJsonFile fp = hasExtension fp && ((== ".json") . map toLower . takeExtension) fp
      jsonFiles     = filter isJsonFile files

  when (null jsonFiles) $ error "no json files found"

  let jsonPaths = map ((fp ++) . ("/" ++)) jsonFiles

  eitherSubgraphs <- sequenceA <$> forM jsonPaths eitherDecodeFileStrict
  case eitherSubgraphs of
    Left e -> error $ show e
    Right subGraphs -> do
      let depGraph = fold subGraphs
      return depGraph
