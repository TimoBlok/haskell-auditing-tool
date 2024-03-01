module Json (
  encodeFile,
  eitherDecodeFileStrict   
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

import Dependency ( Declaration )

instance ToJSON (AdjacencyMap Declaration) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (AdjacencyMap Declaration)

instance ToJSON Declaration where
    toEncoding = genericToEncoding defaultOptions
instance ToJSONKey Declaration
instance FromJSON Declaration
instance FromJSONKey Declaration
