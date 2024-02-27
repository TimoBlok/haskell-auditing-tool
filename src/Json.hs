module Json where 

import Data.Aeson
import Data.Text
import GHC.Generics

import Dependency

instance ToJSON (AdjacencyMap Declaration) where 
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (AdjacencyMap Declaration)

instance Generic Declaration

instance ToJSON Declaration where 
    toEncoding = genericToEncoding defaultOptions
instance ToJSONKey Declaration
instance FromJSON Declaration
instance FromJSONKey Declaration

instance Generic FastString

instance ToJSON FastString where 
    toJSON = toJSON . unpackFS

instance FromJSON FastString where 
    parseJSON (String text) = mkFastString . unpack <$> pure text