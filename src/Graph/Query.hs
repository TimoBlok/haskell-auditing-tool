module Graph.Query (
  queryGraph
) where 

import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Alg
import Data.Set (Set)
import qualified Data.Set as Set

import Dependency
import Options

type QueryResults = [(Declaration, Set Declaration)]
type Query = Declaration

queryGraph :: Options -> [Query] -> DependencyGraph -> QueryResults
queryGraph = undefined