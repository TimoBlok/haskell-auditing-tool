
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | All the types and instances we need for the analysis
module Dependency where 

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, hcat, showSDocOneLine, text)
import Data.Semigroup (Semigroup)
import Data.Map.Strict (Map, unionWith)

import GHC.Generics ( Generic )

import ReadableHelper ( pps )

-- | DependencyGraph (aka the call graph) 
-- together with a map with how many times a decl is called
data Dependencies = Dependencies {
  graph :: DependencyGraph,
  
  -- how many times a certain decl references another certain decl.
  -- We need this because the graph does not keep track of when a decl is referenced multiple times
  callers :: Map (Declaration, Declaration) Int 
  }
  deriving (Generic)

type DependencyGraph = AdjacencyMap Declaration

-- | stores all the data we need to describe a funtion declaration
-- no hashes included in unitId
data Declaration = Declaration
  { declModuleName :: String
  , declUnitId :: String
  , declOccName :: String
  , declIsIO :: Bool
  } deriving (Generic)

type UnitString = String
type ModuleString = String
type OccNameString = String
type IsIO = Bool

mkDeclaration :: UnitString -> ModuleString -> OccNameString -> IsIO -> Declaration
mkDeclaration unit mod occName isIO = Declaration {
  declUnitId = unit, 
  declModuleName = mod, 
  declOccName = occName, 
  declIsIO = isIO}

instance Outputable Declaration where
  ppr decl =
    hcat [text decl.declUnitId, ":" ,text decl.declModuleName, ":", text decl.declOccName]

instance Show Declaration where
  show = pps

instance Eq Declaration where
  d1 == d2 = 
    d1.declModuleName == d2.declModuleName &&
    d1.declUnitId     == d2.declUnitId     && 
    d1.declOccName    == d2.declOccName

instance Ord Declaration where
  compare d1 d2 =
    compare d1.declModuleName d2.declModuleName <> 
    compare d1.declUnitId     d2.declUnitId     <> 
    compare d1.declOccName    d2.declOccName

instance Monoid Dependencies where 
  mempty = Dependencies mempty mempty

instance Semigroup Dependencies where 
  Dependencies {graph=graph1, callers=callers1} <> 
    Dependencies {graph=graph2, callers=callers2} = 
      Dependencies { 
        graph=graph1 <> graph2, 
        callers= unionWith (+) callers1 callers2  -- adds number of callers when collision
        }
