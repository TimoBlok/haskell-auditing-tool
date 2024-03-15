
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | All the types and instances we need for the analysis
module Dependency where 

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, hcat, showSDocOneLine, text)

import GHC.Generics ( Generic )

-- | DependencyGraph (aka the call graph)
type DependencyGraph = AdjacencyMap Declaration

-- | stores all the data we need to describe a funtion declaration
data Declaration = Declaration
    { declModuleName :: String
    , declUnitId :: String
    , declOccName :: String
    } deriving (Generic, Eq)

type UnitString = String
type ModuleString = String
type OccNameString = String

mkDeclaration :: UnitString -> ModuleString -> OccNameString -> Declaration
mkDeclaration unit mod occName = Declaration {
    declUnitId = unit, 
    declModuleName = mod, 
    declOccName = occName}

instance Outputable Declaration where
    ppr decl =
        hcat [text decl.declUnitId, ":",text decl.declModuleName, ".", text decl.declOccName]

instance Show Declaration where
    show = showSDocOneLine defaultSDocContext . ppr

instance Ord Declaration where
    compare d1 d2 =
        compare d1.declModuleName d2.declModuleName
            <> compare d1.declUnitId d2.declUnitId
            <> compare d1.declOccName d2.declOccName

