
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | All the types and instances we need for the analysis
module Dependency where 

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import GHC.Data.FastString (FastString, unconsFS, unpackFS, NonDetFastString (..))
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, hcat, showSDocOneLine)
import GHC.Unit.Module (ModuleName, moduleNameString)

-- | DependencyGraph (aka the call graph)
type DependencyGraph = AdjacencyMap Declaration

-- | stores all the data we need to describe a funtion decleration
data Declaration = Declaration
    { declModuleName :: FastString
    , declUnitId :: FastString
    , declOccName :: FastString
    }
    deriving (Eq)

instance Outputable Declaration where
    ppr decl =
        hcat [ppr decl.declUnitId, ":", ppr decl.declModuleName, ".", ppr decl.declOccName]

instance Show Declaration where
    show = showSDocOneLine defaultSDocContext . ppr

instance Ord Declaration where
    compare d1 d2 =
        fscomp d1.declModuleName d2.declModuleName
            <> fscomp d1.declUnitId d2.declUnitId
            <> fscomp d1.declOccName d2.declOccName
      where
        fscomp f1 f2 = compare (NonDetFastString f1) (NonDetFastString f2)

-- | Our own simplified version of the module type.
-- Only stores the data we wil need.
data ModuleFS = ModuleFS
    { moduleName :: ModuleName
    , moduleUnit :: Maybe FastString
    }
    deriving (Eq)

instance Ord ModuleFS where
    compare m1 m2 =
        compare m1.moduleName m2.moduleName
            <> compare (NonDetFastString <$> m1.moduleUnit) (NonDetFastString <$> m2.moduleUnit)

instance Show ModuleFS where
    show m = mUnit ++ moduleNameString m.moduleName
        where
            mUnit = case m.moduleUnit of
                Nothing -> ""
                Just fs -> unpackFS fs ++ ":"
