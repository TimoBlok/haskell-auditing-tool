{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Responsible for handling everything related to core specific information, using CoreBinds
module Core (
    getDependenciesFromCoreBinds,
) where


import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty

import Data.Map.Strict qualified as Map
import Data.Maybe ( isJust, fromMaybe )
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Builtin.Names (ioTyConName)
import GHC.Core (Alt (..), Bind (..), CoreBind, Expr (..))
import GHC.Core.Type (Var(..), splitTyConApp_maybe, splitForAllTyCoVars, splitFunTys)
import GHC.Core.TyCon (tyConName)
import GHC.Core.TyCo.Rep (Type)
import GHC.Generics (Generic)
import GHC.Types.Name (Name, OccName (occNameFS), nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (varName, idDetails)
import GHC.Types.Id.Info (IdDetails (..))
import GHC.Unit (moduleNameFS)
import GHC.Unit.Module (Module, moduleUnitId, moduleNameString)
import GHC.Unit.Types (GenModule (moduleName), UnitId (..), unitIdString)
import GHC.Data.FastString (unconsFS, unpackFS)
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, hcat, showSDocOneLine)

import Dependency ( Declaration(..), DependencyGraph )
import ReadableHelper

-- | reduceDependencies tidy up the output a bit.
-- Remove duplicate edge and merge top level nodes
reduceDependencies :: DependencyGraph -> DependencyGraph
reduceDependencies = AdjMap.induce isValuable
  where
    isValuable decl =
        -- Ignore useless vars
        not (isKrep decl.declOccName) &&
        not (isNameIgnored decl.declOccName)

    -- has to do with kinds
    isKrep =  (== "$krep") . take 5
    
    -- var that starts with '$tc' and '$tr' doesn't seem relevant
    isNameIgnored =  (\n -> n == "$tc" || n == "$tr") . take 3


getDependenciesFromCoreBinds :: Module -> [CoreBind] -> DependencyGraph
getDependenciesFromCoreBinds genModule coreBinds =
    reduceDependencies $ foldMap (getDependenciesFromCore genModule topVars) coreBinds
  where
    topVars = getTopVars coreBinds

-- | Collect all the top level Vars.
getTopVars :: [CoreBind] -> Set Var
getTopVars = go mempty
  where
    go acc [] = acc
    go acc (x : rest) = case x of
        NonRec b _ -> go (Set.insert b acc) rest
        Rec recs ->
            let recVars = Set.fromList (map fst recs)
             in go (Set.union recVars acc) rest

-- | Process a 'CoreBind' into a 'DependencyGraph'
getDependenciesFromCore :: Module -> Set Var -> CoreBind -> DependencyGraph
getDependenciesFromCore genModule topVars coreBind = case coreBind of

    NonRec b expr ->
      let decl = mkDecl genModule (varName b) $ hasIOResultType b
          deps = getExprDeps expr
        in
          AdjMap.star decl deps

    Rec xs -> foldMap (getDependenciesFromCore genModule topVars . uncurry NonRec) xs
  where
    -- Check if a variable comes from an external module
    isExternalVar :: Var -> Bool
    isExternalVar var = case nameModule_maybe (varName var) of
        Just varGenModule -> varGenModule /= genModule
        Nothing -> False

    getExprDeps :: Expr Var -> [Declaration]
    getExprDeps = \case
        Var var
            | -- Only track external or top level vars
              isExternalVar var || var `Set.member` topVars ->
                [varDecl genModule var]
            | -- And ignore local or shadow vars
              otherwise ->
                []
        Lit _lit -> mempty
        App expr arg -> foldMap getExprDeps [expr, arg]
        Lam _b expr -> getExprDeps expr
        Let bind expr -> getBindDeps bind <> getExprDeps expr
        Case expr _b _type alt -> getExprDeps expr <> foldMap getAltDeps alt
        Cast expr _coer -> getExprDeps expr
        Tick _ expr -> getExprDeps expr
        Type _type -> mempty
        Coercion _coer -> mempty

    getBindDeps :: CoreBind -> [Declaration]
    getBindDeps = \case
        NonRec _b expr -> getExprDeps expr
        Rec xs -> foldMap (\(_b, expr) -> getExprDeps expr) xs

    getAltDeps :: Alt Var -> [Declaration]
    getAltDeps = \case
        Alt _altCon _bs expr -> getExprDeps expr

varDecl :: Module -> Var -> Declaration
varDecl genModule var = case mkGlobalDecl name isWorthKeepingTrackOf of
    Just decl -> decl
    Nothing -> mkDecl genModule name isWorthKeepingTrackOf
  where
    isWorthKeepingTrackOf = hasIOResultType var || isFFICall var
    name  = varName var


mkGlobalDecl :: Name -> Bool -> Maybe Declaration
mkGlobalDecl name isIO = do
    genModule <- nameModule_maybe name
    pure $ mkDecl genModule name isIO

-- | Create a node for the call graph.
mkDecl :: Module -> Name -> Bool -> Declaration
mkDecl genModule name declIsIO = Declaration {declUnitId, declModuleName, declOccName, declIsIO}
  where
    declUnitId = pkgNameH genModule
    declModuleName = modNameH genModule
    declOccName = occNameString (nameOccName name)

isFFICall :: Var -> Bool 
isFFICall var | (FCallId _) <- idDetails var = True
              | otherwise                    = False

hasIOResultType :: Var -> Bool
hasIOResultType var = 
  let 
    resTy = findResType $ varType var
  in 
    case splitTyConApp_maybe resTy of 
      Nothing -> False
      Just (tyCon, _) -> tyConName tyCon == ioTyConName

-- | whole type -> (typeOrValueArgs, resultType)
findResType :: Type -> Type
findResType ty
  | (tyCoVars@(_:_), resTy)    <- splitForAllTyCoVars ty = findResType resTy
  | (scaledTypes@(_:_), resTy) <- splitFunTys ty         = findResType resTy
  | otherwise                                            = ty