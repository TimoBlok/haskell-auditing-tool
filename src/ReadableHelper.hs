{-# LANGUAGE OverloadedRecordDot #-}
module ReadableHelper where

import GHC.Utils.Outputable (Outputable, ppr, defaultSDocContext, showSDocOneLine)
import GHC ( Module, moduleUnit, moduleName )


import Data.List ( init, dropWhileEnd, takeWhile )
import Data.Char ( isDigit )

-- | Get a dis-ambiguated name for a Haskell module
-- e.g. "bifunctors.Bifunctor.Joker"
modNameU :: Module -> String
modNameU m = pkgNameH m <> "." <> modNameH m

-- | Get a human-friendly package name for a Haskell module
-- e.g. "bifunctors"
pkgNameH :: Module -> String
pkgNameH m =
  let
    pkg = pkgNameU m
  in
    if '.' `elem` pkg
      then init . dropWhileEnd isDigit . takeWhile (/= '.') $ pkg
      else pkg
-- | Get a human-friendly name for a Haskell module
-- e.g. "Bifunctor.Joker"
modNameH :: Module -> String
modNameH m = pps m.moduleName

-- | Get a package name for a Haskell module, including a version and hash
-- e.g. "bifunctors-5.6.1-877d0f0f346afdfec0c01d8284a7cf7b3710e175e3747519f4d0eed1c5ee46d6"
pkgNameU :: Module -> String
pkgNameU m = pps m.moduleUnit

ffiNameH :: String -> String
ffiNameH = takeWhile (/= ':') . drop 1 . dropWhile (/= ':')

pps :: Outputable a => a -> String
pps = showSDocOneLine defaultSDocContext . ppr