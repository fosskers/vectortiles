{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Gaia.VectorTile
-- Copyright : (c) Colin Woodbury, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- GIS Vector Tiles, as defined by Mapbox.
--
-- This library implements version 2.1 of the official Mapbox spec, as defined
-- here: https://github.com/mapbox/vector-tile-spec/tree/master/2.1

module Gaia.VectorTile where

import qualified Data.Map.Lazy as M
import           Data.Text (Text)
import           Data.Vector
import qualified Data.Vector.Unboxed as U

---

{- NOTES

Ignores top-level protobuf extensions.
Ignores `Value` extensions
Ignores "UNKNOWN" geometries

-}

-- | A high-level representation of a Vector Tile. At its simplest, a tile
-- is just a list of `Layer`s.
newtype VectorTile = VectorTile { layers :: Vector Layer } deriving (Eq,Show)

data Layer = Layer { version :: Int
                   , name :: Text
                   , points :: Vector (Feature Point)
                   , linestrings :: Vector (Feature LineString)
                   , polygons :: Vector (Feature Polygon)
                   -- Needed? How to structure Feature-shared metadata?
                   , keys :: Vector Text
                   , extent :: Int } deriving (Eq,Show)

-- | Points in space. Using "Record Pattern Synonyms" here allows us to treat
-- `Point` like a normal ADT, while its implementation remains an unboxed
-- @(Int,Int)@.
type Point = (Int,Int)
pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

-- | Points are just vectors in R2, and thus form a Vector space.
instance Monoid Point where
  mempty = Point 0 0
  (Point x y) `mappend` (Point x' y') = Point (x + x') (y + y')

-- | `newtype` compiles away to expose only the unboxed `Vector` at runtime.
newtype LineString = LineString { points :: U.Vector Point } deriving (Eq,Show)

-- | Question: Do we want Polygons to know about their inner polygons?
-- If not, we get the better-performing implementation below.
data Polygon = Polygon { points :: U.Vector Point
                       , inner :: [Polygon] } deriving (Eq,Show)

{-
-- | Very performant for the same reason as `LineString`.
newtype Polygon = Polygon { points :: U.Vector Point } deriving (Eq,Show)
-}

-- | A geographic feature. Features are a set of geometries that share
-- some common theme:
--
-- * Points: schools, gas station locations, etc.
-- * LineStrings: Roads, power lines, rivers, etc.
-- * Polygons: Buildings, water bodies, etc.
--
-- Where, for instance, all school locations may be stored as a single
-- `Feature`, and no `Point` within that `Feature` would represent anything
-- else.
data Feature g = Feature { featureId :: Int
                         , metadata :: M.Map Text Val
                         , geometries :: Vector g } deriving (Eq,Show)

-- Needs expanding.
data Val = Str deriving (Eq,Show)

-- | Dummy class and instances to enforce constraints.
class Geometry a where
instance Geometry Point
instance Geometry LineString
instance Geometry Polygon

-- Just constrain for `Geometry` on the functions!
-- foo :: Geometry g => Feature g -> ...

-- | Euclidean distance.
distance :: Point -> Point -> Float
distance p1 p2 = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where dx = x p1 - x p2
        dy = y p1 - y p2
