{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module    : Geography.VectorTile
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- GIS Vector Tiles, as defined by Mapbox.
--
-- This library implements version 2.1 of the official Mapbox spec, as defined
-- here: https://github.com/mapbox/vector-tile-spec/tree/master/2.1
--
-- Note that currently this library ignores top-level protobuf extensions,
-- `Value` extensions, and "UNKNOWN" geometries.

module Geography.VectorTile where

import qualified Data.Map.Lazy as M
import           Data.Text (Text,pack)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Geography.VectorTile.Geometry
import qualified Geography.VectorTile.Raw as R
import           Text.Printf.TH

---

{- Types -}

-- | A high-level representation of a Vector Tile. At its simplest, a tile
-- is just a list of `Layer`s.
newtype VectorTile = VectorTile { layers :: V.Vector Layer } deriving (Eq,Show)

data Layer = Layer { version :: Int
                   , name :: Text
                   , points :: V.Vector (Feature Point)
                   , linestrings :: V.Vector (Feature LineString)
                   , polygons :: V.Vector (Feature Polygon)
                   -- Needed? How to structure Feature-shared metadata?
                   , keys :: V.Vector Text
                   , extent :: Int } deriving (Eq,Show)

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
                         , geometries :: V.Vector g } deriving (Eq,Show)

-- Needs expanding.
data Val = Str deriving (Eq,Show)

type family Convertable a
type instance Convertable (Feature g) = R.Feature
--type instance Convertable R.Feature = Feature
--type instance Convertable (Feature LineString) = R.Feature
--type instance Convertable (Feature Polygon) = R.Feature
--type instance Convertable R.Feature = Feature g
--type instance Convertable (Vector Point) = Word32   -- cool!
--type instance Convertable

class ToProtobuf a where
  toProto :: Convertable a -> a  -- perhaps?

-- compiles! But how to get the specific instances?
-- Perhaps `Point` needs to be converted to the `geometry` uint32.
--instance Geometry g => ToProtobuf (Feature g) where
--  toProto = undefined

{- FUNCTIONS -}

-- Just constrain for `Geometry` on the functions!
-- foo :: Geometry g => Feature g -> ...

-- | Euclidean distance.
{-}
distance :: Point -> Point -> Float
distance p1 p2 = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where dx = x p1 - x p2
        dy = y p1 - y p2
-}
