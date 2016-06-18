{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
-- /Value/ extensions, and /UNKNOWN/ geometries.
--
-- The order in which to explore the modules of this library is as follows:
--
-- 1. "Geography.VectorTile" (here)
-- 2. "Geography.VectorTile.Geometry"
-- 3. "Geography.VectorTile.Raw"

module Geography.VectorTile
  ( -- * Types
    VectorTile(..)
  , Layer(..)
  , Feature(..)
  , Val(..)
    -- * Protobuf Conversions
    -- ** From Protobuf
    -- | Generally the `tile` function is the only one needed here. Usage:
    --
    -- > import qualified Geography.VectorTile.Raw as R
    -- >
    -- > R.decode someBytes >>= tile
    --
    -- Note that since the "Data.ProtocolBuffers" library does not handle default
    -- values, we handle those specifically defined in /vector_tile.proto/
    -- explicitely here. See:
    --
    -- https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto
  , tile
  , layer
  , features
  , value
    -- ** To Protobuf
  ) where

import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData)
import           Data.Foldable (foldrM)
import           Data.Int
import           Data.List (sortOn, groupBy)
import qualified Data.Map.Lazy as M
import           Data.ProtocolBuffers
import           Data.Text (Text,pack)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Geography.VectorTile.Geometry
import qualified Geography.VectorTile.Raw as R
import           Geography.VectorTile.Util
import           GHC.Generics (Generic)
import           Text.Printf.TH
---

{- Types -}

-- | A high-level representation of a Vector Tile. At its simplest, a tile
-- is just a list of `Layer`s.
--
-- There is potential to implement `layers` as a `M.Map`, with its String-based
-- `name` as a key.
newtype VectorTile = VectorTile { layers :: V.Vector Layer } deriving (Eq,Show,Generic)

instance NFData VectorTile

-- | A layer, which could contain any number of `Feature`s of any `Geometry` type.
-- This codec only respects the canonical three `Geometry` types, and we split
-- them here explicitely to allow for more fine-grained access to each type.
data Layer = Layer { version :: Int  -- ^ The version of the spec we follow. Should always be 2.
                   , name :: Text
                   , points :: V.Vector (Feature Point)
                   , linestrings :: V.Vector (Feature LineString)
                   , polygons :: V.Vector (Feature Polygon)
                   , extent :: Int  -- ^ Default: 4096
                   } deriving (Eq,Show,Generic)

instance NFData Layer

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
--
-- Note: Each `Geometry` type and their /Multi*/ counterpart are considered
-- the same thing, as a `V.Vector` of that `Geometry`.
data Feature g = Feature { featureId :: Int  -- ^ Default: 0
                         , metadata :: M.Map Text Val
                         , geometries :: V.Vector g } deriving (Eq,Show,Generic)

instance NFData g => NFData (Feature g)

-- | Legal Metadata /Value/ types. Note that `S64` are Z-encoded automatically
-- by the underlying "Data.ProtocolBuffers" library.
data Val = St Text | Fl Float | Do Double | I64 Int64 | W64 Word64 | S64 Int64 | B Bool
         deriving (Eq,Show,Generic)

instance NFData Val

-- | Convert a raw `R.VectorTile` of parsed protobuf data into a useable
-- `VectorTile`.
tile :: R.VectorTile -> Either Text VectorTile
tile = fmap (VectorTile . V.fromList) . mapM layer . getField . R.layers

-- | Convert a single raw `R.Layer` of parsed protobuf data into a useable
-- `Layer`.
layer :: R.Layer -> Either Text Layer
layer l = do
  (ps,ls,polys) <- features keys vals . getField $ R.features l
  pure Layer { version = fromIntegral . getField $ R.version l
             , name = getField $ R.name l
             , points = ps
             , linestrings = ls
             , polygons = polys
             , extent = maybe 4096 fromIntegral (getField $ R.extent l) }
  where keys = getField $ R.keys l
        vals = getField $ R.values l

-- | Convert a list of raw `R.Feature`s of parsed protobuf data into `V.Vector`s
-- of each of the three legal `Geometry` types.
--
-- The long type signature is due to the fact that `R.Layer`s and `R.Feature`s
-- are strongly coupled at the protobuf level. In order to achieve higher
-- compression ratios, `R.Layer`s contain all metadata in key/value lists
-- to be shared across their `R.Feature`s, while those `R.Feature`s store only
-- indexes into those lists. As a result, this function needs to be passed
-- those key/value lists from the parent `R.Layer`, and a more isomorphic:
--
-- > feature :: Geometry g => R.Feature -> Either Text (Feature g)
--
-- is not possible.
features :: [Text] -> [R.Val] -> [R.Feature]
  -> Either Text (V.Vector (Feature Point), V.Vector (Feature LineString), V.Vector (Feature Polygon))
features _ _ [] = Left "VectorTile.features: `[R.Feature]` empty"
features keys vals fs = (,,) <$> ps <*> ls <*> polys
  where -- (_:ps':ls':polys':_) = groupBy sameGeom $ sortOn geomBias fs  -- ok ok ok
        ps = foldrM f V.empty $ filter (\f -> getField (R.geom f) == Just R.Point) fs
        ls = foldrM f V.empty $ filter (\f -> getField (R.geom f) == Just R.LineString) fs
        polys = foldrM f V.empty $ filter (\f -> getField (R.geom f) == Just R.Polygon) fs

        f :: Geometry g => R.Feature -> V.Vector (Feature g) -> Either Text (V.Vector (Feature g))
        f x acc = do
          geos <- commands (getField $ R.geometries x) >>= fromCommands
          meta <- getMeta keys vals . getField $ R.tags x
          pure $ Feature { featureId = maybe 0 fromIntegral . getField $ R.featureId x
                         , metadata = meta
                         , geometries = geos
                         } `V.cons` acc

-- | Convert a raw `R.Val` parsed from protobuf data into a useable
-- `Val`. The higher-level `Val` type better expresses the mutual exclusivity
-- of the /Value/ types.
value :: R.Val -> Either Text Val
value v = mtoe "Value decode: No legal Value type offered" $ fmap St (getField $ R.string v)
  <|> fmap Fl  (getField $ R.float v)
  <|> fmap Do  (getField $ R.double v)
  <|> fmap I64 (getField $ R.int64 v)
  <|> fmap W64 (getField $ R.uint64 v)
  <|> fmap (\(Signed n) -> S64 n) (getField $ R.sint v)
  <|> fmap B   (getField $ R.bool v)

{- UTIL -}

-- | Bias a `R.Feature` by its `GeomType`. Used for sorting.
geomBias :: R.Feature -> Int
geomBias = maybe 0 fromEnum . getField . R.geom

-- | Do two `R.Feature`s have the same `GeomType`?
sameGeom :: R.Feature -> R.Feature -> Bool
sameGeom a b = getField (R.geom a) == getField (R.geom b)

getMeta :: [Text] -> [R.Val] -> [Word32] -> Either Text (M.Map Text Val)
getMeta keys vals tags = do
  kv <- map (both fromIntegral) <$> pairs tags
  foldrM (\(k,v) acc -> (\v' -> M.insert (keys !! k) v' acc) <$> (value $ vals !! v)) M.empty kv
