{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Geography.VectorTile
  ( -- * Types
    VectorTile(..)
  , Layer(..)
  , Feature(..)
  , Val(..)
    -- * Conversions
    -- ** From Protobuf
  , tile
  , layer
  , features
  , value
    -- ** To Protobuf
  ) where

import           Control.Applicative ((<|>))
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
import           Text.Printf.TH

---

{- Types -}

-- | A high-level representation of a Vector Tile. At its simplest, a tile
-- is just a list of `Layer`s.
--
-- There is potential to implement `layers` as a `M.Map`, with its String-based
-- `name` as a key.
newtype VectorTile = VectorTile { layers :: V.Vector Layer } deriving (Eq,Show)

-- | A layer.
data Layer = Layer { version :: Int  -- ^ Foo
                   , name :: Text
                   , points :: V.Vector (Feature Point)
                   , linestrings :: V.Vector (Feature LineString)
                   , polygons :: V.Vector (Feature Polygon)
                   -- Needed? How to structure Feature-shared metadata?
--                   , keys :: V.Vector Text
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

-- | Legal Metadata Value types.
data Val = St Text | Fl Float | Do Double | I64 Int64 | W64 Word64 | S64 Int64 | B Bool
         deriving (Eq,Show)

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


-- | Convert a list of raw `R.Feature` of parsed protobuf data into `V.Vector`s
-- of each of the three legal `Geometry` types.
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

-- | Convert a raw `R.Val` parsed from protobuf data into a useable
-- `Val`. The higher-level `Val` type better expresses the mutual exclusivity
-- of the value types.
value :: R.Val -> Either Text Val
value v = mtoe "Value decode: No legal Value type offered" $ fmap St (getField $ R.string v)
  <|> fmap Fl  (getField $ R.float v)
  <|> fmap Do  (getField $ R.double v)
  <|> fmap I64 (getField $ R.int64 v)
  <|> fmap W64 (getField $ R.uint64 v)
  <|> fmap (\(Signed n) -> S64 n) (getField $ R.sint v)
  <|> fmap B   (getField $ R.bool v)
