-- |
-- Module    : Geography.VectorTile
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- GIS Vector Tiles, as defined by Mapbox.
--
-- This library implements version 2.1 of the official Mapbox spec, as defined
-- here: https://github.com/mapbox/vector-tile-spec/tree/master/2.1
--
-- Note that currently this library ignores top-level protobuf extensions,
-- /Value/ extensions, and /UNKNOWN/ geometries.
--
-- == Usage
--
-- This library reads and writes strict `BS.ByteString`s.
-- Given some legal VectorTile file called @roads.mvt@:
--
-- > import qualified Data.ByteString as BS
-- > import           Data.Text (Text)
-- > import           Geography.VectorTile
-- >
-- > -- | Read in raw protobuf data and decode it into a high-level type.
-- > roads :: IO (Either Text VectorTile)
-- > roads = tile <$> BS.readFile "roads.mvt"
--
-- Likewise, use the `untile` function to convert a `VectorTile` back into a `ByteString`.

module Geography.VectorTile
  ( -- * Vector Tiles
    VectorTile(..)
  , tile
  , untile
  , Lens'
  , layers
  , Layer(..)
  , version
  , name
  , points
  , linestrings
  , polygons
  , extent
  , Feature(..)
  , featureId
  , metadata
  , geometries
  , Val(..)
  -- * Geometries
  , Point(..)
  , LineString(..)
  , Polygon(..)
  , area
  , surveyor
  ) where

import           Control.Monad ((>=>))
import           Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Text (Text, pack)
import           Geography.VectorTile.Internal (fromProtobuf, toProtobuf)
import           Geography.VectorTile.VectorTile
import           Geography.VectorTile.Geometry
import           Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

---

-- | Attempt to parse a `VectorTile` from a strict collection of bytes.
tile :: BS.ByteString -> Either Text VectorTile
tile = bimap pack id . messageGet . fromStrict >=> fromProtobuf . fst

-- | Convert a `VectorTile` back into bytes.
untile :: VectorTile -> BS.ByteString
untile = toStrict . messagePut . toProtobuf
