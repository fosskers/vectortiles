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
-- 1. "Geography.VectorTile.VectorTile"
-- 2. "Geography.VectorTile.Geometry"
-- 3. "Geography.VectorTile.Protobuf"
--
-- == Usage
--
-- This library reads and writes strict `ByteString`s. By importing this module,
-- you use the default protobuf backend. Given some legal
-- VectorTile file called @roads.mvt@:
--
-- > import qualified Data.ByteString as BS
-- > import           Data.Text (Text)
-- > import           Geography.VectorTile
-- >
-- > -- | Read in raw protobuf data and decode it into a high-level type.
-- > roads :: IO (Either Text VectorTile)
-- > roads = do
-- >   mvt <- BS.readFile "roads.mvt"
-- >   pure $ decode mvt >>= tile
--
-- Or encode a `VectorTile` back into a `ByteString`:
--
-- > roadsBytes :: VectorTile -> BS.ByteString
-- > roadsBytes = encode . untile


module Geography.VectorTile
  ( -- * High-level Types
    module Geography.VectorTile.VectorTile
  , -- * Protobuf Backend
    -- ** Conversions
    tile
  , untile
  -- ** ByteString Encoding / Decoding
  , PB.decode
  , PB.encode
  ) where

import           Data.Text (Text)
import qualified Geography.VectorTile.Protobuf as PB
import           Geography.VectorTile.VectorTile

---

tile :: PB.RawVectorTile -> Either Text VectorTile
tile = PB.fromProtobuf

untile :: VectorTile -> PB.RawVectorTile
untile = PB.toProtobuf
