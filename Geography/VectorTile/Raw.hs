{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module    : Geography.VectorTile.Raw
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- Raw Vector Tile data is stored as binary protobuf data.
-- This module reads and writes raw protobuf ByteStrings between a data type
-- which closely matches the current Mapbox vector tile spec defined here:
-- https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto
--
-- As this raw version of the data is hard to work with, in practice we convert
-- to a more canonical Haskell type for further processing.
-- See `Geography.VectorTile` for the user-friendly version.
--
-- Please import this module @qualified@ to avoid namespace clashes:
--
-- > import qualified Geography.VectorTile.Raw as R

module Geography.VectorTile.Raw where

import Control.DeepSeq (NFData)
import Data.Int
import Data.ProtocolBuffers
import Data.Text (Text)
import Data.Word
import GHC.Generics (Generic)

---

-- | A list of `Layer`s.
data VectorTile = VectorTile { layers :: Repeated 3 (Message Layer) }
                deriving (Generic,Show,Eq)

instance Encode VectorTile
instance Decode VectorTile
instance NFData VectorTile

data Layer = Layer { version :: Required 15 (Value Word32)
                   , name :: Required 1 (Value Text)
                   , features :: Repeated 2 (Message Feature)
                   , keys :: Repeated 3 (Value Text)
                   , values :: Repeated 4 (Message Val)
                   , extent :: Optional 5 (Value Word32)
                   } deriving (Generic,Show,Eq)

instance Encode Layer
instance Decode Layer
instance NFData Layer

-- | The /Value/ types of metadata fields.
data Val = Val { string :: Optional 1 (Value Text)
               , float :: Optional 2 (Value Float)
               , double :: Optional 3 (Value Double)
               , int64 :: Optional 4 (Value Int64)
               , uint64 :: Optional 5 (Value Word64)
               , sint :: Optional 6 (Value (Signed Int64))  -- ^ Z-encoded.
               , bool :: Optional 7 (Value Bool)
               } deriving (Generic,Show,Eq)

instance Encode Val
instance Decode Val
instance NFData Val

-- | A set of geometries unified by some theme.
data Feature = Feature { featureId :: Optional 1 (Value Word64)
                       , tags :: Packed 2 (Value Word32)
                       , geom :: Optional 3 (Enumeration GeomType)
                       , geometries :: Packed 4 (Value Word32)
                       } deriving (Generic,Show,Eq)

instance Encode Feature
instance Decode Feature
instance NFData Feature

data GeomType = Unknown | Point | LineString | Polygon
              deriving (Generic,Enum,Show,Eq)

instance Encode GeomType
instance Decode GeomType
instance NFData GeomType
