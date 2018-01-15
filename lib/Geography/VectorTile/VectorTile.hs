{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module    : Geography.VectorTile.VectorTile
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- High-level types for representing Vector Tiles.

module Geography.VectorTile.VectorTile
  ( -- * Types
    VectorTile(..)
  , Layer(..)
  , Feature(..)
  , Val(..)
    -- * Lenses
    -- | This section can be safely ignored if one isn't concerned with lenses.
    -- Otherwise, see the following for a good primer on Haskell lenses:
    -- http://hackage.haskell.org/package/lens-tutorial-1.0.1/docs/Control-Lens-Tutorial.html
    --
    -- These lenses are written in a generic way to avoid taking a dependency
    -- on one of the lens libraries.
  , Lens'
  , layers
  , version
  , name
  , points
  , linestrings
  , polygons
  , extent
  , featureId
  , metadata
  , geometries
  ) where

import           Control.DeepSeq (NFData)
import           Data.Int
import qualified Data.Map.Lazy as M
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Word
import           GHC.Generics (Generic)
import           Geography.VectorTile.Geometry

---

-- | Simple Lenses compatible with both lens and microlens.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | A high-level representation of a Vector Tile. Implemented internally
-- as a `M.Map`, so that access to individual layers can be fast if you
-- know the layer names ahead of time.
newtype VectorTile = VectorTile { _layers :: M.Map Text Layer } deriving (Eq,Show,Generic)

layers :: Lens' VectorTile (M.Map Text Layer)
layers f v = VectorTile <$> f (_layers v)
{-# INLINE layers #-}

instance NFData VectorTile

-- | A layer, which could contain any number of `Feature`s of any `Geometry` type.
-- This codec only respects the canonical three `Geometry` types, and we split
-- them here explicitely to allow for more fine-grained access to each type.
data Layer = Layer { _version :: Word  -- ^ The version of the spec we follow. Should always be 2.
                   , _name :: Text
                   , _points :: V.Vector (Feature Point)
                   , _linestrings :: V.Vector (Feature LineString)
                   , _polygons :: V.Vector (Feature Polygon)
                   , _extent :: Word  -- ^ Default: 4096
                   } deriving (Eq,Show,Generic)

version :: Lens' Layer Word
version f l = (\v -> l { _version = v }) <$> f (_version l)
{-# INLINE version #-}

name :: Lens' Layer Text
name f l = (\v -> l { _name = v }) <$> f (_name l)
{-# INLINE name #-}

points :: Lens' Layer (V.Vector (Feature Point))
points f l = (\v -> l { _points = v }) <$> f (_points l)
{-# INLINE points #-}

linestrings :: Lens' Layer (V.Vector (Feature LineString))
linestrings f l = (\v -> l { _linestrings = v }) <$> f (_linestrings l)
{-# INLINE linestrings #-}

polygons :: Lens' Layer (V.Vector (Feature Polygon))
polygons f l = (\v -> l { _polygons = v }) <$> f (_polygons l)
{-# INLINE polygons #-}

extent :: Lens' Layer Word
extent f l = (\v -> l { _extent = v }) <$> f (_extent l)
{-# INLINE extent #-}

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
data Feature g = Feature { _featureId :: Word  -- ^ Default: 0
                         , _metadata :: M.Map Text Val
                         , _geometries :: V.Vector g } deriving (Eq,Show,Generic)

featureId :: Lens' (Feature g) Word
featureId f l = (\v -> l { _featureId = v }) <$> f (_featureId l)
{-# INLINE featureId #-}

metadata :: Lens' (Feature g) (M.Map Text Val)
metadata f l = (\v -> l { _metadata = v }) <$> f (_metadata l)
{-# INLINE metadata #-}

geometries :: Lens' (Feature g) (V.Vector g)
geometries f l = (\v -> l { _geometries = v }) <$> f (_geometries l)
{-# INLINE geometries #-}

instance NFData g => NFData (Feature g)

-- | Legal Metadata /Value/ types. Note that `S64` are Z-encoded automatically
-- by the underlying "Data.ProtocolBuffers" library.
data Val = St Text | Fl Float | Do Double | I64 Int64 | W64 Word64 | S64 Int64 | B Bool
         deriving (Eq,Ord,Show,Generic)

instance NFData Val
