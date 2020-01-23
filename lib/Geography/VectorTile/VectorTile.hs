{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE Rank2Types     #-}

-- |
-- Module    : Geography.VectorTile.VectorTile
-- Copyright : (c) Colin Woodbury 2016 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
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
import qualified Data.ByteString.Lazy as BL
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as M
import           Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Geography.VectorTile.Geometry
import           GHC.Generics (Generic)

---

-- | Simple Lenses compatible with both lens and microlens.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | A high-level representation of a Vector Tile. Implemented internally
-- as a `M.HashMap`, so that access to individual layers can be fast if you
-- know the layer names ahead of time.
--
-- The layer name itself, a lazy `BL.ByteString`, is guaranteed to be UTF-8.
-- If you wish to convert it to `Data.Text.Lazy.Text`, consider
-- `Data.Text.Lazy.Encoding.decodeUtf8`.
newtype VectorTile = VectorTile { _layers :: M.HashMap BL.ByteString Layer } deriving (Eq,Show,Generic)

layers :: Lens' VectorTile (M.HashMap BL.ByteString Layer)
layers f v = VectorTile <$> f (_layers v)
{-# INLINE layers #-}

instance NFData VectorTile

-- | A layer, which could contain any number of `Feature`s of any `Geometry` type.
-- This codec only respects the canonical three `Geometry` types, and we split
-- them here explicitely to allow for more fine-grained access to each type.
data Layer = Layer { _version     :: Word  -- ^ The version of the spec we follow. Should always be 2.
                   , _name        :: BL.ByteString
                   , _points      :: V.Vector (Feature (VS.Vector Point))
                   , _linestrings :: V.Vector (Feature (V.Vector LineString))
                   , _polygons    :: V.Vector (Feature (V.Vector Polygon))
                   , _extent      :: Word  -- ^ Default: 4096
                   } deriving (Eq, Show, Generic)

version :: Lens' Layer Word
version f l = (\v -> l { _version = v }) <$> f (_version l)
{-# INLINE version #-}

name :: Lens' Layer BL.ByteString
name f l = (\v -> l { _name = v }) <$> f (_name l)
{-# INLINE name #-}

points :: Lens' Layer (V.Vector (Feature (VS.Vector Point)))
points f l = (\v -> l { _points = v }) <$> f (_points l)
{-# INLINE points #-}

linestrings :: Lens' Layer (V.Vector (Feature (V.Vector LineString)))
linestrings f l = (\v -> l { _linestrings = v }) <$> f (_linestrings l)
{-# INLINE linestrings #-}

polygons :: Lens' Layer (V.Vector (Feature (V.Vector Polygon)))
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
--
-- Note: The keys to the metadata are `BL.ByteString`, but are guaranteed
-- to be UTF-8.
data Feature gs = Feature { _featureId  :: Word  -- ^ Default: 0
                          , _metadata   :: M.HashMap BL.ByteString Val
                          , _geometries :: gs } deriving (Eq, Show, Generic)

featureId :: Lens' (Feature gs) Word
featureId f l = (\v -> l { _featureId = v }) <$> f (_featureId l)
{-# INLINE featureId #-}

metadata :: Lens' (Feature gs) (M.HashMap BL.ByteString Val)
metadata f l = (\v -> l { _metadata = v }) <$> f (_metadata l)
{-# INLINE metadata #-}

geometries :: Lens' (Feature gs) gs
geometries f l = (\v -> l { _geometries = v }) <$> f (_geometries l)
{-# INLINE geometries #-}

instance (NFData gs) => NFData (Feature gs)

-- | Legal Metadata /Value/ types. Note that `S64` are Z-encoded automatically
-- by the underlying "Text.ProtocolBuffers" library.
data Val = St BL.ByteString | Fl Float | Do Double | I64 Int64 | W64 Word64 | S64 Int64 | B Bool
         deriving (Eq,Ord,Show,Generic,Hashable)

instance NFData Val
