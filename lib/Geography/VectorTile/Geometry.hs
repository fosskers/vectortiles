{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module    : Geography.VectorTile.Geometry
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Geography.VectorTile.Geometry
  ( -- * Geometries
    -- ** Types
    Point(..)
  , LineString(..)
  , Polygon(..)
  -- ** Operations
  , area
  , surveyor
  , distance
  ) where

import           Control.DeepSeq (NFData)
import           Data.Foldable (foldl')
import           Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           GHC.Generics (Generic)

---

-- | A strict pair of integers indicating some location on a discrete grid.
-- @Point 0 0@ is the top-left.
data Point = Point { x :: !Int, y :: !Int } deriving (Eq, Show, Generic)

instance Semigroup Point where
  Point x0 y0 <> Point x1 y1 = Point (x0 + x1) (y0 + y1)
  {-# INLINE (<>) #-}

instance Monoid Point where
  mempty = Point 0 0
  mappend = (<>)

instance NFData Point

-- | /newtype/ compiles away to expose only the `U.Vector` of unboxed `Point`s
-- at runtime.
newtype LineString = LineString { lsPoints :: V.Vector Point } deriving (Eq,Show,Generic)

instance NFData LineString

-- | A polygon aware of its interior rings.
--
-- VectorTiles require that Polygon exteriors have clockwise winding order,
-- and that interior holes have counter-clockwise winding order.
-- These assume that the origin (0,0) is in the *top-left* corner.
data Polygon = Polygon { polyPoints :: V.Vector Point
                       , inner :: Seq.Seq Polygon } deriving (Eq,Show,Generic)

instance NFData Polygon

-- | The area of a `Polygon` is the difference between the areas of its
-- outer ring and inner rings.
area :: Polygon -> Double
area p = surveyor (polyPoints p) + foldl' (\acc i -> acc + area i) 0 (inner p)

-- | The surveyor's formula for calculating the area of a `Polygon`.
-- If the value reported here is negative, then the `Polygon` should be
-- considered an Interior Ring.
--
-- Assumption: The `U.Vector` given has at least 4 `Point`s.
surveyor :: V.Vector Point -> Double
surveyor v = (/ 2) . fromIntegral . V.foldl' (+) 0 $ V.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
  where v' = V.init v
        xs = V.map x v'
        yns = V.map y . V.tail $ V.snoc v' (V.head v')
        yps = V.map y . V.init $ V.cons (V.last v') v'

-- | Euclidean distance.
distance :: Point -> Point -> Double
distance p1 p2 = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where dx = x p1 - x p2
        dy = y p1 - y p2
