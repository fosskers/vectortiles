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
    Point, pattern Point, x, y
  , LineString(..)
  , Polygon(..)
  -- ** Operations
  , area
  , surveyor
  , distance
  ) where

import           Control.DeepSeq (NFData)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           GHC.Generics (Generic)

---

-- | Points in space. Using "Record Pattern Synonyms" here allows us to treat
-- `Point` like a normal ADT, while its implementation remains an unboxed
-- @(Int,Int)@.
type Point = (Int,Int)
pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

-- | /newtype/ compiles away to expose only the `U.Vector` of unboxed `Point`s
-- at runtime.
newtype LineString = LineString { lsPoints :: U.Vector Point } deriving (Eq,Show,Generic)

instance NFData LineString

-- | A polygon aware of its interior rings.
data Polygon = Polygon { polyPoints :: U.Vector Point
                       , inner :: V.Vector Polygon } deriving (Eq,Show,Generic)

instance NFData Polygon

-- | The area of a `Polygon` is the difference between the areas of its
-- outer ring and inner rings.
area :: Polygon -> Double
area p = surveyor (polyPoints p) + sum (V.map area $ inner p)

-- | The surveyor's formula for calculating the area of a `Polygon`.
-- If the value reported here is negative, then the `Polygon` should be
-- considered an Interior Ring.
--
-- Assumption: The `U.Vector` given has at least 4 `Point`s.
surveyor :: U.Vector Point -> Double
surveyor v = (/ 2) . fromIntegral . U.sum $ U.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
  where v' = U.init v
        xs = U.map x v'
        yns = U.map y . U.tail $ U.snoc v' (U.head v')
        yps = U.map y . U.init $ U.cons (U.last v') v'

-- | Euclidean distance.
distance :: Point -> Point -> Double
distance p1 p2 = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where dx = x p1 - x p2
        dy = y p1 - y p2
