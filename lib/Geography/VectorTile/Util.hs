-- |
-- Module    : Geography.VectorTile.Util
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Geography.VectorTile.Util where

import qualified Data.Vector.Storable as VS
import           Geography.VectorTile.Geometry (Point(..))

---

-- | A strict pair of Ints.
data Pair = Pair !Int !Int

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Fails if there is an uneven number of elements.
-- pairsWith :: (a -> Int) -> [a] -> Either Text [Point]
-- pairsWith _ [] = Right []
-- pairsWith f s | odd $ length s = Left "Uneven number of parameters given."
--               | otherwise = Right $ go Empty s
--   where go !acc Empty = acc
--         go !acc (a :<| b :<| cs) = go (acc |> Point (f a) (f b)) cs
--         go !acc (_ :<| Empty) = acc

pairsWith :: (a -> Int) -> [a] -> VS.Vector Point
pairsWith f = VS.unfoldr g
  where g []  = Nothing
        g [_] = Nothing
        g (a:b:cs) = Just (Point (f a) (f b), cs)

-- | Flatten a list of pairs. Equivalent to:
--
-- > ps ^.. each . both
unpairs :: [(a,a)] -> [a]
unpairs = foldr (\(a,b) acc -> a : b : acc) []
{-# INLINE unpairs #-}
