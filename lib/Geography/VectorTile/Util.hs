{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Geography.VectorTile.Util
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Geography.VectorTile.Util where

import Data.Sequence (Seq, (|>), Seq(Empty, (:<|)))
import Data.Text (Text)

---

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Fails if there is an uneven number of elements.
pairsWith :: (a -> b) -> Seq a -> Either Text (Seq (b, b))
pairsWith _ Empty = Right Empty
pairsWith f s | odd $ length s = Left "Uneven number of parameters given."
              | otherwise = Right $ go Empty s
  where go acc Empty = acc
        go acc (x :<| y :<| zs) = go (acc |> (f x, f y)) zs
        go acc (_ :<| Empty) = acc

-- | Flatten a list of pairs. Equivalent to:
--
-- > ps ^.. each . both
unpairs :: [(a,a)] -> [a]
unpairs = foldr (\(a,b) acc -> a : b : acc) []
{-# INLINE unpairs #-}
