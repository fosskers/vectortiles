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
pairs :: [a] -> Either Text [(a,a)]
pairs [] = Right []
pairs [_] = Left "Uneven number of parameters given."
pairs (x:y:zs) = ((x,y) :) <$> pairs zs

-- TODO Delete original `pairs`?

pairs' :: Seq a -> Either Text (Seq (a, a))
pairs' Empty = Right Empty
pairs' s | odd $ length s = Left "Uneven number of parameters given."
         | otherwise = Right $ go Empty s
  where go acc Empty = acc
        go acc (x :<| y :<| zs) = go (acc |> (x,y)) zs
        go acc (_ :<| Empty) = acc

-- | Flatten a list of pairs. Equivalent to:
--
-- > ps ^.. each . both
unpairs :: [(a,a)] -> [a]
unpairs = foldr (\(a,b) acc -> a : b : acc) []

-- | Apply a pure function to both elements of a tuple.
both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

-- | Convert a `Maybe` to an `Either`, with some given default value
-- should the result of the `Maybe` be `Nothing`.
mtoe :: a -> Maybe b -> Either a b
mtoe _ (Just b) = Right b
mtoe a _ = Left a
