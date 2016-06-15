{-# LANGUAGE TypeApplications #-}

module Geography.VectorTile.Geometry where

-- |
-- Module    : Geography.VectorTile.Geometry
-- Copyright : (c) Colin Woodbury, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>

import Data.Bits
import Data.Word
import Data.Int

---

-- | The possible commands, and the values they hold.
data Commands = MoveTo (Int,Int) | LineTo (Int,Int) | ClosePath deriving (Eq,Show)

-- | Z-encode a 64-bit Int.
zig :: Int -> Word32
zig n = fromIntegral $ shift n 1 `xor` shift n (-63)

-- | Decode a Z-encoded Word32 into a 64-bit Int.
unzig :: Word32 -> Int
unzig n = fromIntegral (fromIntegral unzigged :: Int32)
  where unzigged = shift n (-1) `xor` negate (n .&. 1)
