{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Geography.VectorTile.Geometry where

-- |
-- Module    : Geography.VectorTile.Geometry
-- Copyright : (c) Colin Woodbury, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>

import Data.Bits
import Data.Word
import Data.Int
import qualified Data.Text as T
import Text.Printf
import Data.Monoid

---

-- | The possible commands, and the values they hold.
data Command = MoveTo (Int,Int) | LineTo (Int,Int) | ClosePath deriving (Eq,Show)

-- | Z-encode a 64-bit Int.
zig :: Int -> Word32
zig n = fromIntegral $ shift n 1 `xor` shift n (-63)

-- | Decode a Z-encoded Word32 into a 64-bit Int.
unzig :: Word32 -> Int
unzig n = fromIntegral (fromIntegral unzigged :: Int32)
  where unzigged = shift n (-1) `xor` negate (n .&. 1)

parseCommand :: Word32 -> Either T.Text (Int,Int)
parseCommand n = case (cid,count) of
  (1,m) -> Right $ both fromIntegral (1,m)
  (2,m) -> Right $ both fromIntegral (2,m)
  (7,1) -> Right (7,1)
  (7,m) -> Left $ "ClosePath was given a parameter count: " <> T.pack (show m)
  (m,_) -> Left . T.pack $ printf "Invalid command integer '%d' found in: %b" m n
  where cid = n .&. 7
        count = shift n (-3)

commands :: [Word32] -> Either T.Text [Command]
commands [] = Right []
commands (n:ns) = parseCommand n >>= f
  where f (1,count) = do
          mts <- map (MoveTo . both unzig) <$> pairs (take (count * 2) ns)
          (mts ++) <$> commands (drop (count * 2) ns)
        f (2,count) = do
          mts <- map (LineTo . both unzig) <$> pairs (take (count * 2) ns)
          (mts ++) <$> commands (drop (count * 2) ns)
        f (7,_) = (ClosePath :) <$> commands ns
        f _ = Left "Sentinel: You should never see this."

{- UTIL -}

pairs :: [a] -> Either T.Text [(a,a)]
pairs [] = Right []
pairs [_] = Left "Not enough parameters given."
pairs (x:y:zs) = ((x,y) :) <$>  pairs zs

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)
