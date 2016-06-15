{-# LANGUAGE TypeApplications #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BSL
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import qualified Geography.VectorTile.Raw as R
--import qualified Text.ProtocolBuffers.WireMessage as PB
--import qualified Vector_tile.Tile as VT

---

main :: IO ()
main = do
  tile <- BS.readFile "roads.mvt"
--  let ltile = BSL.fromStrict tile
  defaultMain [ bgroup "Decoding"
                [ bench "Raw.VectorTile" $ nf (runGet @R.VectorTile decodeMessage) tile
--                , bench "VT.Tile" $ nf (PB.messageGet @VT.Tile) ltile
                ]
              ]
