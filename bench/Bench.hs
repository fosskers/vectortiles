module Main where

import           Control.Monad ((>=>))
import           Criterion.Main
import qualified Data.ByteString as BS
import           Geography.VectorTile
import qualified Geography.VectorTile.Protobuf as R

---

main :: IO ()
main = do
  op <- BS.readFile "test/onepoint.mvt"
  ls <- BS.readFile "test/linestring.mvt"
  rd <- BS.readFile "test/roads.mvt"
  let op' = fromRight $ R.decode op >>= R.tile
      ls' = fromRight $ R.decode ls >>= R.tile
      rd' = fromRight $ R.decode rd >>= R.tile
  defaultMain [ bgroup "Decoding"
                [ bgroup "onepoint.mvt" $ decodes op
                , bgroup "linestring.mvt" $ decodes ls
                , bgroup "roads.mvt" $ decodes rd
                ]
              , bgroup "Encoding"
                [ bgroup "Point" $ encodes op'
                , bgroup "LineString" $ encodes ls'
                , bgroup "Roads" $ encodes rd'
                ]
              ]

decodes :: BS.ByteString -> [Benchmark]
decodes bs = [ bench "Raw.VectorTile" $ nf R.decode bs
             , bench "VectorTile" $ nf (R.decode >=> R.tile) bs
             ]

encodes :: VectorTile -> [Benchmark]
encodes vt = [ bench "Raw.VectorTile" $ nf R.untile vt
             , bench "ByteString" $ nf (R.encode . R.untile) vt
             ]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"
