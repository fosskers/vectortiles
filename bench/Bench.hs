module Main where

import           Control.Monad ((>=>))
import           Criterion.Main
import qualified Data.ByteString as BS
import           Geography.VectorTile
import qualified Geography.VectorTile.Raw as R

---

main :: IO ()
main = do
  op <- BS.readFile "test/onepoint.mvt"
  ls <- BS.readFile "test/linestring.mvt"
  rd <- BS.readFile "test/roads.mvt"
  let op' = fromRight $ R.decode op >>= tile
      ls' = fromRight $ R.decode ls >>= tile
      rd' = fromRight $ R.decode rd >>= tile
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
             , bench "VectorTile" $ nf (R.decode >=> tile) bs
             ]

encodes :: VectorTile -> [Benchmark]
encodes vt = [ bench "Raw.VectorTile" $ nf untile vt
             , bench "ByteString" $ nf (R.encode . untile) vt
             ]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"
