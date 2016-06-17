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
  defaultMain [ bgroup "Decoding"
                [ bgroup "onepoint.mvt" $ decodes op
                , bgroup "linestring.mvt" $ decodes ls
                , bgroup "roads.mvt" $ decodes rd
                ]
              ]

decodes :: BS.ByteString -> [Benchmark]
decodes bs = [ bench "Raw.VectorTile" $ nf R.decode bs
             , bench "VectorTile" $ nf (R.decode >=> tile) bs
             ]
