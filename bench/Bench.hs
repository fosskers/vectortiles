module Main where

import           Control.Monad ((>=>))
import           Criterion.Main
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Geography.VectorTile
import qualified Geography.VectorTile.Protobuf as R
import           Lens.Micro
import           Lens.Micro.Platform ()  -- Instances only.

---

main :: IO ()
main = do
  op <- BS.readFile "test/onepoint.mvt"
  ls <- BS.readFile "test/linestring.mvt"
  pl <- BS.readFile "test/polygon.mvt"
  rd <- BS.readFile "test/roads.mvt"
  let op' = fromRight $ R.decode op >>= R.tile
      ls' = fromRight $ R.decode ls >>= R.tile
      pl' = fromRight $ R.decode pl >>= R.tile
      rd' = fromRight $ R.decode rd >>= R.tile
  defaultMain [ bgroup "Decoding"
                [ bgroup "onepoint.mvt" $ decodes op
                , bgroup "linestring.mvt" $ decodes ls
                , bgroup "polygon.mvt" $ decodes pl
                , bgroup "roads.mvt" $ decodes rd
                ]
              , bgroup "Encoding"
                [ bgroup "Point" $ encodes op'
                , bgroup "LineString" $ encodes ls'
                , bgroup "Polygon" $ encodes pl'
                , bgroup "Roads" $ encodes rd'
                ]
              , bgroup "Data Access"
                [ bgroup "All Layer Names"
                  [ bench "One Point" $ nf layerNames op
                  , bench "One LineString" $ nf layerNames ls
                  , bench "One Polygon" $ nf layerNames pl
                  , bench "roads.mvt" $ nf layerNames rd
                  ]
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

layerNames :: BS.ByteString -> [Text]
layerNames mvt = t ^.. layers . each . name
  where t = fromRight $ R.decode mvt >>= R.tile

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"
