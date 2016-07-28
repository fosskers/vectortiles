{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad ((>=>))
import           Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as M
import           Data.Text (Text)
import           Geography.VectorTile
import           Geography.VectorTile.Geometry (Polygon)
import           Lens.Micro
import           Lens.Micro.Platform ()  -- Instances only.

---

main :: IO ()
main = do
  op <- BS.readFile "test/onepoint.mvt"
  ls <- BS.readFile "test/linestring.mvt"
  pl <- BS.readFile "test/polygon.mvt"
  rd <- BS.readFile "test/roads.mvt"
  let op' = fromRight $ decode op >>= tile
      ls' = fromRight $ decode ls >>= tile
      pl' = fromRight $ decode pl >>= tile
      rd' = fromRight $ decode rd >>= tile
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
                , bgroup "First Polygon"
                  [ bench "One Polygon" $ nf (firstPoly "OnePolygon") op
                  , bench "roads.mvt - water layer" $ nf (firstPoly "water") rd
                  ]
                ]
              ]

decodes :: BS.ByteString -> [Benchmark]
decodes bs = [ bench "Raw.VectorTile" $ nf decode bs
             , bench "VectorTile" $ nf (decode >=> tile) bs
             ]

encodes :: VectorTile -> [Benchmark]
encodes vt = [ bench "Raw.VectorTile" $ nf untile vt
             , bench "ByteString" $ nf (encode . untile) vt
             ]

layerNames :: BS.ByteString -> [Text]
layerNames mvt = M.keys $ _layers t
  where t = fromRight $ decode mvt >>= tile

firstPoly :: Text -> BS.ByteString -> Maybe Polygon
firstPoly ln mvt = r ^? _Right . layers . ix ln . polygons . _head . geometries . _head
  where r = decode mvt >>= tile

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"
