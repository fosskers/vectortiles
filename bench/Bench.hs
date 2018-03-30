{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import           Geography.VectorTile
import           Lens.Micro
import           Lens.Micro.Platform ()  -- Instances only.

---

main :: IO ()
main = do
  op <- BS.readFile "test/onepoint.mvt"
  ls <- BS.readFile "test/linestring.mvt"
  pl <- BS.readFile "test/polygon.mvt"
  rd <- BS.readFile "test/roads.mvt"
  let op' = fromRight $ tile op
      ls' = fromRight $ tile ls
      pl' = fromRight $ tile pl
      rd' = fromRight $ tile rd
  defaultMain [ bgroup "Decoding"
                [ bgroup "onepoint.mvt"   $ decodes op
                , bgroup "linestring.mvt" $ decodes ls
                , bgroup "polygon.mvt"    $ decodes pl
                , bgroup "roads.mvt"      $ decodes rd
                ]
              , bgroup "Encoding"
                [ bgroup "Point"      $ encodes op'
                , bgroup "LineString" $ encodes ls'
                , bgroup "Polygon"    $ encodes pl'
                , bgroup "Roads"      $ encodes rd'
                ]
              , bgroup "Data Access"
                [ bgroup "All Layer Names"
                  [ bench "One Point"      $ nf layerNames op
                  , bench "One LineString" $ nf layerNames ls
                  , bench "One Polygon"    $ nf layerNames pl
                  , bench "roads.mvt"      $ nf layerNames rd
                  ]
                , bgroup "First Polygon"
                  [ bench "One Polygon" $ nf (firstPoly "OnePolygon") op
                  , bench "roads.mvt - water layer" $ nf (firstPoly "water") rd
                  ]
                ]
              ]

decodes :: BS.ByteString -> [Benchmark]
decodes bs = [ bench "VectorTile" $ nf tile bs ]

encodes :: VectorTile -> [Benchmark]
encodes vt = [ bench "ByteString" $ nf untile vt ]

layerNames :: BS.ByteString -> [BL.ByteString]
layerNames = M.keys . _layers . fromRight . tile

firstPoly :: BL.ByteString -> BS.ByteString -> Maybe Polygon
firstPoly ln mvt = tile mvt ^? _Right . layers . ix ln . polygons . _head . geometries . _head

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"
