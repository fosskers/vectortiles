{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Storable as VS
import           Geography.VectorTile
import           Lens.Micro
import           Lens.Micro.Platform ()

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
  defaultMain
    [ bgroup "Decoding"
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
    , bgroup "Other Functions"
      [ bench "Surveyor - Tiny" $ nf surveyor tinyvec
      , bench "Surveyor - Big"  $ nf surveyor bigvec
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
fromRight _         = error "`Left` given to fromRight!"

tinyvec :: VS.Vector Point
tinyvec = VS.fromList [ Point 1 1, Point 2 1, Point 2 2, Point 1 2, Point 1 1 ]

bigvec :: VS.Vector Point
bigvec = ps <> VS.fromList [ Point 500 1000, Point 1 1 ]
  where ps = VS.fromList $ map (\n -> Point n 1) [ 1 .. 1000 ]
