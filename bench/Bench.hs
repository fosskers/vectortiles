{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                 (replicateM, (>=>))
import           Criterion.Main
import qualified Data.ByteString               as BS
import qualified Data.Foldable                 as F
import qualified Data.Map.Lazy                 as M
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import           Geography.VectorTile
import           Geography.VectorTile.Geometry
import           Lens.Micro
import           Lens.Micro.Platform           ()
import           System.Random                 as SR

-----------------------------------------------------------------------------------------
-- MAIN
-----------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------
-- VT Helpers
-----------------------------------------------------------------------------------------

decodes :: BS.ByteString -> [Benchmark]
decodes bs = [ bench "Raw.VectorTile" $ nf decode bs
             , bench "VectorTile" $ nf (decode >=> tile) bs
             ]

encodes :: VectorTile -> [Benchmark]
encodes vt = [ bench "Raw.VectorTile" $ nf untile vt
             , bench "ByteString" $ nf (encode . untile) vt
             ]

layerNames :: BS.ByteString -> [T.Text]
layerNames mvt = M.keys $ _layers t
  where t = fromRight $ decode mvt >>= tile

firstPoly :: T.Text -> BS.ByteString -> Maybe Polygon
firstPoly ln mvt = r ^? _Right . layers . ix ln . polygons . _head . geometries . _head
  where r = decode mvt >>= tile

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"

-----------------------------------------------------------------------------------------
-- SW Helpers
-----------------------------------------------------------------------------------------

-- Benchmark polygons example

-- main :: IO ()
-- main = do
--   poly1 <- randomPolygons 5000 100
--   poly2 <- randomPolygons 10000 100
--   let vTile1 = mkVectorTile M.empty poly1
--       vTile2 = mkVectorTile  M.empty poly2
--       title1 = "5,000 polys"
--       title2 = "10,000 polys"
--   defaultMain [
--     bgroup "Polygons with 200 points"
--       [
--       bgroup "Untile"
--         [ bench title1 $ nf untile vTile1
--         , bench title2 $ nf untile vTile2
--         ]
--       , bgroup "Encode"
--         [ bench title1 $ nf encode (untile vTile1)
--         , bench title2 $ nf encode (untile vTile2)
--         ]
--       , bgroup "Write"
--         [ bench title1 $ nf (encode . untile) vTile1
--         , bench title2 $ nf (encode . untile) vTile2
--         ]
--       ]
--     ]

-- Benchmark metadatas example

-- main :: IO ()
-- main = do
--   polys  <- randomPolygons 100 12
--   metas1 <- randomMetadatas 10
--   metas2 <- randomMetadatas 100
--   metas3 <- randomMetadatas 1000
--   metas4 <- randomMetadatas 10000
--   metas5 <- randomMetadatas 100000
--   let vTile1 = mkVectorTile metas1 polys
--       vTile2 = mkVectorTile metas2 polys
--       vTile3 = mkVectorTile metas3 polys
--       vTile4 = mkVectorTile metas4 polys
--       vTile5 = mkVectorTile metas5 polys
--       title1 = "10 metadatas"
--       title2 = "100 metadatas"
--       title3 = "1,000 metadatas"
--       title4 = "10,000 metadatas"
--       title5 = "100,000 metadatas"
--   defaultMain [
--     bgroup "Metadatas"
--       [
--       bgroup "Write"
--         [
--           bench title1 $ nf (encode . untile) vTile1
--         , bench title2 $ nf (encode . untile) vTile2
--         , bench title3 $ nf (encode . untile) vTile3
--         , bench title4 $ nf (encode . untile) vTile4
--         , bench title5 $ nf (encode . untile) vTile5
--         ]
--       ]
--     ]

-- VectorTile, Layer, Feature wrappers

mkVectorTile :: M.Map T.Text Val -> V.Vector Polygon -> VectorTile
mkVectorTile metas polys = VectorTile (M.singleton "layerName" $ mkLayer metas polys)

mkLayer :: M.Map T.Text Val -> V.Vector Polygon -> Layer
mkLayer metas polys = Layer 2 "layerName" V.empty V.empty (V.singleton $ mkFeature metas polys) 4096

mkFeature :: M.Map T.Text Val -> V.Vector Polygon -> Feature Polygon
mkFeature = Feature 0

-- Random Metadatas

randomMetadatas :: Int -> IO (M.Map T.Text Val)
randomMetadatas n =
  M.fromList <$> replicateM n randomVal

-- Randomly generated Val

randomVal :: IO (T.Text, Val)
randomVal = do
  vType <- SR.randomRIO (1, 7) :: IO Int
  let ioVal =
        case vType of
          1 -> St  <$> randomText
          2 -> Fl  <$> randomIO
          3 -> Do  <$> randomIO
          4 -> I64 <$> randomIO
          5 -> W64 <$> randomIO
          6 -> S64 <$> randomIO
          _ -> B   <$> randomIO
  val   <- ioVal
  name  <- randomText
  pure (name, val)

randomText :: IO T.Text
randomText = do
  strlen <- SR.randomRIO (1, 15)
  str    <- replicateM strlen SR.randomIO :: IO String
  pure $ T.pack str

-- Random closed Polygons

randomPolygons :: Int -> Int -> IO (V.Vector Polygon)
randomPolygons nPolys nPoints =
  V.fromList <$> replicateM nPolys (randomPolygon nPoints)

randomPolygon :: Int -> IO Polygon
randomPolygon nPoints = do
  points <- randomPolyPoints nPoints
  pure $ Polygon (VU.fromList points) V.empty

-- Randomly generated Point for closed Polygon

randomPolyPoints :: Int -> IO [Point]
randomPolyPoints n = do
  let nPoints    = ceiling (fromIntegral n / 4)
      pointLimit = 10
      limit      = pointLimit * nPoints
  -- Xs
  x1 <- randomXYs nPoints (0, pointLimit)  True  0        S.empty
  x2 <- randomXYs nPoints (0, pointLimit)  False limit    S.empty
  x3 <- randomXYs nPoints (-pointLimit, 0) True  0        S.empty
  x4 <- randomXYs nPoints (-pointLimit, 0) False (-limit) S.empty
  -- Ys
  y1 <- randomXYs nPoints (0, pointLimit)  False limit    S.empty
  y2 <- randomXYs nPoints (-pointLimit, 0) True  0        S.empty
  y3 <- randomXYs nPoints (-pointLimit, 0) False (-limit) S.empty
  y4 <- randomXYs nPoints (0, pointLimit)  True  0        S.empty
  -- Zip
  let points1 = S.zip x1 y1
      points2 = S.zip x2 y2
      points3 = S.zip x3 y3
      points4 = S.zip x4 y4
      start   = S.take 1 points1
      points  = points1 S.>< points2 S.>< points3 S.>< points4 S.>< start
  pure $ F.toList points

randomXYs :: Int -> Point -> Bool -> Int -> S.Seq Int -> IO (S.Seq Int)
randomXYs n p@(from, to) add current acc =
  if n == 0 then
    pure acc
  else do
    newInt <- SR.randomRIO (from, to)
    let new = if add then current + newInt else current - newInt
    randomXYs (n - 1) p add new (acc S.|> new)

-- QuickCheck VectorTile

testVectorTile :: VectorTile -> Bool
testVectorTile vt =
  let
    raw = encode . untile $ vt
    vt' = fromRight . tile . fromRight . decode $ raw
  in
    vt == vt'
