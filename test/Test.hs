{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import           Data.Serialize.Put
import qualified Gaia.VectorTile.Raw as R
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = BS.readFile "streets.mvt" >>= defaultMain . suite

{- SUITES -}

suite :: BS.ByteString -> TestTree
suite vt = testGroup "Unit Tests"
  [ testGroup "Serialization Isomorphism"
    [ testCase ".mvt <-> Raw.Tile" $ fromRaw vt
    , testCase "testTile <-> protobuf" testTileIso
    ]
  ]

fromRaw :: BS.ByteString -> Assertion
fromRaw vt = case back vt of
               Right l -> if runPut (encodeMessage l) == vt
                          then assert True
                          else assertString "Isomorphism failed."
               Left e -> assertFailure e

testTileIso :: Assertion
testTileIso = case back pb of
                 Right tl -> assertEqual "" tl testTile
                 Left e -> assertFailure e
  where pb = runPut $ encodeMessage testTile

back :: BS.ByteString -> Either String R.VectorTile
back = runGet decodeMessage

{- UTIL -}

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

testTile :: R.VectorTile
testTile = R.VectorTile $ putField [l]
  where l = R.Layer { R.version = putField 2
                    , R.name = putField "testlayer"
                    , R.features = putField [f]
                    , R.keys = putField ["somekey"]
                    , R.values = putField [v]
                    , R.extent = putField $ Just 4096
                    }
        f = R.Feature { R.featureId = putField $ Just 0
                      , R.tags = putField [0,0]
                      , R.geom = putField $ Just R.Point
                      , R.geometries = putField [9, 50, 34]  -- MoveTo(+25,+17)
                      }
        v = R.Val { R.string = putField $ Just "Some Value"
                  , R.float = putField Nothing
                  , R.double = putField Nothing
                  , R.int64 = putField Nothing
                  , R.uint64 = putField Nothing
                  , R.sint = putField Nothing
                  , R.bool = putField Nothing
                  }
