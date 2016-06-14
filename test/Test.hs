{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Hex
import           Data.ProtocolBuffers
import           Data.Serialize.Get
import           Data.Serialize.Put
import qualified Geography.VectorTile.Raw as R
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Text.ProtocolBuffers.WireMessage as PB
import qualified Vector_tile.Tile as VT

---

main :: IO ()
main = BS.readFile "onepoint.mvt" >>= defaultMain . suite
--main = BS.readFile "streets.mvt" >>= defaultMain . suite

{- SUITES -}

suite :: BS.ByteString -> TestTree
suite vt = testGroup "Unit Tests"
  [ testGroup "Decoding"
    [ testCase "onepoint.mvt -> Raw.Tile" $ testOnePoint vt
    ]
  , testGroup "Serialization Isomorphism"
    [ testCase ".mvt <-> Raw.Tile" $ fromRaw vt
    , testCase "testTile <-> protobuf bytes" testTileIso
    ]
  , testGroup "Testing auto-generated code"
    [ testCase ".mvt <-> PB.Tile" $ pbRawIso vt
    ]
  , testGroup "Cross-codec Isomorphisms"
    [ testCase "ByteStrings only" crossCodecIso1
    , testCase "Full encode/decode" crossCodecIso
    ]
  ]

testOnePoint :: BS.ByteString -> Assertion
testOnePoint vt = case decodeIt vt of
                    Left e -> assertFailure e
                    Right t -> t @?= onePoint

fromRaw :: BS.ByteString -> Assertion
fromRaw vt = case decodeIt vt of
               Left e -> assertFailure e
               Right l -> hex (encodeIt l) @?= hex vt
--               Right l -> if runPut (encodeMessage l) == vt
--                          then assert True
--                          else assertString "Isomorphism failed."

testTileIso :: Assertion
testTileIso = case decodeIt pb of
                 Right tl -> assertEqual "" tl testTile
                 Left e -> assertFailure e
  where pb = encodeIt testTile

pbRawIso :: BS.ByteString -> Assertion
pbRawIso vt = case pbIso vt of
                Right vt' -> assertEqual "" (hex vt) (hex vt')
                Left e -> assertFailure e

-- | Can an `R.VectorTile` be converted to a `Vector_tile.Tile` and back?
crossCodecIso :: Assertion
crossCodecIso = case pbIso (encodeIt testTile) >>= decodeIt of
                  Left e -> assertFailure e
                  Right t -> t @?= testTile

-- | Will just their `ByteString` forms match?
crossCodecIso1 :: Assertion
crossCodecIso1 = case pbIso vt of
                  Left e -> assertFailure e
                  Right t -> hex t @?= hex vt
  where vt = encodeIt testTile

-- | Isomorphism for Vector_tile.Tile
pbIso :: BS.ByteString -> Either String BS.ByteString
pbIso (BSL.fromStrict -> vt) = do
   (t,_) <- PB.messageGet @VT.Tile vt
   pure . BSL.toStrict $ PB.messagePut @VT.Tile t

decodeIt :: BS.ByteString -> Either String R.VectorTile
decodeIt = runGet decodeMessage

encodeIt :: R.VectorTile -> BS.ByteString
encodeIt = runPut . encodeMessage

{- UTIL -}

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

rawTest :: IO (Either String R.VectorTile)
rawTest = decodeIt <$> BS.readFile "onepoint.mvt"

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

-- | Correct decoding of `onepoint.mvt`
onePoint :: R.VectorTile
onePoint = R.VectorTile $ putField [l]
  where l = R.Layer { R.version = putField 1
                    , R.name = putField "water"
                    , R.features = putField [f]
                    , R.keys = putField []
                    , R.values = putField []
                    , R.extent = putField $ Just 4096
                    }
        f = R.Feature { R.featureId = putField Nothing
                      , R.tags = putField []
                      , R.geom = putField $ Just R.Point
                      , R.geometries = putField [9, 10, 10]  -- MoveTo(+5,+5)
                      }

{-}
onePoint :: R.VectorTile
onePoint = R.VectorTile { layers = Field {runField = Repeated {runRepeated = [Message {runMessage = Layer {version = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = 1}}}}, name = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = "water"}}}}, features = Field {runField = Repeated {runRepeated = [Message {runMessage = Feature {featureId = Field {runField = Optional {runOptional = Last {getLast = Nothing}}}, tags = Field {runField = PackedField {runPackedField = PackedList {unPackedList = []}}}, geom = Field {runField = Optional {runOptional = Last {getLast = Just (Enumeration {runEnumeration = Point})}}}, geometries = Field {runField = PackedField {runPackedField = PackedList {unPackedList = [Value {runValue = 9},Value {runValue = 10},Value {runValue = 10}]}}}}}]}}, keys = Field {runField = Repeated {runRepeated = []}}, values = Field {runField = Repeated {runRepeated = []}}, extent = Field {runField = Optional {runOptional = Last {getLast = Just (Value {runValue = 4096})}}}}}]}}}
-}
