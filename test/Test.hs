{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V
import           Geography.VectorTile
import qualified Geography.VectorTile.Internal as I
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.ProtocolBuffers.Basic (Utf8(..), defaultValue)
import           Text.ProtocolBuffers.WireMessage (Wire, messageGet)
import           Text.ProtocolBuffers.Reflections (ReflectDescriptor)

---

main :: IO ()
main = do
  op <- BS.readFile "test/onepoint.mvt"
  ls <- BS.readFile "test/linestring.mvt"
  pl <- BS.readFile "test/polygon.mvt"
  rd <- BS.readFile "test/roads.mvt"
  cl <- BS.readFile "test/clearlake.mvt"
  defaultMain $ suite op ls pl rd cl

{- SUITES -}

suite :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> TestTree
suite op ls pl rd cl = testGroup "Unit Tests"
  [ testGroup "Protobuf"
    [ testGroup "Decoding"
      [ testCase "onepoint.mvt -> VectorTile" $ tileDecode op
      , testCase "linestring.mvt -> VectorTile" $ tileDecode ls
      , testCase "polygon.mvt -> VectorTile" $ tileDecode pl
      , testCase "roads.mvt -> VectorTile" $ tileDecode rd
      , testCase "clearlake.mvt -> VectorTile" $ tileDecode cl ]
    , testGroup "Encoding"
      [ testGroup "RawVectorTile <-> VectorTile"
        [ testCase "One Point" $ encodeIso onePoint
        , testCase "One LineString" $ encodeIso oneLineString
        , testCase "One Polygon" $ encodeIso onePolygon
        , testCase "roads.mvt" . encodeIso . I.toProtobuf . fromRight $ tile rd
        ]
      ]
    ]
  , testGroup "Geometries"
    [ testCase "area" $ area poly @?= 1
    , testCase "surveyor - outer" . assertBool "surveyor outer" $ surveyor (polyPoints poly) > 0
    , testCase "surveyor - inner" . assertBool "surveyor inner" $ surveyor (V.reverse $ polyPoints poly) < 0
    , testCase "Z-encoding Isomorphism" zencoding
    , testCase "Command Parsing" commandTest
    , testCase "[Word32] <-> [Command]" commandIso
    , testCase "[Word32] <-> V.Vector Point" pointIso
    , testCase "[Word32] <-> V.Vector LineString" linestringIso
    , testCase "[Word32] <-> V.Vector Polygon (2 solid)" polygonIso
    , testCase "[Word32] <-> V.Vector Polygon (1 holed)" polygonIso2
    , testCase "[Word32] <-> V.Vector Polygon (1 holed, 1 solid)" polygonIso3
    ]
  ]

testOnePoint :: BS.ByteString -> Assertion
testOnePoint bs = protobufDecode bs onePoint

testLineString :: BS.ByteString -> Assertion
testLineString bs = protobufDecode bs oneLineString

testPolygon :: BS.ByteString -> Assertion
testPolygon bs = protobufDecode bs onePolygon

protobufDecode :: (ReflectDescriptor a, Wire a, Eq a, Show a) => BS.ByteString -> a -> Assertion
protobufDecode bs res = case messageGet $ BL.fromStrict bs of
                          Left e -> assertFailure e
                          Right (t, _) -> t @?= res

tileDecode :: BS.ByteString -> Assertion
tileDecode bs = assertBool "tileDecode" . isRight $ tile bs

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"

encodeIso :: I.Tile -> Assertion
encodeIso vt = case I.fromProtobuf vt of
                 Left e  -> assertFailure $ T.unpack e
                 Right t -> I.toProtobuf t @?= vt

testTile :: I.Tile
testTile = I.Tile (Seq.singleton l) defaultValue
  where l = defaultValue { I.version  = 2
                         , I.name     = Utf8 "testlayer"
                         , I.features = Seq.singleton f
                         , I.keys     = Seq.singleton $ Utf8 "somekey"
                         , I.values   = Seq.singleton v
                         , I.extent   = Just 4096 }
        f = I.Feature { I.id    = Just 0
                      , I.tags  = Seq.fromList [0,0]
                      , I.type' = Just I.POINT
                      , I.geometry = Seq.fromList [9, 50, 34] }  -- MoveTo(+25,+17)
        v = defaultValue { I.string_value = Just $ Utf8 "Some Value" }

-- | Correct decoding of `onepoint.mvt`
onePoint :: I.Tile
onePoint = I.Tile (Seq.singleton l) defaultValue
  where l = defaultValue { I.version   = 1
                         , I.name      = Utf8 "OnePoint"
                         , I.features  = Seq.singleton f
                         , I.keys      = Seq.Empty
                         , I.values    = Seq.Empty
                         , I.extent    = Just 4096 }
        f = I.Feature { I.id    = Just 0
                      , I.tags  = Seq.Empty
                      , I.type' = Just I.POINT
                      , I.geometry = Seq.fromList [9, 10, 10] }  -- MoveTo(+5,+5)

-- | Correct decoding of `linestring.mvt`
oneLineString :: I.Tile
oneLineString = I.Tile (Seq.singleton l) defaultValue
  where l = defaultValue { I.version   = 1
                         , I.name      = Utf8 "OneLineString"
                         , I.features  = Seq.singleton f
                         , I.keys      = Seq.Empty
                         , I.values    = Seq.Empty
                         , I.extent    = Just 4096 }
        f = I.Feature { I.id    = Just 0
                      , I.tags  = Seq.Empty
                      , I.type' = Just I.LINESTRING
                        -- MoveTo(+5,+5), LineTo(+1195,+1195)
                      , I.geometry = Seq.fromList [9, 10, 10, 10, 2390, 2390] }

-- | Correct decoding of `polygon.mvt`
onePolygon :: I.Tile
onePolygon = I.Tile (Seq.singleton l) defaultValue
  where l = defaultValue { I.version   = 1
                         , I.name      = Utf8 "OnePolygon"
                         , I.features  = Seq.singleton f
                         , I.keys      = Seq.Empty
                         , I.values    = Seq.Empty
                         , I.extent    = Just 4096 }
        f = I.Feature { I.id    = Just 0
                      , I.tags  = Seq.Empty
                      , I.type' = Just I.POLYGON
                      -- MoveTo(+2,+2), LineTo(+3,+2), LineTo(-3,+2), ClosePath
                      , I.geometry = Seq.fromList [9, 4, 4, 18, 6, 4, 5, 4, 15] }

zencoding :: Assertion
zencoding = map (I.unzig . I.zig) vs @?= vs
  where vs = [0,(-1),1,(-2),2,(-3),3,2147483647,(-2147483648)]

commandTest :: Assertion
commandTest = I.commands (Seq.fromList [9,4,4,18,6,4,5,4,15]) @?= Right (
  Seq.fromList [ I.MoveTo $ Seq.singleton (Point 2 2)
               , I.LineTo $ Seq.fromList [ Point 3 2, Point (-3) 2 ]
               , I.ClosePath ]
  )

commandIso :: Assertion
commandIso = (I.uncommands . fromRight $ I.commands cs) @?= cs
  where cs = Seq.fromList [9,4,4,18,6,4,5,4,15]

pointIso :: Assertion
pointIso = cs' @?= cs
  where cs = Seq.fromList [25,4,4,6,6,3,3]
        cs' = fromRight $ I.uncommands . I.toCommands <$> (I.commands cs >>= I.fromCommands @Point)

linestringIso :: Assertion
linestringIso = cs' @?= cs
  where cs = Seq.fromList [9,4,4,18,6,4,5,4,9,4,4,18,6,4,5,4]
        cs' = fromRight $ I.uncommands . I.toCommands <$> (I.commands cs >>= I.fromCommands @LineString)

-- | Two solids
polygonIso :: Assertion
polygonIso = cs' @?= cs
  where cs = Seq.fromList [9,4,4,18,6,4,5,4,15,9,4,4,18,6,4,5,4,15]
        cs' = fromRight $ I.uncommands . I.toCommands <$> (I.commands cs >>= I.fromCommands @Polygon)

-- | One holed
polygonIso2 :: Assertion
polygonIso2 = cs' @?= cs
  where cs = Seq.fromList [9,4,4,26,6,0,0,6,5,0,15,9,2,3,26,0,2,2,0,0,1,15]
        cs' = fromRight $ I.uncommands . I.toCommands <$> (I.commands cs >>= I.fromCommands @Polygon)

-- | One Holed, one solid
polygonIso3 :: Assertion
polygonIso3 = cs' @?= cs
  where cs = Seq.fromList [ 9, 4, 4, 26, 6, 0, 0, 6, 5, 0, 15, 9, 2, 3, 26, 0, 2, 2, 0, 0, 1, 15
             , 9, 4, 4, 26, 6, 0, 0, 6, 5, 0, 15 ]
        cs' = fromRight $ I.uncommands . I.toCommands <$> (I.commands cs >>= I.fromCommands @Polygon)

poly :: Polygon
poly = Polygon ps mempty
  where ps = V.fromList [(Point 0 0), (Point 1 0), (Point 1 1), (Point 0 1), (Point 0 0)]
