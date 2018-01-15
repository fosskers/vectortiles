-- -*- dante-target: "vectortiles-test"; -*-

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
import           Data.Hex
import qualified Data.Sequence as Seq
import           Data.Text (Text, unpack)
import qualified Data.Vector.Unboxed as U
import           Geography.VectorTile
import           Geography.VectorTile.Geometry
import qualified Geography.VectorTile.Protobuf.Internal as R
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile as Tile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature as Feature
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as GeomType
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer as Layer
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Value as Value
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.ProtocolBuffers.Basic (Utf8(..), defaultValue)

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
      [ testCase "onepoint.mvt -> Raw.Tile" $ testOnePoint op
      , testCase "linestring.mvt -> Raw.Tile" $ testLineString ls
      , testCase "polygon.mvt -> Raw.Tile" $ testPolygon pl
      , testCase "roads.mvt -> Raw.Tile" $ testDecode rd
      , testCase "clearlake.mvt -> Raw.Tile" $ testDecode cl
      , testCase "onepoint.mvt -> VectorTile" $ tileDecode op
      , testCase "linestring.mvt -> VectorTile" $ tileDecode ls
      , testCase "polygon.mvt -> VectorTile" $ tileDecode pl
      , testCase "roads.mvt -> VectorTile" $ tileDecode rd
      , testCase "clearlake.mvt -> VectorTile" $ tileDecode cl
      ]
    , testGroup "Encoding"
      [ testGroup "RawVectorTile <-> VectorTile"
        [ testCase "One Point" $ encodeIso onePoint
        , testCase "One LineString" $ encodeIso oneLineString
        , testCase "One Polygon" $ encodeIso onePolygon
        , testCase "roads.mvt" . encodeIso . fromRight $ decode rd
        ]
      ]
    , testGroup "Serialization Isomorphism"
      [ --testCase "onepoint.mvt <-> Raw.Tile" $ fromRaw op
        -- testCase "linestring.mvt <-> Raw.Tile" $ fromRaw ls
--      , testCase "polygon.mvt <-> Raw.Tile" $ fromRaw pl
      --    , testCase "roads.mvt <-> Raw.Tile" $ fromRaw rd
        testCase "testTile <-> protobuf bytes" testTileIso
      ]
    ]
  , testGroup "Geometries"
    [ testCase "Z-encoding Isomorphism" zencoding
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
testOnePoint vt = case decode vt of
                    Left e -> assertFailure $ unpack e
                    Right t -> t @?= onePoint

testLineString :: BS.ByteString -> Assertion
testLineString vt = case decode vt of
                      Left e -> assertFailure $ unpack e
                      Right t -> t @?= oneLineString

testPolygon :: BS.ByteString -> Assertion
testPolygon vt = case decode vt of
                   Left e -> assertFailure $ unpack e
                   Right t -> t @?= onePolygon

-- | For testing is decoding succeeded in generally. Makes no guarantee
-- about the quality of the content, only that the parse succeeded.
testDecode :: BS.ByteString -> Assertion
testDecode = assert . isRight . decode

tileDecode :: BS.ByteString -> Assertion
tileDecode bs = case decode bs of
  Left e -> assertFailure $ unpack e
  Right t -> assert . isRight $ R.fromProtobuf t

fromRaw :: BS.ByteString -> Assertion
fromRaw vt = case decode vt of
               Left e  -> assertFailure $ unpack e
               -- Right l -> encode l @?= vt
               Right l -> hex (encode l) @?= hex vt

testTileIso :: Assertion
testTileIso = case decode $ encode testTile of
                 Right tl -> assertEqual "" tl testTile
                 Left e -> assertFailure $ unpack e

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "`Left` given to fromRight!"

rawTest :: IO (Either Text Tile.Tile)
rawTest = decode <$> BS.readFile "onepoint.mvt"

encodeIso :: Tile.Tile -> Assertion
encodeIso vt = assert . isRight . fmap R.toProtobuf $ R.fromProtobuf vt

testTile :: Tile.Tile
testTile = Tile.Tile (Seq.singleton l) defaultValue
  where l = Layer.Layer { Layer.version   = 2
                        , Layer.name      = Utf8 "testlayer"
                        , Layer.features  = Seq.singleton f
                        , Layer.keys      = Seq.singleton $ Utf8 "somekey"
                        , Layer.values    = Seq.singleton v
                        , Layer.extent    = Just 4096
                        , Layer.ext'field = defaultValue }
        f = Feature.Feature { Feature.id    = Just 0
                            , Feature.tags  = Seq.fromList [0,0]
                            , Feature.type' = Just GeomType.POINT
                            , Feature.geometry = Seq.fromList [9, 50, 34] }  -- MoveTo(+25,+17)
        v = defaultValue { Value.string_value = Just $ Utf8 "Some Value" }

-- | Correct decoding of `onepoint.mvt`
onePoint :: Tile.Tile
onePoint = Tile.Tile (Seq.singleton l) defaultValue
  where l = Layer.Layer { Layer.version   = 1
                        , Layer.name      = Utf8 "OnePoint"
                        , Layer.features  = Seq.singleton f
                        , Layer.keys      = Seq.Empty
                        , Layer.values    = Seq.Empty
                        , Layer.extent    = Just 4096
                        , Layer.ext'field = defaultValue }
        f = Feature.Feature { Feature.id    = Just 0
                            , Feature.tags  = Seq.Empty
                            , Feature.type' = Just GeomType.POINT
                            , Feature.geometry = Seq.fromList [9, 10, 10] }  -- MoveTo(+5,+5)

-- | Correct decoding of `linestring.mvt`
oneLineString :: Tile.Tile
oneLineString = Tile.Tile (Seq.singleton l) defaultValue
  where l = Layer.Layer { Layer.version   = 1
                        , Layer.name      = Utf8 "OneLineString"
                        , Layer.features  = Seq.singleton f
                        , Layer.keys      = Seq.Empty
                        , Layer.values    = Seq.Empty
                        , Layer.extent    = Just 4096
                        , Layer.ext'field = defaultValue }
        f = Feature.Feature { Feature.id    = Just 0
                            , Feature.tags  = Seq.Empty
                            , Feature.type' = Just GeomType.LINESTRING
                            -- MoveTo(+5,+5), LineTo(+1195,+1195)
                            , Feature.geometry = Seq.fromList [9, 10, 10, 10, 2390, 2390] }

-- | Correct decoding of `polygon.mvt`
onePolygon :: Tile.Tile
onePolygon = Tile.Tile (Seq.singleton l) defaultValue
  where l = Layer.Layer { Layer.version   = 1
                        , Layer.name      = Utf8 "OnePolygon"
                        , Layer.features  = Seq.singleton f
                        , Layer.keys      = Seq.Empty
                        , Layer.values    = Seq.Empty
                        , Layer.extent    = Just 4096
                        , Layer.ext'field = defaultValue }
        f = Feature.Feature { Feature.id    = Just 0
                            , Feature.tags  = Seq.Empty
                            , Feature.type' = Just GeomType.POLYGON
                            -- MoveTo(+2,+2), LineTo(+3,+2), LineTo(-3,+2), ClosePath
                            , Feature.geometry = Seq.fromList [9, 4, 4, 18, 6, 4, 5, 4, 15] }

zencoding :: Assertion
zencoding = assert $ map (R.unzig . R.zig) vs @?= vs
  where vs = [0,(-1),1,(-2),2,(-3),3,2147483647,(-2147483648)]

commandTest :: Assertion
commandTest = assert $ R.commands [9,4,4,18,6,4,5,4,15] @?= Right
  [ R.MoveTo $ U.singleton (2,2)
  , R.LineTo $ U.fromList [(3,2),(-3,2)]
  , R.ClosePath ]

commandIso :: Assertion
commandIso = assert $ (R.uncommands . fromRight $ R.commands cs) @?= cs
  where cs = [9,4,4,18,6,4,5,4,15]

pointIso :: Assertion
pointIso = cs' @?= cs
  where cs = [25,4,4,6,6,3,3]
        cs' = fromRight $ R.uncommands . R.toCommands <$> (R.commands cs >>= R.fromCommands @Point)

linestringIso :: Assertion
linestringIso = cs' @?= cs
  where cs = [9,4,4,18,6,4,5,4,9,4,4,18,6,4,5,4]
        cs' = fromRight $ R.uncommands . R.toCommands <$> (R.commands cs >>= R.fromCommands @LineString)

-- | Two solids
polygonIso :: Assertion
polygonIso = cs' @?= cs
  where cs = [9,4,4,18,6,4,5,4,15,9,4,4,18,6,4,5,4,15]
        cs' = fromRight $ R.uncommands . R.toCommands <$> (R.commands cs >>= R.fromCommands @Polygon)

-- | One holed
polygonIso2 :: Assertion
polygonIso2 = cs' @?= cs
  where cs = [9,4,4,26,6,0,0,6,5,0,15,9,2,3,26,0,2,2,0,0,1,15]
        cs' = fromRight $ R.uncommands . R.toCommands <$> (R.commands cs >>= R.fromCommands @Polygon)

-- | One Holed, one solid
polygonIso3 :: Assertion
polygonIso3 = cs' @?= cs
  where cs = [ 9, 4, 4, 26, 6, 0, 0, 6, 5, 0, 15, 9, 2, 3, 26, 0, 2, 2, 0, 0, 1, 15
             , 9, 4, 4, 26, 6, 0, 0, 6, 5, 0, 15 ]
        cs' = fromRight $ R.uncommands . R.toCommands <$> (R.commands cs >>= R.fromCommands @Polygon)

{-}
foo :: FilePath -> IO (Either Text VectorTile)
foo bs = do
  mvt <- BS.readFile bs
  pure $ R.decode mvt >>= tile

-- fmap (V.length . layers <$>) $ foo "roads.mvt"
-}
