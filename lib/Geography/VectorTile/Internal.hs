{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module    : Geography.VectorTile.Internal
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Raw Vector Tile data is stored as binary protobuf data.
-- This module reads and writes raw protobuf ByteStrings between a data type
-- which closely matches the current Mapbox vector tile spec defined here:
-- https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto
--
-- As this raw version of the data is hard to work with, in practice we convert
-- to a more canonical Haskell type for further processing.
-- See "Geography.VectorTile" for the user-friendly version.

module Geography.VectorTile.Internal
  ( -- * Types
    -- ** Protobuf Conversion
    Protobuf(..)
  , Protobuffable(..)
  , ProtobufGeom(..)
    -- ** Decoded Middle-Types
  , Tile.Tile(Tile, layers)
  , Layer.Layer(Layer, version, name, features, keys, values, extent)
  , Feature.Feature(..)
  , Value.Value(..)
  , GeomType.GeomType(..)
    -- * Commands
  , Command(..)
  , commands
  , uncommands
   -- * Z-Encoding
  , zig
  , unzig
    -- * Protobuf Conversions
    -- | Due to Protobuf Layers and Features having their data coupled,
    -- we can't define a `Protobuffable` instance for `VT.Feature`s,
    -- and instead must use the two functions below.
  , feats
  , unfeats
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (fold, foldl', foldrM, foldlM, toList)
import           Data.Int
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as HS
import           Data.Maybe (fromJust)
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (<|), (|>), Seq((:<|)))
import           Data.Text (Text, pack)
import qualified Data.Vector.Unboxed as U
import           Data.Word
import qualified Geography.VectorTile.Geometry as G
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile as Tile
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature as Feature
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as GeomType
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer as Layer
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Value as Value
import           Geography.VectorTile.Util
import qualified Geography.VectorTile.VectorTile as VT
import           Text.Printf
import           Text.ProtocolBuffers.Basic (defaultValue, Utf8(..), utf8)

---

-- | A family of data types which can associated with concrete underlying
-- Protobuf types.
type family Protobuf a = pb | pb -> a
type instance Protobuf VT.VectorTile = Tile.Tile
type instance Protobuf VT.Layer = Layer.Layer
type instance Protobuf VT.Val = Value.Value

-- | A type which can be converted to and from an underlying Protobuf type,
-- according to the `Protobuf` type family.
class Protobuffable a where
  fromProtobuf :: Protobuf a -> Either Text a
  toProtobuf   :: a -> Protobuf a

instance Protobuffable VT.VectorTile where
  fromProtobuf raw = do
    ls <- traverse fromProtobuf . toList $ Tile.layers raw
    pure . VT.VectorTile . M.fromList $ map (\l -> (VT._name l, l)) ls

  toProtobuf vt = Tile.Tile { Tile.layers    = Seq.fromList . map toProtobuf . M.elems $ VT._layers vt
                            , Tile.ext'field = defaultValue }

instance Protobuffable VT.Layer where
  fromProtobuf l = do
    (ps,ls,polys) <- feats (utf8 <$> Layer.keys l) (Layer.values l) $ Layer.features l
    pure VT.Layer { VT._version = fromIntegral $ Layer.version l
                  , VT._name = utf8 $ Layer.name l
                  , VT._points = ps
                  , VT._linestrings = ls
                  , VT._polygons = polys
                  , VT._extent = maybe 4096 fromIntegral (Layer.extent l) }

  toProtobuf l = Layer.Layer { Layer.version   = fromIntegral $ VT._version l
                             , Layer.name      = Utf8 $ VT._name l
                             , Layer.features  = fs
                             , Layer.keys      = Seq.fromList $ map Utf8 ks
                             , Layer.values    = Seq.fromList $ map toProtobuf vs
                             , Layer.extent    = Just . fromIntegral $ VT._extent l
                             , Layer.ext'field = defaultValue }
    where (ks,vs) = totalMeta (VT._points l) (VT._linestrings l) (VT._polygons l)
          (km,vm) = (M.fromList $ zip ks [0..], M.fromList $ zip vs [0..])
          fs = fold [ fmap (unfeats km vm GeomType.POINT) (VT._points l)
                    , fmap (unfeats km vm GeomType.LINESTRING) (VT._linestrings l)
                    , fmap (unfeats km vm GeomType.POLYGON) (VT._polygons l) ]

instance Protobuffable VT.Val where
  fromProtobuf v = mtoe "Value decode: No legal Value type offered" $
        fmap (VT.St . utf8) (Value.string_value v)
    <|> fmap VT.Fl  (Value.float_value v)
    <|> fmap VT.Do  (Value.double_value v)
    <|> fmap VT.I64 (Value.int_value v)
    <|> fmap VT.W64 (Value.uint_value v)
    <|> fmap VT.S64 (Value.sint_value v)
    <|> fmap VT.B   (Value.bool_value v)

  toProtobuf (VT.St v)  = defaultValue { Value.string_value = Just $ Utf8 v }
  toProtobuf (VT.Fl v)  = defaultValue { Value.float_value  = Just v }
  toProtobuf (VT.Do v)  = defaultValue { Value.double_value = Just v }
  toProtobuf (VT.I64 v) = defaultValue { Value.int_value    = Just v }
  toProtobuf (VT.W64 v) = defaultValue { Value.uint_value   = Just v }
  toProtobuf (VT.S64 v) = defaultValue { Value.sint_value   = Just v }
  toProtobuf (VT.B v)   = defaultValue { Value.bool_value   = Just v }

-- | Any classical type considered a GIS "geometry". These must be able
-- to convert between an encodable list of `Command`s.
class ProtobufGeom g where
  fromCommands :: Seq Command -> Either Text (Seq g)
  toCommands :: Seq g -> Seq Command

-- | A valid `RawFeature` of points must contain a single `MoveTo` command
-- with a count greater than 0.
instance ProtobufGeom G.Point where
  fromCommands (MoveTo ps :<| Seq.Empty) = Right $ expand' (0, 0) ps
  fromCommands (c :<| _) = Left . pack $ printf "Invalid command found in Point feature: %s" (show c)
  fromCommands Seq.Empty = Left "No points given!"

  -- | A multipoint geometry must reduce to a single `MoveTo` command.
  toCommands ps = Seq.singleton (MoveTo $ evalState (traverse collapse ps) (0,0))

-- | A valid `RawFeature` of linestrings must contain pairs of:
--
-- A `MoveTo` with a count of 1, followed by one `LineTo` command with
-- a count greater than 0.
instance ProtobufGeom G.LineString where
  fromCommands cs = evalState (f cs) (0,0)
    where f (MoveTo (p :<| Seq.Empty) :<| LineTo ps :<| rs) = do
            curr <- get
            let ls = G.LineString . expand curr . U.fromList . toList $ p <| ps
            put . U.last $ G.lsPoints ls
            fmap (ls <|) <$> f rs
          f Seq.Empty = pure $ Right Seq.Empty
          f _ = pure $ Left "LineString decode: Invalid command sequence given."

  toCommands ls = fold $ evalState (traverse f ls) (0,0)
    where f (G.LineString ps) = do
            l <- U.mapM collapse ps
            pure $ MoveTo (Seq.singleton $ U.head l) <| LineTo (Seq.fromList . U.toList $ U.tail l) <| Seq.Empty

-- | A valid `RawFeature` of polygons must contain at least one sequence of:
--
-- An Exterior Ring, followed by 0 or more Interior Rings.
--
-- Any Ring must have a `MoveTo` with a count of 1, a single `LineTo`
-- with a count of at least 2, and a single `ClosePath` command.
--
-- Performs no sanity checks for malformed Interior Rings.
instance ProtobufGeom G.Polygon where
  fromCommands cs = do
    h :<| t <- evalState (f cs) (0,0)
    let (ps',p') = runState (foldlM g Seq.Empty t) h
    pure $ ps' |> p'  -- Include the last Exterior Ring worked on.
    where f (MoveTo (p :<| Seq.Empty) :<| LineTo ps :<| ClosePath :<| rs) = do
            curr <- get
            let ps' = expand curr . U.fromList . toList $ p <| ps  -- Conversion bottleneck?
            put $ U.last ps'
            fmap (G.Polygon (U.snoc ps' $ U.head ps') Seq.Empty <|) <$> f rs
          f Seq.Empty = pure $ Right Seq.Empty
          f _  = pure . Left . pack $ printf "Polygon decode: Invalid command sequence given: %s" (show cs)
          g acc p | G.area p > 0 = do  -- New external rings.
                      curr <- get
                      put p
                      pure $ acc |> curr
                  | otherwise = do  -- Next internal ring.
                      modify (\s -> s { G.inner = G.inner s |> p })
                      pure acc

  toCommands ps = fold $ evalState (traverse f ps) (0,0)
    where f :: G.Polygon -> State (Int, Int) (Seq Command)
          f (G.Polygon p i) = do
            l <- U.mapM collapse $ U.init p  -- Exclude the final point.
            let cs = MoveTo (Seq.singleton $ U.head l) <| LineTo (Seq.fromList . U.toList $ U.tail l) <| ClosePath <| Seq.Empty
            fold . (cs <|) <$> traverse f i

-- | The possible commands, and the values they hold.
data Command = MoveTo (Seq (Int,Int))
             | LineTo (Seq (Int,Int))
             | ClosePath deriving (Eq,Show)

-- | Z-encode a 64-bit Int.
zig :: Int -> Word32
zig n = fromIntegral $ shift n 1 `xor` shift n (-63)

-- | Decode a Z-encoded Word32 into a 64-bit Int.
unzig :: Word32 -> Int
unzig n = fromIntegral (fromIntegral unzigged :: Int32)
  where unzigged = shift n (-1) `xor` negate (n .&. 1)

-- | Divide a "Command Integer" into its @(Command,Count)@.
parseCmd :: Word32 -> Either Text (Int,Int)
parseCmd n = case (cmd,count) of
  (1,m) -> Right $ both fromIntegral (1,m)
  (2,m) -> Right $ both fromIntegral (2,m)
  (7,1) -> Right (7,1)
  (7,m) -> Left $ "ClosePath was given a parameter count: " <> pack (show m)
  (m,_) -> Left . pack $ printf "Invalid command integer %d found in: %X" m n
  where cmd = n .&. 7
        count = shift n (-3)

-- | Recombine a Command ID and parameter count into a Command Integer.
unparseCmd :: (Int,Int) -> Word32
unparseCmd (cmd,count) = fromIntegral $ (cmd .&. 7) .|. shift count 3

-- | Attempt to parse a list of Command/Parameter integers, as defined here:
--
-- https://github.com/mapbox/vector-tile-spec/tree/master/2.1#43-geometry-encoding
commands :: Seq Word32 -> Either Text (Seq Command)
commands = go (Right Seq.Empty)
  where go !acc Seq.Empty = acc
        go (Left e) _ = Left e
        go (Right !acc) (n :<| ns) = parseCmd n >>= \case
          (1, count) -> do
            let (ls,rs) = Seq.splitAt (count * 2) ns
            mts <- MoveTo . fmap (both unzig) <$> pairs' ls
            go (Right $ acc |> mts) rs
          (2, count) -> do
            let (ls,rs) = Seq.splitAt (count * 2) ns
            mts <- LineTo . fmap (both unzig) <$> pairs' ls
            go (Right $ acc |> mts) rs
          (7, _) -> go (Right $ acc |> ClosePath) ns
          _ -> Left "Sentinel: You should never see this."

-- | Convert a list of parsed `Command`s back into their original Command
-- and Z-encoded Parameter integer forms.
uncommands :: Seq Command -> Seq Word32
uncommands = (>>= f)
  where f (MoveTo ps) = unparseCmd (1, length ps) <| params ps
        f (LineTo ls) = unparseCmd (2, length ls) <| params ls
        f ClosePath   = Seq.singleton $ unparseCmd (7,1)  -- ClosePath, Count 1.

{- FROM PROTOBUF -}

-- | Convert a list of `RawFeature`s of parsed protobuf data into `V.Vector`s
-- of each of the three legal `ProtobufGeom` types.
--
-- The long type signature is due to two things:
--
-- 1. `Feature`s are polymorphic at the high level, but not at the parsed
-- protobuf mid-level. In a @[RawFeature]@, there are features of points,
-- linestrings, and polygons all mixed together.
--
-- 2. `RawLayer`s and `RawFeature`s
-- are strongly coupled at the protobuf level. In order to achieve higher
-- compression ratios, `RawLayer`s contain all metadata in key/value lists
-- to be shared across their `RawFeature`s, while those `RawFeature`s store only
-- indices into those lists. As a result, this function needs to be passed
-- those key/value lists from the parent `RawLayer`, and a more isomorphic:
--
-- > feature :: ProtobufGeom g => RawFeature -> Either Text (Feature g)
--
-- is not possible.
feats :: Seq BL.ByteString -> Seq Value.Value -> Seq Feature.Feature
  -> Either Text (Seq (VT.Feature G.Point), Seq (VT.Feature G.LineString), Seq (VT.Feature G.Polygon))
feats _ _ Seq.Empty = Left "VectorTile.features: `[RawFeature]` empty"
feats keys vals fs = foldlM g mempty fs
  where f :: ProtobufGeom g => Feature.Feature -> Either Text (VT.Feature g)
        f x = VT.Feature
          <$> pure (maybe 0 fromIntegral $ Feature.id x)
          <*> getMeta keys vals (Feature.tags x)
          <*> (commands (Feature.geometry x) >>= fromCommands)
        g (!pnt,!lin,!ply) fe = case Feature.type' fe of
          Just GeomType.POINT      -> (\fe' -> (pnt |> fe', lin, ply)) <$> f fe
          Just GeomType.LINESTRING -> (\fe' -> (pnt, lin |> fe', ply)) <$> f fe
          Just GeomType.POLYGON    -> (\fe' -> (pnt, lin, ply |> fe')) <$> f fe
          _ -> Left "Geometry type of UNKNOWN given."

getMeta :: Seq BL.ByteString -> Seq Value.Value -> Seq Word32 -> Either Text (M.HashMap BL.ByteString VT.Val)
getMeta keys vals tags = do
  kv <- fmap (both fromIntegral) <$> pairs' tags
  foldrM (\(k,v) acc -> (\v' -> M.insert (keys `Seq.index` k) v' acc) <$> fromProtobuf (vals `Seq.index` v)) M.empty kv

{- TO PROTOBUF -}

totalMeta :: Seq (VT.Feature G.Point) -> Seq (VT.Feature G.LineString) -> Seq (VT.Feature G.Polygon) -> ([BL.ByteString], [VT.Val])
totalMeta ps ls polys = (keys, vals)
  where keys = HS.toList $ f ps <> f ls <> f polys
        vals = HS.toList $ g ps <> g ls <> g polys
        f = foldMap (HS.fromMap . fmap (const ()) . VT._metadata)
        g = foldMap (HS.fromList . M.elems . VT._metadata)

-- | Encode a high-level `Feature` back into its mid-level `RawFeature` form.
unfeats :: ProtobufGeom g => M.HashMap BL.ByteString Int -> M.HashMap VT.Val Int -> GeomType.GeomType -> VT.Feature g -> Feature.Feature
unfeats keys vals gt fe = Feature.Feature
                            { Feature.id       = Just . fromIntegral $ VT._featureId fe
                            , Feature.tags     = Seq.fromList $ tags fe
                            , Feature.type'    = Just gt
                            , Feature.geometry = uncommands . toCommands $ VT._geometries fe }
  where tags = unpairs . map f . M.toList . VT._metadata
        f (k,v) = both (fromIntegral . fromJust) (M.lookup k keys, M.lookup v vals)

{- UTIL -}

-- | Transform a `Seq` of `Point`s into one of Z-encoded Parameter ints.
params :: Seq (Int,Int) -> Seq Word32
params = foldl' (\acc (a,b) -> acc |> zig a |> zig b) Seq.Empty

-- | Expand a pair of diffs from some reference point into that of a `Point` value.
expand :: (Int, Int) -> U.Vector (Int, Int) -> U.Vector (Int, Int)
expand = U.postscanl' (\(x, y) (dx, dy) -> (x + dx, y + dy))

expand' :: (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
expand' curr s = Seq.drop 1 $ Seq.scanl (\(x, y) (dx, dy) -> (x + dx, y + dy)) curr s

-- | Collapse a given `Point` into a pair of diffs, relative to
-- the previous point in the sequence. The reference point is moved
-- to the `Point` given.
collapse :: G.Point -> State (Int,Int) (Int,Int)
collapse p = do
  curr <- get
  let diff = (G.x p - G.x curr, G.y p - G.y curr)
  put p
  pure diff
