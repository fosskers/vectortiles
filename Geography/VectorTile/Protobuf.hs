{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module    : Geography.VectorTile.Raw
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- Raw Vector Tile data is stored as binary protobuf data.
-- This module reads and writes raw protobuf ByteStrings between a data type
-- which closely matches the current Mapbox vector tile spec defined here:
-- https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto
--
-- As this raw version of the data is hard to work with, in practice we convert
-- to a more canonical Haskell type for further processing.
-- See `Geography.VectorTile` for the user-friendly version.
--
-- Please import this module @qualified@ to avoid namespace clashes:
--
-- > import qualified Geography.VectorTile.Protobuf as PB

module Geography.VectorTile.Protobuf
  ( -- * Types
    Protobuf(..)
  , Protobuffable(..)
  , ProtobufGeom(..)
  , RawVectorTile(..)
  , RawLayer(..)
  , RawVal(..)
  , RawFeature(..)
  , GeomType(..)
    -- * Commands
  , Command(..)
  , commands
  , uncommands
   -- * Z-Encoding
  , zig
  , unzig
    -- * Protobuf Conversions
    -- ** From Protobuf
    -- | Generally the `tile` function is the only one needed here. Usage:
    --
    -- > import qualified Geography.VectorTile.Protobuf as PB
    -- >
    -- > PB.decode someBytes >>= PB.tile
    --
    -- Note that since the "Data.ProtocolBuffers" library does not handle default
    -- values, we handle those specifically defined in /vector_tile.proto/
    -- explicitely here. See:
    --
    -- https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto
  , features
    -- ** To Protobuf
    -- | To convert from high-level data back into a form that can be encoded
    -- into raw protobuf bytes, use:
    --
    -- > import qualified Geography.VectorTile.Protobuf as PB
    -- >
    -- > PB.encode $ PB.untile someTile
    --
    -- This is a pure process and will succeed every time.
  , unfeature
    -- * ByteString Encoding / Decoding
  , decode
  , encode
  , decodeIO
  , encodeIO
  ) where

import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData)
import           Control.Monad.Trans.State.Lazy
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Foldable (foldrM, foldlM)
import           Data.Int
import           Data.List (nub, elemIndex)
import qualified Data.Map.Lazy as M
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.ProtocolBuffers hiding (decode, encode)
import           Data.Serialize.Get
import           Data.Serialize.Put
import qualified Data.Set as S
import           Data.Text (Text, pack)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           GHC.Generics (Generic)
import qualified Geography.VectorTile.VectorTile as VT
import qualified Geography.VectorTile.Geometry as G
import           Geography.VectorTile.Util
import           Text.Printf.TH

---

-- | A family of data types which can associated with concrete underlying
-- Protobuf types.
type family Protobuf a
type instance Protobuf VT.VectorTile = RawVectorTile
type instance Protobuf VT.Layer = RawLayer
type instance Protobuf VT.Val = RawVal

-- | A type which can be converted to and from an underlying Protobuf type,
-- according to the `Protobuf` type family.
class Protobuffable a where
  fromProtobuf :: Protobuf a -> Either Text a
  toProtobuf :: a -> Protobuf a

instance Protobuffable VT.VectorTile where
  fromProtobuf = fmap (VT.VectorTile . V.fromList) . mapM fromProtobuf . getField . _layers

  toProtobuf vt = RawVectorTile { _layers = putField . V.toList . V.map toProtobuf $ VT._layers vt }

instance Protobuffable VT.Layer where
  fromProtobuf l = do
    (ps,ls,polys) <- features keys vals . getField $ _features l
    pure VT.Layer { VT._version = fromIntegral . getField $ _version l
                  , VT._name = getField $ _name l
                  , VT._points = ps
                  , VT._linestrings = ls
                  , VT._polygons = polys
                  , VT._extent = maybe 4096 fromIntegral (getField $ _extent l) }
      where keys = getField $ _keys l
            vals = getField $ _values l

  toProtobuf l = RawLayer { _version = putField . fromIntegral $ VT._version l
                          , _name = putField $ VT._name l
                          , _features = putField fs
                          , _keys = putField ks
                          , _values = putField $ map toProtobuf vs
                          , _extent = putField . Just . fromIntegral $ VT._extent l }
    where (ks,vs) = totalMeta (VT._points l) (VT._linestrings l) (VT._polygons l)
          fs = V.toList $ V.concat [ V.map (unfeature ks vs Point) (VT._points l)
                                   , V.map (unfeature ks vs LineString) (VT._linestrings l)
                                   , V.map (unfeature ks vs Polygon) (VT._polygons l) ]

instance Protobuffable VT.Val where
  fromProtobuf v = mtoe "Value decode: No legal Value type offered" $ fmap VT.St (getField $ _string v)
    <|> fmap VT.Fl  (getField $ _float v)
    <|> fmap VT.Do  (getField $ _double v)
    <|> fmap VT.I64 (getField $ _int64 v)
    <|> fmap VT.W64 (getField $ _uint64 v)
    <|> fmap (\(Signed n) -> VT.S64 n) (getField $ _sint v)
    <|> fmap VT.B   (getField $ _bool v)

  toProtobuf (VT.St v)  = def { _string = putField $ Just v }
  toProtobuf (VT.Fl v)  = def { _float = putField $ Just v }
  toProtobuf (VT.Do v)  = def { _double = putField $ Just v }
  toProtobuf (VT.I64 v) = def { _int64 = putField $ Just v }
  toProtobuf (VT.W64 v) = def { _uint64 = putField $ Just v }
  toProtobuf (VT.S64 v) = def { _sint = putField . Just $ Signed v }
  toProtobuf (VT.B v)   = def { _bool = putField $ Just v }

-- | A list of `RawLayer`s.
data RawVectorTile = RawVectorTile { _layers :: Repeated 3 (Message RawLayer) }
                   deriving (Generic,Show,Eq)

instance Encode RawVectorTile
instance Decode RawVectorTile
instance NFData RawVectorTile

-- | Contains a pseudo-map of metadata, to be shared across all `RawFeature`s
-- of this `RawLayer`.
data RawLayer = RawLayer { _version :: Required 15 (Value Word32)
                         , _name :: Required 1 (Value Text)
                         , _features :: Repeated 2 (Message RawFeature)
                         , _keys :: Repeated 3 (Value Text)
                         , _values :: Repeated 4 (Message RawVal)
                         , _extent :: Optional 5 (Value Word32)
                         } deriving (Generic,Show,Eq)

instance Encode RawLayer
instance Decode RawLayer
instance NFData RawLayer

-- | The /Value/ types of metadata fields.
data RawVal = RawVal { _string :: Optional 1 (Value Text)
                     , _float :: Optional 2 (Value Float)
                     , _double :: Optional 3 (Value Double)
                     , _int64 :: Optional 4 (Value Int64)
                     , _uint64 :: Optional 5 (Value Word64)
                     , _sint :: Optional 6 (Value (Signed Int64))  -- ^ Z-encoded.
                     , _bool :: Optional 7 (Value Bool)
                     } deriving (Generic,Show,Eq)

instance Encode RawVal
instance Decode RawVal
instance NFData RawVal

-- | A set of geometries unified by some theme.
data RawFeature = RawFeature { _featureId :: Optional 1 (Value Word64)
                             , _tags :: Packed 2 (Value Word32)
                             , _geom :: Optional 3 (Enumeration GeomType)
                             , _geometries :: Packed 4 (Value Word32)
                             } deriving (Generic,Show,Eq)

instance Encode RawFeature
instance Decode RawFeature
instance NFData RawFeature

-- | The four potential Geometry types. The spec allows for encoders to set
-- `Unknown` as the type, but our decoder ignores these.
data GeomType = Unknown | Point | LineString | Polygon
              deriving (Generic,Enum,Show,Eq)

instance Encode GeomType
instance Decode GeomType
instance NFData GeomType

-- | Any classical type considered a GIS "geometry". These must be able
-- to convert between an encodable list of `Command`s.
class ProtobufGeom g where
  fromCommands :: [Command] -> Either Text (V.Vector g)
  toCommands :: V.Vector g -> [Command]

-- | A valid `RawFeature` of points must contain a single `MoveTo` command
-- with a count greater than 0.
instance ProtobufGeom G.Point where
  fromCommands (MoveTo ps : []) = Right . U.convert $ evalState (U.mapM expand ps) (0,0)
  fromCommands (c:_) = Left $ [st|Invalid command found in Point feature: %s|] (show c)
  fromCommands [] = Left "No points given!"

  -- | A multipoint geometry must reduce to a single `MoveTo` command.
  toCommands ps = [MoveTo $ evalState (U.mapM collapse $ U.convert ps) (0,0)]

-- | A valid `RawFeature` of linestrings must contain pairs of:
--
-- A `MoveTo` with a count of 1, followed by one `LineTo` command with
-- a count greater than 0.
instance ProtobufGeom G.LineString where
  fromCommands cs = evalState (f cs) (0,0)
    where f (MoveTo p : LineTo ps : rs) = fmap . V.cons <$> ls <*> f rs
            where ls = G.LineString <$> U.mapM expand (p <> ps)
          f [] = pure $ Right V.empty
          f _  = pure $ Left "LineString decode: Invalid command sequence given."

  toCommands ls = concat $ evalState (mapM f ls) (0,0)
    where f (G.LineString ps) = do
            l <- U.mapM collapse ps
            pure [MoveTo . U.singleton $ U.head l, LineTo $ U.tail l]

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
    ps <- evalState (f cs) (0,0)
    let (h,t) = (V.head ps, V.tail ps)
        (ps',p') = runState (foldlM g V.empty t) h
    pure $ V.snoc ps' p'  -- Include the last Exterior Ring worked on.
    where f (MoveTo p : LineTo ps : ClosePath : rs) = do
            curr <- get
            let h = U.head p
                here = (G.x h + G.x curr, G.y h + G.y curr)
            po <- flip U.snoc here <$> U.mapM expand (U.cons h ps)
            fmap (V.cons (G.Polygon po V.empty)) <$> f rs
          f [] = pure $ Right V.empty
          f _  = pure . Left $ [st|Polygon decode: Invalid command sequence given: %s|] (show cs)
          g acc p | G.area p > 0 = do  -- New external rings.
                      curr <- get
                      put p
                      pure $ V.snoc acc curr
                  | otherwise = do  -- Next internal ring.
                      modify (\s -> s { G.inner = V.snoc (G.inner s) p })
                      pure acc

  toCommands ps = concat $ evalState (mapM f ps) (0,0)
    where f (G.Polygon p i) = do
            l <- U.mapM collapse $ U.init p  -- Exclude the final point.
            let cs = [MoveTo . U.singleton $ U.head l, LineTo $ U.tail l, ClosePath]
            concat . V.cons cs <$> mapM f i

-- | The possible commands, and the values they hold.
data Command = MoveTo (U.Vector (Int,Int))
             | LineTo (U.Vector (Int,Int))
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
  (m,_) -> Left $ [st|Invalid command integer %d found in: %X|] m n
  where cmd = n .&. 7
        count = shift n (-3)

-- | Recombine a Command ID and parameter count into a Command Integer.
unparseCmd :: (Int,Int) -> Word32
unparseCmd (cmd,count) = fromIntegral $ (cmd .&. 7) .|. shift count 3

-- | Attempt to parse a list of Command/Parameter integers, as defined here:
--
-- https://github.com/mapbox/vector-tile-spec/tree/master/2.1#43-geometry-encoding
commands :: [Word32] -> Either Text [Command]
commands [] = Right []
commands (n:ns) = parseCmd n >>= f
  where f (1,count) = do
          mts <- MoveTo . U.fromList . map (both unzig) <$> pairs (take (count * 2) ns)
          (mts :) <$> commands (drop (count * 2) ns)
        f (2,count) = do
          mts <- LineTo . U.fromList . map (both unzig) <$> pairs (take (count * 2) ns)
          (mts :) <$> commands (drop (count * 2) ns)
        f (7,_) = (ClosePath :) <$> commands ns
        f _ = Left "Sentinel: You should never see this."

-- | Convert a list of parsed `Command`s back into their original Command
-- and Z-encoded Parameter integer forms.
uncommands :: [Command] -> [Word32]
uncommands = U.toList . U.concat . map f
  where f (MoveTo ps) = (U.cons $ unparseCmd (1, U.length ps)) $ params ps
        f (LineTo ls) = (U.cons $ unparseCmd (2, U.length ls)) $ params ls
        f ClosePath = U.singleton $ unparseCmd (7,1)  -- ClosePath, Count 1.

-- | Attempt to decode a `BS.ByteString` of raw protobuf data into a mid-level
-- representation of a `RawVectorTile`.
decode :: BS.ByteString -> Either Text RawVectorTile
decode bs = case runGet decodeMessage bs of
  Left e -> Left $ pack e
  Right vt -> Right vt

-- | Encode a mid-level representation of a `RawVectorTile` into raw protobuf data.
encode :: RawVectorTile -> BS.ByteString
encode = runPut . encodeMessage

-- | Given a filename, attempt to decode bytes read from that file.
decodeIO :: FilePath -> IO (Either Text RawVectorTile)
decodeIO = fmap decode . BS.readFile

-- | Write a mid-level representation of a `RawVectorTile` to a file as raw
-- protobuf data.
encodeIO :: RawVectorTile -> FilePath -> IO ()
encodeIO vt fp = BS.writeFile fp $ encode vt

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
features :: [Text] -> [RawVal] -> [RawFeature]
  -> Either Text (V.Vector (VT.Feature G.Point), V.Vector (VT.Feature G.LineString), V.Vector (VT.Feature G.Polygon))
features _ _ [] = Left "VectorTile.features: `[RawFeature]` empty"
features keys vals fs = (,,) <$> ps <*> ls <*> polys
  where -- (_:ps':ls':polys':_) = groupBy sameGeom $ sortOn geomBias fs  -- ok ok ok
        ps = foldrM f V.empty $ filter (\fe -> getField (_geom fe) == Just Point) fs
        ls = foldrM f V.empty $ filter (\fe -> getField (_geom fe) == Just LineString) fs
        polys = foldrM f V.empty $ filter (\fe -> getField (_geom fe) == Just Polygon) fs

        f :: ProtobufGeom g => RawFeature -> V.Vector (VT.Feature g) -> Either Text (V.Vector (VT.Feature g))
        f x acc = do
          geos <- commands (getField $ _geometries x) >>= fromCommands
          meta <- getMeta keys vals . getField $ _tags x
          pure $ VT.Feature { VT._featureId = maybe 0 fromIntegral . getField $ _featureId x
                            , VT._metadata = meta
                            , VT._geometries = geos
                            } `V.cons` acc

getMeta :: [Text] -> [RawVal] -> [Word32] -> Either Text (M.Map Text VT.Val)
getMeta keys vals tags = do
  kv <- map (both fromIntegral) <$> pairs tags
  foldrM (\(k,v) acc -> (\v' -> M.insert (keys !! k) v' acc) <$> (fromProtobuf $ vals !! v)) M.empty kv

{- TO PROTOBUF -}

totalMeta :: V.Vector (VT.Feature G.Point) -> V.Vector (VT.Feature G.LineString) -> V.Vector (VT.Feature G.Polygon) -> ([Text], [VT.Val])
totalMeta ps ls polys = (keys, vals)
  where keys = S.toList . S.unions $ f ps <> f ls <> f polys
        vals = nub . concat $ g ps <> g ls <> g polys  -- `nub` is O(n^2)
        f = V.foldr (\x acc -> M.keysSet (VT._metadata x) : acc) []
        g = V.foldr (\x acc -> M.elems (VT._metadata x) : acc) []

-- | Encode a high-level `Feature` back into its mid-level `RawFeature` form.
unfeature :: ProtobufGeom g => [Text] -> [VT.Val] -> GeomType -> VT.Feature g -> RawFeature
unfeature keys vals gt fe = RawFeature
                            { _featureId = putField . Just . fromIntegral $ VT._featureId fe
                            , _tags = putField $ tags fe
                            , _geom = putField $ Just gt
                            , _geometries = putField . uncommands . toCommands $ VT._geometries fe
                            }
  where tags = unpairs . map f . M.toList . VT._metadata
        f (k,v) = both (fromIntegral . fromJust) (k `elemIndex` keys, v `elemIndex` vals)

{- UTIL -}

-- | A `RawVal` with every entry set to `Nothing`.
def :: RawVal
def = RawVal { _string = putField Nothing
             , _float  = putField Nothing
             , _double = putField Nothing
             , _int64  = putField Nothing
             , _uint64 = putField Nothing
             , _sint   = putField Nothing
             , _bool   = putField Nothing }

-- | Transform a `V.Vector` of `Point`s into one of Z-encoded Parameter ints.
params :: U.Vector (Int,Int) -> U.Vector Word32
params = U.foldr (\(a,b) acc -> U.cons (zig a) $ U.cons (zig b) acc) U.empty

-- | Expand a pair of diffs from some reference point into that
-- of a `Point` value. The reference point is moved to our new `Point`.
expand :: (Int,Int) -> State (Int,Int) G.Point
expand p = do
  curr <- get
  let here = (G.x p + G.x curr, G.y p + G.y curr)
  put here
  pure here

-- | Collapse a given `Point` into a pair of diffs, relative to
-- the previous point in the sequence. The reference point is moved
-- to the `Point` given.
collapse :: G.Point -> State (Int,Int) (Int,Int)
collapse p = do
  curr <- get
  let diff = (G.x p - G.x curr, G.y p - G.y curr)
  put p
  pure diff
