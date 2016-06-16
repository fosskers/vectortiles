{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Geography.VectorTile.Geometry
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>

module Geography.VectorTile.Geometry
  ( -- * Geometries
    -- ** Types
    Geometry(..)
  , Point(..)
  , LineString(..)
  , Polygon(..)
  -- ** Operations
  , area
  , surveyor
  -- * Commands
  , Command(..)
  , commands
  , uncommands
   -- * Z-Encoding
  , zig
  , unzig
  ) where

import           Control.Monad.Trans.State.Lazy
import           Data.Bits
import           Data.Int
import           Data.Monoid
import           Data.Text (Text,pack)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Text.Printf.TH
import           Data.Foldable (foldlM)

---

-- | Points in space. Using "Record Pattern Synonyms" here allows us to treat
-- `Point` like a normal ADT, while its implementation remains an unboxed
-- @(Int,Int)@.
type Point = (Int,Int)
pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

-- | Points are just vectors in R2, and thus form a Vector space.
instance Monoid Point where
  mempty = Point 0 0
  (Point a b) `mappend` (Point a' b') = Point (a + a') (b + b')

-- | `newtype` compiles away to expose only the `U.Vector` of unboxed `Point`s
-- at runtime.
newtype LineString = LineString { points :: U.Vector Point } deriving (Eq,Show)

-- | A polygon aware of its inner polygons.
data Polygon = Polygon { points :: U.Vector Point
                       , inner :: V.Vector Polygon } deriving (Eq,Show)

{-
-- | Very performant for the same reason as `LineString`.
newtype Polygon = Polygon { points :: U.Vector Point } deriving (Eq,Show)
-}

-- | The area of a `Polygon` is the difference between the areas of its
-- outer ring and inner rings.
area :: Polygon -> Float
area p = surveyor (points (p :: Polygon)) + sum (V.map area $ inner p)

-- | The surveyor's formula for calculating the area of a `Polygon`.
-- If the value reported here is negative, then the `Polygon` should be
-- considered an Interior Ring.
--
-- Assumption: The `U.Vector` given has at least 4 `Point`s.
surveyor :: U.Vector Point -> Float
surveyor v = (/ 2) . fromIntegral . U.sum $ U.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
  where v' = U.init v
        xs = U.map x v'
        yns = U.map y . U.tail $ U.snoc v' (U.head v')
        yps = U.map y . U.init $ U.cons (U.last v') v'

-- | Any classical type considered a GIS "geometry". These must be able
-- to convert between an encodable list of `Command`s.
class Geometry a where
  fromCommands :: [Command] -> Either Text (V.Vector a)
  toCommands :: V.Vector a -> [Command]

-- | A valid `R.Feature` of points must contain a single `MoveTo` command
-- with a count greater than 0.
instance Geometry Point where
  fromCommands (MoveTo ps : []) = Right . U.convert $ evalState (U.mapM expand ps) (0,0)
  fromCommands (c:_) = Left $ [st|Invalid command found in Point feature: %s|] (show c)
  fromCommands [] = Left "No points given!"

  -- | A multipoint geometry must reduce to a single `MoveTo` command.
  toCommands ps = [MoveTo $ evalState (U.mapM collapse $ U.convert ps) (0,0)]

-- Need a generalized parser for this, `pipes-parser` might work.
-- | A valid `R.Feature` of linestrings must contain pairs of:
--
-- A `MoveTo` with a count of 1, followed by one `LineTo` command with
-- a count greater than 0.
instance Geometry LineString where
  fromCommands cs = evalState (f cs) (0,0)
    where f (MoveTo p : LineTo ps : rs) = fmap . V.cons <$> ls <*> f rs
            where ls = LineString <$> U.mapM expand (p <> ps)
          f [] = pure $ Right V.empty
          f _  = pure $ Left "LineString decode: Invalid command sequence given."

  toCommands ls = concat $ evalState (mapM f ls) (0,0)
    where f (LineString ps) = do
            curr <- get
            let (h,t) = (U.head ps, U.tail ps)
            put h
            l <- U.mapM collapse t
            pure [MoveTo $ U.singleton (x h - x curr, y h - y curr), LineTo l]

-- Need a generalized parser for this.
-- | A valid `R.Feature` of polygons must contain at least one sequence of:
--
-- An Exterior Ring, followed by 0 or more Interior Rings.
--
-- Any Ring must have a `MoveTo` with a count of 1, a single `LineTo`
-- with a count of at least 2, and a single `ClosePath` command.
--
-- Performs no sanity checks for malformed Interior Rings.
instance Geometry Polygon where
  fromCommands cs = do
    ps <- evalState (f cs) (0,0)
    let (h,t) = (V.head ps, V.tail ps)
        (ps',p') = runState (foldlM g V.empty t) h
    pure $ V.snoc ps' p'  -- Include the last Exterior Ring worked on.
    where f (MoveTo p : LineTo ps : ClosePath : rs) = do
            curr <- get
            let h = U.head p
                here = (x h + x curr, y h + y curr)
            po <- flip U.snoc here <$> U.mapM expand (U.cons h ps)
            fmap (V.cons (Polygon po V.empty)) <$> f rs
          f [] = pure $ Right V.empty
          f _  = pure . Left $ [st|Polygon decode: Invalid command sequence given: %s|] (show cs)
          g acc p | area p > 0 = do  -- New external rings.
                      curr <- get
                      put p
                      pure $ V.snoc acc curr
                  | otherwise = do  -- Next internal ring.
                      modify (\s -> s { inner = V.snoc (inner s) p })
                      pure acc

  toCommands ps = concat $ evalState (mapM f ps) (0,0)
    where f (Polygon p i) = do
            curr <- get
            let (h,t) = (U.head p, U.tail $ U.init p)  -- Exclude the final point.
            put h
            l <- U.mapM collapse t
            let cs = [MoveTo $ U.singleton (x h - x curr, y h - y curr), LineTo l, ClosePath]
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
parseCmd :: Word32 -> Either T.Text (Int,Int)
parseCmd n = case (cmd,count) of
  (1,m) -> Right $ both fromIntegral (1,m)
  (2,m) -> Right $ both fromIntegral (2,m)
  (7,1) -> Right (7,1)
  (7,m) -> Left $ "ClosePath was given a parameter count: " <> T.pack (show m)
  (m,_) -> Left $ [st|Invalid command integer %d found in: %X|] m n
  where cmd = n .&. 7
        count = shift n (-3)

-- | Recombine a Command ID and parameter count into a Command Integer.
unparseCmd :: (Int,Int) -> Word32
unparseCmd (cmd,count) = fromIntegral $ (cmd .&. 7) .|. shift count 3

-- | Attempt to parse a list of Command/Parameter integers, as defined here:
--
-- https://github.com/mapbox/vector-tile-spec/tree/master/2.1#43-geometry-encoding
commands :: [Word32] -> Either T.Text [Command]
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

{- UTIL -}

pairs :: [a] -> Either T.Text [(a,a)]
pairs [] = Right []
pairs [_] = Left "Uneven number of parameters given."
pairs (x:y:zs) = ((x,y) :) <$>  pairs zs

-- | Apply a pure function to both elements of a tuple.
both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

-- | Transform a `V.Vector` of `Point`s into one of Z-encoded Parameter ints.
params :: U.Vector (Int,Int) -> U.Vector Word32
params = U.foldr (\(a,b) acc -> U.cons (zig a) $ U.cons (zig b) acc) U.empty

-- | Expand a pair of diffs from some reference point into that
-- of a `Point` value. The reference point is moved to our new `Point`.
expand :: (Int,Int) -> State (Int,Int) Point
expand p = do
  curr <- get
  let here = (x p + x curr, y p + y curr)
  put here
  pure here

-- | Collapse a given `Point` into a pair of diffs, relative to
-- the previous point in the sequence. The reference point is moved
-- to the `Point` given.
collapse :: Point -> State (Int,Int) (Int,Int)
collapse p = do
  curr <- get
  let diff = (x p - x curr, y p - y curr)
  put p
  pure diff
