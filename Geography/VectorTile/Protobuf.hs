{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module    : Geography.VectorTile.Protobuf
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- Most of the details of Protobuf conversion are kept in
-- "Geometry.VectorTile.Protobuf.Internal", a module which is not intended
-- to be imported.
--
-- A user's main concern here should be the `Protobuffable` class, and its
-- `VectorTile` instance. With it, one can do the following:
--
-- > import Geography.VectorTile.Protobuf
-- >
-- > decode bytes >>= fromProtobuf  -- Either Text VectorTile
--
-- which in fact is sugared in the top-level module of this library as:
--
-- > decode bytes >>= tile

module Geography.VectorTile.Protobuf
  ( -- * Types
    Protobuffable(..)
    -- * ByteString Encoding / Decoding
  , decode
  , encode
  , decodeIO
  , encodeIO
  ) where

import qualified Data.ByteString as BS
import           Data.ProtocolBuffers hiding (decode, encode)
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Text (Text, pack)
import           Geography.VectorTile.Protobuf.Internal

---

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
