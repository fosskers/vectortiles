{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType (GeomType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data GeomType = UNKNOWN
              | POINT
              | LINESTRING
              | POLYGON
                deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                          Prelude'.Generic)

instance P'.Mergeable GeomType

instance Prelude'.Bounded GeomType where
  minBound = UNKNOWN
  maxBound = POLYGON

instance P'.Default GeomType where
  defaultValue = UNKNOWN

toMaybe'Enum :: Prelude'.Int -> P'.Maybe GeomType
toMaybe'Enum 0 = Prelude'.Just UNKNOWN
toMaybe'Enum 1 = Prelude'.Just POINT
toMaybe'Enum 2 = Prelude'.Just LINESTRING
toMaybe'Enum 3 = Prelude'.Just POLYGON
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum GeomType where
  fromEnum UNKNOWN = 0
  fromEnum POINT = 1
  fromEnum LINESTRING = 2
  fromEnum POLYGON = 3
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType")
      . toMaybe'Enum
  succ UNKNOWN = POINT
  succ POINT = LINESTRING
  succ LINESTRING = POLYGON
  succ _
   = Prelude'.error "hprotoc generated code: succ failure for type Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType"
  pred POINT = UNKNOWN
  pred LINESTRING = POINT
  pred POLYGON = LINESTRING
  pred _
   = Prelude'.error "hprotoc generated code: pred failure for type Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType"

instance P'.Wire GeomType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB GeomType

instance P'.MessageAPI msg' (msg' -> GeomType) GeomType where
  getVal m' f' = f' m'

instance P'.ReflectEnum GeomType where
  reflectEnum = [(0, "UNKNOWN", UNKNOWN), (1, "POINT", POINT), (2, "LINESTRING", LINESTRING), (3, "POLYGON", POLYGON)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".vector_tile.Tile.GeomType") ["Geography", "VectorTile", "Protobuf", "Internal"] ["Vector_tile", "Tile"]
        "GeomType")
      ["Geography", "VectorTile", "Protobuf", "Internal", "Vector_tile", "Tile", "GeomType.hs"]
      [(0, "UNKNOWN"), (1, "POINT"), (2, "LINESTRING"), (3, "POLYGON")]
      Prelude'.False

instance P'.TextType GeomType where
  tellT = P'.tellShow
  getT = P'.getRead