{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature (Feature(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.GeomType as Vector_tile.Tile (GeomType)

data Feature = Feature{id :: !(P'.Maybe P'.Word64), tags :: !(P'.Seq P'.Word32), type' :: !(P'.Maybe Vector_tile.Tile.GeomType),
                       geometry :: !(P'.Seq P'.Word32)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Feature where
  mergeAppend (Feature x'1 x'2 x'3 x'4) (Feature y'1 y'2 y'3 y'4)
   = Feature (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default Feature where
  defaultValue = Feature (Prelude'.Just 0) P'.defaultValue (Prelude'.Just (Prelude'.read "UNKNOWN")) P'.defaultValue

instance P'.Wire Feature where
  wireSize ft' self'@(Feature x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 4 x'1 + P'.wireSizePacked 1 13 x'2 + P'.wireSizeOpt 1 14 x'3 + P'.wireSizePacked 1 13 x'4)
  wirePut ft' self'@(Feature x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 4 x'1
             P'.wirePutPacked 18 13 x'2
             P'.wirePutOpt 24 14 x'3
             P'.wirePutPacked 34 13 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = Prelude'.Just new'Field}) (P'.wireGet 4)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{tags = P'.append (tags old'Self) new'Field}) (P'.wireGet 13)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{tags = P'.mergeAppend (tags old'Self) new'Field}) (P'.wireGetPacked 13)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = Prelude'.Just new'Field}) (P'.wireGet 14)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{geometry = P'.append (geometry old'Self) new'Field}) (P'.wireGet 13)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{geometry = P'.mergeAppend (geometry old'Self) new'Field})
                    (P'.wireGetPacked 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Feature) Feature where
  getVal m' f' = f' m'

instance P'.GPB Feature

instance P'.ReflectDescriptor Feature where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 18, 24, 32, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".vector_tile.Tile.Feature\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Feature\"}, descFilePath = [\"Geography\",\"VectorTile\",\"Protobuf\",\"Internal\",\"Vector_tile\",\"Tile\",\"Feature.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Feature.id\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Feature\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Just \"0\", hsDefault = Just (HsDef'Integer 0)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Feature.tags\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Feature\"], baseName' = FName \"tags\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Feature.type\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Feature\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".vector_tile.Tile.GeomType\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"GeomType\"}), hsRawDefault = Just \"UNKNOWN\", hsDefault = Just (HsDef'Enum \"UNKNOWN\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Feature.geometry\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Feature\"], baseName' = FName \"geometry\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Just (WireTag {getWireTag = 32},WireTag {getWireTag = 34}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Feature where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Feature where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "tags" (tags msg)
       P'.tellT "type" (type' msg)
       P'.tellT "geometry" (geometry msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'tags, parse'type', parse'geometry]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'id
         = P'.try
            (do
               v <- P'.getT "id"
               Prelude'.return (\ o -> o{id = v}))
        parse'tags
         = P'.try
            (do
               v <- P'.getT "tags"
               Prelude'.return (\ o -> o{tags = P'.append (tags o) v}))
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'geometry
         = P'.try
            (do
               v <- P'.getT "geometry"
               Prelude'.return (\ o -> o{geometry = P'.append (geometry o) v}))