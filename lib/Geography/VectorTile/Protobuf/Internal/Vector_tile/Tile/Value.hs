{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Value (Value(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Value = Value{string_value :: !(P'.Maybe P'.Utf8), float_value :: !(P'.Maybe P'.Float), double_value :: !(P'.Maybe P'.Double),
                   int_value :: !(P'.Maybe P'.Int64), uint_value :: !(P'.Maybe P'.Word64), sint_value :: !(P'.Maybe P'.Int64),
                   bool_value :: !(P'.Maybe P'.Bool), ext'field :: !(P'.ExtField)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage Value where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.Mergeable Value where
  mergeAppend (Value x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (Value y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
   = Value (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)

instance P'.Default Value where
  defaultValue
   = Value P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue

instance P'.Wire Value where
  wireSize ft' self'@(Value x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 2 x'2 + P'.wireSizeOpt 1 1 x'3 + P'.wireSizeOpt 1 3 x'4 +
             P'.wireSizeOpt 1 4 x'5
             + P'.wireSizeOpt 1 18 x'6
             + P'.wireSizeOpt 1 8 x'7
             + P'.wireSizeExtField x'8)
  wirePut ft' self'@(Value x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
             P'.wirePutOpt 21 2 x'2
             P'.wirePutOpt 25 1 x'3
             P'.wirePutOpt 32 3 x'4
             P'.wirePutOpt 40 4 x'5
             P'.wirePutOpt 48 18 x'6
             P'.wirePutOpt 56 8 x'7
             P'.wirePutExtField x'8
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{string_value = Prelude'.Just new'Field}) (P'.wireGet 9)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{float_value = Prelude'.Just new'Field}) (P'.wireGet 2)
             25 -> Prelude'.fmap (\ !new'Field -> old'Self{double_value = Prelude'.Just new'Field}) (P'.wireGet 1)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{int_value = Prelude'.Just new'Field}) (P'.wireGet 3)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{uint_value = Prelude'.Just new'Field}) (P'.wireGet 4)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{sint_value = Prelude'.Just new'Field}) (P'.wireGet 18)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{bool_value = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [8 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Value) Value where
  getVal m' f' = f' m'

instance P'.GPB Value

instance P'.ReflectDescriptor Value where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 21, 25, 32, 40, 48, 56])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".vector_tile.Tile.Value\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Value\"}, descFilePath = [\"Geography\",\"VectorTile\",\"Protobuf\",\"Internal\",\"Vector_tile\",\"Tile\",\"Value.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.string_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"string_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.float_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"float_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.double_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"double_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.int_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"int_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.uint_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"uint_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.sint_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"sint_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Value.bool_value\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Value\"], baseName' = FName \"bool_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 8},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Value where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Value where
  textPut msg
   = do
       P'.tellT "string_value" (string_value msg)
       P'.tellT "float_value" (float_value msg)
       P'.tellT "double_value" (double_value msg)
       P'.tellT "int_value" (int_value msg)
       P'.tellT "uint_value" (uint_value msg)
       P'.tellT "sint_value" (sint_value msg)
       P'.tellT "bool_value" (bool_value msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'string_value, parse'float_value, parse'double_value, parse'int_value, parse'uint_value, parse'sint_value,
                   parse'bool_value])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'string_value
         = P'.try
            (do
               v <- P'.getT "string_value"
               Prelude'.return (\ o -> o{string_value = v}))
        parse'float_value
         = P'.try
            (do
               v <- P'.getT "float_value"
               Prelude'.return (\ o -> o{float_value = v}))
        parse'double_value
         = P'.try
            (do
               v <- P'.getT "double_value"
               Prelude'.return (\ o -> o{double_value = v}))
        parse'int_value
         = P'.try
            (do
               v <- P'.getT "int_value"
               Prelude'.return (\ o -> o{int_value = v}))
        parse'uint_value
         = P'.try
            (do
               v <- P'.getT "uint_value"
               Prelude'.return (\ o -> o{uint_value = v}))
        parse'sint_value
         = P'.try
            (do
               v <- P'.getT "sint_value"
               Prelude'.return (\ o -> o{sint_value = v}))
        parse'bool_value
         = P'.try
            (do
               v <- P'.getT "bool_value"
               Prelude'.return (\ o -> o{bool_value = v}))