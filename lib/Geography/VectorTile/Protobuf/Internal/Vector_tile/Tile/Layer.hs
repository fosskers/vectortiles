{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer (Layer(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Feature as Vector_tile.Tile (Feature)
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Value as Vector_tile.Tile (Value)

data Layer = Layer{version :: !(P'.Word32), name :: !(P'.Utf8), features :: !(P'.Seq Vector_tile.Tile.Feature),
                   keys :: !(P'.Seq P'.Utf8), values :: !(P'.Seq Vector_tile.Tile.Value), extent :: !(P'.Maybe P'.Word32),
                   ext'field :: !(P'.ExtField)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage Layer where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.Mergeable Layer where
  mergeAppend (Layer x'1 x'2 x'3 x'4 x'5 x'6 x'7) (Layer y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = Layer (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)

instance P'.Default Layer where
  defaultValue = Layer 1 P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue (Prelude'.Just 4096) P'.defaultValue

instance P'.Wire Layer where
  wireSize ft' self'@(Layer x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 13 x'1 + P'.wireSizeReq 1 9 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 9 x'4 +
             P'.wireSizeRep 1 11 x'5
             + P'.wireSizeOpt 1 13 x'6
             + P'.wireSizeExtField x'7)
  wirePut ft' self'@(Layer x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'2
             P'.wirePutRep 18 11 x'3
             P'.wirePutRep 26 9 x'4
             P'.wirePutRep 34 11 x'5
             P'.wirePutOpt 40 13 x'6
             P'.wirePutReq 120 13 x'1
             P'.wirePutExtField x'7
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             120 -> Prelude'.fmap (\ !new'Field -> old'Self{version = new'Field}) (P'.wireGet 13)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{name = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{features = P'.append (features old'Self) new'Field}) (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{keys = P'.append (keys old'Self) new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{values = P'.append (values old'Self) new'Field}) (P'.wireGet 11)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{extent = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [16 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Layer) Layer where
  getVal m' f' = f' m'

instance P'.GPB Layer

instance P'.ReflectDescriptor Layer where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 120]) (P'.fromDistinctAscList [10, 18, 26, 34, 40, 120])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".vector_tile.Tile.Layer\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Layer\"}, descFilePath = [\"Geography\",\"VectorTile\",\"Protobuf\",\"Internal\",\"Vector_tile\",\"Tile\",\"Layer.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.version\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"version\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 120}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just \"1\", hsDefault = Just (HsDef'Integer 1)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.name\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.features\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"features\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".vector_tile.Tile.Feature\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Feature\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.keys\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"keys\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.values\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"values\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".vector_tile.Tile.Value\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Value\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.Layer.extent\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\",MName \"Layer\"], baseName' = FName \"extent\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just \"4096\", hsDefault = Just (HsDef'Integer 4096)}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 16},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Layer where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Layer where
  textPut msg
   = do
       P'.tellT "version" (version msg)
       P'.tellT "name" (name msg)
       P'.tellT "features" (features msg)
       P'.tellT "keys" (keys msg)
       P'.tellT "values" (values msg)
       P'.tellT "extent" (extent msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'version, parse'name, parse'features, parse'keys, parse'values, parse'extent]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'version
         = P'.try
            (do
               v <- P'.getT "version"
               Prelude'.return (\ o -> o{version = v}))
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'features
         = P'.try
            (do
               v <- P'.getT "features"
               Prelude'.return (\ o -> o{features = P'.append (features o) v}))
        parse'keys
         = P'.try
            (do
               v <- P'.getT "keys"
               Prelude'.return (\ o -> o{keys = P'.append (keys o) v}))
        parse'values
         = P'.try
            (do
               v <- P'.getT "values"
               Prelude'.return (\ o -> o{values = P'.append (values o) v}))
        parse'extent
         = P'.try
            (do
               v <- P'.getT "extent"
               Prelude'.return (\ o -> o{extent = v}))