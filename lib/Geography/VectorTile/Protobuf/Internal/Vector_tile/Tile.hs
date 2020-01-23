{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile (Tile(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Geography.VectorTile.Protobuf.Internal.Vector_tile.Tile.Layer as Vector_tile.Tile (Layer)

data Tile = Tile{layers :: !(P'.Seq Vector_tile.Tile.Layer), ext'field :: !(P'.ExtField)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage Tile where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.Mergeable Tile where
  mergeAppend (Tile x'1 x'2) (Tile y'1 y'2) = Tile (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Tile where
  defaultValue = Tile P'.defaultValue P'.defaultValue

instance P'.Wire Tile where
  wireSize ft' self'@(Tile x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeExtField x'2)
  wirePutWithSize ft' self'@(Tile x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutRepWithSize 26 11 x'1, P'.wirePutExtFieldWithSize x'2]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown' P'.discardUnknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown' P'.discardUnknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{layers = P'.append (layers old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [16 <= field'Number && field'Number <= 8191] then P'.loadExtension field'Number wire'Type old'Self
                    else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Tile) Tile where
  getVal m' f' = f' m'

instance P'.GPB Tile

instance P'.ReflectDescriptor Tile where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".vector_tile.Tile\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\"], baseName = MName \"Tile\"}, descFilePath = [\"Geography\",\"VectorTile\",\"Protobuf\",\"Internal\",\"Vector_tile\",\"Tile.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".vector_tile.Tile.layers\", haskellPrefix' = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule' = [MName \"Vector_tile\",MName \"Tile\"], baseName' = FName \"layers\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".vector_tile.Tile.Layer\", haskellPrefix = [MName \"Geography\",MName \"VectorTile\",MName \"Protobuf\",MName \"Internal\"], parentModule = [MName \"Vector_tile\",MName \"Tile\"], baseName = MName \"Layer\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 16},FieldId {getFieldId = 8191})], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Tile where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Tile where
  textPut msg
   = do
       P'.tellT "layers" (layers msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'layers]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'layers
         = P'.try
            (do
               v <- P'.getT "layers"
               Prelude'.return (\ o -> o{layers = P'.append (layers o) v}))