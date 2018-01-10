{ mkDerivation, base, bytestring, cereal, containers, criterion
, deepseq, hex, microlens, microlens-platform, protobuf, stdenv
, tasty, tasty-hunit, text, transformers, vector
}:
mkDerivation {
  pname = "vectortiles";
  version = "1.2.0.7";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq protobuf text
    transformers vector
  ];
  testHaskellDepends = [
    base bytestring cereal containers hex protobuf tasty tasty-hunit
    text vector
  ];
  benchmarkHaskellDepends = [
    base bytestring cereal containers criterion microlens
    microlens-platform protobuf text vector
  ];
  homepage = "https://github.com/fosskers/vectortiles";
  description = "GIS Vector Tiles, as defined by Mapbox";
  license = stdenv.lib.licenses.asl20;
}
