{ mkDerivation, base, bytestring, cereal, containers, criterion
, deepseq, hex, microlens, microlens-platform, protocol-buffers
, protocol-buffers-descriptor, stdenv, tasty, tasty-hunit, text
, transformers, vector
}:
mkDerivation {
  pname = "vectortiles";
  version = "1.2.0.7";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq protocol-buffers
    protocol-buffers-descriptor text transformers vector
  ];
  testHaskellDepends = [
    base bytestring cereal containers hex protocol-buffers
    protocol-buffers-descriptor tasty tasty-hunit text vector
  ];
  benchmarkHaskellDepends = [
    base bytestring cereal containers criterion microlens
    microlens-platform protocol-buffers protocol-buffers-descriptor
    text vector
  ];
  homepage = "https://github.com/fosskers/vectortiles";
  description = "GIS Vector Tiles, as defined by Mapbox";
  license = stdenv.lib.licenses.asl20;
}
