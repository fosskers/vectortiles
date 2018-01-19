{ mkDerivation, base, bytestring, containers, criterion, deepseq
, hashable, hex, microlens, microlens-platform, parallel
, protocol-buffers, protocol-buffers-descriptor, stdenv, tasty
, tasty-hunit, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "vectortiles";
  version = "1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers deepseq hashable parallel
    protocol-buffers protocol-buffers-descriptor text transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring containers hashable hex protocol-buffers
    protocol-buffers-descriptor tasty tasty-hunit text
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion hashable microlens
    microlens-platform protocol-buffers protocol-buffers-descriptor
    text unordered-containers vector
  ];
  homepage = "https://github.com/fosskers/vectortiles";
  description = "GIS Vector Tiles, as defined by Mapbox";
  license = stdenv.lib.licenses.bsd3;
}
