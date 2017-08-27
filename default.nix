{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, lens, lens-aeson, megaparsec, mtl, network
, network-simple, parallel, parallel-io, parsers, pipes
, pipes-network, pipes-safe, reflex, reflex-dom, stdenv, text
, transformers, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "hash";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async attoparsec base bytestring containers lens lens-aeson
    megaparsec mtl network network-simple parallel parallel-io parsers
    pipes pipes-network pipes-safe reflex reflex-dom text transformers
    trifecta unordered-containers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
