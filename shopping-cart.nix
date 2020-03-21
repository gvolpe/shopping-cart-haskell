{ mkDerivation, aeson, async, base, bytestring, co-log-core
, containers, dhall, exceptions, hedgehog, hedis, lens
, postgresql-simple, raw-strings-qq, refined, servant
, servant-server, stdenv, template-haskell, text, uuid, wai
, wai-cors, warp, wreq
}:
mkDerivation {
  pname = "shopping-cart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring co-log-core containers dhall exceptions
    hedis lens postgresql-simple raw-strings-qq refined servant
    servant-server template-haskell text uuid wai wai-cors warp wreq
  ];
  executableHaskellDepends = [ base refined template-haskell uuid ];
  testHaskellDepends = [ base hedgehog refined template-haskell ];
  description = "The Shopping Cart developed in PFP Scala for Haskell";
  license = stdenv.lib.licenses.asl20;
}
