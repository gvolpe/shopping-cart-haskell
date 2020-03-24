{ mkDerivation, aeson, async, base, bytestring, co-log-core
, containers, dhall, exceptions, hedgehog, hedis, lens
, postgresql-simple, raw-strings-qq, refined, retry, servant
, servant-server, stdenv, template-haskell, text, unliftio
, utf8-string, uuid, wai, wai-cors, warp, witherable, wreq
}:
mkDerivation {
  pname = "shopping-cart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring co-log-core containers dhall exceptions
    hedis lens postgresql-simple raw-strings-qq refined retry servant
    servant-server template-haskell text unliftio utf8-string uuid wai
    wai-cors warp witherable wreq
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hedgehog refined template-haskell text uuid
  ];
  description = "The Shopping Cart developed in PFP Scala for Haskell";
  license = stdenv.lib.licenses.asl20;
}
