{ mkDerivation, aeson, async, base, bytestring, containers, dhall
, exceptions, postgresql-simple, raw-strings-qq, refined, servant
, servant-server, stdenv, text, uuid, wai, wai-cors, warp, wreq
}:
mkDerivation {
  pname = "shopping-cart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers dhall exceptions
    postgresql-simple raw-strings-qq refined servant servant-server
    text uuid wai wai-cors warp wreq
  ];
  executableHaskellDepends = [ base postgresql-simple ];
  description = "The Shopping Cart developed in PFP Scala for Haskell";
  license = stdenv.lib.licenses.asl20;
}
