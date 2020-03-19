{ mkDerivation, aeson, async, base, bytestring, containers, dhall
, exceptions, numhask, postgresql-simple, refined, stdenv, text
, uuid, wreq
}:
mkDerivation {
  pname = "shopping-cart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers dhall exceptions numhask
    postgresql-simple refined text uuid wreq
  ];
  executableHaskellDepends = [ base postgresql-simple ];
  description = "The Shopping Cart developed in PFP Scala for Haskell";
  license = stdenv.lib.licenses.asl20;
}
