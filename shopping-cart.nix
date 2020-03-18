{ mkDerivation, aeson, async, base, bytestring, containers, dhall
, exceptions, refined, stdenv, text, wreq
}:
mkDerivation {
  pname = "shopping-cart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers dhall exceptions refined
    text wreq
  ];
  executableHaskellDepends = [ base ];
  description = "The Shopping Cart developed in PFP Scala for Haskell";
  license = stdenv.lib.licenses.asl20;
}
