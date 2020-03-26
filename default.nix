{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/1fe82110feb.tar.gz") {}, compiler ? "ghc865" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./shopping-cart.nix { }
