{ compiler ? "ghc884" }:

let
  pkgs = import (
    builtins.fetchTarball {
      name   = "nixos-unstable-2020-08-15";
      url    = "https://github.com/NixOS/nixpkgs-channels/archive/96745f022835.tar.gz";
      sha256 = "1jfiaib3h6gmffwsg7d434di74x5v5pbwfifqw3l1mcisxijqm3s";
    }
  ) {};

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = newPkgs: oldPkgs: rec {
      # Override to use the `par-dual-1.0.0.0` package
      par-dual =
        pkgs.haskell.lib.dontCheck (
          newPkgs.callCabal2nix "par-dual" (
            builtins.fetchGit {
              url = "https://github.com/gvolpe/par-dual.git";
              rev = "49ad0c2102e061d38133540a2d6dcf75d4dac69c";
            }
          ) {}
        );
    };
  };
in
{
  pkgs = pkgs;
  hp = hp;
}
