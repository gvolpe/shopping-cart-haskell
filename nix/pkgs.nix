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
      par-dual =
        pkgs.haskell.lib.dontCheck (
          newPkgs.callCabal2nix "par-dual" (
            builtins.fetchGit {
              url = "https://github.com/gvolpe/par-dual.git";
              rev = "49ad0c2102e061d38133540a2d6dcf75d4dac69c";
            }
          ) {}
        );

      postgresql-resilient =
        pkgs.haskell.lib.dontCheck (
          newPkgs.callCabal2nix "postgresql-resilient" (
            builtins.fetchGit {
              url = "https://github.com/gvolpe/postgresql-resilient.git";
              rev = "851df8dd6220656948f8e0f665d499371146854d";
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
