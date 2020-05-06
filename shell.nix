let
  # Override to use the `par-dual-1.0.0.0` package
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          par-dual =
            haskellPackagesNew.callCabal2nix "par-dual" (builtins.fetchGit {
              url = "https://github.com/gvolpe/par-dual.git";
              rev = "49ad0c2102e061d38133540a2d6dcf75d4dac69c";
            }) {};
          };
        };
      };
    };

  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/10100a97c89.tar.gz") { inherit config; };

  inherit (pkgs) haskellPackages;
  drv = haskellPackages.callCabal2nix "shopping-cart" ./. {};

in
  {
    my_project = drv;
    shell = haskellPackages.shellFor {
      withHoogle = true;
      packages = p: [drv];
      buildInputs = with haskellPackages; [ hlint cabal-install ];
      shellHook = ''
        export NIX_GHC="$(which ghc)"
        export NIX_GHCPKG="$(which ghc-pkg)"
        export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
      '';
    };
  }.shell
