let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/1fe82110feb.tar.gz") {};
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
