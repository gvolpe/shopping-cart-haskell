let
  inherit (import nix/pkgs.nix {}) pkgs hp;

  drv = hp.callCabal2nix "shopping-cart" ./. {};
in
{
  my_project = drv;
  shell = hp.shellFor {
    withHoogle = true;
    packages = p: [ drv ];
    buildInputs = with hp; [
      brittany
      cabal-install
      haskell-language-server
      hlint
    ];
    shellHook = ''
      export NIX_GHC="$(which ghc)"
      export NIX_GHCPKG="$(which ghc-pkg)"
      export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
      export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
    '';
  };
}.shell
