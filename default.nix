{ packages ? import nix/pkgs.nix { inherit compiler; }, compiler ? "ghc884" }:

packages.hp.callCabal2nix "shopping-cart" ./. {}
