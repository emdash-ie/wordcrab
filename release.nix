let
  pkgs = import <nixpkgs> { };

in
  { wordcrab = pkgs.haskellPackages.callPackage ./wordcrab.nix { };
  }
