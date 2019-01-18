{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  compiler = "ghc863";
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    lens mtl
    tasty tasty-hunit tasty-smallcheck
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "geb-dna";
  buildInputs = [ ghc pkgs.haskellPackages.ghcid ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
