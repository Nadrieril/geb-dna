{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  compiler = "ghc844";
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    lens mtl
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "geb-dna";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
