{ pkgs ? (import <nixpkgs> {}).pkgs }:

let

  env = pkgs.haskellngPackages.ghcWithPackages (p: with p; [
    text mtl transformers warp cabal-install
  ]);

in

pkgs.stdenv.mkDerivation {
  name = "heyefi";
  buildInputs = [ env ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
