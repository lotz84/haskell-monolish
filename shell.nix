{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

with pkgs;
let
  monolish = callPackage ./nix/monolish.nix { mkDerivation = stdenv.mkDerivation; };
in
pkgs.mkShell {
  buildInputs = [
    ghc
    stack
    ormolu
    haskell-language-server
    haskellPackages.implicit-hie

    monolish
  ];

  shellHook = ''
    export NIX_GHC="${ghc}/bin/ghc"
    export NIX_GHCPKG="${ghc}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghc}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR="${ghc}/lib/ghc-$($NIX_GHC --numeric-version)"
    export MONOLISH_DIR="${monolish}"
  '';
}

