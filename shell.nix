{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    ghcid
    haskell.compiler.ghc8104
    just
    jq
    ormolu
    shellcheck
  ];
}
