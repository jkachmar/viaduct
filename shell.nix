{ pkgs ? import <nixpkgs> { } }:

let
  # General-purpose tools, formatters, etc.
  devTools = with pkgs; [
    just
    jq
    nixpkgs-fmt
    shellcheck
  ];

  # Haskell-specific tools, formatters, etc.
  haskellDeps = (with pkgs; [
    cabal-install
    ghcid
    haskell.compiler.ghc8104
    ormolu
  ]) ++ (with pkgs.haskellPackages; [
    cabal-fmt
  ]);

  # System-level dependencies that some libraries may require (e.g. zlib).
  systemDeps = with pkgs; [
    zlib.dev
  ];
in

pkgs.mkShell {
  buildInputs = devTools ++ haskellDeps ++ systemDeps;
}
