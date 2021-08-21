{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs.lib.attrsets) optionalAttrs;
  inherit (pkgs.lib.strings) makeLibraryPath;
  inherit (pkgs.stdenv) isLinux;

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
    zlib
  ];
in

pkgs.mkShell ({
  buildInputs = devTools ++ haskellDeps ++ systemDeps;
} // optionalAttrs isLinux {
  # Ensure that all 'systemDeps' are installed appropriately on Linux. 
  LD_LIBRARY_PATH = makeLibraryPath systemDeps;
})
