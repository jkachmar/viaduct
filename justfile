package := "viaduct"
main := "app"
ghc_opts := "-j"

cabal_dir := "cabal"
build_dir := cabal_dir + "/build"
repl_dir := cabal_dir + "/repl"
test_dir := cabal_dir + "/test"

build:
  cabal build {{package}} \
    --builddir '{{build_dir}}' \
    --ghc-options {{ghc_opts}}

test suite="unit":
  cabal test {{package}}:test:{{suite}} \
    --builddir '{{build_dir}}' \
    --ghc-options {{ghc_opts}}

repl target=package:
  cabal repl {{target}} \
    --builddir '{{repl_dir}}' \
    --ghc-options '{{ghc_opts}}' \
    --repl-options '-fobject-code'

ghcid target=package:
  ghcid \
    --restart 'viaduct.cabal' \
    --command \
      "cabal repl {{target}} \
         --builddir '{{repl_dir}}' \
         --ghc-options '{{ghc_opts}}' \
         --repl-options '-fobject-code'"

ghcid-test suite="unit":
  ghcid \
    --restart 'viaduct.cabal' \
    --test "main" \
    --command \
      "cabal repl {{package}}:test:{{suite}} \
         --builddir '{{repl_dir}}' \
         --ghc-options '{{ghc_opts}}' \
         --repl-options '-fobject-code'"
