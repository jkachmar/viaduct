package := "viaduct"
main := "app"
ghc_opts := "-j"
repl_opts := '-O0 -fobject-code'

cabal_dir := "cabal"
build_dir := cabal_dir + "/build"
repl_dir := cabal_dir + "/repl"
test_dir := cabal_dir + "/test"

build target=package:
  cabal build {{target}} \
    --builddir '{{build_dir}}' \
    --ghc-options {{ghc_opts}}

_main-target := package + ":exe:" + main
build-main: (build _main-target)

exec:
  cabal exec \
    --builddir {{build_dir}} \
    --ghc-options {{ghc_opts}} \
    -- {{main}}

exec-jq:
  cabal exec \
    --builddir {{build_dir}} \
    --ghc-options {{ghc_opts}} \
    -- {{main}} | jq .

test suite="unit":
  cabal test {{package}}:test:{{suite}} \
    --builddir {{build_dir}} \
    --ghc-options {{ghc_opts}}

repl target=package:
  cabal repl {{target}} \
    --builddir {{repl_dir}} \
    --ghc-options {{ghc_opts}} \
    --repl-options {{repl_opts}}

ghcid target=package:
  ghcid \
    --restart 'viaduct.cabal' \
    --command \
      "cabal repl {{target}} \
         --builddir {{repl_dir}} \
         --ghc-options {{ghc_opts}} \
         --repl-options {{repl_opts}}"

ghcid-test suite="unit":
  ghcid \
    --restart 'viaduct.cabal' \
    --test "main" \
    --command \
      "cabal repl {{package}}:test:{{suite}} \
         --builddir {{repl_dir}} \
         --ghc-options {{ghc_opts}} \
         --repl-options {{repl_opts}}"

format *FLAGS:
  ./scripts/ormolu.sh {{FLAGS}}
  ./scripts/cabal-fmt.sh {{FLAGS}}
