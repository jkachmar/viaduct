package := "viaduct"
main := "app"

# Number of jobs for 'cabal-install' to run in parallel when building.
#
# Defaults to the number of logical cores on the host machine.
jobs := ""

# GHC compilation options, passed
ghc_opts := ""
repl_opts := "-O0 -fobject-code"

cabal_dir := "cabal"
build_dir := cabal_dir + "/build"
repl_dir := cabal_dir + "/repl"
test_dir := cabal_dir + "/test"

build target=package:
  cabal build {{target}} \
    -j{{jobs}} \
    --builddir '{{build_dir}}' \
    --ghc-options '{{ghc_opts}}'

_main-target := package + ":exe:" + main
build-main: (build _main-target)

# Run the main application.
exec:
  cabal exec --builddir {{build_dir}} -- {{main}}

# Run the main application and pipe its output to 'jq'.
exec-jq:
  cabal exec --builddir {{build_dir}} -- {{main}} | jq .

test suite="unit":
  cabal test {{package}}:test:{{suite}} \
    -j{{jobs}} \
    --builddir {{build_dir}} \
    --ghc-options '{{ghc_opts}}'

repl target=package:
  cabal repl {{target}} \
    -j{{jobs}} \
    --builddir {{repl_dir}} \
    --ghc-options '{{ghc_opts}}' \
    --repl-options {{repl_opts}}

ghcid target=package:
  ghcid \
    --restart 'viaduct.cabal' \
    --command \
      "cabal repl {{target}} \
         -j{{jobs}} \
         --builddir {{repl_dir}} \
         --ghc-options '{{ghc_opts}}' \
         --repl-options {{repl_opts}}"

ghcid-test suite="unit":
  ghcid \
    --restart 'viaduct.cabal' \
    --test "main" \
    --command \
      "cabal repl {{package}}:test:{{suite}} \
         -j{{jobs}} \
         --builddir {{repl_dir}} \
         --ghc-options '{{ghc_opts}}' \
         --repl-options {{repl_opts}}"

# Format all Haskell and Cabal files tracked in git.
format *FLAGS:
  ./scripts/ormolu.sh {{FLAGS}}
  ./scripts/cabal-fmt.sh {{FLAGS}}
