#! /usr/bin/env sh

################################################################################
# Pre-flight checks.
################################################################################

set -eu

command -v git >/dev/null 2>&1 || { echo >&2 "git is not installed, aborting."; exit 1; }

cd "$( git rev-parse --show-toplevel )"

command -v grep >/dev/null 2>&1 || { echo >&2 "grep is not installed, aborting."; exit 1; }
command -v awk  >/dev/null 2>&1 || { echo >&2 "awk is not installed, aborting."; exit 1; }
command -v sed  >/dev/null 2>&1 || { echo >&2 "sed is not installed, aborting."; exit 1; }

ORMOLU_VERSION="0.1.4.1"
( ormolu -v 2>/dev/null | grep -q "$ORMOLU_VERSION" ) || ( echo "please install ormolu $ORMOLU_VERSION (eg., run 'cabal install ormolu' and ensure ormolu is on your PATH.)"; exit 1 )
echo "ormolu version: $ORMOLU_VERSION"

################################################################################
# Argument parsing.
#
# cf. https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
################################################################################

ARG_ORMOLU_BRANCH=0
ARG_ORMOLU_MODE="inplace"

USAGE="
This shell script can either:

  (a) apply ormolu formatting in-place to all haskell modules in your working
  copy; this is mostly for migrating from manually-formatted projects to
  ormolu-formatted ones

  (b) check all modules for formatting and fail if ormolu needs to be applied;
  this is mostly integed to be run by a continuous integration service to make
  sure no branches with non-ormolu formatting get merged

For every-day dev work, consider using one of the ormolu editor integrations
(see https://github.com/tweag/ormolu#editor-integration).

USAGE: $0
    -h        show this help text
    -b BRANCH only run against files changed with respect some target branch
    -c        set ormolu mode to 'check'
              default: ${ARG_ORMOLU_MODE}
"

while getopts "b:ch" opt; do
  case ${opt} in
    c ) ARG_ORMOLU_MODE="check"
      ;;
    b ) ARG_ORMOLU_BRANCH=1
        ARG_ORMOLU_BRANCH_TARGET="$OPTARG"
      ;;
    h ) echo "$USAGE" 1>&2
        exit 0
      ;;
    # Respond to invalid flags with the help message.
    \? ) echo "$USAGE" 1>&2
         exit 0
      ;;
  esac
done
shift $((OPTIND -1))

if [ "$#" -ne 0 ]; then
  echo "$USAGE" 1>&2
  exit 1
fi

################################################################################
# Execution.
################################################################################

# NOTE: This must be kept in-line with the project's 'default-extensions'.
#
# This behavior can be changed once we've upgraded to 'ormolu-0.2.0.0', which
# supports parsing a project's 'default-extensions' from the cabal file.
LANGUAGE_EXTS="\
--ghc-opt -XDerivingStrategies \
--ghc-opt -XDerivingVia \
--ghc-opt -XGeneralizedNewtypeDeriving \
--ghc-opt -XImportQualifiedPost \
--ghc-opt -XNamedFieldPuns \
--ghc-opt -XNoImplicitPrelude \
--ghc-opt -XOverloadedStrings \
--ghc-opt -XScopedTypeVariables \
--ghc-opt -XStrictData \
"

echo "ormolu mode: $ARG_ORMOLU_MODE"
echo "language extensions: $LANGUAGE_EXTS"

FAILURES=0

if [ $ARG_ORMOLU_BRANCH = 1 ]; then
  hsfiles=$(git diff --name-only "$ARG_ORMOLU_BRANCH_TARGET" | grep '\.hs$')
else
  hsfiles=$(git ls-files '*.hs')
fi

for hsfile in $hsfiles; do
    FAILED=0
    # Intended splitting of 'LANGUAGE_EXTS'.
    # shellcheck disable=SC2086
    ormolu \
      --mode $ARG_ORMOLU_MODE \
      --check-idempotence $LANGUAGE_EXTS \
      "$hsfile" \
    || FAILED=1

    if [ "$FAILED" = "1" ]; then
        FAILURES=$((FAILURES+1))
        echo "$hsfile...  *** FAILED"
    else
        echo "$hsfile...  ok"
    fi
done

if [ "$FAILURES" != 0 ]; then
    echo "ormolu failed on $FAILURES files."
    if [ "$ARG_ORMOLU_MODE" = "check" ]; then
        printf "\n\nyou can fix this by running 'just format' from the git repo root.\n\n"
    fi
    exit 1
fi
