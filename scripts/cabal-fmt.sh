#! /usr/bin/env sh

################################################################################
# Pre-flight checks.
################################################################################

set -eu

command -v git >/dev/null 2>&1 || { echo >&2 "git is not installed, aborting."; exit 1; }

cd "$( git rev-parse --show-toplevel )"

command -v cabal-fmt >/dev/null 2>&1 || { echo >&2 "cabal-fmt is not installed, aborting."; exit 1; }

CABAL_FMT_VERSION="0.1.5.1"
( cabal-fmt --version 2>/dev/null | grep -q "$CABAL_FMT_VERSION" ) || ( echo "please install cabal-fmt $CABAL_FMT_VERSION (eg., run 'cabal install cabal-fmt' and ensure cabal-fmt is on your PATH.)"; exit 1 )
echo "cabal-fmt version: $CABAL_FMT_VERSION"

################################################################################
# Argument parsing.
#
# cf. https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
################################################################################

ARG_CABAL_FMT_BRANCH=0
ARG_CABAL_FMT_MODE="--inplace"

USAGE="
This shell script can either:

  (a) apply cabal-fmt formatting in-place to all cabal files in your working
  copy; this is mostly for migrating from manually-formatted projects to
  cabal-fmt-formatted ones

  (b) check all modules for formatting and fail if cabal-fmt needs to be applied;
  this is mostly integed to be run by a continuous integration service to make
  sure no branches with non-cabal-fmt formatting get merged

USAGE: $0
    -h        show this help text
    -b BRANCH only run against files changed with respect some target branch
    -c        set cabal-fmt mode to 'check'
              default: ${ARG_CABAL_FMT_MODE}
"

while getopts "b:ch" opt; do
  case ${opt} in
    c ) ARG_CABAL_FMT_MODE="--check"
      ;;
    b ) ARG_CABAL_FMT_BRANCH=1
        ARG_CABAL_FMT_BRANCH_TARGET="$OPTARG"
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

################################################################################
# Execution.
################################################################################

FAILURES=0

if [ $ARG_CABAL_FMT_BRANCH = 1 ]; then
  cblfiles=$(git diff --name-only "$ARG_CABAL_FMT_BRANCH_TARGET" | grep '\.cabal$')
else
  cblfiles=$(git ls-files '*.cabal')
fi

for cblfile in $cblfiles; do
    FAILED=0
    # Intended splitting of 'LANGUAGE_EXTS'.
    # shellcheck disable=SC2086
    cabal-fmt \
      --indent 2 \
      $ARG_CABAL_FMT_MODE \
      "$cblfile" \
    || FAILED=1

    if [ "$FAILED" = "1" ]; then
        FAILURES=$((FAILURES+1))
        echo "$cblfile...  *** FAILED"
    else
        echo "$cblfile...  ok"
    fi
done

if [ "$FAILURES" != 0 ]; then
    echo "cabal-fmt failed on $FAILURES files."
    if [ "$ARG_CABAL_FMT_MODE" = "check" ]; then
        printf "\n\nyou can fix this by running 'just format' from the git repo root.\n\n"
    fi
    exit 1
fi
