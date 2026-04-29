set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

alias c := check
alias t := test
alias b := build

[private]
default:
  @just --list

# run "nix-fast-build" to run the nix checks
check:
  nix fmt

  nix-fast-build \
    --flake ".#checks.$(nix eval --impure --raw --expr builtins.currentSystem)" \
    --no-link \
    --skip-cached

# run cabal tests
test:
  cabal test

# build with -Werror and strict linting flags.
build:
  cabal build \
    --ghc-options="-Werror \
      -Wall \
      -Wcompat \
      -Widentities \
      -Wincomplete-record-updates \
      -Wincomplete-uni-patterns \
      -Wmissing-deriving-strategies \
      -Wredundant-constraints \
      -Wunused-packages"

version := `awk '/^version:/ {print $2}' tasty-cache.cabal`

# publish to hackage
publish: build
  cabal sdist

  cabal check

  cabal haddock --haddock-for-hackage --enable-doc

  cabal upload --publish -t ${HACKAGE_TOKEN} \
    dist-newstyle/sdist/tasty-cache-{{version}}.tar.gz

  cabal upload --publish -t ${HACKAGE_TOKEN} \
    --documentation dist-newstyle/tasty-cache-{{version}}-docs.tar.gz
