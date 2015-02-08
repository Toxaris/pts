#!/bin/sh -x
[ -z "$CABAL" ] && CABAL=cabal

set -e

rm -rf dist
$CABAL clean
$CABAL check

$CABAL install --only-dependencies --enable-tests -fdevel-tools -fpts-generators
$CABAL configure --enable-tests -fdevel-tools -fpts-generators
$CABAL build

$CABAL sdist

dist/build/tests/tests --hide-successes --maximum-generated-tests=10000 --maximum-unsuitable-generated-tests=10000 --jxml=junit-log.xml
