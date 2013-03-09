#!/bin/sh -x
[ -z "$CABAL" ] && CABAL=cabal

set -e

$CABAL clean

set +e
# Ignore failures from these tasks - for now.
$CABAL check
set -e

$CABAL install --only-dependencies
$CABAL configure --enable-tests
$CABAL build

$CABAL sdist

dist/build/tests/tests --hide-successes --maximum-generated-tests=10000 --jxml=junit-log.xml

nameBase=`runhaskell src-tools/package-info.hs --package`
$CABAL install dist/$nameBase.tar.gz --enable-tests
