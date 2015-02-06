#!/bin/sh -x
[ -z "$CABAL" ] && CABAL=cabal

set -e

$CABAL clean
$CABAL check

$CABAL install --only-dependencies --enable-tests
$CABAL configure --enable-tests
$CABAL build

$CABAL sdist

dist/build/tests/tests --hide-successes --maximum-generated-tests=10000 --jxml=junit-log.xml

nameBase=`runhaskell src-tools/package-info.hs --package`
$CABAL install dist/$nameBase.tar.gz --enable-tests
