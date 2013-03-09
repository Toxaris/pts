#!/bin/sh -x

set -e

cabal clean

set +e
# Ignore failures from these tasks - for now.
cabal check
set -e

cabal install --only-dependencies
cabal configure --enable-tests
cabal build

cabal sdist

dist/build/tests/tests --hide-successes --maximum-generated-tests=10000 --jxml=junit-log.xml

nameBase=`runhaskell src-tools/package-info.hs --package`
cabal install dist/$nameBase.tar.gz --enable-tests
