#!/bin/sh -vx

set -e

cabal clean

# Ignore failures from cabal check - for now.
set +e
cabal check
set -e

cabal install --only-dependencies
cabal configure --enable-tests
cabal build
dist/build/tests/tests --hide-successes --maximum-generated-tests=10000 --jxml=junit-log.xml

cabal sdist

nameBase=`runhaskell src-tools/package-info.hs --package`
cabal install dist/$nameBase.tar.gz --enable-tests
