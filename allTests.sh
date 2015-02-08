#!/bin/sh -x
. mostTests.sh

nameBase=`runhaskell src-tools/package-info.hs --package`
$CABAL install dist/$nameBase.tar.gz --enable-tests
