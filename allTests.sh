#!/bin/sh -x
. mostTests.sh

nameBase=`./dist/build/package-info/package-info --package`
$CABAL install dist/$nameBase.tar.gz --enable-tests -fdevel-tools
