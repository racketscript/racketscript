#!/usr/bin/env bash

rm -rf ~/rs-orig/racketscript-compiler/racketscript/compiler/runtime/
rm -rf js-build/

cp -r racketscript-compiler/racketscript/compiler/runtime/ ~/rs-orig/racketscript-compiler/racketscript/compiler/runtime/

pushd ~/rs-orig/
./racketscript-compiler/bin/racks foobar.rkt

cp -r js-build/ ~/racketscript/js-build/

popd
