#!/usr/bin/env bash

rm -rf ~/rs-orig/racketscript-compiler/racketscript/compiler/runtime/
rm -rf js-build/runtime/

cp -r racketscript-compiler/racketscript/compiler/runtime/ ~/rs-orig/racketscript-compiler/racketscript/compiler/

pushd ~/rs-orig/
./racketscript-compiler/bin/racks foobar.rkt

cp -r js-build/runtime/ ~/racketscript/js-build/

popd
