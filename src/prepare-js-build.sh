#!/bin/sh

# quick and dirty integration

cp runtime/*.js js-build
cd js-build

rm -f compiled.js
traceur --out compiled.js `find -iname \*.js | grep -v bootstrap.js`
