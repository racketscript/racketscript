#!/usr/bin/env sh

racks -l expander.rktl
mv foo.js js-build/modules/
cd js-build/
node modules/foo.js
cd ..
