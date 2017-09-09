.PHONY: build setup setup-extras clean \
eslint eslint-fix tscheck \
test unit-test integration-test coverage coverage-unit-test

SETUP_BIN_DIR=$(shell racket -e "(require setup/dirs) (display (find-user-console-bin-dir))")
SETUP_PKGS_DIR=$(shell racket -e "(require setup/dirs) (display (find-user-pkgs-dir))")
TSC=node_modules/.bin/tsc
ESLINT=node_modules/.bin/eslint

## Compiler recipes

$(SETUP_BIN_DIR)/racks:
	raco pkg install --auto -t dir racketscript-compiler/ || \
	  raco pkg update --link racketscript-compiler/
	raco pkg install --auto -t dir racketscript-extras/ || \
	  raco pkg update --link racketscript-extras/
setup: | $(SETUP_BIN_DIR)/racks

$(SETUP_PKGS_DIR)/cover:
	raco pkg install --auto cover || true
$(SETUP_PKGS_DIR)/glob:
	raco pkg install --auto glob || true
setup-extras: | setup $(SETUP_PKGS_DIR)/cover $(SETUP_PKGS_DIR)/glob

build: setup-extras
	raco make -j 2 -v racketscript-compiler/racketscript/compiler/main.rkt \
	  tests/fixture.rkt

clean: clean-compiler
	raco pkg remove racketscript-extras || true
	raco pkg remove racketscript-compiler || true
	rm -rf test/compiled/ coverage/ tmp/ js-build/
	bash -c 'rm -rf racketscript-compiler/racketscript/compiler/**/*.{dep,zo}'

## JavaScript

eslint: | node_modules
	$(ESLINT) ./racketscript-compiler/racketscript/compiler/runtime/

eslint-fix: | node_modules
	$(ESLINT) --fix ./racketscript-compiler/racketscript/compiler/runtime/

# Typecheck JavaScript
tscheck: node_modules
	$(TSC) --noEmit --allowJs --checkJs --strict --lib es2017 --target es2017 \
	racketscript-compiler/racketscript/compiler/runtime/kernel.js

node_modules: package.json package-lock.json
	npm install && touch node_modules

## Test recipes

test: unit-test integration-test

unit-test: build
	@echo "    RACKETSCRIPT UNIT-TEST   "
	@echo "++++++++++++++++++++++++"
	raco test -t racketscript-compiler/

integration-test: build
	@echo "    RACKETSCRIPT INTEGRATION TEST    "
	@echo "++++++++++++++++++++++++++++++++"
	raco test -t tests/fixture.rkt

## Coverage recipes

coverage-unit-test: build
	@echo " RACKETSCRIPT COVERAGE UNIT-TEST    "
	@echo "++++++++++++++++++++++++++++++++++"
	raco cover -d ./coverage/unit racketscript-compiler/racketscript/

coverage: build
	@echo "    RACKETSCRIPT COVERAGE    "
	@echo "++++++++++++++++++++++++"
	COVERAGE_MODE=1 raco cover -d ./coverage/all -b racketscript-compiler \
	    tests/fixture.rkt
