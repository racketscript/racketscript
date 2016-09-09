##   mmmmm                  m
##   #   "#  mmm   mmmm   mm#mm  m   m   m mm   mmm
##   #mmmm" "   #  #" "#    #    #   #   #"  " #"  #
##   #   "m m"""#  #   #    #    #   #   #     #""""
##   #    " "mm"#  ##m#"    "mm  "mm"#   #     "#mm"
##               #
##               "

.PHONY: build setup setup-extra clean 
.PHONY: test unit-test integration-test test
.PHONY: coverage coverage-unit-test
.PHONY: eslint jslint

## Compile recipes

build:
	@echo "     RAPTURE BUILD      "
	@echo "++++++++++++++++++++++++"
	@echo
	@echo "NOTE: Make sure nodejs, npm, traceur and gulp are installed and"
	@echo "      exists in PATH. See \`make setup\`"
	@echo
	raco make -v rapture-compiler/rapture/compiler/main.rkt
	raco make -v tests/fixture.rkt

setup:
	raco pkg install --auto -t dir rapture-compiler/ || \
	    raco pkg update --link rapture-compiler/

setup-extra:
	npm install -g traceur js-beautify eslint jshint gulp
	raco pkg install --auto cover glob

clean:
	raco pkg remove rapture-compiler

## Coverage recipes

coverage-unit-test:
	@echo "    RAPTURE COVERAGE UNIT-TEST    "
	@echo "++++++++++++++++++++++++++++++++++"
	raco cover -d ./coverage/unit rapture-compiler/rapture/

coverage:
	@echo "    RAPTURE COVERAGE    "
	@echo "++++++++++++++++++++++++"
	raco cover -d ./coverage/all rapture-compiler/rapture/  \
	    tests/fixture.rkt

## JavaScript

eslint:
	@echo "    RAPTURE RUNTIME LINT    "
	@echo "++++++++++++++++++++++++++++"
	eslint ./rapture-compiler/rapture/compiler/runtime/ || true

jshint:
	@echo "    RAPTURE RUNTIME LINT    "
	@echo "++++++++++++++++++++++++++++"
	jshint ./rapture-compiler/rapture/compiler/runtime/ || true

## Test recipes

test:
	@echo "     RAPTURE TEST       "
	@echo "++++++++++++++++++++++++"
	raco test -t rapture-compiler/ tests/fixture.rkt

unit-test:
	@echo "    RAPTURE UNIT-TEST   "
	@echo "++++++++++++++++++++++++"
	raco test -t rapture-compiler/

integration-test:
	@echo "    RAPTURE INTEGRATION TEST    "
	@echo "++++++++++++++++++++++++++++++++"
	raco test -t tests/fixture.rkt
