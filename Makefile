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
	@echo "     RACKETSCRIPT BUILD      "
	@echo "++++++++++++++++++++++++++++++"
	@echo
	@echo "NOTE: Make sure nodejs, npm, traceur and gulp are installed and"
	@echo "      exists in PATH. See \`make setup\`"
	@echo
	raco make -v racketscript-compiler/racketscript/compiler/main.rkt
	raco make -v tests/fixture.rkt

setup:
	raco pkg install --auto -t dir racketscript-compiler/ || \
	    raco pkg update --link racketscript-compiler/

setup-extra:
	npm install -g traceur js-beautify eslint jshint gulp
	raco pkg install --auto cover glob

clean:
	raco pkg remove racketscript-compiler

## Coverage recipes

coverage-unit-test:
	@echo " RACKETSCRIPT COVERAGE UNIT-TEST    "
	@echo "++++++++++++++++++++++++++++++++++"
	raco cover -d ./coverage/unit racketscript-compiler/racketscript/

coverage:
	@echo "    RACKETSCRIPT COVERAGE    "
	@echo "++++++++++++++++++++++++"
	raco cover -d ./coverage/all racketscript-compiler/racketscript/  \
	    tests/fixture.rkt

## JavaScript

eslint:
	@echo "    RACKETSCRIPT RUNTIME LINT    "
	@echo "++++++++++++++++++++++++++++"
	eslint ./racketscript-compiler/racketscript/compiler/runtime/ || true

jshint:
	@echo "    RACKETSCRIPT RUNTIME LINT    "
	@echo "++++++++++++++++++++++++++++"
	jshint ./racketscript-compiler/racketscript/compiler/runtime/ || true

## Test recipes

test:
	@echo "     RACKETSCRIPT TEST       "
	@echo "++++++++++++++++++++++++"
	raco test -t racketscript-compiler/ tests/fixture.rkt

unit-test:
	@echo "    RACKETSCRIPT UNIT-TEST   "
	@echo "++++++++++++++++++++++++"
	raco test -t racketscript-compiler/

integration-test:
	@echo "    RACKETSCRIPT INTEGRATION TEST    "
	@echo "++++++++++++++++++++++++++++++++"
	raco test -t tests/fixture.rkt
