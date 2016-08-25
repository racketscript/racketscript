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
	raco make -v src/main.rkt

setup:
	npm install -g traceur gulp ## these tools must be in PATH
	raco pkg install --auto threading graph

setup-extra:
	npm install -g js-beautify eslint jshint
	raco pkg install --auto cover glob

clean:
	rm -rf src/compiled

## Coverage recipes

coverage-unit-test:
	@echo "    RAPTURE COVERAGE UNIT-TEST    "
	@echo "++++++++++++++++++++++++++++++++++"
	raco cover -d ./coverage/unit src/

coverage:
	@echo "    RAPTURE COVERAGE    "
	@echo "++++++++++++++++++++++++"
	raco cover -d ./coverage/all src/ tests/fixture.rkt

## JavaScript

eslint:
	@echo "    RAPTURE RUNTIME LINT    "
	@echo "++++++++++++++++++++++++++++"
	eslint ./src/runtime/ || true

jshint:
	@echo "    RAPTURE RUNTIME LINT    "
	@echo "++++++++++++++++++++++++++++"
	jshint ./src/runtime/ || true

## Test recipes

test:
	@echo "     RAPTURE TEST       "
	@echo "++++++++++++++++++++++++"
	raco test -t src/ tests/fixture.rkt

unit-test:
	@echo "    RAPTURE UNIT-TEST   "
	@echo "++++++++++++++++++++++++"
	raco test -t src/

integration-test:
	@echo "    RAPTURE INTEGRATION TEST    "
	@echo "++++++++++++++++++++++++++++++++"
	raco test -t tests/fixture.rkt
