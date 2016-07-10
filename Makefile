##   mmmmm                  m
##   #   "#  mmm   mmmm   mm#mm  m   m   m mm   mmm
##   #mmmm" "   #  #" "#    #    #   #   #"  " #"  #
##   #   "m m"""#  #   #    #    #   #   #     #""""
##   #    " "mm"#  ##m#"    "mm  "mm"#   #     "#mm"
##               #
##               "

.PHONY: all build setup clean coverage-unit-test coverage unit-test integration-test test

all: build

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
	npm install -g traceur js-beautify gulp  ## these tools must be in PATH
	raco pkg install threading cover glob

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

## Test recipes

unit-test:
	@echo "    RAPTURE UNIT-TEST   "
	@echo "++++++++++++++++++++++++"
	raco test -t src/

integration-test:
	@echo "    RAPTURE INTEGRATION TEST    "
	@echo "++++++++++++++++++++++++++++++++"
	raco test -t tests/fixture.rkt

test:
	@echo "     RAPTURE TEST       "
	@echo "++++++++++++++++++++++++"
	raco test -t src/ tests/fixture.rkt
