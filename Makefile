##   mmmmm                  m
##   #   "#  mmm   mmmm   mm#mm  m   m   m mm   mmm
##   #mmmm" "   #  #" "#    #    #   #   #"  " #"  #
##   #   "m m"""#  #   #    #    #   #   #     #""""
##   #    " "mm"#  ##m#"    "mm  "mm"#   #     "#mm"
##               #
##               "

.PHONY: all build setup clean coverage unit-test integration-test test-all

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

coverage:
	@echo "    RAPTURE COVERAGE    "
	@echo "++++++++++++++++++++++++"
	raco cover -d ./coverage src/

## Test recipes

unit-test:
	@echo "    RAPTURE UNIT-TEST   "
	@echo "++++++++++++++++++++++++"
	raco test -t src/

integration-test:
	@echo "    RAPTURE INTEGRATION TEST    "
	@echo "++++++++++++++++++++++++++++++++"
	raco test -t tests/fixture.rkt

all-test:
	@echo "     RAPTURE TEST       "
	@echo "++++++++++++++++++++++++"
	raco test -t src/ tests/fixture.rkt
