##   mmmmm                  m
##   #   "#  mmm   mmmm   mm#mm  m   m   m mm   mmm
##   #mmmm" "   #  #" "#    #    #   #   #"  " #"  #
##   #   "m m"""#  #   #    #    #   #   #     #""""
##   #    " "mm"#  ##m#"    "mm  "mm"#   #     "#mm"
##               #
##               "

.PHONY: all build setup clean

all: build

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
	raco pkg install threading cover

clean:
	rm -rf src/compiled


coverage:
	@echo "    RAPTURE COVERAGE    "
	@echo "++++++++++++++++++++++++"
	raco cover -d ./coverage src/
