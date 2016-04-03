##   mmmmm                  m
##   #   "#  mmm   mmmm   mm#mm  m   m   m mm   mmm
##   #mmmm" "   #  #" "#    #    #   #   #"  " #"  #
##   #   "m m"""#  #   #    #    #   #   #     #""""
##   #    " "mm"#  ##m#"    "mm  "mm"#   #     "#mm"
##               #
##               "

setup:
	npm install -g traceur js-beautify gulp  ## these tools must be in PATH
	raco pkg install threading

build:
	raco make -v src/main.rkt

clean:
	rm -rf src/compiled
