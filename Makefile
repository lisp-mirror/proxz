# This must be the first this in Makefile.common
TOP := $(dir $(lastword $(MAKEFILE_LIST)))
QUICK=make.lisp

all: sbcl

ecl:
	ecl -load $(QUICK)

sbcl:
	sbcl --load $(QUICK)

clisp:
	clisp -c $(QUICK)

cmucl:
	lisp -load $(QUICK)

clean:
	rm -f proxz.fasl proxz.fas *.*~
