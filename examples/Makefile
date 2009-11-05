# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-usb.

# Tools
OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild

# Use classic-display when compiling under a terminal which does not
# support ANSI sequence:
ifeq ($(TERM),dumb)
OCAMLBUILD += -classic-display
endif

all: best

best:
	$(OCAMLBUILD) best

byte:
	$(OCAMLBUILD) byte

native:
	$(OCAMLBUILD) native

clean:
	$(OCAMLBUILD) -clean

.PHONY: best all byte native clean
