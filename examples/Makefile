# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-usb.

# Tools
OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild

OCAMLBUILD += -use-ocamlfind

# Use classic-display when compiling under a terminal which does not
# support ANSI sequence:
ifeq ($(TERM),dumb)
OCAMLBUILD += -classic-display
endif

SRCS := list_devices.ml
BIN  := $(SRCS:.ml=.byte)

all: $(BIN)

%.byte: %.ml
	@$(OCAMLBUILD) $@

%.native: %.ml
	@$(OCAMLBUILD) $@

clean:
	@$(OCAMLBUILD) -clean

.PHONY: best all byte native clean
