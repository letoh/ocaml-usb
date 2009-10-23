# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-usb.

# +------------------------------------------------------------------+
# | Configuration                                                    |
# +------------------------------------------------------------------+

# Tools
OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild

# Use classic-display when compiling under a terminal which does not
# support ANSI sequence:
ifeq ($(TERM),dumb)
OCAMLBUILD += -classic-display
endif

# Avoid compilation of native plugin if ocamlopt is not available
ifeq ($(shell if which ocamlopt >/dev/null; then echo yes; fi),)
OCAMLBUILD += -byte-plugin
endif

# +------------------------------------------------------------------+
# | Rules                                                            |
# +------------------------------------------------------------------+

.PHONY: all
all:
	$(OCAMLBUILD) all

.PHONY: doc
doc:
	$(OCAMLBUILD) usb.docdir/index.html

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name ocaml-usb-`head -n 1 VERSION`

.PHONY: install
install:
	$(OCAMLFIND) install usb _build/META \
	  $(wildcard _build/src/*.mli) \
	  $(wildcard _build/src/*.cmi) \
	  $(wildcard _build/src/*.cmx) \
	  $(wildcard _build/*.a) \
	  $(wildcard _build/*.cma) \
	  $(wildcard _build/*.cmxa) \
	  $(wildcard _build/*.so)

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove usb

.PHONY: reinstall
reinstall: uninstall install

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	cd examples && $(MAKE) clean
