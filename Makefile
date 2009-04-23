# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of ocaml-usb.

OC = ocamlbuild
OF = ocamlfind

# Use classic-display when compiling under a terminal which do not
# support ANSI sequence
ifeq ($(TERM),dumb)
OC += -classic-display
endif

# Project name
NAME = usb

.PHONY: all
all:
	$(OC) META $(NAME).cma $(NAME).cmxa

.PHONY: examples
examples:
	$(OC) `cat examples.itarget`

.PHONY: doc
doc:
	$(OC) $(NAME).docdir/index.html

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(NAME)-`head -n 1 VERSION`

.PHONY: install
install:
	$(OF) install $(NAME) _build/META \
	  _build/src/*.mli \
	  _build/src/*.cmi \
	  _build/src/*.cmx \
	  _build/*.a \
	  _build/*.cma \
	  _build/*.cmxa \
	  _build/*.so

.PHONY: uninstall
uninstall:
	$(OF) remove $(NAME)

.PHONY: reinstall
reinstall: uninstall install

.PHONY: clean
clean:
	$(OC) -clean
