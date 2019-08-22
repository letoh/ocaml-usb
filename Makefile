# Copyright : (c) 2019, letoh
# License   : BSD-3-Clause
#
PKG := $(basename $(wildcard *.opam))

default all: build

build clean:
	@dune $@

$(PKG).install: build

install uninstall: ARGS := $(if $(PREFIX),--prefix=$(PREFIX),)
install uninstall: $(PKG).install
	@dune $@ $(ARGS)

reinstall: uninstall install

distclean: clean
	@./configure --distclean || true

doc:
	@dune build @doc

.PHONY: default install uninstall reinstall clean distclean
