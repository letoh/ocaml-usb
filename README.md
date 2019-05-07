OCaml-USB
=========

OCaml-USB is a binding to libusb-1.0. It uses Lwt to make it easy to
use asynchronous IO features of libusb-1.0.

Dependencies
------------

* [OCaml](http://caml.inria.fr/ocaml/) (>= 3.11)
* [findlib](http://projects.camlcity.org/projects/findlib.html)
* [lwt](http://ocsigen.org/lwt/) (>= 2.0.0)
* [libusb-1.0](http://www.libusb.info/)

For building the development version, you also need to install
[oasis](http://oasis.forge.ocamlcore.org/) (>= 0.3.0).

Installation
------------

To build and install ocaml-usb:

    $ ./configure
    $ make
    $ make install

### Documentation _(optional)_

To build the documentation:

    $ make doc

It will then be installed by `make install`.

### Tests _(optional)_

To build and execute tests:

    $ ./configure --enable-tests
    $ make test
