# OCaml-USB

OCaml-USB is a binding to libusb-1.0. It uses Lwt to make it easy to
use asynchronous IO features of libusb-1.0.


## Dependencies

* [OCaml](http://ocaml.org/) (>= 4.02.0)
* [findlib](http://projects.camlcity.org/projects/findlib.html)
* [lwt](http://ocsigen.org/lwt/) (>= 2.4.7)
* [lwt_ppx](https://github.com/ocsigen/lwt)
* [conf-pkg-config](https://opam.ocaml.org/packages/conf-pkg-config/)
* [libusb](https://libusb.info/) (>= 1.0.0)

For building the development version, you also need to install
* [dune](https://github.com/ocaml/dune)


## Installation

### From opam

```bash
opam install usb
```


### From source

```bash
./configure
make
make install
```

## Examples

```bash
cd examples
make
```

