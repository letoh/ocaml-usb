(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

open Printf
open Ocamlbuild_plugin

(* List of syntax extensions used internally. It is a list of:

   (tag, byte-code-file)

   - tag is the tag that must be used inside the source tree (in _tags
   files) in order to have file preprocessed with the given syntax extension

   - byte-code-file is the byte-code for the syntax extension
*)
let intern_syntaxes = []

(* +-----------+
   | Ocamlfind |
   +-----------+ *)

(* Packages we want to use in the program *)
let packages = [
  (* The camlp4 packages is just used to tell that we want to
     preprocess a file with camlp4 *)
  "camlp4";

  (* Handling of quotations of the form <:expr< >>, in original and
     revised syntax *)
  "camlp4.quotations.o";
  "camlp4.quotations.r";

  (* the "EXTEND ... END" syntax extension *)
  "camlp4.extend";

  (* The camlp4 library *)
  "camlp4.lib";

  (* Macro *)
  "camlp4.macro";

  (* Other packages we want to use *)
  "lwt";
  "lwt.syntax";
  "str";
  "xml-light";
]

(* List of syntaxes *)
let syntaxes = [
  (* Original syntax *)
  "camlp4o";

  (* Revised syntax *)
  "camlp4r"
]

(* +-------+
   | Utils |
   +-------+ *)

(* Given the tag [tag] add the command line options [f] to all stages
   of compilatiopn but linking *)
let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

(* Same as [flag_all_stages_except_link] but also flag the linking
   stage *)
let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

(* Define a internal library with required depency. File using it
   (like samples) must be tagged with "use_name".

   For example if the library sources are in the directory "src", and
   samples in directory "samples", you can have in myocamlbuild.ml:

     define_lib ~dir:"src" "foo"

   and in the _tags file:

     <samples/**/*>: use_foo
*)
let define_lib ?dir name =
  ocaml_lib ?dir name;
  dep ["ocaml"; "byte"; "use_" ^ name] [name ^ ".cma"];
  dep ["ocaml"; "native"; "use_" ^ name] [name ^ ".cmxa"]

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_version _ =
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"

let _ =
  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        let ocamlfind x = S[A"ocamlfind"; A x] in
        Options.ocamlc   := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc"

    | After_rules ->

        define_lib ~dir:"src" "usb";

        (* +-------------------+
           | Internal syntaxes |
           +-------------------+ *)

        List.iter
          (fun (tag, file) ->
             (* add "-ppopt file" to files using the syntax extension *)
             flag_all_stages_except_link tag & S[A"-ppopt"; A file];

             (* Make them depends on the syntax extension *)
             dep ["ocaml"; "ocamldep"; tag] [file])
          intern_syntaxes;

        (* +-----------------+
           | Ocamlfind stuff |
           +-----------------+ *)

        (* When one link an OCaml binary, one should use -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun package -> flag_all_stages ("pkg_" ^ package) (S[A"-package"; A package]))
          packages;

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          syntaxes;

        (* +---------+
           | C stubs |
           +---------+ *)

        (* Search 'pkg-config': *)
        let pkg_config = try
          Command.search_in_path "pkg-config"
        with
            Not_found ->
              failwith "The program ``pkg-config'' is required but not found, please intall it"
        in
        let get_args cmd =
          Command.execute ~quiet:true & Cmd(S[cmd; Sh ">"; A"pkg-config.output"]);
          List.map (fun arg -> A arg) (string_list_of_file "pkg-config.output")
        in

        (* Get flags for libusb-1.0 using pkg-config: *)
        let usb_opt = get_args & S[A pkg_config; A"--cflags"; A"libusb-1.0"]
        and usb_lib = get_args & S[A pkg_config; A"--libs"; A"libusb-1.0"] in

        (* Dependency for automatic compliation of C stubs: *)
        dep ["link"; "ocaml"; "use_stubs"] ["libusb_stubs.a"];

        (* Link code using C stubs with '-lusb_stubs': *)
        flag ["link"; "library"; "ocaml"; "use_stubs"] & S[A"-cclib"; A"-lusb_stubs"];

        (* For libraries add also a '-dllib' option for automatic
           addition of '-cclib -lusb_stubs' when using the library: *)
        flag ["link"; "library"; "ocaml"; "byte"; "use_stubs"] & S[A"-dllib"; A"-lusb_stubs"];

        (* Add flags for linking with the C library libusb: *)
        flag ["ocamlmklib"; "c"; "use_libusb"] & S usb_lib;

        (* C stubs using libusb must be compiled with libusb specifics
           flags: *)
        flag ["c"; "compile"; "use_libusb"] & S(List.map (fun arg -> S[A"-ccopt"; arg]) usb_opt);

        (* +-------+
           | Other |
           +-------+ *)

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())]
                     (read_file "META.in")], "META"))

    | _ -> ()
  end
