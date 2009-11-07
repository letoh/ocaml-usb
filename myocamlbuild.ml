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

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let try_exec command =
  try
    let _ = run_and_read command in
    true
  with _ ->
    false

let () =
  if not (try_exec "ocamlfind printconf") then begin
    prerr_endline "ocamlfind is not available, please install it";
    exit 1
  end

let have_native = try_exec "ocamlfind ocamlopt -version"

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

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
  "lwt.preemptive";
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

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

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
        Options.ocamldoc := S[A"ocamlfind"; A"ocamldoc"; A"-hide-warnings"]

    | After_rules ->

        (* +---------------------------------------------------------+
           | Virtual targets                                         |
           +---------------------------------------------------------+ *)

        let virtual_rule name deps =
          rule name ~stamp:name ~deps (fun _ _ -> Nop)
        in

        virtual_rule "all" & "META" :: if have_native then ["usb.cma"; "usb.cmxa"; "usb.cmxs"] else ["usb.cma"];
        virtual_rule "byte" & ["META"; "usb.cma"];
        virtual_rule "native" & ["META"; "usb.cmxa"; "usb.cmxs"];

        (* +---------------------------------------------------------+
           | Shared libraries                                        |
           +---------------------------------------------------------+ *)

        rule "shared libraries (cmxs)"
          ~dep:"%.cmxa" ~prod:"%.cmxs"
          (fun env _ -> Cmd(S[!(Options.ocamlopt); A"-cclib"; A"-L."; A"-shared"; A"-linkall"; A(env "%.cmxa"); A"-o"; A(env "%.cmxs")]));

        (* +---------------------------------------------------------+
           | Ocamlfind stuff                                         |
           +---------------------------------------------------------+ *)

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

        (* +---------------------------------------------------------+
           | C stubs                                                 |
           +---------------------------------------------------------+ *)

        flag ["c"; "compile"] & S[A"-ccopt"; A"-Wall"];

        (* Search 'pkg-config': *)
        let pkg_config = try
          Command.search_in_path "pkg-config"
        with
            Not_found ->
              failwith "The program ``pkg-config'' is required but not found, please intall it"
        in
        let get_args cmd =
          with_temp_file "ocaml-usb" "pkg-config"
            (fun tmp ->
               Command.execute ~quiet:true & Cmd(S[cmd; Sh ">"; A tmp]);
               List.map (fun arg -> A arg) (string_list_of_file tmp))
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

        let ccopt = S(List.map (fun arg -> S[A"-ccopt"; arg]) usb_opt)
        and cclib = S(List.map (fun arg -> S[A"-cclib"; arg]) usb_lib) in

        (* C stubs using libusb must be compiled with libusb specifics
           flags: *)
        flag ["c"; "compile"; "use_libusb"] & ccopt;

        (* OCaml llibraries must depends on the C library libusb: *)
        flag ["link"; "ocaml"; "use_libusb"] & cclib;

        (* +---------------------------------------------------------+
           | Other                                                   |
           +---------------------------------------------------------+ *)

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())]
                     (read_file "META.in")], "META"))

    | _ -> ()
  end
