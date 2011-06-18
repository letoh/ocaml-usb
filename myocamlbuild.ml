(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let pkg_config flags package =
  with_temp_file "lwt" "pkg-config"
    (fun tmp ->
       Command.execute ~quiet:true & Cmd(S[A "pkg-config"; A("--" ^ flags); A package; Sh ">"; A tmp]);
       List.map (fun arg -> A arg) (string_list_of_file tmp))

let define_c_library ~name ~c_name =
  let tag = Printf.sprintf "use_%s" name in

  (* Get flags for using pkg-config: *)
  let opt = pkg_config "cflags" c_name and lib = pkg_config "libs" c_name in

  (* Add flags for linking with the C library: *)
  flag ["ocamlmklib"; "c"; tag] & S lib;

  (* C stubs using the C library must be compiled with the library
     specifics flags: *)
  flag ["c"; "compile"; tag] & S(List.map (fun arg -> S[A"-ccopt"; arg]) opt);

  (* OCaml libraries must depends on the C library: *)
  flag ["link"; "ocaml"; tag] & S(List.map (fun arg -> S[A"-cclib"; arg]) lib)

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false
         | After_rules ->
             define_c_library ~name:"libusb" ~c_name:"libusb-1.0";
             flag ["c"; "compile"; "use_libusb"] & S[A"-package"; A"lwt"]
         | _ ->
             ())
