(*
 * list_devices.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

open Lwt

let _ =
  List.iter
    (fun dev ->
       Printf.printf "Bus %03d Device %03d\n" (USB.get_bus_number dev) (USB.get_device_address dev))
    (USB.get_device_list ())
