(*
 * list_devices.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

let () =
  List.iter
    (fun dev ->
       Printf.printf "Bus %03d Device %03d\n" (Usb.get_bus_number dev) (Usb.get_device_address dev))
    (Usb.get_device_list ())
