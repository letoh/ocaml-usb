(*
 * USB.ml
 * ------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

type device
type device_handle

(* +--------+
   | Errors |
   +--------+ *)

type error =
  | Error_io
  | Error_invalid_param
  | Error_access
  | Error_no_device
  | Error_not_found
  | Error_busy
  | Error_timeout
  | Error_overflow
  | Error_pipe
  | Error_interrupted
  | Error_no_mem
  | Error_not_supported
  | Error_other

exception Error of string * error

let _ = Callback.register_exception "ocaml-usb error" (Error("", Error_io))

(* +----------------+
   | Stub functions |
   +----------------+ *)

external ml_usb_init : unit -> unit = "ml_usb_init"
external ml_usb_exit : unit -> unit = "ml_usb_exit"
external ml_usb_set_debug : int -> unit = "ml_usb_set_debug"
external ml_usb_get_device_list : unit -> device list = "ml_usb_get_device_list"
external ml_usb_get_bus_number : device -> int = "ml_usb_get_bus_number"
external ml_usb_get_device_address : device -> int = "ml_usb_get_device_address"
external ml_usb_get_max_packet_size : device -> int -> int = "ml_usb_get_max_packet_size"
external ml_usb_open : device -> device_handle = "ml_usb_open"
external ml_usb_open_device_with_vid_pid : int -> int -> device_handle option = "ml_usb_open_device_with_vid_pid"
external ml_usb_close : device_handle -> unit = "ml_usb_close"
external ml_usb_get_device : device_handle -> device = "ml_usb_get_device"
external ml_usb_claim_interface : device_handle -> int -> unit = "ml_usb_claim_interface"
external ml_usb_release_interface : device_handle -> int -> unit = "ml_usb_release_interface"

(* +----------------+
   | Initialization |
   +----------------+ *)

(* Every function of this module must take care of forcing this before
   doing anything: *)
let init = lazy(
  ml_usb_init ();
  let exit = lazy(ml_usb_exit ()) in
  at_exit (fun _ -> Lazy.force exit)
)

let set_debug level =
  Lazy.force init;
  ml_usb_set_debug (match level with
                      | `quiet -> 0
                      | `error -> 1
                      | `warning -> 2
                      | `verbose -> 3)

(* +---------------------------------+
   | Device handling and enumeration |
   +---------------------------------+ *)

let get_device_list () =
  Lazy.force init;
  ml_usb_get_device_list ()

let get_bus_number = ml_usb_get_bus_number
let get_device_address = ml_usb_get_device_address
let get_max_packet_size = ml_usb_get_max_packet_size
let open_device = ml_usb_open
let open_device_with ~vendor_id ~product_id = ml_usb_open_device_with_vid_pid vendor_id product_id
let close = ml_usb_close
let get_device = ml_usb_get_device
let claim_interface = ml_usb_claim_interface
let release_interface = ml_usb_release_interface
