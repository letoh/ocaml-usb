(*
 * USB.ml
 * ------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

type device
type handle

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

(* +-------+
   | Types |
   +-------+ *)

type direction = In | Out
type endpoint = int

(* +----------------+
   | Stub functions |
   +----------------+ *)

type 'a result =
  | OK of 'a
  | Fail of string * error

external ml_usb_init : unit -> unit = "ml_usb_init"
external ml_usb_exit : unit -> unit = "ml_usb_exit"
external ml_usb_set_debug : int -> unit = "ml_usb_set_debug"
external ml_usb_get_device_list : unit -> device list = "ml_usb_get_device_list"
external ml_usb_get_bus_number : device -> int = "ml_usb_get_bus_number"
external ml_usb_get_device_address : device -> int = "ml_usb_get_device_address"
external ml_usb_get_max_packet_size : device -> direction -> endpoint -> int = "ml_usb_get_max_packet_size"
external ml_usb_open : device -> handle = "ml_usb_open"
external ml_usb_open_device_with_vid_pid : int -> int -> handle option = "ml_usb_open_device_with_vid_pid"
external ml_usb_close : handle -> unit = "ml_usb_close"
external ml_usb_get_device : handle -> device = "ml_usb_get_device"
external ml_usb_claim_interface : handle -> int -> unit = "ml_usb_claim_interface"
external ml_usb_release_interface : handle -> int -> unit = "ml_usb_release_interface"
external ml_usb_handle_events : unit -> unit = "ml_usb_handle_events"
external ml_usb_collect_sources : Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list * Unix.file_descr list * float option = "ml_usb_collect_sources"
external ml_usb_interrupt_recv : handle * endpoint * int * string * int * int * (int result -> unit) -> unit = "ml_usb_interrupt_recv"
external ml_usb_interrupt_send : handle * endpoint * int * string * int * int * (int result -> unit) -> unit = "ml_usb_interrupt_send"

(* +------------------------+
   | Event-loop integration |
   +------------------------+ *)

let filter_select now select set_r set_w set_e timeout =
  let setr, set_w, timeout' = ml_usb_collect_sources set_r set_w in
  let res = select set_r set_w set_e (Lwt_main.min_timeout timeout timeout') in
  ml_usb_handle_events ();
  res

let hook = ref filter_select

(* +----------------+
   | Initialization |
   +----------------+ *)

(* Every function of this module must take care of forcing this before
   doing anything: *)
let init = lazy(
  ml_usb_init ();
  Lwt_main.add_hook hook Lwt_main.select_filters;
  let exit = lazy(Lwt_main.remove_hook hook Lwt_main.select_filters;
                  ml_usb_exit ()) in
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
let get_max_packet_size ~device ~direction ~endpoint = ml_usb_get_max_packet_size device direction endpoint
let open_device = ml_usb_open
let open_device_with ~vendor_id ~product_id =
  Lazy.force init;
  ml_usb_open_device_with_vid_pid vendor_id product_id
let close = ml_usb_close
let get_device = ml_usb_get_device
let claim_interface = ml_usb_claim_interface
let release_interface = ml_usb_release_interface

(* +----+
   | IO |
   +----+ *)

let handle_result w = function
  | OK x -> Lwt.wakeup w x
  | Fail(name, err) -> Lwt.wakeup_exn w (Error(name, err))

let interrupt_recv ~handle ~endpoint ?(timeout=0.0) ~buffer ~offset ~length () =
  if offset < 0 || offset > String.length buffer - length then invalid_arg "USB.interrupt_recv";
  let w = Lwt.wait () in
  ml_usb_interrupt_recv (handle, endpoint, truncate (timeout *. 1000.0), buffer, offset, length, handle_result w);
  ml_usb_handle_events ();
  w

let interrupt_send ~handle ~endpoint ?(timeout=0.0) ~buffer ~offset ~length () =
  if offset < 0 || offset > String.length buffer - length then invalid_arg "USB.interrupt_send";
  let w = Lwt.wait () in
  ml_usb_interrupt_send (handle, endpoint, truncate (timeout *. 1000.0), buffer, offset, length, handle_result w);
  ml_usb_handle_events ();
  w

