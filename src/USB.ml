(*
 * USB.ml
 * ------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

open Lwt_unix
open Lwt

(* +-----------------------------------------------------------------+
   | Errors                                                          |
   +-----------------------------------------------------------------+ *)

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

exception Error of error * string

let _ = Callback.register_exception "ocaml-usb:Error" (Error(Error_access, ""))

type transfer_error =
  | Transfer_error
  | Transfer_timed_out
  | Transfer_cancelled
  | Transfer_stall
  | Transfer_no_device
  | Transfer_overflow

exception Transfer_error of transfer_error * string

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type device
type handle
type direction = In | Out
type endpoint = int
type interface = int
type recipient =
  | Device
  | Interface
  | Endpoint
  | Other
type request_type =
  | Standard
  | Class
  | Vendor
  | Reserved
type request = int
type configuration = int

(* +-----------------------------------------------------------------+
   | Stub functions                                                  |
   +-----------------------------------------------------------------+ *)

(* Result of a transfer: *)
type 'a result =
  | OK of int
  | Error of transfer_error * string

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
external ml_usb_claim_interface : handle -> interface -> unit = "ml_usb_claim_interface"
external ml_usb_release_interface : handle -> interface -> unit = "ml_usb_release_interface"
external ml_usb_get_configuration : handle -> configuration = "ml_usb_get_configuration"
external ml_usb_set_configuration : handle -> configuration -> unit = "ml_usb_set_configuration"
external ml_usb_kernel_driver_active : handle -> interface -> bool = "ml_usb_kernel_driver_active"
external ml_usb_detach_kernel_driver : handle -> interface -> unit = "ml_usb_detach_kernel_driver"
external ml_usb_attach_kernel_driver : handle -> interface -> unit = "ml_usb_attach_kernel_driver"
external ml_usb_handle_events : unit -> unit = "ml_usb_handle_events"
external ml_usb_collect_sources : Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list * Unix.file_descr list * float option = "ml_usb_collect_sources"
external ml_usb_bulk_recv : handle * endpoint * int * string * int * int * (int result -> unit) -> unit = "ml_usb_bulk_recv"
external ml_usb_bulk_send : handle * endpoint * int * string * int * int * (int result -> unit) -> unit = "ml_usb_bulk_send"
external ml_usb_interrupt_recv : handle * endpoint * int * string * int * int * (int result -> unit) -> unit = "ml_usb_interrupt_recv"
external ml_usb_interrupt_send : handle * endpoint * int * string * int * int * (int result -> unit) -> unit = "ml_usb_interrupt_send"
external ml_usb_control_recv : handle * endpoint * int * string * int * int * (int result -> unit) * recipient * request_type * request * int * int * int -> unit = "ml_usb_control_recv"
external ml_usb_control_send : handle * endpoint * int * string * int * int * (int result -> unit) * recipient * request_type * request * int * int * int -> unit = "ml_usb_control_send"
external ml_usb_reset_device : handle -> unit = "ml_usb_reset_device"

(* +-----------------------------------------------------------------+
   | Event-loop integration                                          |
   +-----------------------------------------------------------------+ *)

let filter_select now select set_r set_w set_e timeout =
  let set_r, set_w, timeout' = ml_usb_collect_sources set_r set_w in
  let res = select set_r set_w set_e (Lwt_main.min_timeout timeout timeout') in
  ml_usb_handle_events ();
  res

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

(* Every function of this module must take care of forcing this before
   doing anything: *)
let init = lazy(
  ml_usb_init ();
  let hook = Lwt_sequence.add_l filter_select Lwt_main.select_filters in
  let exit = lazy(Lwt_sequence.remove hook;
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

(* +-----------------------------------------------------------------+
   | Device handling and enumeration                                 |
   +-----------------------------------------------------------------+ *)

let get_device_list () =
  Lazy.force init;
  ml_usb_get_device_list ()

let get_bus_number = ml_usb_get_bus_number
let get_device_address = ml_usb_get_device_address
let get_max_packet_size ~device ~direction ~endpoint = ml_usb_get_max_packet_size device direction endpoint
let open_device = ml_usb_open
let close = ml_usb_close
let get_device = ml_usb_get_device
let claim_interface handle interface = try_lwt (ml_usb_claim_interface handle interface; return ())
let release_interface handle interface = Lwt_preemptive.detach (fun _ -> ml_usb_release_interface handle interface) ()
let kernel_driver_active = ml_usb_kernel_driver_active
let detach_kernel_driver = ml_usb_detach_kernel_driver
let attach_kernel_driver = ml_usb_attach_kernel_driver
let get_configuration handle = try_lwt return (ml_usb_get_configuration handle)
let set_configuration handle conf = Lwt_preemptive.detach (fun _ -> ml_usb_set_configuration handle conf) ()
let reset_device handle = Lwt_preemptive.detach (fun _ -> ml_usb_reset_device handle) ()

let open_device_with ~vendor_id ~product_id =
  Lazy.force init;
  match ml_usb_open_device_with_vid_pid vendor_id product_id with
    | Some handle -> handle
    | None -> failwith (Printf.sprintf "no such usb device (vendor-id=0x%04x, product-id=0x%04x)" vendor_id product_id)

(* +-----------------------------------------------------------------+
   | IOs                                                             |
   +-----------------------------------------------------------------+ *)

(* Handle the result of a transfer *)
let handle_result w = function
  | OK x -> Lwt.wakeup w x
  | Error(error, msg) -> Lwt.wakeup_exn w (Transfer_error(error, msg))

let make_timeout = function
  | None -> 0
  | Some t -> truncate (t *. 1000.0)

let transfer name func ~handle ~endpoint ?timeout buffer offset length =
  if offset < 0 || offset > String.length buffer - length then invalid_arg ("USB." ^ name);
  let w = Lwt.wait () in
  func (handle, endpoint, make_timeout timeout, buffer, offset, length, handle_result w);
  w

let bulk_recv = transfer "bulk_recv" ml_usb_bulk_recv
let bulk_send = transfer "bulk_send" ml_usb_bulk_send
let interrupt_recv = transfer "interrupt_recv" ml_usb_interrupt_recv
let interrupt_send = transfer "interrupt_send" ml_usb_interrupt_send

let control_transfer name func ~handle ~endpoint ?timeout ~recipient ~request_type ~request ~value ~index ~length buffer offset length =
  if offset < 0 || offset > String.length buffer - length then invalid_arg ("USB." ^ name);
  let w = Lwt.wait () in
  func (handle, endpoint, make_timeout timeout, buffer, offset, length, handle_result w, recipient, request_type, request, value, index, length);
  w

let control_recv = control_transfer "control_recv" ml_usb_control_recv
let control_send = control_transfer "control_send" ml_usb_control_send

(* +-----------------------------------------------------------------+
   | Standard request                                                |
   +-----------------------------------------------------------------+ *)

module Request =
struct
  type t = request
  let get_status = 0x00
  let clear_feature = 0x01
  let set_feature = 0x03
  let set_address = 0x05
  let get_descriptor = 0x06
  let set_descriptor = 0x07
  let get_configuration = 0x08
  let set_configuration = 0x09
  let get_interface = 0x0a
  let set_interface = 0x0b
  let synch_frame = 0x0c
end
