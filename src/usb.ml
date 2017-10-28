(*
 * USB.ml
 * ------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

open Sexplib.Std
open Lwt.Infix
open Lwt_unix

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

exception Transfer of transfer_error * string

let error_message = function
  | Error_io -> "Input/output error"
  | Error_invalid_param -> "Invalid parameter"
  | Error_access -> "Access denied"
  | Error_no_device -> "No such device"
  | Error_not_found -> "Entity not found"
  | Error_busy -> "Resource busy"
  | Error_timeout -> "Operation timed out"
  | Error_overflow -> "Overflow"
  | Error_pipe -> "Pipe error"
  | Error_interrupted -> "System call interrupted"
  | Error_no_mem -> "Insufficient memory"
  | Error_not_supported -> "Operation not supported or unimplemented on this platform"
  | Error_other -> "Other error"

let transfer_error_message = function
  | Transfer_error -> "Transfer failed"
  | Transfer_timed_out -> "Transfer timed out"
  | Transfer_cancelled -> "Transfer was cancelled"
  | Transfer_stall -> "Transfer stalled"
  | Transfer_no_device -> "Device was disconnected"
  | Transfer_overflow -> "Device sent more data than requested"

let () =
  Printexc.register_printer
    (function
      | Error(err, fun_name) ->
        Some(Printf.sprintf "USB error: '%s' failed: %s" fun_name (error_message err))
      | Transfer(err, fun_name) ->
        Some(Printf.sprintf "USB transfer error: '%s' failed: %s" fun_name (transfer_error_message err))
      | exn ->
        None)

let handle_error f arg =
  try
    f arg
  with
    | Error(err, fun_name) ->
        Printf.eprintf "%s: %s failed: %s\n%!" Sys.argv.(0) fun_name (error_message err);
        exit 2
    | Transfer(err, fun_name) ->
        Printf.eprintf "%s: %s failed: %s\n%!" Sys.argv.(0) fun_name (transfer_error_message err);
        exit 2

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type device
type device_handle
type transfer

type handle_state =
  | State_closed
  | State_detach
      (* A detached operation is being performed on the device. *)
  | State_ok

type handle = {
  handle : device_handle;
  mutable state : handle_state;
  mutex : Lwt_mutex.t;
}

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
type iso_result =
  | Iso_ok of int
  | Iso_error of transfer_error * string

(* +-----------------------------------------------------------------+
   | Stub functions                                                  |
   +-----------------------------------------------------------------+ *)

(* Result of a transfer: *)
type 'a result =
  | OK of 'a
  | Error of transfer_error

external ml_usb_init : unit -> unit = "ml_usb_init"
external ml_usb_exit : unit -> unit = "ml_usb_exit"
external ml_usb_set_debug : int -> unit = "ml_usb_set_debug"
external ml_usb_get_next_timeout : unit -> float = "ml_usb_get_next_timeout"
external ml_usb_handle_events : unit -> unit = "ml_usb_handle_events"
external ml_usb_get_device_list : unit -> device list = "ml_usb_get_device_list"
external ml_usb_get_bus_number : device -> int = "ml_usb_get_bus_number"
external ml_usb_get_device_address : device -> int = "ml_usb_get_device_address"
external ml_usb_get_max_packet_size : device -> direction -> endpoint -> int = "ml_usb_get_max_packet_size"
external ml_usb_open : device -> device_handle = "ml_usb_open"
external ml_usb_open_device_with_vid_pid : int -> int -> device_handle option = "ml_usb_open_device_with_vid_pid"
external ml_usb_close : device_handle -> unit = "ml_usb_close"
external ml_usb_get_device : device_handle -> device = "ml_usb_get_device"
external ml_usb_kernel_driver_active : device_handle -> interface -> bool = "ml_usb_kernel_driver_active"
external ml_usb_detach_kernel_driver : device_handle -> interface -> unit = "ml_usb_detach_kernel_driver"
external ml_usb_attach_kernel_driver : device_handle -> interface -> unit = "ml_usb_attach_kernel_driver"
external ml_usb_bulk_recv : device_handle * endpoint * int * string * int * int * (int result -> unit) -> transfer = "ml_usb_bulk_recv"
external ml_usb_bulk_send : device_handle * endpoint * int * string * int * int * (int result -> unit) -> transfer = "ml_usb_bulk_send"
external ml_usb_interrupt_recv : device_handle * endpoint * int * string * int * int * (int result -> unit) -> transfer = "ml_usb_interrupt_recv"
external ml_usb_interrupt_send : device_handle * endpoint * int * string * int * int * (int result -> unit) -> transfer = "ml_usb_interrupt_send"
external ml_usb_control_recv : device_handle * endpoint * int * string * int * int * (int result -> unit) * recipient * request_type * request * int * int -> transfer = "ml_usb_control_recv"
external ml_usb_control_send : device_handle * endpoint * int * string * int * int * (int result -> unit) * recipient * request_type * request * int * int -> transfer = "ml_usb_control_send"
external ml_usb_iso_recv : device_handle * endpoint * int * string * int * int * (int result list result -> unit) * int * int list -> transfer = "ml_usb_iso_recv"
external ml_usb_iso_send : device_handle * endpoint * int * string * int * int * (int result list result -> unit) * int * int list -> transfer = "ml_usb_iso_send"
external ml_usb_cancel_transfer : transfer -> unit = "ml_usb_cancel_transfer"

external ml_usb_claim_interface_job : device_handle -> interface -> [ `claim_interface ] job = "ml_usb_claim_interface_job"
external ml_usb_claim_interface_result : [ `claim_interface ] job -> unit = "ml_usb_claim_interface_result"
external ml_usb_claim_interface_free : [ `claim_interface ] job -> unit = "ml_usb_claim_interface_free"

external ml_usb_release_interface_job : device_handle -> interface -> [ `release_interface ] job = "ml_usb_release_interface_job"
external ml_usb_release_interface_result : [ `release_interface ] job -> unit = "ml_usb_release_interface_result"
external ml_usb_release_interface_free : [ `release_interface ] job -> unit = "ml_usb_release_interface_free"

external ml_usb_get_configuration_job : device_handle -> [ `get_configuration ] job = "ml_usb_get_configuration_job"
external ml_usb_get_configuration_result : [ `get_configuration ] job -> configuration = "ml_usb_get_configuration_result"
external ml_usb_get_configuration_free : [ `get_configuration ] job -> unit = "ml_usb_get_configuration_free"

external ml_usb_set_configuration_job : device_handle -> configuration -> [ `set_configuration ] job = "ml_usb_set_configuration_job"
external ml_usb_set_configuration_result : [ `set_configuration ] job -> unit = "ml_usb_set_configuration_result"
external ml_usb_set_configuration_free : [ `set_configuration ] job -> unit = "ml_usb_set_configuration_free"

external ml_usb_set_interface_alt_setting_job : device_handle -> interface -> int -> [ `set_interface_alt_setting ] job = "ml_usb_set_interface_alt_setting_job"
external ml_usb_set_interface_alt_setting_result : [ `set_interface_alt_setting ] job -> unit = "ml_usb_set_interface_alt_setting_result"
external ml_usb_set_interface_alt_setting_free : [ `set_interface_alt_setting ] job -> unit = "ml_usb_set_interface_alt_setting_free"

external ml_usb_clear_halt_job : device_handle -> endpoint -> [ `clear_halt ] job = "ml_usb_clear_halt_job"
external ml_usb_clear_halt_result : [ `clear_halt ] job -> unit = "ml_usb_clear_halt_result"
external ml_usb_clear_halt_free : [ `clear_halt ] job -> unit = "ml_usb_clear_halt_free"

external ml_usb_reset_device_job : device_handle -> [ `reset_device ] job = "ml_usb_reset_device_job"
external ml_usb_reset_device_result : [ `reset_device ] job -> unit = "ml_usb_reset_device_result"
external ml_usb_reset_device_free : [ `reset_device ] job -> unit = "ml_usb_reset_device_free"

(* +-----------------------------------------------------------------+
   | Lwt integration                                                 |
   +-----------------------------------------------------------------+ *)

let timeout_fired = ref false
let timeout_event = ref Lwt_engine.fake_event

let enter_iter () =
  timeout_fired := false;
  let timeout = ml_usb_get_next_timeout () in
  if timeout >= 0. then
    timeout_event := Lwt_engine.on_timer timeout false (fun _ -> timeout_fired := true)

let leave_iter () =
  Lwt_engine.stop_event !timeout_event;
  if !timeout_fired then ml_usb_handle_events ()

let events = Hashtbl.create 42

let insert_pollfd fd check_readable check_writable =
  let acc = [] in
  let acc = if check_readable then Lwt_engine.on_readable fd (fun _ -> ml_usb_handle_events ()) :: acc else acc in
  let acc = if check_writable then Lwt_engine.on_writable fd (fun _ -> ml_usb_handle_events ()) :: acc else acc in
  Hashtbl.add events fd acc

let remove_pollfd fd =
  List.iter Lwt_engine.stop_event (Hashtbl.find events fd);
  Hashtbl.remove events fd

let () =
  Callback.register "ocaml-usb:insert-pollfd" insert_pollfd;
  Callback.register "ocaml-usb:remove-pollfd" remove_pollfd

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

(* Every function of this module must take care of forcing this before
   doing anything: *)
let init = lazy(
  (* Initializes libusb. *)
  ml_usb_init ();
  (* Integrate libusb timoeuts into lwt. *)
  ignore (Lwt_sequence.add_r enter_iter Lwt_main.enter_iter_hooks);
  ignore (Lwt_sequence.add_r leave_iter Lwt_main.leave_iter_hooks);
  (* Cleanup libusb on exit. *)
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

(* +-----------------------------------------------------------------+
   | Device handling and enumeration                                 |
   +-----------------------------------------------------------------+ *)

let get_device_list () =
  Lazy.force init;
  ml_usb_get_device_list ()

let make_handle device_handle = {
  handle = device_handle;
  state = State_ok;
  mutex = Lwt_mutex.create ();
}

(* Check that a handle is valid. It must be called before using a
   handle. *)
let check_handle handle =
  if handle.state = State_closed then failwith "device handle closed"

let detach handle job result free =
  Lwt_mutex.with_lock handle.mutex begin fun () ->
       check_handle handle;
       handle.state <- State_detach;
       Lwt.finalize
         (fun () -> execute_job ?async_method:None ~job:(job ()) ~result ~free)
         (fun () ->
            if handle.state = State_detach
            then handle.state <- State_ok;
            Lwt.return ())
  end

let get_bus_number = ml_usb_get_bus_number
let get_device_address = ml_usb_get_device_address
let get_max_packet_size ~device ~direction ~endpoint = ml_usb_get_max_packet_size device direction endpoint
let open_device device = make_handle (ml_usb_open device)

let close handle =
  if handle.state <> State_closed then begin
    handle.state <- State_closed;
    ml_usb_close handle.handle
  end

let get_device handle = ml_usb_get_device handle.handle

let claim_interface handle interface =
  detach
    handle
    (fun () -> ml_usb_claim_interface_job handle.handle interface)
    ml_usb_claim_interface_result
    ml_usb_claim_interface_free

let release_interface handle interface =
  detach
    handle
    (fun () -> ml_usb_release_interface_job handle.handle interface)
    ml_usb_release_interface_result
    ml_usb_release_interface_free

let kernel_driver_active handle =
  check_handle handle;
  ml_usb_kernel_driver_active handle.handle

let detach_kernel_driver handle =
  check_handle handle;
  ml_usb_detach_kernel_driver handle.handle

let attach_kernel_driver handle =
  check_handle handle;
  ml_usb_attach_kernel_driver handle.handle

let get_configuration handle =
  detach
    handle
    (fun () -> ml_usb_get_configuration_job handle.handle)
    ml_usb_get_configuration_result
    ml_usb_get_configuration_free

let set_configuration handle configuration =
  detach
    handle
    (fun () -> ml_usb_set_configuration_job handle.handle configuration)
    ml_usb_set_configuration_result
    ml_usb_set_configuration_free

let set_interface_alt_setting handle interface alt_setting =
  detach
    handle
    (fun () -> ml_usb_set_interface_alt_setting_job handle.handle interface alt_setting)
    ml_usb_set_interface_alt_setting_result
    ml_usb_set_interface_alt_setting_free

let clear_halt handle endpoint =
  detach
    handle
    (fun () -> ml_usb_clear_halt_job handle.handle endpoint)
    ml_usb_clear_halt_result
    ml_usb_clear_halt_free

let reset_device handle =
  detach
    handle
    (fun () -> ml_usb_reset_device_job handle.handle)
    ml_usb_reset_device_result
    ml_usb_reset_device_free

let open_device_with ~vendor_id ~product_id =
  Lazy.force init;
  match ml_usb_open_device_with_vid_pid vendor_id product_id with
    | Some device_handle -> make_handle device_handle
    | None -> failwith (Printf.sprintf "no such usb device (vendor-id=0x%04x, product-id=0x%04x)" vendor_id product_id)

(* +-----------------------------------------------------------------+
   | USB descriptors                                                 |
   +-----------------------------------------------------------------+ *)

module Class = struct
  type t =
    | Per_interface
    | Audio
    | Communication
    | Hid
    | Physical
    | Printer
    | Image
    | Mass_storage
    | Hub
    | Data
    | Smart_card
    | Content_security
    | Video
    | Personal_healthcare
    | Audio_video_device
    | Billboard_device_class
    | Usb_type_c_bridge_class
    | Diagnostic_device
    | Wireless_controler
    | Misc
    | Application_specific
    | Vendor_specific
  [@@deriving sexp]

  let to_int = function
    | Per_interface -> 0x00
    | Audio -> 0x01
    | Communication -> 0x02
    | Hid -> 0x03
    | Physical -> 0x05
    | Printer -> 0x06
    | Image -> 0x07
    | Mass_storage -> 0x08
    | Hub -> 0x09
    | Data -> 0x0a
    | Smart_card -> 0x0b
    | Content_security -> 0x0d
    | Video -> 0x0e
    | Personal_healthcare -> 0x0f
    | Audio_video_device -> 0x10
    | Billboard_device_class -> 0x11
    | Usb_type_c_bridge_class -> 0x12
    | Diagnostic_device -> 0xdc
    | Wireless_controler -> 0xe0
    | Misc -> 0xef
    | Application_specific -> 0xfe
    | Vendor_specific -> 0xff

  let of_int = function
    | 0x00 -> Per_interface
    | 0x01 -> Audio
    | 0x02 -> Communication
    | 0x03 -> Hid
    | 0x05 -> Physical
    | 0x06 -> Image
    | 0x07 -> Printer
    | 0x08 -> Mass_storage
    | 0x09 -> Hub
    | 0x0a -> Data
    | 0x0b -> Smart_card
    | 0x0d -> Content_security
    | 0x0e -> Video
    | 0x0f -> Personal_healthcare
    | 0x10 -> Audio_video_device
    | 0x11 -> Billboard_device_class
    | 0x12 -> Usb_type_c_bridge_class
    | 0xdc -> Diagnostic_device
    | 0xe0 -> Wireless_controler
    | 0xef -> Misc
    | 0xfe -> Application_specific
    | 0xff -> Vendor_specific
    | _ -> invalid_arg "Class.of_int"
end

type device_descriptor = {
  usb : int;
  device_class : Class.t;
  device_sub_class : int;
  device_protocol : int;
  max_packet_size : int;
  vendor_id : int;
  product_id : int;
  device : int;
  index_manufacturer : int;
  index_product : int;
  index_serial_number : int;
  configurations : int;
} [@@deriving sexp]

module Endpoint = struct
  type direction = Input | Output
  let direction_of_int i = if i <= 1 lsl 7 then Input else Output

  type transfer =
    | Control
    | Bulk
    | Interrupt
    | Isochronous of synchronisation * usage
  and synchronisation = No_sync | Async | Adaptative | Sync
  and usage = Data | Feedback | Explicit | Reserved
  [@@deriving sexp]

  let transfer_of_int i =
    match i land 0x03 with
    | 0 -> Control
    | 2 -> Bulk
    | 3 -> Interrupt
    | 1 -> begin
      let sync = match (i lsr 2) land 0x03 with
        | 0 -> No_sync
        | 1 -> Async
        | 2 -> Adaptative
        | 3 -> Sync
        | _ -> assert false in
      let usage = match (1 lsr 4) land 0x03 with
        | 0 -> Data
        | 1 -> Feedback
        | 2 -> Explicit
        | 3 -> Reserved
        | _ -> assert false in
      Isochronous (sync, usage)
    end
    | _ -> assert false

  type raw_descriptor = {
    endpoint_address : int;
    attributes : int;
    max_packet_size : int;
    interval : int;
    refresh : int;
    sync_address : int;
  } [@@deriving sexp]

  type descriptor = {
    address : int ;
    transfer : transfer ;
    max_packet_size : int ;
    interval : int ;
    refresh : int ;
    sync_address : int ;
  } [@@deriving sexp]

  let descriptor_of_raw r = {
    address = r.endpoint_address land 0x07 ;
    transfer = transfer_of_int r.attributes ;
    max_packet_size = r.max_packet_size ;
    interval = r.interval ;
    refresh = r.refresh ;
    sync_address = r.sync_address ;
  }
end

type interface_descriptor = {
  interface : int;
  alternate_setting : int;
  interface_class : Class.t;
  interface_sub_class : int;
  interface_protocol : int;
  index_interface : int;
  endpoints : Endpoint.raw_descriptor array;
} [@@deriving sexp]

type config_descriptor = {
  configuration_value : int;
  index_configuration : int;
  attributes : int;
  max_power : int;
  interfaces : interface_descriptor array array;
} [@@deriving sexp]

external get_device_descriptor : device -> device_descriptor = "ml_usb_get_device_descriptor"
external get_active_config_descriptor : device -> config_descriptor = "ml_usb_get_active_config_descriptor"
external get_config_descriptor : device -> int -> config_descriptor = "ml_usb_get_config_descriptor"
external get_config_descriptor_by_value : device -> int -> config_descriptor = "ml_usb_get_config_descriptor_by_value"

module DT =
struct
  type t = int
  let device = 0x01
  let config = 0x02
  let string = 0x03
  let interface = 0x04
  let endpoint = 0x05
  let hid = 0x21
  let report = 0x22
  let physical = 0x23
  let hub = 0x2
end

(* +-----------------------------------------------------------------+
   | IOs                                                             |
   +-----------------------------------------------------------------+ *)

(* Handle the result of a transfer *)
let handle_result func_name w = function
  | OK x -> Lwt.wakeup w x
  | Error error -> Lwt.wakeup_exn w (Transfer(error, func_name))

let make_timeout = function
  | None -> 0
  | Some t -> Pervasives.truncate (t *. 1000.0)

let transfer name func ~handle ~endpoint ?timeout buffer offset length =
  check_handle handle;
  if offset < 0 || length < 0 || offset > String.length buffer - length then invalid_arg ("USB." ^ name);
  let waiter, wakener = Lwt.task () in
  let transfer = func (handle.handle, endpoint, make_timeout timeout, buffer, offset, length, handle_result name wakener) in
  Lwt.on_cancel waiter (fun () -> ml_usb_cancel_transfer transfer);
  waiter

let bulk_recv = transfer "bulk_recv" ml_usb_bulk_recv
let bulk_send = transfer "bulk_send" ml_usb_bulk_send
let interrupt_recv = transfer "interrupt_recv" ml_usb_interrupt_recv
let interrupt_send = transfer "interrupt_send" ml_usb_interrupt_send

let control_transfer name func ~handle ~endpoint ?timeout ?(recipient=Device) ?(request_type=Standard) ~request ~value ~index buffer offset length =
  check_handle handle;
  if offset < 0 || length < 0 || offset > String.length buffer - length then invalid_arg ("USB." ^ name);
  let waiter, wakener = Lwt.task () in
  let transfer = func (handle.handle, endpoint, make_timeout timeout, buffer, offset, length, handle_result name wakener, recipient, request_type, request, value, index) in
  Lwt.on_cancel waiter (fun () -> ml_usb_cancel_transfer transfer);
  waiter

let control_recv = control_transfer "control_recv" ml_usb_control_recv
let control_send = control_transfer "control_send" ml_usb_control_send

let handle_iso_result func_name w = function
  | OK l -> Lwt.wakeup w (List.rev_map (function
                                          | OK x -> Iso_ok x
                                          | Error error -> Iso_error(error, func_name)) l)
  | Error error -> Lwt.wakeup_exn w (Transfer(error, func_name))

let iso_transfer name func ~handle ~endpoint ?timeout buffer offset lengths =
  check_handle handle;
  if lengths = [] then
    Lwt.return []
  else begin
    List.iter (fun length -> if length < 0 then invalid_arg ("USB." ^ name)) lengths;
    let length = List.fold_left (+) 0 lengths in
    if offset < 0 || offset > String.length buffer - length then invalid_arg ("USB." ^ name);
    let waiter, wakener = Lwt.task () in
    let transfer = func (handle.handle, endpoint, make_timeout timeout, buffer, offset, length, handle_iso_result name wakener, List.length lengths, lengths) in
    Lwt.on_cancel waiter (fun () -> ml_usb_cancel_transfer transfer);
    waiter
  end

let iso_recv = iso_transfer "iso_recv" ml_usb_iso_recv
let iso_send = iso_transfer "iso_send" ml_usb_iso_send

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

(* +-----------------------------------------------------------------+
   | Helpers                                                         |
   +-----------------------------------------------------------------+ *)

let get_string_descriptor ?timeout ?lang_id handle index =
  let data = Bytes.create 255 in
  begin match lang_id with
    | Some lang_id ->
      Lwt.return lang_id
    | None ->
      (* Guess the default language id *)
      control_recv
        ~handle
        ~endpoint:0
        ?timeout
        ~request:Request.get_descriptor
        ~value:(DT.string lsl 8)
        ~index:0
        data 0 (String.length data) >>= fun n ->
      if n < 4 then
        Lwt.fail (Failure "USB.get_string_descriptor: cannot retreive default lang id")
      else
        Lwt.return (Char.code data.[2] lor (Char.code data.[3] lsl 8))
  end >>= fun lang_id ->
  control_recv
    ~handle
    ~endpoint:0
    ?timeout
    ~request:Request.get_descriptor
    ~value:(DT.string lsl 8 lor index)
    ~index:lang_id
    data 0 (String.length data) >>= fun n ->
  let len = Char.code data.[0] in
  if Char.code data.[1] <> DT.string || len > n then
    Lwt.fail (Failure "USB.get_string_descriptor: invalid control packet")
  else
    Lwt.return (String.sub data 2 (len - 2))
