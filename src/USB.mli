(*
 * USB.mli
 * -------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

(** Module for USB communication *)

(** {6 Errors} *)

type error =
  | Error_io
      (** Input/output error *)
  | Error_invalid_param
      (** Invalid parameter *)
  | Error_access
      (** Access denied (insufficient permissions) *)
  | Error_no_device
      (** No such device (it may have been disconnected) *)
  | Error_not_found
      (** Entity not found *)
  | Error_busy
      (** Resource busy *)
  | Error_timeout
      (** Operation timed out *)
  | Error_overflow
      (** Overflow *)
  | Error_pipe
      (** Pipe error *)
  | Error_interrupted
      (** System call interrupted (perhaps due to signal) *)
  | Error_no_mem
      (** Insufficient memory *)
  | Error_not_supported
      (** Operation not supported or unimplemented on this platform *)
  | Error_other
      (** Other error *)

exception Error of string * error
  (** [Error(fun_name, error)] exception raised when an error
      happen. [fun_name] is the function name which raised the
      exception and [error] is the error itself. *)

(** {6 Types} *)

(** A USB endpoint direction *)
type direction = In | Out

(** A USB endpoint number *)
type endpoint = int

(** {6 Miscellanies} *)

val init : unit Lazy.t
  (** When forced, [init] initialises libusb. This is automatically
      done so you do not need to do it manually. By the way you can do
      it to catch initialisation errors. *)

val set_debug : [ `quiet | `error | `warning | `verbose ] -> unit
  (** [set_debug level] set the debug level. *)

(** {6 Device informations} *)

type device
  (** Representation of a device description *)

val get_device_list : unit -> device list
  (** Returns a list of USB devices currently attached to the
      system. *)

val get_bus_number : device -> int
  (** Get the number of the bus that a device is connected to. *)

val get_device_address : device -> int
  (** Get the address of the device on the bus it is connected to. *)

val get_max_packet_size : device : device -> direction : direction -> endpoint : endpoint -> int
  (** [get_max_packet_size ~device ~direction ~endpoint] Convenience
      function to retrieve the [wMaxPacketSize] value for a particular
      endpoint in the active device configuration.

      This is usually 64. *)

(** {6 Device use} *)

type handle
  (** A handle allows you to perform I/O on the device in question. *)

val open_device : device -> handle
  (** Open a device and obtain a device handle.

      A handle allows you to perform I/O on the device in question. *)

val open_device_with : vendor_id : int -> product_id : int -> handle option
  (** [open_device_with ~vendor_id ~product_id]

      Convenience function for finding a device with a particular
      idVendor/idProduct combination.

      This function has limitations and is hence not intended for use
      in real applications: if multiple devices have the same IDs it
      will only give you the first one, etc. *)

val get_device : handle -> device
  (** Get the underlying device for a handle *)

val claim_interface : handle -> int -> unit
  (** [claim_interface handle interface_number]

      Claim an interface on a given device handle.

      You must claim the interface you wish to use before you can
      perform I/O on any of its endpoints. *)

val release_interface : handle -> int -> unit
  (** Release an interface previously claimed with libusb_claim_interface().

      You should release all claimed interfaces before closing a
      device handle.

      This is a blocking function. A [SET_INTERFACE] control request
      will be sent to the device, resetting interface state to the
      first alternate setting. *)

(** {6 IO} *)
(*
val bulk_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  buffer : string ->
  offset : int ->
  length : int -> int Lwt.t
  (** [bulk_recv ~handle ~endpoint ?timeout ~buffer ~offset ~length] *)

val bulk_send :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  buffer : string ->
  offset : int ->
  length : int -> int Lwt.t
  (** [bulk_send ~handle ~endpoint ?timeout ~buffer ~offset ~length] *)
*)
val interrupt_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  buffer : string ->
  offset : int ->
  length : int -> unit -> int Lwt.t
  (** [interrupt_recv ~handle ~endpoint ?timeout ~buffer ~offset ~length] *)

val interrupt_send :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  buffer : string ->
  offset : int ->
  length : int -> unit -> int Lwt.t
  (** [interrupt_send ~handle ~endpoint ?timeout ~buffer ~offset ~length] *)
(*
val iso_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  packet_count : int ->
  buffer : string ->
  offset : int ->
  length : int -> int Lwt.t
  (** [iso_recv ~handle ~endpoint ?timeout ~packet_count ~buffer ~offset ~length] *)

val iso_send :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  packet_count : int ->
  buffer : string ->
  offset : int ->
  length : int -> int Lwt.t
  (** [iso_send ~handle ~endpoint ?timeout ~packet_count ~buffer ~offset ~length] *)

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

val control_send :
  handle : handle ->
  endpoint : endpoint ->
  recipient : recipient ->
  request_type : request_type ->
  request : request ->
  value : int ->
  index : int ->
  buffer : string ->
  offset : int ->
  length : int -> unit Lwt.t

val control_recv :
  handle : handle ->
  endpoint : endpoint ->
  recipient : recipient ->
  request_type : request_type ->
  request : request ->
  value : int ->
  index : int ->
  buffer : string ->
  offset : int ->
  length : int -> unit Lwt.t

(** {6 Standard requests} *)

val request_get_status : request
  (** Request status of the specific recipient *)

val request_clear_feature : request
  (** Clear or disable a specific feature *)

val request_set_feature : request
  (** Set or enable a specific feature *)

val request_set_address : request
  (** Set device address for all future accesses *)

val request_get_descriptor : request
  (** Get the specified descriptor *)

val request_set_descriptor : request
  (** Used to update existing descriptors or add new descriptors *)

val request_get_configuration : request
  (** Get the current device configuration value *)

val request_set_configuration : request
  (** Set device configuration *)

val request_get_interface : request
  (** Return the selected alternate setting for the specified interface *)

val request_set_interface : request
  (** Select an alternate interface for the specified interface *)

val request_synch_frame : request
  (** Set then report an endpoint's synchronization frame *)
*)
