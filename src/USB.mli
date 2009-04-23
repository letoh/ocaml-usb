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

(** {6 Miscellanies} *)

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

val get_max_packet_size : device -> int -> int
  (** [get_max_packet_size device endpoint] Convenience function to
      retrieve the [wMaxPacketSize] value for a particular endpoint in
      the active device configuration.

      This is usually 64. *)

(** {6 Device use} *)

type device_handle
  (** A handle allows you to perform I/O on the device in question. *)

val open_device : device -> device_handle
  (** Open a device and obtain a device handle.

      A handle allows you to perform I/O on the device in question. *)

val open_device_with : vendor_id : int -> product_id : int -> device_handle option
  (** [open_device_with ~vendor_id ~product_id]

      Convenience function for finding a device with a particular
      idVendor/idProduct combination.

      This function has limitations and is hence not intended for use
      in real applications: if multiple devices have the same IDs it
      will only give you the first one, etc. *)

val get_device : device_handle -> device
  (** Get the underlying device for a handle *)

val claim_interface : device_handle -> int -> unit
  (** [claim_interface handle interface_number]

      Claim an interface on a given device handle.

      You must claim the interface you wish to use before you can
      perform I/O on any of its endpoints. *)

val release_interface : device_handle -> int -> unit
  (** Release an interface previously claimed with libusb_claim_interface().

      You should release all claimed interfaces before closing a
      device handle.

      This is a blocking function. A [SET_INTERFACE] control request
      will be sent to the device, resetting interface state to the
      first alternate setting. *)
