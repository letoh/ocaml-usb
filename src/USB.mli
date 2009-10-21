(*
 * USB.mli
 * -------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

(** Module for USB communication *)

(** {6 General errors} *)

(** Any function of this module may raise one of the following
    errors: *)
type error =
  | Error_io
      (** Error on IOs *)

  | Error_invalid_param
      (** Invalid parameter. If this error is raised, then there is a
          bug in ocaml-usb. Please fill a bug report in this case. *)

  | Error_access
      (** Access denied to a peripheral *)

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

exception Error of error * string
  (** [Error(error, msg)] is raised when libusb returns an
      error. [msg] is a human readable description of the error. *)

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
      endpoint in the active device configuration. *)

(** {6 Device use} *)

type handle
  (** A handle allows you to perform I/O on the device in question. *)

type interface = int
    (** An interface number on a device *)

val open_device : device -> handle
  (** Open a device and obtain a device handle.

      A handle allows you to perform I/O on the device in question. *)

val close : handle -> unit
  (** Close a previously opened device handle *)

val open_device_with : vendor_id : int -> product_id : int -> handle
  (** [open_device_with ~vendor_id ~product_id]

      Convenience function for finding a device with a particular
      idVendor/idProduct combination.

      @raise Failure if the device is not found. *)

val get_device : handle -> device
  (** Get the underlying device for a handle *)

val kernel_driver_active : handle -> interface -> bool
  (** Determine if a kernel driver is active on an interface.

      If a kernel driver is active, you cannot claim the interface,
      and libusb will be unable to perform I/O. *)

val detach_kernel_driver : handle -> interface -> unit
  (** Detach a kernel driver from an interface.

      If successful, you will then be able to claim the interface and
      perform I/O. *)

val attach_kernel_driver : handle -> interface -> unit
  (** Re-attach an interface's kernel driver, which was previously
      detached using {!detach_kernel_driver}. *)

val claim_interface : handle -> interface -> unit Lwt.t
  (** [claim_interface handle interface_number]

      Claim an interface on a given device handle.

      You must claim the interface you wish to use before you can
      perform I/O on any of its endpoints. *)

val release_interface : handle -> interface -> unit Lwt.t
  (** Release an interface previously claimed with libusb_claim_interface().

      You should release all claimed interfaces before closing a
      device handle.

      This is a blocking function. A [SET_INTERFACE] control request
      will be sent to the device, resetting interface state to the
      first alternate setting. *)

type configuration = int
    (** A device configuration *)

val get_configuration : handle -> configuration Lwt.t
  (** [get_configuration handle] returns the current configuration of
      a device *)

val set_configuration : handle -> configuration -> unit Lwt.t
  (** [set_configuration handle conf] change the current configuration
      of a device *)

val clear_halt : handle -> endpoint -> unit Lwt.t
  (** [clear_halt handle endpoint] clears the halt/stall condition for
      an endpoint. *)

val reset_device : handle -> unit Lwt.t
  (** [reset_device handle] reset the given device *)

(** {6 IOs} *)

(** {8 Errors} *)

(** Transfers may fails with any of the following error: *)
type transfer_error =
  | Transfer_error
      (** Transfer failed *)

  | Transfer_timed_out
      (** Transfer timed out *)

  | Transfer_cancelled
      (** Transfer was cancelled *)

  | Transfer_stall
      (** For bulk/interrupt endpoints: halt condition detected
          (endpoint stalled). For control endpoints: control request not
          supported. *)

  | Transfer_no_device
      (** Device was disconnected *)

  | Transfer_overflow
      (** Device sent more data than requested *)

exception Transfer_error of transfer_error * string
  (** Exception raised when a transfer fail. The second argument is a
      human description of the error. *)

(** {8 Bulk transfers} *)

val bulk_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  string -> int -> int -> int Lwt.t
  (** [bulk_recv ~handle ~endpoint ?timeout buffer offset length] *)

val bulk_send :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  string -> int -> int -> int Lwt.t
  (** [bulk_send ~handle ~endpoint ?timeout buffer offset length] *)

(** {8 Interrupt transfers} *)

val interrupt_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  string -> int -> int -> int Lwt.t
  (** [interrupt_recv ~handle ~endpoint ?timeout buffer offset length] *)

val interrupt_send :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  string -> int -> int -> int Lwt.t
  (** [interrupt_send ~handle ~endpoint ?timeout buffer offset length] *)

(** {8 Isochronous transfers} *)

(** TODO *)

(** {8 Control transfers} *)

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
  ?timeout : float ->
  recipient : recipient ->
  request_type : request_type ->
  request : request ->
  value : int ->
  index : int ->
  string -> int -> int -> int Lwt.t

val control_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  recipient : recipient ->
  request_type : request_type ->
  request : request ->
  value : int ->
  index : int ->
  string -> int -> int -> int Lwt.t

(** Standard requests *)
module Request : sig
  type t = request

  val get_status : t
    (** Request status of the specific recipient *)

  val clear_feature : t
    (** Clear or disable a specific feature *)

  val set_feature : t
    (** Set or enable a specific feature *)

  val set_address : t
    (** Set device address for all future accesses *)

  val get_descriptor : t
    (** Get the specified descriptor *)

  val set_descriptor : t
    (** Used to update existing descriptors or add new descriptors *)

  val get_configuration : t
    (** Get the current device configuration value *)

  val set_configuration : t
    (** Set device configuration *)

  val get_interface : t
    (** Return the selected alternate setting for the specified interface *)

  val set_interface : t
    (** Select an alternate interface for the specified interface *)

  val synch_frame : t
    (** Set then report an endpoint's synchronization frame *)
end
