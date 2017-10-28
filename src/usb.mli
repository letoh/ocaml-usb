(*
 * USB.mli
 * -------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

(** Module for USB communication *)

val handle_error : ('a -> 'b) -> 'a -> 'b
  (** [handle_unix_error f x] applies [f] to [x] and returns the
      result.  If the exception {!Error} or {!Transport} is raised, it
      prints a message describing the error and exits with code 2. *)

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
  (** [Error(error, func_name)] is raised when libusb returns an
      error. [func_name] is a the name of the function which
      failed. *)

val error_message : error -> string
  (** [error_message error] returns a human readable description of
      the error *)

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

val set_interface_alt_setting : handle -> interface -> int -> unit Lwt.t
  (** [set_interface_alt_setting handle interface alternate_setting]
      activates an alternate setting for an interface.  *)

val clear_halt : handle -> endpoint -> unit Lwt.t
  (** [clear_halt handle endpoint] clears the halt/stall condition for
      an endpoint. *)

val reset_device : handle -> unit Lwt.t
  (** [reset_device handle] reset the given device *)

(** {6 USB descriptors} *)

(** Device class codes *)
module Class : sig
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

  val to_int : t -> int
  val of_int : int -> t
end

type device_descriptor = {
  usb : int;
  (** USB specification release number in binary-coded decimal.

      A value of 0x0200 indicates USB 2.0, 0x0110 indicates USB 1.1,
      etc. *)

  device_class : Class.t;
  (** USB-IF class code for the device. *)

  device_sub_class : int;
  (** USB-IF subclass code for the device, qualified by the
      [device_class] value. *)

  device_protocol : int;
  (** USB-IF protocol code for the device, qualified by the
      [device_class] and [device_subclass] values. *)

  max_packet_size : int;
  (** Maximum packet size for endpoint 0. *)

  vendor_id : int;
  (** USB-IF vendor ID. *)

  product_id : int;
  (** USB-IF product ID. *)

  device : int;
  (** Device release number in binary-coded decimal. *)

  index_manufacturer : int;
  (** Index of string descriptor describing manufacturer. *)

  index_product : int;
  (** Index of string descriptor describing product. *)

  index_serial_number : int;
  (** Index of string descriptor containing device serial number. *)

  configurations : int;
  (** Number of possible configurations. *)
} [@@deriving sexp]

val get_device_descriptor : device -> device_descriptor
  (** Get the USB device descriptor for a given device. *)

module Endpoint : sig
  type raw_descriptor = {
    endpoint_address : int;
    (** The address of the endpoint described by this descriptor. *)

    attributes : int;
    (** Attributes which apply to the endpoint when it is configured
        using the {!cd_configuration_value}. *)

    max_packet_size : int;
    (** Maximum packet size this endpoint is capable of
        sending/receiving. *)

    interval : int;
    (** Interval for polling endpoint for data transfers. *)

    refresh : int;
    (** For audio devices only: the rate at which synchronization
        feedback is provided. *)

    sync_address : int;
    (** For audio devices only: the address if the synch endpoint. *)
  } [@@deriving sexp]

  type direction = Input | Output
  type transfer =
    | Control
    | Bulk
    | Interrupt
    | Isochronous of synchronisation * usage
  and synchronisation = No_sync | Async | Adaptative | Sync
  and usage = Data | Feedback | Explicit | Reserved

  type descriptor = {
    address : int ;
    transfer: transfer ;
    max_packet_size : int ;
    interval : int ;
    refresh : int ;
    sync_address : int ;
  } [@@deriving sexp]

  val descriptor_of_raw : raw_descriptor -> descriptor
end

type interface_descriptor = {
  interface : int;
  (** Number of this interface. *)

  alternate_setting : int;
  (** Value used to select this alternate setting for this
      interface. *)

  interface_class : Class.t;
  (** USB-IF class code for this interface. *)

  interface_sub_class : int;
  (** USB-IF subclass code for this interface, qualified by the
      [interface_class] value. *)

  interface_protocol : int;
  (** USB-IF protocol code for this interface, qualified by the
      [interface_class] and [interface_sub_class] values. *)

  index_interface : int;
  (** Index of string descriptor describing this interface. *)

  endpoints : Endpoint.raw_descriptor array;
  (** Array of endpoint descriptors. *)
} [@@deriving sexp]

type config_descriptor = {
  configuration_value : int;
  (** Identifier value for this configuration *)

  index_configuration : int;
  (** Index of string descriptor describing this configuration. *)

  attributes : int;
  (** A bitmask, representing configuration characteristics. *)

  max_power : int;
  (** Maximum power consumption of the USB device from this bus in
      this configuration when the device is fully opreation.

      Expressed in units of 2 mA. *)

  interfaces : interface_descriptor array array;
  (** Array of interfaces supported by this configuration.

      [interface.(iface).(altsetting)] designate the interface
      descriptor for interface [iface] with alternate setting
      [altsetting]. *)
} [@@deriving sexp]

val get_active_config_descriptor : device -> config_descriptor
  (** Get the USB configuration descriptor for the currently active
      configuration. *)

val get_config_descriptor : device -> int -> config_descriptor
  (** Get a USB configuration descriptor based on its index. *)

val get_config_descriptor_by_value : device -> int -> config_descriptor
  (** Get a USB configuration descriptor with a specific
      [configuration_value]. *)

val get_string_descriptor : ?timeout : float -> ?lang_id : int -> handle -> int -> string Lwt.t
  (** Retrieve a string descriptor from a device. *)

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

exception Transfer of transfer_error * string
  (** [Transfer(error, func_name)] Exception raised when a transfer
      fail. *)

val transfer_error_message : transfer_error -> string
  (** [transfer_error_message error] *)

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

(** Result of the transfer of one packet in an isochronous
    transfer: *)
type iso_result =
  | Iso_ok of int
      (** The packet has been transfered successfully *)
  | Iso_error of transfer_error * string
      (** [Iso_error(error, func_name)] An error occured *)

val iso_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  string -> int -> int list -> iso_result list Lwt.t

val iso_send :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  string -> int -> int list -> iso_result list Lwt.t

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
  ?recipient : recipient ->
  ?request_type : request_type ->
  request : request ->
  value : int ->
  index : int ->
  string -> int -> int -> int Lwt.t
  (** Sends a control packet.

      @param recipient defaults to {!Device}
      @param request_type defaults to {!Standard} *)

val control_recv :
  handle : handle ->
  endpoint : endpoint ->
  ?timeout : float ->
  ?recipient : recipient ->
  ?request_type : request_type ->
  request : request ->
  value : int ->
  index : int ->
  string -> int -> int -> int Lwt.t
  (** Receives a control packet.

      @param recipient defaults to {!Device}
      @param request_type defaults to {!Standard} *)

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
