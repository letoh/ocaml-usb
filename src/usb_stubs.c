/*
 * usb_stubs.c
 * -----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <libusb.h>

/* +--------+
   | Errors |
   +--------+ */

int error_num_of_code(int code)
{
  switch (code) {
  case LIBUSB_ERROR_IO:
    return 0;
  case LIBUSB_ERROR_INVALID_PARAM:
    return 1;
  case LIBUSB_ERROR_ACCESS:
    return 2;
  case LIBUSB_ERROR_NO_DEVICE:
    return 3;
  case LIBUSB_ERROR_NOT_FOUND:
    return 4;
  case LIBUSB_ERROR_BUSY:
    return 5;
  case LIBUSB_ERROR_TIMEOUT:
    return 6;
  case LIBUSB_ERROR_OVERFLOW:
    return 7;
  case LIBUSB_ERROR_PIPE:
    return 8;
  case LIBUSB_ERROR_INTERRUPTED:
    return 9;
  case LIBUSB_ERROR_NO_MEM:
    return 10;
  case LIBUSB_ERROR_NOT_SUPPORTED:
    return 11;
  default:
    return 12;
  }
}

/* If code != 0, raise [USB.Error(fun_name, err)] */
void check_error(int code, char *fun_name)
{
  if (code) {
    value args[2];
    args[0] = caml_copy_string(fun_name);
    args[1] = Val_int(error_num_of_code(code));
    caml_raise_with_args(*caml_named_value("ocaml-usb error"), 2, args);
  }
}

#define check(code, fun_name) check_error((int)(code), (fun_name))

/* +----------------+
   | Initialization |
   +----------------+ */

void ml_usb_init()
{
  int res = libusb_init(NULL);
  check(res, "init");
}

void ml_usb_exit()
{
  libusb_exit(NULL);
}

void ml_usb_set_debug(value level)
{
  libusb_set_debug(NULL, Int_val(level));
}

/* +-------------------------+
   | Device and enumerations |
   +-------------------------+ */

#define Ptr_val(v) ((long)(*(void**)Data_custom_val(v)))

int ml_usb_compare(value v1, value v2)
{
  return (int)(Ptr_val(v1) - Ptr_val(v2));
}

long ml_usb_hash(value v)
{
  return Ptr_val(v);
}

#define Device_val(v) *(libusb_device**)Data_custom_val(v)

void ml_usb_device_finalize(value dev)
{
  libusb_unref_device(Device_val(dev));
}

static struct custom_operations device_ops = {
  "usb.device",
  ml_usb_device_finalize,
  ml_usb_compare,
  ml_usb_hash,
  custom_serialize_default,
  custom_deserialize_default
};

#define Handle_val(v) *(libusb_device_handle**)Data_custom_val(v)

void ml_usb_device_handle_funalize(value handle)
{
  libusb_close(Handle_val(handle));
}

static struct custom_operations handle_ops = {
  "usb.device.handle",
  ml_usb_device_handle_funalize,
  ml_usb_compare,
  ml_usb_hash,
  custom_serialize_default,
  custom_deserialize_default
};

value alloc_device(libusb_device *device)
{
  value x = caml_alloc_custom(&device_ops, sizeof(libusb_device*), 0, 1);
  Device_val(x) = device;
  return x;
}

value alloc_handle(libusb_device_handle *handle)
{
  value x = caml_alloc_custom(&handle_ops, sizeof(libusb_device_handle*), 0, 1);
  Handle_val(x) = handle;
  return x;
}

CAMLprim value ml_usb_get_device_list(value unit)
{
  CAMLparam1(unit);
  libusb_device **devices;

  size_t cnt = libusb_get_device_list(NULL, &devices);
  if (cnt < 0)
    check_error(cnt, "get_device_list");

  /* Convert the array to a caml list */
  size_t i;
  value x = Val_int(0);
  for (i = 0; i < cnt; i++) {
    value y = caml_alloc_tuple(2);
    Store_field(y, 0, alloc_device(devices[i]));
    Store_field(y, 1, x);
    x = y;
  }

  /* Free the list but not the devices */
  libusb_free_device_list(devices, 0);

  CAMLreturn(x);
}

value ml_usb_get_bus_number(value dev)
{
  return Val_int(libusb_get_bus_number(Device_val(dev)));
}

value ml_usb_get_device_address(value dev)
{
  return Val_int(libusb_get_device_address(Device_val(dev)));
}

value ml_usb_get_max_packet_size(value dev, value endpoint)
{
  int res = libusb_get_max_packet_size(Device_val(dev), Int_val(endpoint));
  check(res, "get_max_packet_size");
  return Val_int(res);
}

CAMLprim value ml_usb_open(value dev)
{
  CAMLparam1(dev);
  libusb_device_handle *handle = NULL;
  int res = libusb_open(Device_val(dev), &handle);
  check(res, "open");
  CAMLreturn(alloc_handle(handle));
}

CAMLprim value ml_usb_open_device_with_vid_pid(value vid, value pid)
{
  CAMLparam2(vid, pid);
  libusb_device_handle *handle = libusb_open_device_with_vid_pid(NULL, Int_val(vid), Int_val(pid));
  if (handle == NULL)
    CAMLreturn(Val_int(0));
  else {
    value some = caml_alloc_tuple(1);
    Store_field(some, 0, alloc_handle(handle));
    CAMLreturn(some);
  }
}

value ml_usb_close(value handle)
{
  libusb_close(Handle_val(handle));
  return Val_unit;
}

CAMLprim value ml_usb_get_device(value handle)
{
  CAMLparam1(handle);
  libusb_device *device = libusb_get_device(Handle_val(handle));
  libusb_ref_device(device);
  CAMLreturn(alloc_device(device));
}

value ml_usb_claim_interface(value handle, value interface)
{
  int res = libusb_claim_interface(Handle_val(handle), Int_val(interface));
  check(res, "claim_interface");
  return Val_unit;
}

value ml_usb_release_interface(value handle, value interface)
{
  int res = libusb_release_interface(Handle_val(handle), Int_val(interface));
  check(res, "release_interface");
  return Val_unit;
}


