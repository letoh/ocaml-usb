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
#include <poll.h>
#include <string.h>
#include <stdio.h>

/* +--------+
   | Errors |
   +--------+ */

int ml_usb_error_num_of_code(int code)
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

void ml_usb_error(int code, char *fun_name)
{
  value args[2];
  args[0] = caml_copy_string(fun_name);
  args[1] = Val_int(ml_usb_error_num_of_code(code));
  caml_raise_with_args(*caml_named_value("ocaml-usb error"), 2, args);
}

void *ml_usb_malloc(size_t size)
{
  void *ptr = malloc(size);
  if (ptr == NULL) caml_failwith("ocaml-usb: out of memory");
  return ptr;
}

struct libusb_transfer *ml_usb_alloc_transfer(int count)
{
  struct libusb_transfer *transfer = libusb_alloc_transfer(count);
  if (transfer == NULL) caml_failwith("ocaml-usb: out of memory");
  memset(transfer, 0, sizeof(struct libusb_transfer));
  return transfer;
}

/* +----------------+
   | Initialization |
   +----------------+ */

void ml_usb_init()
{
  int res = libusb_init(NULL);
  if (res) ml_usb_error(res, "init");
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

#define Endpoint_val(endpoint, direction) (Int_val(endpoint) | (Int_val(direction) == 0 ? LIBUSB_ENDPOINT_IN : LIBUSB_ENDPOINT_OUT))

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
    ml_usb_error(cnt, "get_device_list");

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

value ml_usb_get_max_packet_size(value dev, value direction, value endpoint)
{
  int res = libusb_get_max_packet_size(Device_val(dev), Endpoint_val(endpoint, direction));
  if (res) ml_usb_error(res, "get_max_packet_size");
  return Val_int(res);
}

CAMLprim value ml_usb_open(value dev)
{
  CAMLparam1(dev);
  libusb_device_handle *handle = NULL;
  int res = libusb_open(Device_val(dev), &handle);
  if (res) ml_usb_error(res, "open");
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
  if (res) ml_usb_error(res, "claim_interface");
  return Val_unit;
}

value ml_usb_release_interface(value handle, value interface)
{
  int res = libusb_release_interface(Handle_val(handle), Int_val(interface));
  if (res) ml_usb_error(res, "release_interface");
  return Val_unit;
}

void ml_usb_handle_events()
{
  struct timeval tp = { 0, 0 };
  int res = libusb_handle_events_timeout(NULL, &tp);
  if (res) ml_usb_error(res, "handle_event_timeout");
}

CAMLprim value ml_usb_collect_sources(value lr, value lw)
{
  CAMLparam2(lr, lw);

  const struct libusb_pollfd **pollfds = libusb_get_pollfds(NULL);

  if (pollfds) {
    const struct libusb_pollfd **fds;
    for (fds = pollfds; *fds; fds++) {
      value fd = Val_int((*fds)->fd);
      short ev = (*fds)->events;
      if (ev & POLLIN) {
        value x = caml_alloc_tuple(2);
        Store_field(x, 0, fd);
        Store_field(x, 1, lr);
        lr = x;
      }
      if (ev & POLLOUT) {
        value x = caml_alloc_tuple(2);
        Store_field(x, 0, fd);
        Store_field(x, 1, lw);
        lw = x;
      }
    };
    free(pollfds);
  }

  struct timeval tp;
  int res = libusb_get_next_timeout(NULL, &tp);
  if (res == 1) {
    value x = caml_alloc_tuple(1);
    Store_field(x, 0, copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6));
    value r = caml_alloc_tuple(3);
    Store_field(r, 0, lr);
    Store_field(r, 1, lw);
    Store_field(r, 2, x);
    CAMLreturn(r);
  }
  if (res != 0) ml_usb_error(res, "get_next_timeout");
  value r = caml_alloc_tuple(3);
  Store_field(r, 0, lr);
  Store_field(r, 1, lw);
  Store_field(r, 2, Val_int(0));
  CAMLreturn(r);
}

struct transfer_recv {
  value callback;
  value buffer;
  int offset;
};

void handle_recv(struct libusb_transfer *transfer)
{
  struct transfer_recv *tr = (struct transfer_recv*)(transfer->user_data);
  char *dest = String_val(tr->buffer) + tr->offset;
  value f = tr->callback;
  caml_remove_generational_global_root(&(tr->callback));
  caml_remove_generational_global_root(&(tr->buffer));
  value result;
  if (transfer->status == LIBUSB_TRANSFER_COMPLETED) {
    memcpy(dest, transfer->buffer, transfer->actual_length);
    result = caml_alloc(1, 0);
    Store_field(result, 0, Val_int(transfer->actual_length));
  } else {
    result = caml_alloc(2, 1);
    Store_field(result, 0, caml_copy_string("transfer"));
    Store_field(result, 1, Val_int(12));
  }
  free(tr);
  free(transfer->buffer);
  libusb_free_transfer(transfer);
  callback(f, result);
}

CAMLprim value ml_usb_interrupt_recv(value desc)
{
  CAMLparam1(desc);
  struct transfer_recv *tr = (struct transfer_recv*)ml_usb_malloc(sizeof(struct transfer_recv));
  struct libusb_transfer *transfer = ml_usb_alloc_transfer(0);
  transfer->dev_handle = Handle_val(Field(desc, 0));
  transfer->endpoint = Int_val(Field(desc, 1)) | LIBUSB_ENDPOINT_IN;
  transfer->type = LIBUSB_TRANSFER_TYPE_INTERRUPT;
  transfer->timeout = Int_val(Field(desc, 2));
  transfer->buffer = (unsigned char *)ml_usb_malloc(Int_val(Field(desc, 5)));
  transfer->length = Int_val(Field(desc, 5));
  transfer->user_data = (void*)tr;
  transfer->callback = handle_recv;
  tr->callback = Field(desc, 6);
  tr->buffer = Field(desc, 3);
  tr->offset = Field(desc, 4);
  caml_register_generational_global_root(&(tr->callback));
  caml_register_generational_global_root(&(tr->buffer));

  int res = libusb_submit_transfer(transfer);
  if (res) ml_usb_error(res, "submit_transfer");
  CAMLreturn(Val_unit);
}

void handle_send(struct libusb_transfer *transfer)
{
  value f = (value)(transfer->user_data);
  caml_remove_generational_global_root((value*)(&(transfer->user_data)));
  value result;
  if (transfer->status == LIBUSB_TRANSFER_COMPLETED) {
    result = caml_alloc(1, 0);
    Store_field(result, 0, Val_int(transfer->actual_length));
  } else {
    result = caml_alloc(2, 1);
    Store_field(result, 0, caml_copy_string("transfer"));
    Store_field(result, 1, Val_int(12));
  }
  free(transfer->buffer);
  libusb_free_transfer(transfer);
  callback(f, result);
}

CAMLprim value ml_usb_interrupt_send(value desc)
{
  CAMLparam1(desc);
  struct libusb_transfer *transfer = ml_usb_alloc_transfer(0);
  transfer->dev_handle = Handle_val(Field(desc, 0));
  transfer->endpoint = Int_val(Field(desc, 1)) | LIBUSB_ENDPOINT_OUT;
  transfer->type = LIBUSB_TRANSFER_TYPE_INTERRUPT;
  transfer->timeout = Int_val(Field(desc, 2));
  transfer->buffer = (unsigned char *)ml_usb_malloc(Int_val(Field(desc, 5)));
  memcpy(transfer->buffer, String_val(Field(desc, 3)) + Long_val(Field(desc, 4)), Long_val(Field(desc, 5)));
  transfer->length = Int_val(Field(desc, 5));
  transfer->user_data = (void*)Field(desc, 6);
  transfer->callback = handle_send;
  caml_register_generational_global_root((value*)(&(transfer->user_data)));

  int res = libusb_submit_transfer(transfer);
  if (res) ml_usb_error(res, "submit_transfer");
  CAMLreturn(Val_unit);
}
