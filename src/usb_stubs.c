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
#include <caml/signals.h>
#include <libusb.h>
#include <poll.h>
#include <string.h>
#include <stdio.h>
#include <ev.h>
#include <sys/time.h>
#include <lwt_unix.h>

/* +-----------------------------------------------------------------+
   | Errors                                                          |
   +-----------------------------------------------------------------+ */

static void ml_usb_error(int code, char *fun_name)
{
  int num;
  value arg[2];
  switch(code) {
  case LIBUSB_ERROR_IO:
    num = Val_int(0);
    break;
  case LIBUSB_ERROR_INVALID_PARAM:
    num = Val_int(1);
    break;
  case LIBUSB_ERROR_ACCESS:
    num = Val_int(2);
    break;
  case LIBUSB_ERROR_NO_DEVICE:
    num = Val_int(3);
    break;
  case LIBUSB_ERROR_NOT_FOUND:
    num = Val_int(4);
    break;
  case LIBUSB_ERROR_BUSY:
    num = Val_int(5);
    break;
  case LIBUSB_ERROR_TIMEOUT:
    num = Val_int(6);
    break;
  case LIBUSB_ERROR_OVERFLOW:
    num = Val_int(7);
    break;
  case LIBUSB_ERROR_PIPE:
    num = Val_int(8);
    break;
  case LIBUSB_ERROR_INTERRUPTED:
    num = Val_int(9);
    break;
  case LIBUSB_ERROR_NO_MEM:
    num = Val_int(10);
    break;
  case LIBUSB_ERROR_NOT_SUPPORTED:
    num = Val_int(11);
    break;
  case LIBUSB_ERROR_OTHER:
    num = Val_int(12);
    break;
  default: {
    char str[512];
    sprintf(str, "ocaml-usb: unknown error (%d)", code);
    caml_failwith(str);
  }
  }
  arg[0] = num;
  arg[1] = caml_copy_string(fun_name);
  caml_raise_with_args(*caml_named_value("ocaml-usb:Error"), 2, arg);
}

static void *ml_usb_malloc(size_t size)
{
  void *ptr = malloc(size);
  if (ptr == NULL) caml_failwith("ocaml-usb: out of memory");
  return ptr;
}

static struct libusb_transfer *ml_usb_alloc_transfer(int count)
{
  struct libusb_transfer *transfer = libusb_alloc_transfer(count);
  if (transfer == NULL) caml_failwith("ocaml-usb: out of memory");
  return transfer;
}

/* +-----------------------------------------------------------------+
   | Event-loop integration                                          |
   +-----------------------------------------------------------------+ */

void ml_usb_handle_events()
{
  struct timeval tp = { 0, 0 };
  int res = libusb_handle_events_timeout(NULL, &tp);
  if (res) ml_usb_error(res, "handle_event_timeout");
}

static ev_timer timer_watcher;

static void ml_usb_timer_prepare(struct ev_loop *loop, ev_prepare *watcher, int revents)
{
  struct timeval tp;
  if (libusb_get_next_timeout(NULL, &tp) == 1) {
    /* There is a timeout */
    ev_timer_set(&timer_watcher, tp.tv_sec + (tp.tv_usec * 1e-3), 0);
    ev_timer_start(NULL, &timer_watcher);
  }
}

static void ml_usb_timer_check(struct ev_loop *loop, ev_check *watcher, int revents)
{
  if (ev_is_active(&timer_watcher)) {
    ev_timer_stop(NULL, &timer_watcher);
    ml_usb_handle_events();
  }
}

struct node {
  struct ev_io watcher;
  struct node* next;
};

static struct node* watchers = NULL;

static void ml_usb_handle_io(struct ev_loop *loop, ev_io *watcher, int revents)
{
  ml_usb_handle_events();
}

static void ml_usb_add_watcher(int fd, int event)
{
  struct node* node = (struct node*)ml_usb_malloc(sizeof(struct node));
  node->next = watchers;
  watchers = node;
  ev_io_init(&(node->watcher), ml_usb_handle_io, fd, event);
  ev_io_start(NULL, &(node->watcher));
}

static void ml_usb_add_pollfd(int fd, short events, void *user_data)
{
  if (events & POLLIN)
    ml_usb_add_watcher(fd, EV_READ);
  if (events & POLLOUT)
    ml_usb_add_watcher(fd, EV_WRITE);
}

static void ml_usb_remove_pollfd(int fd, void *user_data)
{
  struct node** store = &watchers;
  struct node* node = watchers;
  while (node) {
    if (node->watcher.fd == fd) {
      ev_io_stop(NULL, &(node->watcher));
      node = node->next;
      *store = node;
    } else {
      store = &(node->next);
      node = node->next;
    }
  }
}

/* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ */

static void nop() {}

static ev_prepare prepare_watcher;
static ev_check check_watcher;

CAMLprim value ml_usb_init()
{
  int res = libusb_init(NULL);
  if (res) ml_usb_error(res, "init");

  libusb_set_pollfd_notifiers(NULL,
                              ml_usb_add_pollfd,
                              ml_usb_remove_pollfd,
                              NULL);

  ev_prepare_init(&prepare_watcher, ml_usb_timer_prepare);
  ev_set_priority(&prepare_watcher, EV_MINPRI);
  ev_prepare_start(EV_DEFAULT, &prepare_watcher);

  ev_check_init(&check_watcher, ml_usb_timer_check);
  ev_set_priority(&check_watcher, EV_MAXPRI);
  ev_check_start(EV_DEFAULT, &check_watcher);

  ev_init(&timer_watcher, nop);
  ev_set_priority(&timer_watcher, EV_MINPRI);

  return Val_unit;
}

CAMLprim value ml_usb_exit()
{
  libusb_exit(NULL);
  return Val_unit;
}

CAMLprim value ml_usb_set_debug(value level)
{
  libusb_set_debug(NULL, Int_val(level));
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Device and enumerations                                         |
   +-----------------------------------------------------------------+ */

#define Endpoint_val(endpoint, direction) (Int_val(endpoint) | (Int_val(direction) == 0 ? LIBUSB_ENDPOINT_IN : LIBUSB_ENDPOINT_OUT))

#define Ptr_val(v) ((long)(*(void**)Data_custom_val(v)))

static int ml_usb_compare(value v1, value v2)
{
  return (int)(Ptr_val(v1) - Ptr_val(v2));
}

static long ml_usb_hash(value v)
{
  return Ptr_val(v);
}

#define Device_val(v) *(libusb_device**)Data_custom_val(v)

static void ml_usb_device_finalize(value dev)
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

static void ml_usb_device_handle_finalize(value vhandle)
{
  libusb_device_handle *handle = Handle_val(vhandle);
  if (handle) {
    Handle_val(vhandle) = NULL;
    libusb_close(handle);
  }
}

static struct custom_operations handle_ops = {
  "usb.device.handle",
  ml_usb_device_handle_finalize,
  ml_usb_compare,
  ml_usb_hash,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_device(libusb_device *device)
{
  value x = caml_alloc_custom(&device_ops, sizeof(libusb_device*), 0, 1);
  Device_val(x) = device;
  return x;
}

static value alloc_handle(libusb_device_handle *handle)
{
  value x = caml_alloc_custom(&handle_ops, sizeof(libusb_device_handle*), 0, 1);
  Handle_val(x) = handle;
  return x;
}

CAMLprim value ml_usb_get_device_list(value unit)
{
  CAMLparam1(unit);
  CAMLlocal2(x, y);

  libusb_device **devices;

  size_t cnt = libusb_get_device_list(NULL, &devices);
  if ((int)cnt < 0)
    ml_usb_error(cnt, "get_device_list");

  /* Convert the array to a caml list */
  size_t i;
  x = Val_int(0);
  for (i = 0; i < cnt; i++) {
    y = caml_alloc_tuple(2);
    Store_field(y, 0, alloc_device(devices[i]));
    Store_field(y, 1, x);
    x = y;
  }

  /* Free the list but not the devices */
  libusb_free_device_list(devices, 0);

  CAMLreturn(x);
}

CAMLprim value ml_usb_get_bus_number(value dev)
{
  return Val_int(libusb_get_bus_number(Device_val(dev)));
}

CAMLprim value ml_usb_get_device_address(value dev)
{
  return Val_int(libusb_get_device_address(Device_val(dev)));
}

CAMLprim value ml_usb_get_max_packet_size(value dev, value direction, value endpoint)
{
  int res = libusb_get_max_packet_size(Device_val(dev), Endpoint_val(endpoint, direction));
  if (res < 0) ml_usb_error(res, "get_max_packet_size");
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
  CAMLlocal1(some);
  libusb_device_handle *handle = libusb_open_device_with_vid_pid(NULL, Int_val(vid), Int_val(pid));
  if (handle == NULL)
    CAMLreturn(Val_int(0));
  else {
    some = caml_alloc_tuple(1);
    Store_field(some, 0, alloc_handle(handle));
    CAMLreturn(some);
  }
}

CAMLprim value ml_usb_close(value vhandle)
{
  libusb_device_handle *handle = Handle_val(vhandle);
  if (handle) {
    Handle_val(vhandle) = NULL;
    libusb_close(handle);
  }
  return Val_unit;
}

CAMLprim value ml_usb_get_device(value handle)
{
  CAMLparam1(handle);
  libusb_device *device = libusb_get_device(Handle_val(handle));
  libusb_ref_device(device);
  CAMLreturn(alloc_device(device));
}

CAMLprim value ml_usb_kernel_driver_active(value handle, value interface)
{
  int res = libusb_kernel_driver_active(Handle_val(handle), Int_val(interface));
  switch (res) {
  case 0:
    return Val_false;
  case 1:
    return Val_true;
  default:
    ml_usb_error(res, "kernel_driver_active");
    return Val_false;
  }
}

CAMLprim value ml_usb_detach_kernel_driver(value handle, value interface)
{
  int res = libusb_detach_kernel_driver(Handle_val(handle), Int_val(interface));
  if (res) ml_usb_error(res, "detach_kernel_driver");
  return Val_unit;
}

CAMLprim value ml_usb_attach_kernel_driver(value handle, value interface)
{
  int res = libusb_attach_kernel_driver(Handle_val(handle), Int_val(interface));
  if (res) ml_usb_error(res, "attach_kernel_driver");
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: claim_interface                                            |
   +-----------------------------------------------------------------+ */

struct job_claim_interface {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int interface;
  int result;
};

#define Job_claim_interface(v) *(struct job_claim_interface**)Data_custom_val(v)

static void worker_claim_interface(struct job_claim_interface *job)
{
  job->result = libusb_claim_interface(job->handle, job->interface);
}

CAMLprim value ml_usb_claim_interface_job(value val_handle, value val_interface)
{
  struct job_claim_interface *job = lwt_unix_new(struct job_claim_interface);
  job->job.worker = (lwt_unix_job_worker)worker_claim_interface;
  job->handle = Handle_val(val_handle);
  job->interface = Int_val(val_interface);
  return Val_unit;
}

CAMLprim value ml_usb_claim_interface_result(value val_job)
{
  struct job_claim_interface *job = Job_claim_interface(val_job);
  if (job->result) ml_usb_error(job->result, "claim_interface");
  return Val_unit;
}

CAMLprim value ml_usb_claim_interface_free(value val_job)
{
  struct job_claim_interface *job = Job_claim_interface(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: release_interface                                          |
   +-----------------------------------------------------------------+ */

struct job_release_interface {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int interface;
  int result;
};

#define Job_release_interface(v) *(struct job_release_interface**)Data_custom_val(v)

static void worker_release_interface(struct job_release_interface *job)
{
  job->result = libusb_release_interface(job->handle, job->interface);
}

CAMLprim value ml_usb_release_interface_job(value val_handle, value val_interface)
{
  struct job_release_interface *job = lwt_unix_new(struct job_release_interface);
  job->job.worker = (lwt_unix_job_worker)worker_release_interface;
  job->handle = Handle_val(val_handle);
  job->interface = Int_val(val_interface);
  return Val_unit;
}

CAMLprim value ml_usb_release_interface_result(value val_job)
{
  struct job_release_interface *job = Job_release_interface(val_job);
  if (job->result) ml_usb_error(job->result, "release_interface");
  return Val_unit;
}

CAMLprim value ml_usb_release_interface_free(value val_job)
{
  struct job_release_interface *job = Job_release_interface(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: get_configuration                                          |
   +-----------------------------------------------------------------+ */

struct job_get_configuration {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int configuration;
  int result;
};

#define Job_get_configuration(v) *(struct job_get_configuration**)Data_custom_val(v)

static void worker_get_configuration(struct job_get_configuration *job)
{
  job->result = libusb_get_configuration(job->handle, &(job->configuration));
}

CAMLprim value ml_usb_get_configuration_job(value val_handle)
{
  struct job_get_configuration *job = lwt_unix_new(struct job_get_configuration);
  job->job.worker = (lwt_unix_job_worker)worker_get_configuration;
  job->handle = Handle_val(val_handle);
  return Val_unit;
}

CAMLprim value ml_usb_get_configuration_result(value val_job)
{
  struct job_get_configuration *job = Job_get_configuration(val_job);
  if (job->result) ml_usb_error(job->result, "get_configuration");
  return Val_int(job->configuration);
}

CAMLprim value ml_usb_get_configuration_free(value val_job)
{
  struct job_get_configuration *job = Job_get_configuration(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: set_configuration                                          |
   +-----------------------------------------------------------------+ */

struct job_set_configuration {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int configuration;
  int result;
};

#define Job_set_configuration(v) *(struct job_set_configuration**)Data_custom_val(v)

static void worker_set_configuration(struct job_set_configuration *job)
{
  job->result = libusb_set_configuration(job->handle, job->configuration);
}

CAMLprim value ml_usb_set_configuration_job(value val_handle, value val_configuration)
{
  struct job_set_configuration *job = lwt_unix_new(struct job_set_configuration);
  job->job.worker = (lwt_unix_job_worker)worker_set_configuration;
  job->handle = Handle_val(val_handle);
  job->configuration = Int_val(val_configuration);
  return Val_unit;
}

CAMLprim value ml_usb_set_configuration_result(value val_job)
{
  struct job_set_configuration *job = Job_set_configuration(val_job);
  if (job->result) ml_usb_error(job->result, "set_configuration");
  return Val_unit;
}

CAMLprim value ml_usb_set_configuration_free(value val_job)
{
  struct job_set_configuration *job = Job_set_configuration(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: set_interface_alt_setting                                  |
   +-----------------------------------------------------------------+ */

struct job_set_interface_alt_setting {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int interface;
  int alt_setting;
  int result;
};

#define Job_set_interface_alt_setting(v) *(struct job_set_interface_alt_setting**)Data_custom_val(v)

static void worker_set_interface_alt_setting(struct job_set_interface_alt_setting *job)
{
  job->result = libusb_set_interface_alt_setting(job->handle, job->interface, job->alt_setting);
}

CAMLprim value ml_usb_set_interface_alt_setting_job(value val_handle, value val_interface, value val_alt_setting)
{
  struct job_set_interface_alt_setting *job = lwt_unix_new(struct job_set_interface_alt_setting);
  job->job.worker = (lwt_unix_job_worker)worker_set_interface_alt_setting;
  job->handle = Handle_val(val_handle);
  job->interface = Int_val(val_interface);
  job->alt_setting = Int_val(val_alt_setting);
  return Val_unit;
}

CAMLprim value ml_usb_set_interface_alt_setting_result(value val_job)
{
  struct job_set_interface_alt_setting *job = Job_set_interface_alt_setting(val_job);
  if (job->result) ml_usb_error(job->result, "set_interface_alt_setting");
  return Val_unit;
}

CAMLprim value ml_usb_set_interface_alt_setting_free(value val_job)
{
  struct job_set_interface_alt_setting *job = Job_set_interface_alt_setting(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: clear_halt                                                 |
   +-----------------------------------------------------------------+ */

struct job_clear_halt {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int endpoint;
  int result;
};

#define Job_clear_halt(v) *(struct job_clear_halt**)Data_custom_val(v)

static void worker_clear_halt(struct job_clear_halt *job)
{
  job->result = libusb_clear_halt(job->handle, job->endpoint);
}

CAMLprim value ml_usb_clear_halt_job(value val_handle, value val_endpoint)
{
  struct job_clear_halt *job = lwt_unix_new(struct job_clear_halt);
  job->job.worker = (lwt_unix_job_worker)worker_clear_halt;
  job->handle = Handle_val(val_handle);
  job->endpoint = Int_val(val_endpoint);
  return Val_unit;
}

CAMLprim value ml_usb_clear_halt_result(value val_job)
{
  struct job_clear_halt *job = Job_clear_halt(val_job);
  if (job->result) ml_usb_error(job->result, "clear_halt");
  return Val_unit;
}

CAMLprim value ml_usb_clear_halt_free(value val_job)
{
  struct job_clear_halt *job = Job_clear_halt(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: reset_device                                               |
   +-----------------------------------------------------------------+ */

struct job_reset_device {
  struct lwt_unix_job job;
  libusb_device_handle *handle;
  int result;
};

#define Job_reset_device(v) *(struct job_reset_device**)Data_custom_val(v)

static void worker_reset_device(struct job_reset_device *job)
{
  job->result = libusb_reset_device(job->handle);
}

CAMLprim value ml_usb_reset_device_job(value val_handle)
{
  struct job_reset_device *job = lwt_unix_new(struct job_reset_device);
  job->job.worker = (lwt_unix_job_worker)worker_reset_device;
  job->handle = Handle_val(val_handle);
  return Val_unit;
}

CAMLprim value ml_usb_reset_device_result(value val_job)
{
  struct job_reset_device *job = Job_reset_device(val_job);
  if (job->result) ml_usb_error(job->result, "reset_device");
  return Val_unit;
}

CAMLprim value ml_usb_reset_device_free(value val_job)
{
  struct job_reset_device *job = Job_reset_device(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | USB descriptors                                                 |
   +-----------------------------------------------------------------+ */

CAMLprim value ml_usb_get_device_descriptor(value device)
{
  CAMLparam1(device);
  CAMLlocal1(result);

  struct libusb_device_descriptor desc;
  int res = libusb_get_device_descriptor(Device_val(device), &desc);
  if (res) ml_usb_error(res, "get_device_descriptor");

  result = caml_alloc_tuple(12);
  Store_field(result, 0, Val_int(desc.bcdUSB));
  Store_field(result, 1, Val_int(desc.bDeviceClass));
  Store_field(result, 2, Val_int(desc.bDeviceSubClass));
  Store_field(result, 3, Val_int(desc.bDeviceProtocol));
  Store_field(result, 4, Val_int(desc.bMaxPacketSize0));
  Store_field(result, 5, Val_int(desc.idVendor));
  Store_field(result, 6, Val_int(desc.idProduct));
  Store_field(result, 7, Val_int(desc.bcdDevice));
  Store_field(result, 8, Val_int(desc.iManufacturer));
  Store_field(result, 9, Val_int(desc.iProduct));
  Store_field(result, 10, Val_int(desc.iSerialNumber));
  Store_field(result, 11, Val_int(desc.bNumConfigurations));

  CAMLreturn(result);
}

static value copy_config_descriptor(struct libusb_config_descriptor *cd)
{
  CAMLparam0();
  CAMLlocal5(result, iface, ifaces, altsettings, endpoint);
  CAMLlocal1(endpoints);
  result = caml_alloc_tuple(5);
  Store_field(result, 0, Val_int(cd->bConfigurationValue));
  Store_field(result, 1, Val_int(cd->iConfiguration));
  Store_field(result, 2, Val_int(cd->bmAttributes));
  Store_field(result, 3, Val_int(cd->MaxPower));
  ifaces = caml_alloc_tuple(cd->bNumInterfaces);
  Store_field(result, 4, ifaces);
  int i, j, k;
  for (i = 0; i < cd->bNumInterfaces; i++) {
    altsettings = caml_alloc_tuple(cd->interface[i].num_altsetting);
    Store_field(ifaces, i, altsettings);
    for (j = 0; j < cd->interface[i].num_altsetting; j++) {
      iface = caml_alloc_tuple(7);
      Store_field(altsettings, j, iface);
      Store_field(iface, 0, Val_int(cd->interface[i].altsetting[j].bInterfaceNumber));
      Store_field(iface, 1, Val_int(cd->interface[i].altsetting[j].bAlternateSetting));
      Store_field(iface, 2, Val_int(cd->interface[i].altsetting[j].bInterfaceClass));
      Store_field(iface, 3, Val_int(cd->interface[i].altsetting[j].bInterfaceSubClass));
      Store_field(iface, 4, Val_int(cd->interface[i].altsetting[j].bInterfaceProtocol));
      Store_field(iface, 5, Val_int(cd->interface[i].altsetting[j].iInterface));
      endpoints = caml_alloc_tuple(cd->interface[i].altsetting[j].bNumEndpoints);
      Store_field(iface, 6, endpoints);
      for (k = 0; k < cd->interface[i].altsetting[j].bNumEndpoints; k++) {
        endpoint = caml_alloc_tuple(6);
        Store_field(endpoints, k, endpoint);
        Store_field(endpoint, 0, Val_int(cd->interface[i].altsetting[j].endpoint[k].bEndpointAddress));
        Store_field(endpoint, 1, Val_int(cd->interface[i].altsetting[j].endpoint[k].bmAttributes));
        Store_field(endpoint, 2, Val_int(cd->interface[i].altsetting[j].endpoint[k].wMaxPacketSize));
        Store_field(endpoint, 3, Val_int(cd->interface[i].altsetting[j].endpoint[k].bInterval));
        Store_field(endpoint, 4, Val_int(cd->interface[i].altsetting[j].endpoint[k].bRefresh));
        Store_field(endpoint, 5, Val_int(cd->interface[i].altsetting[j].endpoint[k].bSynchAddress));
      }
    }
  }
  libusb_free_config_descriptor(cd);
  CAMLreturn(result);
}

CAMLprim value ml_usb_get_active_config_descriptor(value device)
{
  struct libusb_config_descriptor *cd;
  int res = libusb_get_active_config_descriptor(Device_val(device), &cd);
  if (res) ml_usb_error(res, "get_active_config_descriptor");
  return copy_config_descriptor(cd);
}

CAMLprim value ml_usb_get_config_descriptor(value device, value index)
{
  struct libusb_config_descriptor *cd;
  int res = libusb_get_config_descriptor(Device_val(device), Int_val(index), &cd);
  if (res) ml_usb_error(res, "get_config_descriptor");
  return copy_config_descriptor(cd);
}

CAMLprim value ml_usb_get_config_descriptor_by_value(value device, value val)
{
  struct libusb_config_descriptor *cd;
  int res = libusb_get_config_descriptor_by_value(Device_val(device), Int_val(val), &cd);
  if (res) ml_usb_error(res, "get_config_descriptor_by_value");
  return copy_config_descriptor(cd);
}

/* +-----------------------------------------------------------------+
   | IOs                                                             |
   +-----------------------------------------------------------------+ */

#define Transfer_val(v) *(struct libusb_transfer**)Data_custom_val(v)

static struct custom_operations transfer_ops = {
  "usb.transfer",
  custom_finalize_default,
  ml_usb_compare,
  ml_usb_hash,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_transfer(struct libusb_transfer *transfer)
{
  value x = caml_alloc_custom(&transfer_ops, sizeof(struct libusb_transfer*), 0, 1);
  Transfer_val(x) = transfer;
  return x;
}

CAMLprim value ml_usb_cancel_transfer(value transfer)
{
  int res = libusb_cancel_transfer(Transfer_val(transfer));
  if (res) ml_usb_error(res, "cancel_transfer");
  return Val_unit;
}

/* Allocate a buffer, taking cares of remarks about overflows from the
   libsub documentation: */
static unsigned char *ml_usb_alloc_buffer(int length)
{
  int rest = length % 512;
  if (rest) length = length - rest + 512;
  return (unsigned char*)ml_usb_malloc(length);
}

/* Convert an error transfer status to an exception */
static value ml_usb_transfer_error(enum libusb_transfer_status status)
{
  switch(status) {
  case LIBUSB_TRANSFER_ERROR:
    return Val_int(0);
  case LIBUSB_TRANSFER_TIMED_OUT:
    return Val_int(1);
  case LIBUSB_TRANSFER_CANCELLED:
    return Val_int(2);
  case LIBUSB_TRANSFER_STALL:
    return Val_int(3);
  case LIBUSB_TRANSFER_NO_DEVICE:
    return Val_int(4);
  case LIBUSB_TRANSFER_OVERFLOW:
    return Val_int(5);
  default:
    return Val_int(0);
  }
}


/* Construct the result of an isochronous transfer: */
static value ml_usb_iso_result(struct libusb_transfer *transfer)
{
  CAMLparam0();
  CAMLlocal3(list, x, y);
  int i;
  for (i = 0; i < transfer->num_iso_packets; i++) {
    if (transfer->iso_packet_desc[i].status == LIBUSB_TRANSFER_COMPLETED) {
      x = caml_alloc(1, 0);
      Store_field(x, 0, Val_int(transfer->iso_packet_desc[i].actual_length));
    } else {
      x = caml_alloc(1, 1);
      Store_field(x, 0, ml_usb_transfer_error(transfer->status));
    }
    y = caml_alloc_tuple(2);
    Store_field(y, 0, x);
    Store_field(y, 1, list);
    list = y;
  }
  CAMLreturn(list);
}

/* Handler for device-to-host transfers: */
static void ml_usb_handle_recv(struct libusb_transfer *transfer)
{
  LWT_UNIX_CHECK;

  CAMLparam0();
  CAMLlocal2(meta, result);

  /* Metadata of the transfer: */
  meta = (value)(transfer->user_data);

  if (transfer->status == LIBUSB_TRANSFER_COMPLETED) {
    /* Copy bytes from the C memory to the caml string: */
    memcpy(String_val(Field(meta, 1)) + Long_val(Field(meta, 2)),
           transfer->buffer, transfer->actual_length);
    /* Returns [OK actual_length] */
    result = caml_alloc(1, 0);
    if (transfer->num_iso_packets == 0)
      /* Classic transfer */
      Store_field(result, 0, Val_int(transfer->actual_length));
    else
      /* Isochronous transfer */
      Store_field(result, 0, ml_usb_iso_result(transfer));
  } else {
    /* Returns [Error status] */
    result = caml_alloc(1, 1);
    Store_field(result, 0, ml_usb_transfer_error(transfer->status));
  }

  /* Unregister the memory root: */
  caml_remove_generational_global_root((value*)(&(transfer->user_data)));

  /* Cleanup allocated structures: */
  free(transfer->buffer);
  libusb_free_transfer(transfer);

  /* Call the ocaml handler: */
  caml_callback(Field(meta, 0), result);
  CAMLreturn0;
}

/* Handler for host-to-device transfers: */
void ml_usb_handle_send(struct libusb_transfer *transfer)
{
  LWT_UNIX_CHECK;

  CAMLparam0();
  CAMLlocal2(caml_func, result);

  /* Metadata contains only the caml callback: */
  caml_func = (value)(transfer->user_data);

  if (transfer->status == LIBUSB_TRANSFER_COMPLETED) {
    result = caml_alloc(1, 0);
    if (transfer->num_iso_packets == 0)
      /* Classic transfer */
      Store_field(result, 0, Val_int(transfer->actual_length));
    else
      /* Isochronous transfer */
      Store_field(result, 0, ml_usb_iso_result(transfer));
  } else {
    /* Returns [Error status] */
    result = caml_alloc(1, 1);
    Store_field(result, 0, ml_usb_transfer_error(transfer->status));
  }

  /* Unregister the memory root: */
  caml_remove_generational_global_root((value*)(&(transfer->user_data)));

  /* Cleanup allocated structures: */
  free(transfer->buffer);
  libusb_free_transfer(transfer);

  /* Call the ocaml handler: */
  caml_callback(caml_func, result);
  CAMLreturn0;
}

/* Alloc a transfer and fill it with common informations: */
struct libusb_transfer *ml_usb_transfer(value desc /* the description provided by the caml function: */,
                                        value meta /* metadata for the callback */,
                                        enum libusb_endpoint_direction direction,
                                        int num_iso_packets)
{
  struct libusb_transfer *transfer = ml_usb_alloc_transfer(num_iso_packets);
  transfer->dev_handle = Handle_val(Field(desc, 0));
  transfer->endpoint = Int_val(Field(desc, 1)) | direction;
  transfer->timeout = Int_val(Field(desc, 2));
  transfer->buffer = ml_usb_alloc_buffer(Int_val(Field(desc, 5)));
  transfer->length = Int_val(Field(desc, 5));
  transfer->user_data = (void*)meta;
  transfer->num_iso_packets = num_iso_packets;

  /* Register metadata as a memory root, because we need it for the
     callback which will be called later: */
  caml_register_generational_global_root((value*)(&(transfer->user_data)));

  return transfer;
}

/* Device-to-host transfers, for interrupt or bulk transfers: */
CAMLprim value ml_usb_recv(value desc, enum libusb_transfer_type type, int num_iso_packets)
{
  CAMLparam1(desc);
  CAMLlocal1(meta);

  /* Metadata for the transfer:  */
  meta = caml_alloc_tuple(3);
  /* - the caml callback: */
  Store_field(meta, 0, Field(desc, 6));
  /* - the caml buffer: */
  Store_field(meta, 1, Field(desc, 3));
  /* - the offset in the buffer: */
  Store_field(meta, 2, Field(desc, 4));

  struct libusb_transfer *transfer = ml_usb_transfer(desc, meta, LIBUSB_ENDPOINT_IN, num_iso_packets);
  transfer->callback = ml_usb_handle_recv;
  transfer->type = type;

  int res = libusb_submit_transfer(transfer);
  if (res) ml_usb_error(res, "submit_transfer");

  CAMLreturn(alloc_transfer(transfer));
}

/* Host-to-device transfers, for interrupt or bulk transfers: */
CAMLprim value ml_usb_send(value desc, enum libusb_transfer_type type, int num_iso_packets)
{
  /* Metadata contains only the callback: */
  struct libusb_transfer *transfer = ml_usb_transfer(desc, Field(desc, 6), LIBUSB_ENDPOINT_OUT, num_iso_packets);
  transfer->callback = ml_usb_handle_send;
  transfer->type = type;

  /* Copy data to send from the managed memory to the C memory: */
  memcpy(transfer->buffer, String_val(Field(desc, 3)) + Long_val(Field(desc, 4)), Long_val(Field(desc, 5)));

  int res = libusb_submit_transfer(transfer);
  if (res) ml_usb_error(res, "submit_transfer");

  return alloc_transfer(transfer);
}

CAMLprim value ml_usb_bulk_recv(value desc)
{
  return ml_usb_recv(desc, LIBUSB_TRANSFER_TYPE_BULK, 0);
}

CAMLprim value ml_usb_bulk_send(value desc)
{
  return ml_usb_send(desc, LIBUSB_TRANSFER_TYPE_BULK, 0);
}

CAMLprim value ml_usb_interrupt_recv(value desc)
{
  return ml_usb_recv(desc, LIBUSB_TRANSFER_TYPE_INTERRUPT, 0);
}

CAMLprim value ml_usb_interrupt_send(value desc)
{
  return ml_usb_send(desc, LIBUSB_TRANSFER_TYPE_INTERRUPT, 0);
}

/* Generic function which filling the data section of a control transfer: */
CAMLprim value ml_usb_control(value desc, enum libusb_endpoint_direction direction)
{
  struct libusb_control_setup *control = (struct libusb_control_setup*)String_val(Field(desc,3));
  control->bmRequestType = Int_val(Field(desc, 7)) | (Int_val(Field(desc, 8)) << 5) | direction;
  control->bRequest =  Int_val(Field(desc, 9));
  control->wValue =  libusb_cpu_to_le16(Int_val(Field(desc, 10)));
  control->wIndex =  libusb_cpu_to_le16(Int_val(Field(desc, 11)));
  int length = Int_val(Field(desc, 5));
  control->wLength = libusb_cpu_to_le16(length);
  Field(desc, 5) = Val_int(length + LIBUSB_CONTROL_SETUP_SIZE);
  if (direction == LIBUSB_ENDPOINT_IN)
    return ml_usb_recv(desc, LIBUSB_TRANSFER_TYPE_CONTROL, 0);
  else
    return ml_usb_send(desc, LIBUSB_TRANSFER_TYPE_CONTROL, 0);
}

CAMLprim value ml_usb_control_recv(value desc)
{
  return ml_usb_control(desc, LIBUSB_ENDPOINT_IN);
}

CAMLprim value ml_usb_control_send(value desc)
{
  return ml_usb_control(desc, LIBUSB_ENDPOINT_OUT);
}

CAMLprim value ml_usb_iso(value desc, enum libusb_endpoint_direction direction)
{
  int num_iso_packets = Int_val(Field(desc, 7));
  value val_transfer;
  if (direction == LIBUSB_ENDPOINT_IN)
    val_transfer = ml_usb_recv(desc, LIBUSB_TRANSFER_TYPE_ISOCHRONOUS, num_iso_packets);
  else
    val_transfer = ml_usb_send(desc, LIBUSB_TRANSFER_TYPE_ISOCHRONOUS, num_iso_packets);
  struct libusb_transfer *transfer = Transfer_val(val_transfer);
  int i;
  value x = Field(desc, 8);
  for (i = 0; i < num_iso_packets; i++, x = Field(x, 1))
    transfer->iso_packet_desc[i].length = Int_val(Field(x, 0));
  return val_transfer;
}

CAMLprim value ml_usb_iso_recv(value desc)
{
  return ml_usb_iso(desc, LIBUSB_ENDPOINT_IN);
}

CAMLprim value ml_usb_iso_send(value desc)
{
  return ml_usb_iso(desc, LIBUSB_ENDPOINT_OUT);
}
