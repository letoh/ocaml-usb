(*
 * krobot_get_firmware.ml
 * ----------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-usb.
 *)

open Lwt

let _ =
  let handle = USB.open_device_with ~vendor_id:0x04D8 ~product_id:0x0001 in
  if USB.kernel_driver_active handle 0 then USB.detach_kernel_driver handle 0;
  USB.claim_interface handle 0;
  let buffer = String.make 64 '\000' in
  buffer.[0] <- '\002';
  buffer.[1] <- '\002';
  Lwt_main.run
    (USB.interrupt_send ~handle ~endpoint:1 buffer 0 64
     >>= fun _ -> USB.interrupt_recv ~handle ~endpoint:1 buffer 0 64
     >>= fun _ -> Lwt_io.put_line Lwt_io.stdout (String.sub buffer 0 63))
