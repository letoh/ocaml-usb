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
  USB.with_device (USB.find_device ~vendor_id:0x04D8 ~product_id:0x0001)
    (fun handle ->
       let buffer = String.make 64 '\000' in
       buffer.[0] <- '\002';
       buffer.[1] <- '\002';
       let_lwt x = (launch_truc or toto ()) in
       Lwt_main.run
         (perform
            USB.interrupt_send ~handle ~endpoint:1 buffer 0 64;
            USB.interrupt_recv ~handle ~endpoint:1 buffer 0 64;
            Lwt_io.put_line Lwt_io.stdout (String.sub buffer 0 63)))
