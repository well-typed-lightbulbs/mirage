(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

cstruct tcpv4 {
  uint16_t src_port;
  uint16_t dst_port;
  uint32_t sequence;
  uint32_t ack_number;
  uint8_t  dataoff;
  uint8_t  flags;
  uint16_t window;
  uint16_t checksum;
  uint16_t urg_ptr
} as big_endian

open Cstruct

(* XXX note that we overwrite the lower half of dataoff
 * with 0, so be careful when implemented CWE flag which 
 * sits there *)
let get_data_offset buf = ((get_tcpv4_dataoff buf) lsr 4) * 4
let set_data_offset buf v = set_tcpv4_dataoff buf (v lsl 4)

let get_fin buf = ((get_uint8 buf 13) land (1 lsl 0)) > 0
let get_syn buf = ((get_uint8 buf 13) land (1 lsl 1)) > 0
let get_rst buf = ((get_uint8 buf 13) land (1 lsl 2)) > 0
let get_psh buf = ((get_uint8 buf 13) land (1 lsl 3)) > 0
let get_ack buf = ((get_uint8 buf 13) land (1 lsl 4)) > 0
let get_urg buf = ((get_uint8 buf 13) land (1 lsl 5)) > 0
let get_ece buf = ((get_uint8 buf 13) land (1 lsl 6)) > 0
let get_cwr buf = ((get_uint8 buf 13) land (1 lsl 7)) > 0

let set_fin buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 0))
let set_syn buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 1))
let set_rst buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 2))
let set_psh buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 3))
let set_ack buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 4))
let set_urg buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 5))
let set_ece buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 6))
let set_cwr buf = set_uint8 buf 13 ((get_uint8 buf 13) lor (1 lsl 7))

let get_options buf =
  if get_data_offset buf > 20 then
    Options.unmarshal (shift buf sizeof_tcpv4) else []

let set_options buf ts =
  Options.marshal buf ts

open Lwt

(* Obtain write buffer, and size the data payload view to datalen
* if it is specified *)
let get_writebuf ?datalen dest_ip ip =
  lwt buf = Ipv4.get_writebuf ~proto:`TCP ~dest_ip ip in
  (* shift buffer by IPv4 header. TODO size by path MTU *)
  match datalen with
  |None ->
    let buf = Cstruct.shift buf sizeof_tcpv4 in
    return buf
  |Some datalen ->
    let buf = Cstruct.sub buf sizeof_tcpv4 datalen in
    return buf
