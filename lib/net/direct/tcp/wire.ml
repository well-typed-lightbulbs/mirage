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

let get_payload buf =
  Cstruct.shift buf (get_data_offset buf)

open Lwt
open Nettypes
open Printf

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

cstruct pseudo_header {
  uint32_t src;
  uint32_t dst;
  uint8_t res;
  uint8_t proto;
  uint16_t len
} as big_endian 

type id = {
  dest_port: int;               (* Remote TCP port *)
  dest_ip: ipv4_addr;           (* Remote IP address *)
  local_port: int;              (* Local TCP port *)
  local_ip: ipv4_addr;          (* Local IP address *)
}

let checksum ~src ~dst =
  let pbuf = OS.Io_page.get () in
  fun pkt ->
    set_pseudo_header_src pbuf (ipv4_addr_to_uint32 src);
    set_pseudo_header_dst pbuf (ipv4_addr_to_uint32 dst);
    set_pseudo_header_res pbuf 0;
    set_pseudo_header_proto pbuf 6;
    let plen = Cstruct.len pkt in
    set_pseudo_header_len pbuf plen;
    Checksum.ones_complement2 pbuf sizeof_pseudo_header pkt plen

(* Output a general TCP packet, checksum it, and if a reference is provided,
   also record the sent packet for retranmission purposes *)
let xmit ~ip ~id ?(rst=false) ?(syn=false) ?(fin=false) ?(psh=false) ~rx_ack ~seq ~window ~options data =
  let sequence = Sequence.to_int32 seq in
  let ack_number = match rx_ack with Some n -> Sequence.to_int32 n |None -> 0l in
  printf "TCP.xmit %s.%d->%s.%d rst %b syn %b fin %b psh %b seq %lu ack %lu %s datalen %d\n%!"
    (ipv4_addr_to_string id.local_ip) id.local_port (ipv4_addr_to_string id.dest_ip) id.dest_port
    rst syn fin psh sequence ack_number (Options.prettyprint options) 
    (match data with |None -> -1 |Some d -> Cstruct.len d);
  (* Adjust a TCP write buffer with the appropriate header fields and checksum *)
  let adjust_tcp_header ?(options_len=0) hdr =
    let data_off = (sizeof_tcpv4 / 4) + (options_len / 4) in
    set_tcpv4_src_port hdr id.local_port;
    set_tcpv4_dst_port hdr id.dest_port;
    set_tcpv4_sequence hdr sequence;
    set_tcpv4_ack_number hdr ack_number;
    set_data_offset hdr data_off;
    set_tcpv4_flags hdr 0;
    if rx_ack <> None then set_ack hdr;
    if rst then set_rst hdr;
    if syn then set_syn hdr;
    if fin then set_fin hdr;
    if psh then set_psh hdr;
    set_tcpv4_window hdr window;
    set_tcpv4_checksum hdr 0;
    set_tcpv4_urg_ptr hdr 0;
    let checksum = checksum ~src:id.local_ip ~dst:id.dest_ip hdr in
    set_tcpv4_checksum hdr checksum
  in
  match data, options with
  |None, [] -> (* No data, no options, so just obtain a TCP header buf *)
    printf "TCP.xmit: no data, no option\n%!";
    lwt hdr = get_writebuf ~datalen:0 id.dest_ip ip in
    let _ = Cstruct.shift_left hdr sizeof_tcpv4 in
    adjust_tcp_header hdr;
    Ipv4.write ip hdr
  |None, options -> (* No data, options, so get TCP buf, marshal options and send *)
    printf "TCP.xmit: no data, options %s\n%!" (Options.prettyprint options);
    lwt hdr = get_writebuf id.dest_ip ip in
    let options_len = Options.marshal hdr options in 
    (* Shift the buffer to turn it into an IPv4 view *)
    let hdr = Cstruct.sub hdr 0 options_len in
    let _ = Cstruct.shift_left hdr sizeof_tcpv4 in
    adjust_tcp_header ~options_len hdr;
    Ipv4.write ip hdr
  |Some data, [] ->
    printf "TCP.xmit: some %d data, no options\n%!" (Cstruct.len data);
    let _ = Cstruct.shift_left data sizeof_tcpv4 in
    adjust_tcp_header data;
    Ipv4.write ip data
  |Some data, options ->
    printf "TCP.xmit: some %d data, options %sA\n%!" (Cstruct.len data) (Options.prettyprint options);
    (* Obtain a new TCP header and write options into it *)
    lwt hdr = get_writebuf ~datalen:0 id.dest_ip ip in
    let options_len = Options.marshal hdr options in
    let hdr = Cstruct.sub hdr 0 options_len in
    let _ = Cstruct.shift_left hdr sizeof_tcpv4 in
    adjust_tcp_header ~options_len hdr;
    Ipv4.writev ip ~header:hdr [data]
