(*
 * Copyright (c) 2010 http://github.com/barko 00336ea19fcb53de187740c490f764f4
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt
open Printf

(* A bounded queue to receive data segments and let readers block on 
   receiving them. Also supports a monitor that is informed when the
   queue size changes *)
module Rx = struct
  
  type t = {
    q: OS.Io_page.t option Lwt_sequence.t; 
    wnd: Window.t;
    writers: unit Lwt.u Lwt_sequence.t;
    readers: OS.Io_page.t option Lwt.u Lwt_sequence.t;
    mutable watcher: int32 Lwt_mvar.t option;
    mutable max_size: int32;
    mutable cur_size: int32;
  }
  
  let create ~max_size ~wnd =
    let q = Lwt_sequence.create () in
    let writers = Lwt_sequence.create () in
    let readers = Lwt_sequence.create () in
    let watcher = None in
    let cur_size = 0l in
    { q; wnd; writers; readers; max_size; cur_size; watcher }
  
  let notify_size_watcher t =
    let rx_wnd = max 0l (Int32.sub t.max_size t.cur_size) in
    Window.set_rx_wnd t.wnd rx_wnd;
    match t.watcher with
    |None -> return ()
    |Some w -> Lwt_mvar.put w t.cur_size
    
  let seglen s =
    match s with
    | None -> 0 
    | Some b -> Cstruct.len b

  let add_r t s =
    if t.cur_size > t.max_size then
      let th,u = Lwt.task () in
      let node = Lwt_sequence.add_r u t.writers in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      (* Update size before blocking, which may push cur_size above max_size *)
      t.cur_size <- Int32.(add t.cur_size (of_int (seglen s)));
      notify_size_watcher t >>
      lwt () = th in
      ignore(Lwt_sequence.add_r s t.q);
      return ()
    else begin
      (match Lwt_sequence.take_opt_l t.readers with
      |None ->
        t.cur_size <- Int32.(add t.cur_size (of_int (seglen s)));
        ignore(Lwt_sequence.add_r s t.q);
        notify_size_watcher t
      |Some u -> 
        return (Lwt.wakeup u s)
      );
    end
      
  let take_l t =
    if Lwt_sequence.is_empty t.q then begin
      let th,u = Lwt.task () in
      let node = Lwt_sequence.add_r u t.readers in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      th
    end else begin
      let s = Lwt_sequence.take_l t.q in
      t.cur_size <- Int32.(sub t.cur_size (of_int (seglen s)));
      notify_size_watcher t >>
      if t.cur_size < t.max_size then begin
        match Lwt_sequence.take_opt_l t.writers with
        |None -> ()
        |Some w -> Lwt.wakeup w ()
      end;
      return s
    end
  
  let cur_size t = t.cur_size
  let max_size t = t.max_size
  
  let monitor t mvar =
    t.watcher <- Some mvar

end

(* The application can obtain views to write into, and can always
 * write roughly a page's worth of data. Once a view has been committed
 * it may not be reused, as the stack may segment up the underlying buffer
 * if data can be sent. *)
module Tx = struct

  type get_writebuf = unit -> OS.Io_page.t Lwt.t
  type t = {
    get_writebuf: get_writebuf;
    wnd: Window.t;
    writers: unit Lwt.u Lwt_sequence.t;
    txq: Segment.Tx.q;
    mutable buffer: OS.Io_page.t;
  }

  let create ~wnd ~txq ~get_writebuf = 
    lwt buffer = get_writebuf () in
    let writers = Lwt_sequence.create () in
    return { wnd; writers; txq; buffer; get_writebuf }

  (* Check how many bytes are available to write to the wire *)
  let available_cwnd t = 
    Window.tx_available t.wnd

  let rec write t data =
    let l = Int32.of_int (Cstruct.len data) in
    let avail_len = available_cwnd t in
    match avail_len < l with
    |true -> 
       (* For now, just wait for more space *)
       Printf.printf "TCP.Ubuf.write: blocking [%ld / %ld]\n" avail_len l;
       let th,u = Lwt.task () in
       let node = Lwt_sequence.add_r u t.writers in
       Lwt.on_cancel th (fun () -> Lwt_sequence.remove node);
       th >> write t data
    |false -> 
       lwt () = Segment.Tx.output ~flags:Segment.Tx.Psh t.txq data in
       lwt buf = t.get_writebuf () in
       t.buffer <- buf;
       return ()

  (* XXX What should the semantics be? additive to t.buffer? *)
  let get_writebuf t =
    return t.buffer

  (* Indicate that more bytes are available for waiting writers.
     Note that sz does not take window scaling into account, and so
     should be passed as unscaled (i.e. from the wire) here.
     Window will internally scale it up. *)
  let free t sz =
    match Lwt_sequence.take_opt_l t.writers with
    |None -> return ()
    |Some w ->
      Lwt.wakeup w ();
      return ()
end
