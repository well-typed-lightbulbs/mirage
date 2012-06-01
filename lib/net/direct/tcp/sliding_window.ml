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

open Printf

(* Type of sequence number. TODO functorize *)
type seq = Sequence.t
type seq_waiters = unit Lwt.u Lwt_sequence.t

(* Window advances left to right only. No going backwards! *)
type t = {
  mutable l: seq;
  mutable m: seq;
  mutable r: seq;
  mutable l_dirty: bool;
  mutable m_dirty: bool;
  mutable r_dirty: bool;
  lm_w: seq_waiters;
  mr_w: seq_waiters;
}

(* Initialise a new window with initial [isn] and maximum size [mss] *)
let create ~isn ~mss =
  let l = isn in
  let m = isn in
  let r = Sequence.add isn mss in
  let l_dirty = false in
  let m_dirty = false in
  let r_dirty = false in
  let lm_w = Lwt_sequence.create () in
  let mr_w = Lwt_sequence.create () in
  { l; m; r; l_dirty; m_dirty; r_dirty; lm_w; mr_w }

(* Test if a sequence number is valid for a window *)
let valid t seq =
  printf "SW.valid %s <= %s <= %s\n" (Sequence.to_string t.l)
    (Sequence.to_string seq) (Sequence.to_string t.r);
  Sequence.between seq t.l t.r

let get_l t = t.l
let get_m t = t.m
let get_r t = t.r

(* TODO enforce l <= m <= r in these additions *)
let add_l t seq =
  printf "SW.incr_l %s + %s\n" (Sequence.to_string t.l) (Sequence.to_string seq);
  t.l <- Sequence.add t.l seq;
  t.l_dirty <- true

let add_m t seq =
  printf "SW.incr_m %s + %s\n" (Sequence.to_string t.m) (Sequence.to_string seq);
  t.m <- Sequence.add t.m seq;
  t.m_dirty <- true

let add_r t seq =
  printf "SW.incr_r %s + %s\n" (Sequence.to_string t.r) (Sequence.to_string seq);
  t.r <- Sequence.add t.r seq;
  t.r_dirty <- true

let wait_lm t =
  let th,u = Lwt.task () in
  let node = Lwt_sequence.add_r u t.lm_w in
  Lwt.on_cancel th (fun () -> Lwt_sequence.remove node);
  th

let wait_mr t =
  let th,u = Lwt.task () in
  let node = Lwt_sequence.add_r u t.mr_w in
  Lwt.on_cancel th (fun () -> Lwt_sequence.remove node);
  th

let rec wakeup_all w =
  match Lwt_sequence.take_opt_l w with
  |None -> ()
  |Some u -> Lwt.wakeup u (); wakeup_all w

(* Wakeup any bound waiters to a variable *)
let wakeup t =
  match t.m_dirty with
  |false -> ()
  |true ->
     if t.l_dirty then wakeup_all t.lm_w;
     if t.r_dirty then wakeup_all t.mr_w

(* Bind a function that is recalculated every time the
 * values of the edges change *)
let rec bind_lm fn t =
  let () = fn t.l t.m in
  lwt () = wait_lm t in
  bind_lm fn t

let rec bind_mr fn t =
  let () = fn t.l t.m in
  lwt () = wait_mr t in
  bind_mr fn t
