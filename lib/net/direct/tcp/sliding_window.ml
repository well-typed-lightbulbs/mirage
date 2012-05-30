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

(* An edge only increments and can have waiters for certain
 * conditions to be satisfied on it *)
module Edge = struct
  type t = {
    mutable seq: seq;
  }

  let create ~seq = { seq }
  let seq t = t.seq
end
  
(* Window advances left to right only. No going backwards! *)
type t = {
  l: Edge.t;
  m: Edge.t; 
  r: Edge.t;
}

(* Initialise a new window with initial [isn] and maximum size [mss] *)
let create ~isn ~mss =
  let l = Edge.create ~seq:isn in
  let m = Edge.create ~seq:isn in
  let r = Edge.create ~seq:(Sequence.add isn mss) in
  { l; m; r }

(* Test if a sequence number is valid for a window *)
let valid ~seq t =
  let lseq = Edge.seq t.l in
  let rseq = Edge.seq t.r in
  printf "SW.valid %s < %s < %s\n" (Sequence.to_string lseq)
    (Sequence.to_string seq) (Sequence.to_string rseq);
  Sequence.between seq (Edge.seq t.l) (Edge.seq t.r)
