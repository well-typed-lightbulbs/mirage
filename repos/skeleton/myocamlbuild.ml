(*
 * Copyright (c) 2010-2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2010-2011 Thomas Gazagnaire <thomas@ocamlpro.com>
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

open Ocamlbuild_plugin
open Command
open Ocamlbuild_pack.Ocaml_compiler
open Ocamlbuild_pack.Ocaml_utils
open Ocamlbuild_pack.Tools
open Printf

let ps = Printf.sprintf
let ep = Printf.eprintf

let profiling = false
let native_p4 = true

(* Utility functions (e.g. to execute a command and return lines read) *)
module Util = struct
  let split s ch =
    let x = ref [] in
    let rec go s =
      let pos = String.index s ch in
      x := (String.before s pos)::!x;
      go (String.after s (pos + 1))
    in
    try
      go s
    with Not_found -> !x

    let split_nl s = split s '\n'

    let run_and_read x = List.hd (split_nl (Ocamlbuild_pack.My_unix.run_and_read x))
end

let ocaml_libdir = Util.run_and_read "ocamlc -where"

(* OS detection *)
module OS = struct

  type u = Linux | Darwin | FreeBSD
  type t = Unix of u | Xen | Node

  let host =
    match String.lowercase (Util.run_and_read "uname -s") with
    | "linux"  -> Unix Linux
    | "darwin" -> Unix Darwin
    | "freebsd" -> Unix FreeBSD
    | os -> Printf.eprintf "`%s` is not a supported host OS\n" os; exit (-1)

  let unix_ext = match host with
    | Unix Linux  -> "linux"
    | Unix Darwin -> "macosx"
    | Unix FreeBSD -> "freebsd"
    | _ -> Printf.eprintf "unix_ext called on a non-UNIX host OS\n"; exit (-1)

end

let debug = false
let profiling = false
let trace = false

(* Rules to directly invoke GCC rather than go through OCaml. *)
module CC = struct

  let cc = getenv "CC" ~default:"cc"
  let ar = getenv "AR" ~default:"ar"
  let debug_cflags = ["-Wall"; "-g"; "-O3"]
  let normal_cflags = ["-Wall"; "-g"; "-O3"]

  let cc_call ~tags ~flags ~dep ~prod =
    let tags = tags++"cc"++"c" in
    let inc = A (sprintf "-I%s/%s" Pathname.pwd (Filename.dirname dep)) in
    Cmd (S (A cc :: inc :: flags @ [T tags; A"-o"; Px prod; P dep]))

  let cc_archive ~mode clib a path env builder =
    let clib = env clib and a = env a and path = env path in
    let objs = List.map (fun x -> path / x) (string_list_of_file clib) in
    let objs = List.map (fun x -> (Filename.chop_extension x) ^ ("."^mode ^ ".o")) objs in
    let objs = List.map Outcome.good (builder (List.map (fun x -> [x]) objs)) in
    Cmd(S[A ar; A"rc"; Px a; T(tags_of_pathname a++"c"++"archive"); atomize objs])

  let register_c_mode ~mode ~descr ~fn =
    let prod = sprintf "%%.%s.o" mode in
    rule (sprintf "cc: .c -> %s (%s)" prod descr)
      ~prod ~deps:["%.c"; "%.c.deps"]
      (fun env builder ->
         let deps = string_list_of_file (env "%.c.deps") in
         let cdeps = List.filter (Pathname.check_extension "c") deps in
         let _ = List.map Outcome.good (builder (List.map (fun x -> [x]) cdeps)) in
         let dep = env "%.c" in
         let prod = env prod in
         fn ~dep ~prod
      );
    rule (sprintf "archive: cclib %s.o -> %s.a archive" mode mode)
      ~prod:(sprintf "%%(path:<**/>)lib%%(libname:<*>).%s.a" mode)
      ~dep:"%(path)lib%(libname).cclib"
      (cc_archive ~mode "%(path)lib%(libname).cclib" (sprintf "%%(path)lib%%(libname).%s.a" mode) "%(path)")

  let () =
    let debug_fn ~dep ~prod =
      let tags = tags_of_pathname dep ++ "compile" in
      let flags = List.map (fun x -> A x) ("-c" :: debug_cflags) in
      cc_call ~tags ~flags ~dep ~prod in
    let normal_fn ~dep ~prod =
      let tags = tags_of_pathname dep ++ "compile" in
      let flags = List.map (fun x -> A x) ("-c" :: normal_cflags) in
      cc_call ~tags ~flags ~dep ~prod in
    register_c_mode ~mode:"d" ~descr:"debug" ~fn:debug_fn;
    register_c_mode ~mode:"n" ~descr:"normal" ~fn:normal_fn

  let () =
    rule "cc: .c -> .c.deps.raw (raw gcc dependency list)"
      ~prod:"%.c.deps.raw" ~dep:"%.c"
      (fun env _ ->
        let prod = env "%.c.deps.raw" in
        let dep = env "%.c" in
        let flags = [A"-MM";] in
        let tags = tags_of_pathname dep ++ "depend" in
        cc_call ~tags ~flags ~dep ~prod
      );
    rule "cc: .c.deps.raw -> .c.deps (dependency list excluding source)"
      ~prod:"%.c.deps" ~dep:"%.c.deps.raw"
      (fun env builder ->
        let input = env "%.c.deps.raw" in
        let output = env "%.c.deps" in
        match string_list_of_file input with
        |targ::src::deps ->
           let deps = List.filter (fun x -> x <> "\\") deps in
           let deps = List.map (fun x -> x ^ "\n") deps in
           Echo (deps, output)
        |_ -> failwith "error in dependency file .deps"
      )
(*
  let cc_cflags = List.map (fun x -> A x) !cflags

  let cc_c tags arg out =
    let tags = tags++"cc"++"c" in
    Cmd (S (A cc :: [ A"-c"; T(tags++"compile"); A"-o"; Px out; P arg]))

  let cc_compile_c_implem ?tag c o env build =
    let c = env c and o = env o in
    cc_c (tags_of_pathname c++"implem"+++tag) c o

  let () =
    rule "cc: .nc.c -> .c"
      ~prod:"%.nc.c" ~dep:"%.c"
      (fun env _ -> cp (env "%.c") (env "%.nc.c"));

    rule "cc: .bc.c -> .c"
      ~prod:"%.bc.c" ~dep:"%.c"
      (fun env _ -> cp (env "%.c") (env "%.bc.c"));

    rule "cc: .c -> .o include ocaml dir"
      ~tags:["cc"; "c"]
      ~prod:"%.o" ~dep:"%.c"
      (cc_compile_c_implem "%.c" "%.o");

    rule "cc: .S -> .o assembly compile"
      ~prod:"%.o" ~dep:"%.S"
      (cc_compile_c_implem ~tag:"asm" "%.S" "%.o");

    rule "cc: _linux.c -> _os.c platform file"
      ~prod:"%_os.c"
      ~dep:("%_" ^ OS.unix_ext ^ ".c")
      (fun env _ ->
        let filename = env "%" in
        ln_s (ps "%s_%s.c" (Pathname.basename filename) OS.unix_ext) (ps "%s_os.c" filename);
      );

    rule "archive: cclib .o -> .a archive"
      ~prod:"%(path:<**/>)lib%(libname:<*> and not <*.*>).a"
      ~dep:"%(path)lib%(libname).cclib"
      (cc_archive "%(path)lib%(libname).cclib" "%(path)lib%(libname).a" "%(path)")
*)

end

(* Xen cross compilation *)
(*
module Xen = struct
  open CC
  (* All the xen cflags for compiling against an embedded environment *)
  let xen_incs =
    (* base GCC standard include dir *)
    let gcc_install =
      let cmd = ps "LANG=C %s -print-search-dirs | sed -n -e 's/install: \\(.*\\)/\\1/p'" cc in
      let dir = Util.run_and_read cmd in
      Filename.concat dir "include" in
    (* root dir of xen bits *)
    let rootdir = ps "%s/runtime_xen" Pathname.pwd in
    let root_incdir = ps "%s/include" rootdir in
    (* Basic cflags *)
    let all_cflags = List.map (fun x -> A x)
      [ "-U"; "__linux__"; "-U"; "__FreeBSD__";
        "-U"; "__sun__"; "-D__MiniOS__";
        "-D__MiniOS__"; "-D__x86_64__";
        "-D__XEN_INTERFACE_VERSION__=0x00030205";
        "-D__INSIDE_MINIOS__";
        "-nostdinc"; "-std=gnu99"; "-fno-stack-protector";
        "-m64"; "-mno-red-zone"; "-fno-reorder-blocks";
        "-fstrict-aliasing"; "-momit-leaf-frame-pointer"; "-mfancy-math-387"
      ] in
    (* Include dirs *)
    let incdirs= A ("-I"^gcc_install) :: List.flatten (
      List.map (fun x ->[A"-isystem"; A (ps "%s/%s" root_incdir x)])
        [""; "mini-os"; "mini-os/x86"]) in
    all_cflags @ incdirs

  (* The private libm include dir *)
  let libm_incs = [ A (ps "-I%s/runtime_xen/libm" Pathname.pwd) ]

  (* defines used by the ocaml runtime, as well as includes *)
  let ocaml_debug_inc = if debug then [A "-DDEBUG"] else []
  let ocaml_incs = [
    A "-DCAML_NAME_SPACE"; A "-DTARGET_amd64"; A "-DSYS_xen";
    A (ps "-I%s/runtime_xen/ocaml" Pathname.pwd) ] @ ocaml_debug_inc

  let ocaml_asmrun = [ A"-DNATIVE_CODE" ]
  let ocaml_byterun = [ ]

  (* ocaml system include directory i.e. /usr/lib/ocaml *)
  let ocaml_sys_incs = [ A"-I"; Px (Util.run_and_read "ocamlc -where"); ]

  (* dietlibc bits, mostly extra warnings *)
  let dietlibc_incs = [
    A "-Wextra"; A "-Wchar-subscripts"; A "-Wmissing-prototypes";
    A "-Wmissing-declarations"; A "-Wno-switch"; A "-Wno-unused"; 
    A "-Wredundant-decls"; A "-D__dietlibc__";
    A (ps "-I%s/runtime_xen/dietlibc" Pathname.pwd)
  ]

  let cc_cflags = List.map (fun x -> A x) !cflags

  let cc_c tags arg out =
    let tags = tags++"cc"++"c" in
    Cmd (S (A cc :: [ A"-c"; T(tags++"compile"); A"-o"; Px out; P arg]))

  let cc_compile_c_implem ?tag c o env build =
    let c = env c and o = env o in
    cc_c (tags_of_pathname c++"implem"+++tag) c o

  let cc_archive clib a path env build =
    let clib = env clib and a = env a and path = env path in
    let objs = List.map (fun x -> path / x) (string_list_of_file clib) in
    let results = build (List.map (fun x -> [x]) objs) in
    let objs = List.map (function
      | Outcome.Good o -> o
      | Outcome.Bad exn -> raise exn) results in
    Cmd(S[A ar; A"rc"; Px a; T(tags_of_pathname a++"c"++"archive"); atomize objs])

  let () =
    rule "cc: .nc.c -> .c"
      ~prod:"%.nc.c" ~dep:"%.c"
      (fun env _ -> cp (env "%.c") (env "%.nc.c"));

    rule "cc: .bc.c -> .c"
      ~prod:"%.bc.c" ~dep:"%.c"
      (fun env _ -> cp (env "%.c") (env "%.bc.c"));

    rule "cc: .c -> .o include ocaml dir"
      ~tags:["cc"; "c"]
      ~prod:"%.o" ~dep:"%.c"
      (cc_compile_c_implem "%.c" "%.o");

    rule "cc: .S -> .o assembly compile"
      ~prod:"%.o" ~dep:"%.S"
      (cc_compile_c_implem ~tag:"asm" "%.S" "%.o");

    rule "cc: _linux.c -> _os.c platform file"
      ~prod:"%_os.c"
      ~dep:("%_" ^ OS.unix_ext ^ ".c")
      (fun env _ ->
        let filename = env "%" in
        ln_s (ps "%s_%s.c" (Pathname.basename filename) OS.unix_ext) (ps "%s_os.c" filename);
      );

    rule "archive: cclib .o -> .a archive"
      ~prod:"%(path:<**/>)lib%(libname:<*> and not <*.*>).a"
      ~dep:"%(path)lib%(libname).cclib"
      (cc_archive "%(path)lib%(libname).cclib" "%(path)lib%(libname).a" "%(path)")
end

let _ = dispatch begin function
  | After_rules ->
    (* do not compile and pack with the standard lib *)
    flag ["ocaml"; "compile"; ] & S [A"-nostdlib"; A"-annot"];
    if debug then flag ["ocaml"; "compile"] & S [A"-g"];
    flag ["ocaml"; "pack"; ] & S [A"-nostdlib"];
    if profiling then flag ["ocaml"; "compile"; "native" ] & S [A"-p"];
    (* ocamldoc always uses the JSON generator *)
    let docgenerator = "../../../docs/_build/odoc_json.cmxs" in
    flag ["ocaml"; "doc"] & S [A"-g"; Px docgenerator ];
    (* use pa_`lib` syntax extension if the _tags file specifies it *)
    let p4_build = "../../../syntax/_build" in
    let camlp4_bc =
      S[A"-pp"; 
        A (ps "camlp4o -I %s str.cma pa_mirage.cma -cow-no-open %s %s" 
             p4_build (if debug then "-lwt-debug" else "")
             (if trace then "pa_trace.cma" else "")
          )
       ] 
    in
    let camlp4_nc = 
      S[A"-pp";
        A (ps "camlp4o.opt -I %s str.cmxs pa_mirage.cmxs -cow-no-open %s %s"
             p4_build (if debug then "-lwt-debug" else "")
             (if trace then "pa_trace.cmxs" else "")
          )] 
    in
    let camlp4_cmd = if native_p4 then camlp4_nc else camlp4_bc in
    flag ["ocaml"; "compile" ; "pa_mirage"] & camlp4_cmd;
    flag ["ocaml"; "ocamldep"; "pa_mirage"] & camlp4_cmd;
    flag ["ocaml"; "infer_interface"; "pa_mirage"] & camlp4_cmd;
    flag ["ocaml"; "doc"; "pa_mirage"] & camlp4_cmd;
    (* add a dependency to the local pervasives, only used in stdlib compile *)
    dep ["ocaml"; "compile"; "need_pervasives"] ["std/pervasives.cmi"];

    (* base cflags for C code *)
    flag ["c"; "compile"] & S CC.cc_cflags;
    flag ["asm"; "compile"] & S [A "-D__ASSEMBLY__"];
    if profiling then
      flag ["c"; "compile"] & S [A"-pg"];

    (* xen code needs special cflags *)
    flag ["c"; "compile"; "include_xen"] & S CC.xen_incs;
    flag ["c"; "compile"; "include_libm"] & S CC.libm_incs;
    flag ["c"; "compile"; "include_ocaml"] & S CC.ocaml_incs;
    flag ["c"; "compile"; "ocaml_byterun"] & S CC.ocaml_byterun;
    flag ["c"; "compile"; "ocaml_asmrun"] & S CC.ocaml_asmrun;
    flag ["c"; "compile"; "include_system_ocaml"] & S CC.ocaml_sys_incs;
    flag ["c"; "compile"; "include_dietlibc"] & S CC.dietlibc_incs;
    flag ["c"; "compile"; "pic"] & S [A"-fPIC"]
  | _ -> ()
end
*)

let _ = dispatch begin function
  | After_rules ->
     flag ["cc";"depend"; "include_syscaml"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "include_syscaml"] & S [A("-I"^ocaml_libdir)];
  | _ -> ()
end
