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

(* Configuration rules for packages *)
module Configure = struct
  (* Read a list of lines from files in _config/<arg> *)
  let config x = 
    try string_list_of_file (Pathname.pwd ^ "/_config/" ^ x)
    with Sys_error _ ->
      eprintf "_config/%s not found: run ./configure first\n%!" x;
      exit 1

  (* Preprocessor flags for syntax extensions *)
  let ppflags () =
    let camlp4o flags = S ((List.map (fun x -> A x) flags)) in
    (* Include the camlp4 flags to build an extension *)
    flag ["ocaml";"pp";"build_syntax"] & (camlp4o (config "syntax.build"));
    (* Syntax extensions for the libraries being built *)
    let flags = config "syntax.deps" in
    if List.length flags > 0 then flag ["ocaml";"pp"] & (camlp4o flags);
    (* Extra extensions for test files from the extension just built *)
    match flags @ (config "syntax.test") with
    |[] -> ()
    |tflags ->
      flag ["ocaml";"pp";"build_test"] & (camlp4o tflags);
      dep ["ocaml";"ocamldep";"build_test"] & (config "syntax.test")

  let flags () =
    (* Include path for dependencies *)
    let oincs = List.map (fun x -> Sh x) (config "flags.inc") in
    flag ["ocaml"; "ocamldep"] & S oincs;
    flag ["ocaml"; "compile"] & S oincs;
    (* NOTE: we cannot use the built-in use_camlp4, as that forces
     * camlp4lib.cma to be linked with the mllib target, which results
     * in a non-functioning extension as it will be loaded twice.
     * So this simply includes the directory, which leads to a working archive *) 
    let p4incs = [A"-I"; A"+camlp4"] in
    flag ["ocaml"; "ocamldep"; "build_syntax"] & S p4incs;
    flag ["ocaml"; "compile"; "build_syntax"] & S p4incs;
    (* For the test libraries, include the just-built library path *)
    List.iter (ocaml_lib ~tag_name:"build_test" ~dir:"lib") (config "lib.built");
    let to_p x = List.map (fun x -> P x) (config x) in
    dep ["ocaml"; "native"; "build_test"] (config "lib.built.native");
    dep ["ocaml"; "byte"; "build_test"] (config "lib.built.byte");
    flag ["ocaml"; "link"; "native"; "build_test"] & S(to_p "archives.native");
    flag ["ocaml"; "link"; "byte"; "build_test"] & S(to_p "archives.byte");
    (* Include the -cclib for any bindings being built *)
    let ccinc_n = (A"-ccopt")::(A"-Lruntime"):: 
     (List.flatten (List.map (fun x -> [A"-cclib"; A("-l"^x)]) (config "clibs.normal"))) in
    dep ["link"; "library"; "ocaml"] (List.map (fun lib -> "runtime/lib"^lib^".a") (config "clibs.normal"));
    flag ["link"; "library"; "ocaml"; "byte"] & S ccinc_n;
    flag ["link"; "library"; "ocaml"; "native"] & S ccinc_n
end

(* Rules to directly invoke GCC rather than go through OCaml. *)
module CC = struct

  let cc = getenv "CC" ~default:"cc"
  let ar = getenv "AR" ~default:"ar"
  let debug_cflags = ["-Wall"; "-g"; "-O1"]
  let normal_cflags = ["-Wall"; "-O3"]

  let cc_call ~tags ~flags dep prod env builder =
    let tags = tags ++ "cc" in
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
      ~prod ~deps:["%.c"] (*; "%.c.deps"] *)
      (fun env builder ->
         let dep = env "%.c" in
         let prod = env prod in
         let tags = tags_of_pathname (env dep) ++ "c" in 
         fn tags dep prod env builder
      );
    rule (sprintf "cc: .S -> %s (%s)" prod descr)
     ~prod ~dep:"%.S"
     (fun env builder ->
         let dep = env "%.S" in
         let prod = env prod in
         let tags = tags_of_pathname (env dep) ++ "asm" in 
         fn tags dep prod env builder
     );
    rule (sprintf "archive: cclib %s.o -> %s.a archive" mode mode)
      ~prod:(sprintf "%%(path:<**/>)lib%%(libname:<*>).%s.a" mode)
      ~dep:"%(path)lib%(libname).cclib"
      (cc_archive ~mode "%(path)lib%(libname).cclib" (sprintf "%%(path)lib%%(libname).%s.a" mode) "%(path)")

  let () =
    let debug_fn tags dep prod env builder =
      let tags = tags ++ "compile" in
      let flags = List.map (fun x -> A x) ("-c" :: debug_cflags) in
      cc_call ~tags ~flags dep prod env builder in
    let normal_fn tags dep prod env builder =
      let tags = tags ++ "compile" in
      let flags = List.map (fun x -> A x) ("-c" :: normal_cflags) in
      cc_call ~tags ~flags dep prod env builder in
    register_c_mode ~mode:"d" ~descr:"debug" ~fn:debug_fn;
    register_c_mode ~mode:"n" ~descr:"normal" ~fn:normal_fn

  let rules () =
    rule "cc: .c -> .c.deps.raw (raw gcc dependency list)"
      ~prod:"%.c.deps.raw" ~dep:"%.c"
      (fun env builder ->
        let prod = env "%.c.deps.raw" in
        let dep = env "%.c" in
        let flags = [A"-MM";] in
        let tags = tags_of_pathname dep ++ "depend" in
        cc_call ~tags ~flags dep prod env builder
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

  let flags () =
     flag ["cc";"depend"; "build_runtime"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "build_runtime"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "asm"] & S [A"-D__ASSEMBLY__"]
end

(* Xen cross compilation *)
module Xen = struct
  (* All the xen cflags for compiling against an embedded environment *)
  let xen_incs =
    (* base GCC standard include dir *)
    let gcc_install =
      let cmd = ps "LANG=C %s -print-search-dirs | sed -n -e 's/install: \\(.*\\)/\\1/p'" CC.cc in
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
  let debug = false
  let ocaml_debug_inc = if debug then [A "-DDEBUG"] else []
  let ocaml_incs = [
    A "-DCAML_NAME_SPACE"; A "-DTARGET_amd64"; A "-DSYS_xen";
    A (ps "-I%s/runtime_xen/ocaml" Pathname.pwd) ] @ ocaml_debug_inc

  let ocaml_asmrun = [ A"-DNATIVE_CODE" ]
  let ocaml_byterun = [ ]

  (* dietlibc bits, mostly extra warnings *)
  let dietlibc_incs = [
    A "-Wextra"; A "-Wchar-subscripts"; A "-Wmissing-prototypes";
    A "-Wmissing-declarations"; A "-Wno-switch"; A "-Wno-unused"; 
    A "-Wredundant-decls"; A "-D__dietlibc__";
    A (ps "-I%s/runtime_xen/dietlibc" Pathname.pwd)
  ]

  let rules () =
    let xen_cflags = xen_incs @ dietlibc_incs in
    let xen_fn ~cflags tags dep prod env builder =
      let tags = tags ++ "compile" ++ "xen" in
      let flags = A"-c" :: xen_cflags @ (List.map (fun x -> A x) cflags) in
      CC.cc_call ~tags ~flags dep prod env builder in
    CC.register_c_mode ~mode:"xn" ~descr:"xen normal" ~fn:(xen_fn ~cflags:CC.normal_cflags);
    CC.register_c_mode ~mode:"xd" ~descr:"xen debug" ~fn:(xen_fn ~cflags:CC.debug_cflags);
    (* Custom rule to copy an ocaml file to .nc.c or .bc.c for native/bytecode *)
    rule "cc: .nc.c -> .c"
      ~prod:"%.nc.c" ~dep:"%.c"
      (fun env _ -> cp (env "%.c") (env "%.nc.c"));
    rule "cc: .bc.c -> .c"
      ~prod:"%.bc.c" ~dep:"%.c"
      (fun env _ -> cp (env "%.c") (env "%.bc.c"))

  let flags () =
    flag ["ocaml_asmrun"] & S[A"-DNATIVE_CODE"];
    flag ["ocaml_byterun"] & S[A"-DBYTE_CODE"]
end

let _ = Options.make_links := false;;

let _ = dispatch begin function
  | Before_rules ->
     CC.rules ();
     Xen.rules ();
  | After_rules ->
     CC.flags ();
     Xen.flags ();
     Configure.ppflags ();
     Configure.flags ()
  | _ -> ()
end
