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

(* XXX this wont work with a custom ocamlc not on the path *)
let ocaml_libdir = Util.run_and_read "ocamlc -where"

(* Configuration rules for packages *)
module Configure = struct
  (* Read a list of lines from files in _config/<arg> *)
  let config x = 
    try string_list_of_file (Pathname.pwd ^ "/_config/" ^ x)
    with Sys_error _ ->
      eprintf "_config/%s not found: run ./configure first\n%!" x;
      exit 1

  (* Read a config file as a shell fragment to be appended directly. Repeat
   * lines are also filtered, but ordering preserved. *)
  let config_sh x = Sh (String.concat " " (config x))

  (* Test to see if a flag file exists *)
  let test_flag x = Sys.file_exists (sprintf "%s/_config/flag.%s" Pathname.pwd x)
  let opt_flag fn a = if test_flag "opt" then List.map fn a else []
  let natdynlink_flag fn a = if test_flag "natdynlink" then opt_flag fn a else []

  (* Flags for building and using syntax extensions *)
  let ppflags () =
    (* Syntax extensions for the libraries being built *)
    flag ["ocaml"; "pp"] & S [config_sh "syntax.deps"];
    (* Include the camlp4 flags to build an extension *)
    flag ["ocaml"; "pp"; "build_syntax"] & S [config_sh "syntax.build"];
    (* NOTE: we cannot use the built-in use_camlp4, as that forces
     * camlp4lib.cma to be linked with the mllib target, which results
     * in a non-functioning extension as it will be loaded twice.
     * So this simply includes the directory, which leads to a working archive *) 
    let p4incs = [A"-I"; A"+camlp4"] in
    flag ["ocaml"; "ocamldep"; "build_syntax"] & S p4incs;
    flag ["ocaml"; "compile"; "build_syntax"] & S p4incs
 
  (* General flags for building libraries and binaries *)
  let libflags () =
    (* Include path for dependencies *)
    flag ["ocaml"; "ocamldep"] & S [config_sh "flags.ocaml"];
    flag ["ocaml"; "compile"] & S [config_sh "flags.ocaml"];
    (* Include the -cclib for any C bindings being built *)
    let ccinc = (A"-ccopt")::(A"-Lruntime"):: 
      (List.flatten (List.map (fun x -> [A"-cclib"; A("-l"^x)]) (config "clibs"))) in
    let clibs_files = List.map (sprintf "runtime/lib%s.a") (config "clibs") in
    dep ["link"; "library"; "ocaml"] clibs_files;
    flag ["link"; "library"; "ocaml"; "byte"] & S ccinc;
    flag ["link"; "library"; "ocaml"; "native"] & S ccinc

  (* Flags for building test binaries, which include just-built extensions and libs *)
  let testflags () =
    List.iter (ocaml_lib ~tag_name:"use_lib" ~dir:"lib") (List.map ((^)"lib/") (config "lib"));
    (* The test binaries also depend on the just-built libraries *)
    let lib_nc = List.map (fun x -> "lib/"^x-.-"cmxa") (config "lib") in
    let lib_bc = List.map (fun x -> "lib/"^x-.-"cma") (config "lib") in
    dep ["ocaml"; "native"; "use_lib"] lib_nc;
    dep ["ocaml"; "byte"; "use_lib"] lib_bc;
    let lib_nc_sh = config_sh "archives.native" :: (List.map (fun x -> P x) lib_nc) in
    let lib_bc_sh = config_sh "archives.byte" :: (List.map (fun x -> P x) lib_bc) in
    flag ["ocaml"; "link"; "native"; "program"] & S lib_nc_sh;
    flag ["ocaml"; "link"; "byte"; "program"] & S lib_bc_sh;
    (* TODO gen native syntax.deps *)
    let syntax_bc = List.map (sprintf "syntax/%s.cma") (config "syntax") in
    let syntax_bc_use = List.map (fun x -> P x) syntax_bc in
    flag ["ocaml"; "pp"; "use_syntax"] & S syntax_bc_use;
    dep ["ocaml"; "ocamldep"; "use_syntax"] syntax_bc

  let flags () =
    ppflags ();
    libflags ();
    testflags ()

  (* Create an .all target based on _config flags *)
  let rules () =
    rule "build all targets: %.all contains what was built" ~prod:"%.all"
      (fun env builder ->
        let build_lib ~byte ?native ?natdynlink () =
          let libs = List.map ((^)"lib/") (config "lib") in
          let byte = List.flatten (List.map (fun lib -> List.map (fun e->lib^e) byte) libs) in
          let native = match native with None -> []
            |Some n -> if test_flag "opt" then List.map (fun x -> x^n) libs else [] in
          let natdynlink = match natdynlink with None -> []
            |Some n -> if test_flag "natdynlink" then List.map (fun x -> x^n) libs else [] in
          byte @ native @ natdynlink in
        let libs = build_lib ~byte:[".cmi";".cma"] ~native:".cmxa" ~natdynlink:".cmxs" () in
        (* Build runtime libs *)
        let runtimes = List.map (sprintf "runtime/lib%s.a") (config "clibs") in
        (* Build syntax extensions *)
        let syntaxes =
          let syn = config "syntax" in
          let bc = List.map (fun x -> sprintf "syntax/%s.cma" x) syn in
          let nc = opt_flag (fun x -> sprintf "syntax/%s.cmxa" x) syn in
          let ncs = natdynlink_flag (fun x -> sprintf "syntax/%s.cmxs" x) syn in
          bc @ nc @ ncs in
        (* Execute the rules and echo everything built into the %.all file *) 
        let targs = libs @ runtimes @ syntaxes in
        let out = List.map Outcome.good (builder (List.map (fun x -> [x]) targs)) in
        Echo ((List.map (fun x -> x^"\n") out), (env "%.all")) 
      )
end

(* Rules to directly invoke GCC rather than go through OCaml. *)
module CC = struct

  let cc = getenv "CC" ~default:"cc"
  let ar = getenv "AR" ~default:"ar"
  let cflags = getenv "CFLAGS" ~default:"-Wall -O2"

  let cc_call tags dep prod env builder =
    let dep = env dep and prod = env prod in
    let tags = tags_of_pathname dep++"cc"++"compile"++tags in 
    let flags = [A"-c"; Sh cflags] in
    let inc = A (sprintf "-I%s/%s" Pathname.pwd (Filename.dirname dep)) in
    Cmd (S (A cc :: inc :: flags @ [T tags; A"-o"; Px prod; P dep]))

  let cc_archive clib a path env builder =
    let clib = env clib and a = env a and path = env path in
    let objs = List.map (fun x -> path / x) (string_list_of_file clib) in
    let objs = List.map (fun x -> (Filename.chop_extension x)^".o") objs in
    let objs = List.map Outcome.good (builder (List.map (fun x -> [x]) objs)) in
    Cmd(S[A ar; A"rc"; Px a; T(tags_of_pathname a++"c"++"archive"); atomize objs])

  let rules () = 
    rule "cc: %.c -> %.o" ~prod:"%.o" ~dep:"%.c" (cc_call "c" "%.c" "%.o");
    rule "cc: %.S -> %.o" ~prod:"%.o" ~dep:"%.c" (cc_call "asm" "%.S" "%.o");
    rule "archive: cclib .o -> .a archive"
      ~prod:"%(path:<**/>)lib%(libname:<*>).a"
      ~dep:"%(path)lib%(libname).cclib"
      (cc_archive "%(path)lib%(libname).cclib" "%(path)lib%(libname).a" "%(path)")

  let flags () =
     flag ["cc";"depend"; "c"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "c"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "asm"] & S [A"-D__ASSEMBLY__"]
end

let _ = Options.make_links := false;;

let _ = dispatch begin function
  | Before_rules ->
     Configure.rules ();
     CC.rules ();
  | After_rules ->
     Configure.flags ();
     CC.flags ();
  | _ -> ()
end
