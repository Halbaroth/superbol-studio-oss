(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ez_file.V1

type fileloc = [ `Word of string | `Alphanum of string ]

type lookup_config =
  {
    lookup_path: string list;
    lookup_exts: string list;
  }

type lookup_error =
  {
    lookup_libname: string;
    lookup_config: lookup_config;
  }

(** Filename extensions that we should treat as copybooks and not main
    programs. *)
let copybook_extensions =  (* this must be a subset of {!libfile_extensions}. *)
  ["cpy"; "cbx"]

let libfile_extensions =
  ["cpy"; "cbl"; "cob"; "cbx"]

let lookup_config ?(libexts = libfile_extensions) libpath =
  {
    lookup_path = libpath;
    lookup_exts = List.map String.lowercase_ascii libexts;
  }

let pp_lookup_config ppf { lookup_path; lookup_exts } =
  let pp_path ppf = function
    | [] -> Pretty.string ppf "<empty>"
    | path -> Pretty.path ppf path
  in
  Pretty.print ppf
    "@[- search@ path:@ %a@]@;\
     @[- filename@ extensions:@ %a@]"
    pp_path lookup_path Fmt.(list ~sep:sp @@ fmt "%S") lookup_exts

(* --- *)

type directory =
  {
    dir: string;
    files: string StringMap.t;                (* key is basename in lowercase *)
  }

let find_lib ~lookup_config:({ lookup_path = libpath;
                               lookup_exts = libexts } as lookup_config)
    ?fromfile ?libname textname : (string, lookup_error) result =
  let libpath = match libname, fromfile with
    | None, _ ->
        libpath
    | Some (`Word d | `Alphanum d), Some file
      when EzFile.is_relative d ->
        EzFile.[concat (dirname file) d]
    | Some (`Word d | `Alphanum d), _ ->
        [d]
  in
  let libpath_files =
    List.map begin fun dir ->
      let files =
        let files = try Sys.readdir dir with Sys_error _ -> [||] in
        let map = ref StringMap.empty in
        Array.iter begin fun file ->
          map := StringMap.add (String.lowercase_ascii file) file !map
        end files;
        !map
      in
      { dir; files }
    end libpath
  in
  let try_file libname exts =
    let base = String.lowercase_ascii libname in
    let without_ext d =
      Ok (Filename.concat d.dir @@ StringMap.find base d.files)
    and with_ext d ext =
      Ok (Filename.concat d.dir @@ StringMap.find (base ^ "." ^ ext) d.files)
    in
    let rec iter_path path =
      match path with
      | [] -> Error { lookup_libname = libname;
                      lookup_config = { lookup_config with
                                        lookup_path = libpath } }
      | d :: path -> iter_exts d path exts
    and iter_exts d path exts =
      match exts with
      | [] -> (try without_ext d with Not_found -> iter_path path)
      | ext :: exts -> (try with_ext d ext with Not_found -> iter_exts d path exts)
    in
    iter_path libpath_files
  in
  match textname with
  | `Alphanum w ->                        (* assume no more filename extension *)
      try_file w []
  | `Word w ->
      match try_file w libexts with
      | Ok lib -> Ok lib
      | Error err -> Error { err with lookup_libname = w }

let pp_lookup_error ppf { lookup_libname; lookup_config } =
  Pretty.print ppf
    "@[<v>@[Library@ `%s'@ not@ found@]@;%a@]"
    lookup_libname pp_lookup_config lookup_config
