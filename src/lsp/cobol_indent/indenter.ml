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

open Indent_type

(*
  source_format: source format
  str: the string representation of the source Cobol code
  rdl: the indentation error information of the source code
  range: range of file to indent
  result: the Cobol code correctly indented (string)
*)
let indenter ~source_format (str:string) (rdl:indent_record list) range =
  let do_one_record (strl:string list) (rd:indent_record) =
    let lnum = rd.lnum in
    let offset = rd.offset_modif - rd.offset_orig in
    let str = List.nth strl (lnum - 1) in
    let newstr =
      match source_format with
      | Cobol_config.SF SFFree ->
        if offset > 0 then
          let space = String.make offset ' ' in
          space^str
        else
          String.sub str (-offset) (String.length str + offset)
      (*TODO: must change if Auto <> SF SFFixed once*)
      | SF SFFixed | Auto ->
        let len = String.length str in
        let str1 = String.sub str 0 7 in
        let str = String.sub str 7 (len-7) in
        let str =
          if offset > 0 then
            let space = String.make offset ' ' in
            space^str
          else
            String.sub str (-offset) (String.length str + offset)
          in
        str1^str
      (*TODO*)
      | _ -> str
    in
    List.mapi (fun i str -> if i = lnum - 1 then newstr else str) (strl)
  in
  let strl = String.split_on_char '\n' str in
  let strl = List.fold_left (fun acc rd -> do_one_record acc rd) strl rdl in
  let strl =
    match range with
    | None -> strl
    | Some {start_line; end_line} ->
      EzList.drop (start_line - 1) @@ EzList.take end_line strl
  in
  String.concat "\n" strl

(*indent a range of file, with the default indent_config*)
let indent_range ~dialect ~source_format ~range ~filename ~contents =
  let state =
    Cobol_preproc.fold_source_lines ~dialect ~source_format
      ~skip_compiler_directives_text:true
      ~f:(fun _lnum line acc -> Indent_check.check_indentation line acc)
      (String { filename; contents })
      { scope = BEGIN; context  = []; acc = []; range }
  in
  (* NB: note here we ignore diagnostics *)
  let ind_recds = state.result.acc in
  indenter ~source_format contents ind_recds state.result.range

(*indent a range of file, with the user-defined indent_config*)
let indent_range ~dialect ~source_format ~indent_config ~range ~filename ~contents =
  begin match indent_config with
    | Some indent_config -> Indent_config.set_config ~indent_config
    | None -> ()
  end;
  indent_range ~dialect ~source_format ~range ~filename ~contents