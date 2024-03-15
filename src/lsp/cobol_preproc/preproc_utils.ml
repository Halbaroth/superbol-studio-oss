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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX
open Preproc_outputs.TYPES

module OUT = Preproc_outputs

module Make (Config: Cobol_config.T) = struct

  let safe_partial_replacing_when_src_literal ~loc =
    Config.safe_partial_replacing_when_src_literal#verify ~loc |>
    OUT.of_config_verif |>
    OUT.map_result ~f:(function Some s -> s = `Safe | None -> false)

  let replacing' ?repl_dir repl_from repl_to =
    match repl_dir, ~&repl_from with
    | None, (`PseudoText src | `Alphanum src) ->
        Text_processor.replacing (src &@<- repl_from) repl_to
    | Some repl_dir, `PseudoText src ->
        Text_processor.replacing ~partial:{ repl_dir; repl_strict = false }
          (src &@<- repl_from) repl_to
    | Some repl_dir, `Alphanum src ->
        let { result = repl_strict; diags } =
          let loc = ~@repl_to in
          match ~&repl_to with
          | [{ payload = PseudoWord [{ payload = PwText str; _ }]; _ }]
            when str = "" ->
              safe_partial_replacing_when_src_literal ~loc
          | [{ payload = PseudoWord [{ payload = PwText str; _ }]; _ }]
            when String.contains str ' ' ||     (* TODO: properly check spaces *)
                 String.contains str '\t' ->                         (* reject *)
              OUT.error_result false @@
              Forbidden { loc; stuff = Operand_with_spaces }
          | [{ payload = PseudoWord (_::_::_); _ }] | _::_::_ ->
              OUT.error_result false @@
              Forbidden { loc; stuff = Multiword_operand }
          | _ ->
              OUT.result false
        in
        OUT.with_more_diags ~diags @@
        Text_processor.replacing ~partial:{ repl_dir; repl_strict }
          (src &@<- repl_from) repl_to

  let filter_map_4_list_with_diags'
    : 'a option with_diags with_loc list -> 'a with_loc list with_diags = fun l ->
    List.fold_left
      begin fun (result, diags) { payload = { result = r; diags = d }; loc } ->
        (match r with None -> result | Some r -> (r &@ loc) :: result),
        Preproc_diagnostics.union diags d
      end ([], Preproc_diagnostics.none) l |>
    fun (result, diags) -> { result = List.rev result; diags }

end
