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

(** Gathers some types used to define options for the parser engine. *)
(* We are only defining types here so an MLI would only be redundant. *)

(** Switch for the recovery mechanism *)
type recovery =
  | DisableRecovery
  | EnableRecovery of recovery_options
and recovery_options =
  {
    silence_benign_recoveries: bool; (** Whether to silence reports about some
                                         missing tokens (e.g, periods).  *)
  }

(** Scanners for EXECs are functions fed with EXEC(UTE)/END-EXEC text blocks
    (including the "EXEC" and "END-EXEC" text words). *)
type exec_scanner = Cobol_preproc.Text.t -> Cobol_common.Exec_block.t

type 'm memory =
  | Amnesic: Cobol_common.Behaviors.amnesic memory
  | Eidetic: Cobol_common.Behaviors.eidetic memory

type parser_options =
  {
    verbose: bool;
    show: [`Pending] list;
    recovery: recovery;
    config: Cobol_config.t;
    exec_scanner: exec_scanner;
  }

let default_recovery =
  EnableRecovery { silence_benign_recoveries = false }

let default ~exec_scanner =
  {
    verbose = false;
    show = [`Pending];
    recovery = default_recovery;
    config = Cobol_config.default;
    exec_scanner;
  }
