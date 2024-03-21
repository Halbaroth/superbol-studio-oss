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

open Cobol_ptree

module Overlay_manager: Cobol_preproc.Src_overlay.MANAGER

val relation_condition
  : neg: bool
  -> Cobol_ptree.binary_relation
  -> (Cobol_ptree.logop * Cobol_ptree.flat_combined_relation) option
  -> Cobol_ptree.condition


type data_division_sentence =
  | S_DATA_DIVISION
  | S_FILE_SECTION
  | S_FILE_SECTION_PART of file_descr with_loc list with_loc
  | S_WORKING_STORAGE_SECTION of working_storage_section with_loc
  | S_LOCAL_STORAGE_SECTION of local_storage_section with_loc
  | S_LINKAGE_SECTION of linkage_section with_loc
  | S_COMMUNICATION_SECTION of communication_section with_loc
  | S_REPORT_SECTION of report_section with_loc
  | S_SCREEN_SECTION of screen_section with_loc

val build_data_division : data_division_sentence list with_loc ->
  data_division with_loc option
