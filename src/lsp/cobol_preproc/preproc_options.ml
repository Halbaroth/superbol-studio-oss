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

type preproc_options =
  {
    verbose: bool;
    libpath: string list;
    config: Cobol_config.t;
    source_format: Cobol_config.source_format_spec;
    env: Preproc_env.t;
  }

let default =
  {
    verbose = false;
    libpath = [];
    config = Cobol_config.default;
    source_format = Cobol_config.Auto;
    env = Preproc_env.empty;
  }
