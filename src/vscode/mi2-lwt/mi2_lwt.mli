(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

type t
(** Type of Mi2 GDB session. *)

val src : Logs.src

val run :
  ?cwd:string ->
  ?env:(string * string) list ->
  path:string ->
  ?args:string list ->
  unit ->
  t Lwt.t

val events : t -> Mi2_parser.async Lwt_stream.t
val streams : t -> Mi2_parser.stream Lwt_stream.t

val stderr : t -> string Lwt_stream.t
(** [stderr t] returns a lazy stream of the error output of GDB itself. *)

val send_command : t -> string -> Mi2_parser.result Lwt.t
(** [send_command t cmd] sends a synchronous Mi2 or CLI command to GDB. The
    returned promises is resolved after receiving the GDB answer. *)

val close : t -> unit Lwt.t
(** [close t] closes the GDB session. An attached target is not killed. *)
