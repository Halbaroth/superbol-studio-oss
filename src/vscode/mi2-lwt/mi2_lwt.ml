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

module Record = Mi2_parser.Record

type 'a promise = 'a Lwt.t * 'a Lwt.u

type t = {
  process : Lwt_process.process_full;
  mutable tok : int;
  promises : (int, Record.result promise) Hashtbl.t;
  events : Record.async Lwt_stream.t;
  streams : Record.stream Lwt_stream.t;
  stderr : string Lwt_stream.t;
}

let src = Logs.Src.create ~doc:"Mi2-lwt" "Mi2_lwt"

module Log = (val Logs.src_log src : Logs.LOG)

let rec read_stdout process () =
  match%lwt Lwt_io.read_line process#stdout with
  | exception Lwt_io.Channel_closed _ ->
      (* Happen when the gdb process is ended. *)
      Lwt.return_none
  | exception End_of_file -> Lwt.return_none
  | "(gdb) " ->
      (* REVIEW: vérifier que ça fonctionne bien sous Windows aussi. *)
      read_stdout process ()
  | line -> (
      let lexbuf = Lexing.from_string ~with_positions:false line in
      match Mi2_parser.parse lexbuf with
      | result -> Lwt.return_some result
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Log.err (fun k ->
              k "unexpected parsing error: %s@ %s" (Printexc.to_string exn)
                (Printexc.raw_backtrace_to_string bt));
          read_stdout process ())

let read_stderr process () =
  match%lwt Lwt_io.read_line process#stderr with
  | exception End_of_file ->
      (* Happen when the gdb process is ended. *)
      Lwt.return_none
  | exception Lwt_io.Channel_closed _ -> Lwt.return_none
  | line -> Lwt.return_some line

let write_stdin process fmt =
  Format.kasprintf (Lwt_io.write_line process#stdin) fmt

let handle_node (promises : (int, _ promise) Hashtbl.t) push_event push_stream
    (node : Record.t) =
  match node with
  | Result ({ token; _ } as rnode) -> (
      match token with
      | None ->
          (* All the Mi commands in this wrapped uses a token. *)
          assert false
      | Some tk -> (
          match Hashtbl.find promises tk with
          | exception Not_found ->
              (* All the Mi command resolver must be registered before
                 scheduling the promise. *)
              assert false
          | _, resolver -> Lwt.wakeup_later resolver rnode))
  | Async ev -> push_event (Some ev)
  | Stream stream -> push_stream (Some stream)

let close t =
  let try_to_close = Lwt_unix.with_timeout 1. @@ fun () -> t.process#close in
  match%lwt try_to_close with
  | exception Lwt_unix.Timeout -> Lwt.return t.process#terminate
  | _ -> Lwt.return_unit

let send_command t command =
  if String.contains command '\n' then invalid_arg "Mi2_lwt.send_command";
  Log.debug (fun k -> k "Command sent: %S" command);
  let promise, resolver = Lwt.wait () in
  t.tok <- t.tok + 1;
  let tok = t.tok in
  Hashtbl.replace t.promises tok (promise, resolver);
  Lwt.on_termination promise (fun () -> Hashtbl.remove t.promises tok);
  match%lwt write_stdin t.process "%d%s" tok command with
  | exception exn ->
      Lwt.wakeup_later_exn resolver exn;
      promise
  | () -> promise

let make ?cwd ?env ~path ?args () =
  let env =
    match env with
    | None -> None
    | Some e ->
        List.to_seq e
        |> Seq.map (fun (k, v) -> Format.sprintf "%s=%s" k v)
        |> Array.of_seq |> Option.some
  in
  let argv = match args with None -> [ path ] | Some a -> path :: a in
  let argv = Array.of_list argv in
  let process = Lwt_process.open_process_full ?cwd ?env (path, argv) in
  let promises = Hashtbl.create 17 in
  let events, push_event = Lwt_stream.create () in
  let streams, push_stream = Lwt_stream.create () in
  let stderr = Lwt_stream.from @@ read_stderr process in
  (* TODO: move this code into run? *)
  Lwt.async (fun () ->
      let stdout = Lwt_stream.from @@ read_stdout process in
      let handle_node = handle_node promises push_event push_stream in
      Lwt_stream.iter handle_node stdout);
  { process; tok = 0; promises; events; streams; stderr }

let run ?cwd ?env ~path ?args () =
  let t = make ?cwd ?env ~path ?args () in
  (* NOTE: mi-async is GDB 7.8 (2014), was target-async before *)
  (* TODO: check the startup. *)
  let%lwt _ = send_command t "-gdb-set mi-async on" in
  (* assert (class_ = Done); *)
  Lwt.return t

let[@inline] events { events; _ } = events
let[@inline] streams { streams; _ } = streams
let[@inline] stderr { stderr; _ } = stderr
