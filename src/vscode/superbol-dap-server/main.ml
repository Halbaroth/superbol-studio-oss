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

module Dp = struct
  include Debug_protocol

  module U = struct
    include Yojson.Safe.Util

    let filter_object p json =
      let l = List.filter (fun (name, _value) -> p name) @@ to_assoc json in
      `Assoc l
  end

  module Launch_command_ex = struct
    include Launch_command

    module Arguments = struct
      type t = {
        no_debug : bool option;
        __restart : Any.t option;
        extra : Any.t;
      }

      let of_yojson json =
        let no_debug = U.member "noDebug" json |> U.to_bool_option in
        let __restart =
          match U.member "__restart" json with
          | exception U.Undefined _ -> None
          | v -> Some v
        in
        let extra =
          U.filter_object (fun s ->
            match s with
            | "noDebug" | "__restart" -> false
            | _ -> true) json
        in
        Ok { no_debug; __restart; extra }

      let to_yojson { no_debug; __restart; extra } =
        let b = Launch_command.Arguments.to_yojson { no_debug; __restart } in
        (* FIXME: [combine] doesn't handle collisions. *)
        U.combine b extra
    end
  end

  module Attach_command_ex = struct
    include Attach_command

    module Arguments = struct
      type t = {
        __restart : Any.t option;
        extra : Any.t;
      }

      let of_yojson json =
        let __restart =
          match U.member "__restart" json with
          | exception U.Undefined _ -> None
          | v -> Some v
        in
        let extra =
          U.filter_object (fun s ->
            match s with
            | "noDebug" | "__restart" -> false
            | _ -> true) json
        in
        Ok { __restart; extra }

      let to_yojson { __restart; extra } =
        let b = Attach_command.Arguments.to_yojson { __restart } in
        (* FIXME: [combine] doesn't handle collisions. *)
        U.combine b extra
    end
  end
end

let src = Logs.Src.create ~doc:"DAP server" "DAP"

module Log = (val Logs.src_log src : Logs.LOG)

type ('a, 'r) command =
  (module Dp.COMMAND with type Arguments.t = 'a and type Result.t = 'r)

type ('a, 'r) handler = 'a -> 'r Lwt.t

module Server : sig
  type t

  val create : in_:Lwt_io.input_channel -> out:Lwt_io.output_channel -> unit -> t
  val set_command_handler : t -> ('a, 'r) command -> ('a, 'r) handler -> unit
  val remove_command_handler : t -> ('a, 'r) command -> unit
  val start : t -> unit Lwt.t
end = struct
  type t = {
    in_ : Lwt_io.input_channel;
    out : Lwt_io.output_channel;
    mutable next_seq : int;
    handlers : (string, any_handler) Hashtbl.t;
    cancel_resolvers : (int, unit Lwt.u) Hashtbl.t;
  }

  and any_handler = t -> Dp.Request.t -> raw:string -> unit Lwt.t

  let parse_message : string Angstrom.t =
    let open Angstrom in
    let eol = string "\r\n" in
    let colon = string ": " in
    let is_colon = function ':' -> true | _ -> false in
    let is_eol = function '\r' -> true | _ -> false in
    let header_field =
      lift2
        (fun key value -> key, value)
        (take_till is_colon <* colon)
        (take_till is_eol <* eol)
    in
    (many1 header_field <* eol) >>= fun headers ->
      (* FIXME: handle wrong length here. *)
      let content_length =
        int_of_string (List.assoc "Content-Length" headers)
      in
      take content_length

  let create ~in_ ~out () =
    let srv = {
      in_;
      out;
      next_seq = 0;
      handlers = Hashtbl.create 17;
      cancel_resolvers = Hashtbl.create 17;
    }
    in
    srv

  let next_seq srv =
    let seq = srv.next_seq in
    srv.next_seq <- srv.next_seq + 1;
    seq

  let dispatch_request (srv : t) req raw =
    match Hashtbl.find srv.handlers req.Dp.Request.command with
    | exception Not_found ->
        Log.warn (fun k -> k "Cannot find handler");
        Lwt.return_unit
    | handler ->
        let (promise, resolver) = Lwt.wait () in
        Hashtbl.replace srv.cancel_resolvers req.Dp.Request.seq resolver;
        let finalizer () =
          Hashtbl.remove srv.cancel_resolvers req.Dp.Request.seq;
          Lwt.return_unit
        in
        Lwt.finalize
          (fun () ->
            Lwt.pick [
              handler srv req ~raw;
              promise
            ])
          finalizer

  let remove_command_handler (type a r) srv
    (module Command : Dp.COMMAND with type Arguments.t = a and type Result.t = r) =
    (* FIXME: Is it correct the remove a command without cancelling all the
       action associated with? *)
    Hashtbl.remove srv.handlers Command.type_

  let send_message srv json =
    let open Lwt.Syntax in
    let raw = Yojson.Safe.to_string json in
    let* () = Lwt_io.write srv.out "Content-Length: " in
    let* () = Lwt_io.write srv.out @@ string_of_int @@ String.length raw in
    let* () = Lwt_io.write srv.out "\r\n\r\n" in
    let* () = Lwt_io.write srv.out raw in
    let* () = Lwt_io.flush srv.out in
    Log.debug (fun k -> k "Message sent -- %a" Yojson.Safe.pp json);
    Lwt.return_unit

  let inject_extra_arguments (type a r)
    (module Command : Dp.COMMAND with type Arguments.t = a and type Result.t = r)
    (args : a) =
    match args with
    | _ -> assert false

  let set_command_handler (type a r) srv
    (module Command : Dp.COMMAND with type Arguments.t = a and type Result.t = r)
    (h : (a, r) handler) =
    let open Lwt.Syntax in
    let handler srv (req : Dp.Request.t) ~raw:_ =
      match Command.Arguments.of_yojson req.arguments with
      | Error s ->
          Log.debug (fun k -> k "cannot parse arguments: %s" s);
          Lwt.return_unit
      | Ok args ->
          let* r = h args in
          let body = Command.Result.to_yojson r in
          let res =
            Dp.Response.make
              ~seq:(next_seq srv)
              ~type_:Dp.Response.Type.Response
              ~request_seq:req.seq
              ~success:true
              ~command:Command.type_
              ~body
              ()
          in
          send_message srv @@ Dp.Response.to_yojson res
    in
    Hashtbl.replace srv.handlers Command.type_ handler

  let dispatch srv raw =
    match Yojson.Safe.from_string raw with
    | exception Yojson.Json_error e -> (
        Log.err (fun k -> k "Received ill-formatted request: %s" e);
        Log.debug (fun k -> k "Raw message: %s" raw);
        Lwt.return_unit)
    | json -> (
        Log.debug (fun k -> k "Received message: %a" Yojson.Safe.pp json);
        match Dp.Protocol_message.of_yojson json with
        | Error s -> (
            Log.err (fun k -> k "Invalid message: %s" s);
            Lwt.return_unit)
        | Ok { type_ = Dp.Protocol_message.Type.Event; _ }
        | Ok { type_ = Dp.Protocol_message.Type.Response; _ } ->
            failwith "not implemented"
        | Ok { type_ = Dp.Protocol_message.Type.Request; _ } ->
            dispatch_request srv (Dp.Request.of_yojson json |> Result.get_ok) raw
        | _ ->
            failwith "unsupported message type")

  let start srv =
    let open Lwt.Syntax in
    try
      let* (_ : _ * _ result) =
        Angstrom_lwt_unix.parse_many parse_message (dispatch srv) srv.in_
      in
      Lwt.return_unit
    with Lwt_io.Channel_closed _ ->
      Lwt.return_unit
end

module Compat = struct
  module Filename : sig
    val temp_dir : ?temp_dir:string -> ?perms:int -> string -> string -> string

    include module type of Filename
  end = struct
    open Filename

    let prng_key =
      (* Domain.DLS.new_key *)
      Random.State.make_self_init ()

    external open_desc: string -> open_flag list -> int -> int = "caml_sys_open"
    external close_desc: int -> unit = "caml_sys_close"

    let temp_file_name temp_dir prefix suffix =
      let random_state = (* Domain.DLS.get *) prng_key in
      let rnd = (Random.State.bits random_state) land 0xFFFFFF in
      concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

    let current_temp_dir_name = get_temp_dir_name ()

    let temp_dir ?(temp_dir = (* Domain.DLS.get *) current_temp_dir_name)
    ?(perms = 0o700) prefix suffix =
      let rec try_name counter =
        let name = temp_file_name temp_dir prefix suffix in
        try
          Sys.mkdir name perms;
        name
        with Sys_error _ as e ->
          if counter >= 20 then raise e else try_name (counter + 1)
        in try_name 0

    include Filename
  end
end

module Cmd : sig
  type t = private {
    socket_path : string option;
  }

  val parse : unit -> t Cmdliner.Cmd.eval_exit
end = struct
  module C = Cmdliner

  type t = {
    socket_path : string option;
  }

  let connection_section = "CONNECTION"

  let socket_path =
    let doc = "Path to the UNIX local socket" in
    C.Arg.(
      value
      & opt (some filepath) None
      & info [ "s"; "socket" ] ~docs:connection_section ~doc)

  let t =
    let open C.Term.Syntax in
    let doc = "Superbol DAP server" in
    C.Cmd.make (C.Cmd.info "dap" ~doc)
    @@
    let+ socket_path in
    { socket_path }

  let parse () = Cmdliner.Cmd.eval_value' t
end

module Reporter : sig
  val lwt_reporter : unit -> Logs.reporter
end = struct
  let app_style = `Cyan
  let err_style = `Red
  let warn_style = `Yellow
  let info_style = `Blue
  let debug_style = `Green

  let pp_brackets pp_v ppf v = Fmt.char ppf '['; pp_v ppf v; Fmt.char ppf ']'

  let pp_h ppf style h =
    Fmt.char ppf '[';
    Fmt.(styled style string) ppf h;
    Fmt.char ppf ']';
    Fmt.char ppf ' '

  let pp_header ppf (l, h) =
    match l with
    | Logs.App ->
        begin match h with
        | None -> ()
        | Some h ->
            pp_brackets Fmt.(styled app_style string) ppf h; Fmt.char ppf ' '
        end
    | Logs.Error ->
        pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
    | Logs.Warning ->
        pp_h ppf warn_style (match h with None -> "WARNING" | Some h -> h)
    | Logs.Info ->
        pp_h ppf info_style (match h with None -> "INFO" | Some h -> h)
    | Logs.Debug ->
        pp_h ppf debug_style (match h with None -> "DEBUG" | Some h -> h)

  let add_colors fd oc =
    if Unix.isatty fd then
      Fmt_tty.setup ~style_renderer:`Ansi_tty oc
    else
      Format.formatter_of_out_channel oc

  let lwt_reporter () =
    let stdout = add_colors Unix.stdout stdout in
    let stderr = add_colors Unix.stderr stderr in
    let buf_fmt ~like =
      let b = Buffer.create 512 in
      let ppf = Fmt.with_buffer ~like b in
      ( ppf,
        fun () -> let m = Buffer.contents b in Buffer.reset b; m )
    in
    let app, app_flush = buf_fmt ~like:stdout in
    let dst, dst_flush = buf_fmt ~like:stderr in
    let reporter = Logs_fmt.reporter ~pp_header ~app ~dst () in
    let report src level ~over k msgf =
      let k () =
        let write () =
          match level with
          | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
          | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
        in
        let unblock () = over (); Lwt.return_unit in
        Lwt.finalize write unblock |> Lwt.ignore_result;
        k ()
      in
      reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
    in
    { Logs.report = report }
end

module Capability_set : sig
  type unseal_t
  type t

  val empty : unseal_t
  val add : ('a, 'r) command -> ('a, 'r) handler -> unseal_t -> unseal_t
  val seal : unseal_t -> t
  val set_commands : Server.t -> t -> unit
end = struct
  type any_cap = Cap : ('a, 'r) command * ('a, 'r) handler -> any_cap

  module M = Map.Make (String)

  type unseal_t = any_cap M.t
  type t = unseal_t

  let empty = M.empty

  let add (type a r)
    ((module Command : Dp.COMMAND
      with type Arguments.t = a and type Result.t = r) as c) (h : (a, r) handler) s =
    M.add Command.type_ (Cap (c, h)) s

  let seal s =
    let no_capabilities = Dp.Capabilities.make () in
    let capabilities =
    M.fold (fun type_ _ (acc : Dp.Capabilities.t) ->
      match type_ with
      | "initialize" -> failwith "unexpected initialize capability in the set"
      | "cancel" ->
          { acc with supports_cancel_request = Some true }
      | "readMemory" ->
          { acc with supports_read_memory_request = Some true }
      | "configurationDone" ->
          { acc with supports_configuration_done_request = Some true }
      | "setVariable" ->
          { acc with supports_set_variable = Some true }
      | "restart" ->
          { acc with supports_restart_request = Some true }
      | _ -> acc
    ) s no_capabilities
    in
    let handle_initialize_request ?extra:_ (_arg : Dp.Initialize_command.Arguments.t) =
      Lwt.return capabilities
    in
    add (module Dp.Initialize_command) handle_initialize_request s

  let set_commands server s =
    M.iter (fun _ (Cap (m, h)) ->
      Server.set_command_handler server m h) s
end

module Gdb = struct
  module LaunchArguments = struct
  end
end

module Session = struct
end

module Handlers = struct
  (* Mandatory *)

  let handle_launch_ex (arg : Dp.Launch_command_ex.Arguments.t) =
    Log.debug (fun k -> k "Extra arguments: %a" Yojson.Safe.pp arg.extra);
    Lwt.return ()

  let handle_attach_ex (arg : Dp.Attach_command_ex.Arguments.t) =
    Log.debug (fun k -> k "Extra arguments: %a" Yojson.Safe.pp arg.extra);
    Lwt.return ()

  let handle_disconnect (_arg : Dp.Disconnect_command.Arguments.t) =
    Lwt.return ()

  let handle_set_breakpoints (_arg : Dp.Set_breakpoints_command.Arguments.t) =
    let dummy = Dp.Set_breakpoints_command.Result.make ~breakpoints:[] () in
    Lwt.return dummy

  let handle_threads (_arg : Dp.Threads_command.Arguments.t) =
    let dummy = Dp.Threads_command.Result.make ~threads:[] () in
    Lwt.return dummy

  let handle_stack_trace (_arg : Dp.Stack_trace_command.Arguments.t) =
    let dummy = Dp.Stack_trace_command.Result.make ~stack_frames:[] () in
    Lwt.return dummy

  let handle_scopes (_arg : Dp.Scopes_command.Arguments.t) =
    let dummy = Dp.Scopes_command.Result.make ~scopes:[] () in
    Lwt.return dummy

  let handle_variables (_arg : Dp.Variables_command.Arguments.t) =
    let dummy = Dp.Variables_command.Result.make ~variables:[] () in
    Lwt.return dummy

  let handle_continue (_arg : Dp.Continue_command.Arguments.t) =
    let dummy = Dp.Continue_command.Result.make () in
    Lwt.return dummy

  let handle_next (_arg : Dp.Next_command.Arguments.t) =
    Lwt.return ()

  let handle_step_in (_arg : Dp.Step_in_command.Arguments.t) =
    Lwt.return ()

  let handle_step_out (_arg : Dp.Step_out_command.Arguments.t) =
    Lwt.return ()

  (* Optionals *)

  let handle_configuration_done (_arg : Dp.Configuration_done_command.Arguments.t) =
    Lwt.return ()

  let handle_restart (_arg : Dp.Restart_command.Arguments.t) =
    Lwt.return ()

  let handle_set_variable (_arg : Dp.Set_variable_command.Arguments.t) =
    let dummy =
      Dp.Set_variable_command.Result.make ~value:"dummy" ()
    in
    Lwt.return dummy

  let all = Capability_set.(
    empty
    |> add (module Dp.Launch_command_ex) handle_launch_ex
    |> add (module Dp.Attach_command_ex) handle_attach_ex
    |> add (module Dp.Disconnect_command) handle_disconnect
    |> add (module Dp.Set_breakpoints_command) handle_set_breakpoints
    |> add (module Dp.Threads_command) handle_threads
    |> add (module Dp.Stack_trace_command) handle_stack_trace
    |> add (module Dp.Scopes_command) handle_scopes
    |> add (module Dp.Variables_command) handle_variables
    |> add (module Dp.Continue_command) handle_continue
    |> add (module Dp.Next_command) handle_next
    |> add (module Dp.Step_in_command) handle_step_in
    |> add (module Dp.Step_out_command) handle_step_out
    |> add (module Dp.Configuration_done_command) handle_configuration_done
    |> add (module Dp.Restart_command) handle_restart
    |> add (module Dp.Set_variable_command) handle_set_variable
    |> seal)
end

let pp_sockaddr ppf sockaddr =
  match sockaddr with
  | Unix.ADDR_UNIX s -> Fmt.string ppf s
  | ADDR_INET (s, p) -> Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr s) p

let (//) = Filename.concat

let create_socket_path () =
  let temp_dir = Compat.Filename.temp_dir "superbol-dap-server-" "" in
  temp_dir // "server.sock"

let with_open_socket k =
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let finally () = try Unix.close fd with _ -> () in
  Fun.protect ~finally @@ fun () -> k @@ Lwt_unix.of_unix_file_descr fd

let run_server fd sockaddr =
  let open Lwt.Syntax in
  Log.info (fun k -> k "listening on: %a" pp_sockaddr sockaddr);
  let* _server =
    Lwt_io.establish_server_with_client_address ~fd ~no_close:true sockaddr
    @@ fun _sockaddr (in_, out) ->
      let server = Server.create ~in_ ~out () in
      Capability_set.set_commands server Handlers.all;
      Server.start server
  in
  Lwt.return_unit

let () =
  let open Lwt.Syntax in
  Logs.set_reporter @@ Reporter.lwt_reporter ();
  Logs.set_level ~all:true (Some Logs.Debug);
  match Cmd.parse () with
  | `Exit code -> exit code
  | `Ok opts ->
    let socket_path =
      match opts.socket_path with
      | Some s -> s
      | None -> create_socket_path ()
    in
    (* TODO: with_open_socket is useless? *)
    with_open_socket @@ fun fd ->
    let sockaddr = Unix.ADDR_UNIX socket_path in
    let forever, _ = Lwt.wait () in
    Lwt_main.run @@
      let* _ = run_server fd sockaddr in
      forever
