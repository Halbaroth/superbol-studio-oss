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

module Gdb = struct
  module LaunchArguments = struct
    type t = {
      target : string;
      arguments : string;
      cwd : string option;
      (* group : string list; *)
      (* env : (string * string) list; *)
      verbose : bool;
      tty : bool;
      useCobcrun : bool;
      cobcrunPath : string;
      targetWrapperPath : string;
      (* sourceDirs : string list; *)
    }
    [@@deriving yojson, show, fields]

    let is_field s = List.mem s Fields.names
  end

  module AttachArguments = struct
    type t = {
      pid : string option;
      remoteDebugger : string option;
      target : string;
      arguments : string;
      cwd : string option;
      group : string list;
      env : (string * string) list;
      verbose : bool;
      useCobcrun : bool;
      cobcrunPath : string;
      targetWrapperPath : string;
      sourceDirs : string list;
    }
    [@@deriving yojson, show, fields]

    let is_field s = List.mem s Fields.names
  end
end

module Dp = struct
  include Debug_protocol

  let filter_object p json =
    let open Yojson.Safe.Util in
    let l = List.filter (fun (name, _value) -> p name) @@ to_assoc json in
    `Assoc l

  module Launch_command_ex = struct
    include Launch_command

    module Arguments = struct
      type t = {
        no_debug : bool option;
        __restart : Any.t option;
        extra : Any.t;
      }

      let of_yojson json =
        let open Yojson.Safe.Util in
        let no_debug = member "noDebug" json |> to_bool_option in
        let __restart =
          match member "__restart" json with
          | exception Undefined _ -> None
          | v -> Some v
        in
        let extra = filter_object Gdb.LaunchArguments.is_field json in
        Ok { no_debug; __restart; extra }

      let to_yojson { no_debug; __restart; extra } =
        let open Yojson.Safe.Util in
        let b = Launch_command.Arguments.to_yojson { no_debug; __restart } in
        (* FIXME: [combine] doesn't handle collisions. *)
        combine b extra
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
        let open Yojson.Safe.Util in
        let __restart =
          match member "__restart" json with
          | exception Undefined _ -> None
          | v -> Some v
        in
        let extra = filter_object Gdb.AttachArguments.is_field json in
        Ok { __restart; extra }

      let to_yojson { __restart; extra } =
        let open Yojson.Safe.Util in
        let b = Attach_command.Arguments.to_yojson { __restart } in
        (* FIXME: [combine] doesn't handle collisions. *)
        combine b extra
    end
  end
end

let src = Logs.Src.create ~doc:"DAP server" "DAP"

module Log = (val Logs.src_log src : Logs.LOG)

type ('a, 'r) command =
  (module Dp.COMMAND with type Arguments.t = 'a and type Result.t = 'r)

type ('a, 'r) handler = 'a -> 'r Lwt.t

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
  val set_commands : Debug_rpc.t -> t -> unit
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
      Debug_rpc.set_command_handler server m h) s
end

module Session = struct
end

module Handlers = struct
  (* Mandatory *)

  let handle_launch_ex (arg : Dp.Launch_command_ex.Arguments.t) =
    Log.debug (fun k -> k "Extra arguments: %a" Yojson.Safe.pp arg.extra);
    match Gdb.LaunchArguments.of_yojson arg.extra with
    | Ok gdb_arguments ->
      Log.debug (fun k -> k "Gdb arguments: %a" Gdb.LaunchArguments.pp gdb_arguments);
      Lwt.return ()
    | Error s ->
      Log.err (fun k -> k "Cannot read gdb arguments: %s" s);
      Lwt.return ()

  let handle_attach_ex (arg : Dp.Attach_command_ex.Arguments.t) =
    Log.debug (fun k -> k "Extra arguments: %a" Yojson.Safe.pp arg.extra);
    match Gdb.AttachArguments.of_yojson arg.extra with
    | Ok gdb_arguments ->
      Log.debug (fun k -> k "Gdb arguments: %a" Gdb.AttachArguments.pp gdb_arguments);
      Lwt.return ()
    | Error s ->
      Log.err (fun k -> k "Cannot read gdb arguments: %s" s);
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
      let server = Debug_rpc.create ~in_ ~out () in
      Capability_set.set_commands server Handlers.all;
      Debug_rpc.start server
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
