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

module Dp = Debug_protocol

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
    let+ socket_path
    in
    { socket_path }

  let parse () = Cmdliner.Cmd.eval_value' t
end

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
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

module Capability_set : sig
  type ('a, 'r) handler = 'a -> 'r Lwt.t

  type ('a, 'r) command =
    (module Dp.COMMAND with type Arguments.t = 'a and type Result.t = 'r)

  type unseal_t
  type t

  val empty : unseal_t
  val add : ('a, 'r) command -> ('a, 'r) handler -> unseal_t -> unseal_t
  val seal : unseal_t -> t
  val set_commands : Debug_rpc.t -> t -> unit
end = struct
  type ('a, 'r) handler = 'a -> 'r Lwt.t

  type ('a, 'r) command =
    (module Dp.COMMAND with type Arguments.t = 'a and type Result.t = 'r)

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
      | "configurationDone" ->
          { acc with supports_configuration_done_request = Some true }
      | "setVariable" ->
          { acc with supports_set_variable = Some true }
      | _ -> acc
    ) s no_capabilities
    in
    let handle_initialize_request (arg : Dp.Initialize_command.Arguments.t) =
      Lwt.return capabilities
    in
    add (module Dp.Initialize_command) handle_initialize_request s

  let set_commands server s =
    M.iter (fun _ (Cap (m, h)) ->
      Debug_rpc.set_command_handler server m h) s
end

module Handlers = struct
  module Session = struct
    module Mi2 = Superbol_debugger.Mi2
    module Handles = Superbol_debugger.Types.Handles
    module IntMap = Superbol_debugger.Types.IntMap
    module DebuggerVariable = Superbol_debugger.Types.DebuggerVariable

    type int_or_string =
      | Int of int
      | String of string

    type var_cat =
      | Local
      | Global

    type t = {
      server : Debug_rpc.t;
      miDebugger : Mi2.t;
      showDetails : bool;
      mutable needContinue : bool;
      mutable started : bool;
      mutable attached : bool;
      mutable crashed : bool;
      mutable quit : bool;
      mutable variableHandles : (int_or_string * var_cat) Handles.t;
      mutable globalVariables : DebuggerVariable.t list Promise.t IntMap.t;
    }
  end

  let handle_configuration_done_request (arg : Dp.Configuration_done_command.Arguments.t) =
    Lwt.return ()

  let handle_launch_request (arg : Dp.Launch_command.Arguments.t) =
    Lwt.return ()

  let handle_attach_request (arg : Dp.Attach_command.Arguments.t) =
    Lwt.return ()

  let handle_restart_request (arg : Dp.Restart_command.Arguments.t) =
    Lwt.return ()

  let all  = Capability_set.(
    empty
    |> add (module Dp.Configuration_done_command) handle_configuration_done_request
    |> add (module Dp.Launch_command) handle_launch_request
    |> add (module Dp.Attach_command) handle_attach_request
    |> add (module Dp.Restart_command) handle_restart_request
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

let with_open_socket path k =
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let finally () = try Unix.close fd with _ -> () in
  Fun.protect ~finally @@ fun () -> k @@ Lwt_unix.of_unix_file_descr fd

let run_server fd sockaddr =
  let open Lwt.Syntax in
  Logs.info (fun k -> k "listening on: %a" pp_sockaddr sockaddr);
  let* _server =
    Lwt_io.establish_server_with_client_address ~fd ~no_close:true sockaddr
    @@ fun _sockaddr (in_, out) ->
      Logs.debug (fun k -> k "got a new connection");
      let server = Debug_rpc.create ~in_ ~out () in
      Capability_set.set_commands server Handlers.all;
      Debug_rpc.start server
  in
  Lwt.return_unit

let () =
  let open Lwt.Syntax in
  Logs.set_reporter @@ lwt_reporter ();
  Logs.set_level ~all:true (Some Logs.Debug);
  match Cmd.parse () with
  | `Exit code -> exit code
  | `Ok opts ->
    let socket_path =
      match opts.socket_path with
      | Some s -> s
      | None -> create_socket_path ()
    in
    with_open_socket socket_path @@ fun fd ->
    let sockaddr = Unix.ADDR_UNIX socket_path in
    let forever, _ = Lwt.wait () in
    Lwt_main.run @@
      let* _ = run_server fd sockaddr in
      forever
