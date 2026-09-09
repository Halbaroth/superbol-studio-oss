type t = string

module GdbLaunchArguments = struct
  type t = {
    target : string;
    arguments : string;
    cwd : string;
    useCobcrun : bool;
    gdbTargetWrapperPath : string;
  }
end

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

let (//) = Filename.concat

let capabilities = Dp.Capabilities.{
  supports_configuration_done_request = None;
  supports_function_breakpoints = None;
  supports_conditional_breakpoints = None;
  supports_hit_conditional_breakpoints = None;
  supports_evaluate_for_hovers = None;
  exception_breakpoint_filters = None;
  supports_step_back = None;
  supports_set_variable = None;
  supports_restart_frame = None;
  supports_goto_targets_request = None;
  supports_step_in_targets_request = None;
  supports_completions_request = None;
  completion_trigger_characters = None;
  supports_modules_request = None;
  additional_module_columns = None;
  supported_checksum_algorithms = None;
  supports_restart_request = None;
  supports_exception_options = None;
  supports_value_formatting_options = None;
  supports_exception_info_request = None;
  support_terminate_debuggee = None;
  support_suspend_debuggee = None;
  supports_delayed_stack_trace_loading = None;
  supports_loaded_sources_request = None;
  supports_log_points = None;
  supports_terminate_threads_request = None;
  supports_set_expression = None;
  supports_terminate_request = None;
  supports_data_breakpoints = None;
  supports_read_memory_request = None;
  supports_write_memory_request = None;
  supports_disassemble_request = None;
  supports_cancel_request = None;
  supports_breakpoint_locations_request = None;
  supports_clipboard_context = None;
  supports_stepping_granularity = None;
  supports_instruction_breakpoints = None;
  supports_exception_filter_options = None;
  supports_single_thread_execution_requests = None;
  supports_data_breakpoint_bytes = None;
  breakpoint_modes = None;
  supports_ansistyling = None;
}

let handle_initialize (arg : Dp.Initialize_command.Arguments.t) =
  Logs.debug (fun k -> k "capabilities sending...");
  Lwt.return capabilities

let pp_sockaddr ppf sockaddr =
  match sockaddr with
  | Unix.ADDR_UNIX s -> Fmt.string ppf s
  | ADDR_INET (s, p) -> Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr s) p

let create_fresh_socket () =
  let temp_dir = Compat.Filename.temp_dir "superbol-dap-server-" "" in
  let path = temp_dir // "server.sock" in
  Unix.close @@ Unix.openfile path [ O_RDWR; O_CREAT; O_EXCL ] 0o600;
  path

let with_open_socket path k =
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let finally () = try Unix.close fd with _ -> () in
  Fun.protect ~finally @@ fun () -> k @@ Lwt_unix.of_unix_file_descr fd

let run_server fd sockaddr =
  let open Lwt.Syntax in
  Logs.info (fun k -> k "listening on %a" pp_sockaddr sockaddr);
  let* _server =
    Lwt_io.establish_server_with_client_address ~fd sockaddr
    @@ fun _sockaddr (in_, out) ->
      Logs.debug (fun k -> k "got a new connection");
      let server = Debug_rpc.create ~in_ ~out () in
      Debug_rpc.set_command_handler server
        (module Dp.Initialize_command) handle_initialize;
      Debug_rpc.start server
  in
  Lwt.return_unit

let () =
  Logs.set_reporter @@ lwt_reporter ();
  Logs.set_level ~all:true (Some Logs.Debug);
  match Cmd.parse () with
  | `Exit code -> exit code
  | `Ok opts ->
    let socket_path =
      match opts.socket_path with
      | Some s -> s
      | None -> create_fresh_socket ()
    in
    with_open_socket socket_path @@ fun fd ->
    let sockaddr = Unix.ADDR_UNIX socket_path in
    (* let forever, _ = Lwt.wait () in *)
    Lwt_main.run (run_server fd sockaddr)
