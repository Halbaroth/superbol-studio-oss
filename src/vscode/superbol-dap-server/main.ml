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
      group : string list [@default []];
      env : (string * string) list [@default []];
      verbose : bool; (* TODO: useless for a remote server? *)
      tty : bool;
      useCobcrun : bool;
      cobcrunPath : string;
      targetWrapperPath : string;
      sourceDirs : string list [@default []];
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
      sourceDirs : string list [@default []];
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

module Mi2 = Superbol_gdb_adapter.Mi2
module Types = Superbol_gdb_adapter.Types
module VariableDetail = Types.VariableDetail
module Util = Superbol_gdb_adapter.Util
module DebuggerVariable = Types.DebuggerVariable
module StrMap = Types.StrMap

module Handles = struct
  module IntMap = Types.IntMap

  type 'a t = {
    mutable map : 'a IntMap.t;
    mutable next : int;
    start : int;
  }

  let create start =
    { map = IntMap.empty; next = start; start }

  let reset handles =
    handles.map <- IntMap.empty;
    handles.next <- handles.start

  let set handles value =
    let handle = handles.next in
    handles.next <- handles.next + 1;
    handles.map <- IntMap.add handle value handles.map;
    handle

  let get handle handles =
    IntMap.find handle handles.map

  let get_opt handle handles =
    IntMap.find_opt handle handles.map
end

let stackHandlesStart = 1000
let varHandlesStart = 512 * 256 + stackHandlesStart

module Session = struct
  type int_or_string =
    | Int of int
    | String of string

  type var_cat =
    | Local
    | Global

  type t = {
    debugger : Mi2.t;
    showDetails : bool;
    mutable started : bool;
    mutable needContinue : bool;
    mutable attached : bool;
    mutable variableHandles : (int_or_string * var_cat) Handles.t;
    mutable globalVariables : Types.DebuggerVariable.t list Lwt.t Types.IntMap.t;
  }

  let create () =
    (* let showDetails = Settings.displayVariableAttributes () in *)
    let debugger = Mi2.create () in
    { debugger; started = false; needContinue = false; attached = false;
    variableHandles = Handles.create varHandlesStart;
  globalVariables = Types.IntMap.empty;
  showDetails = true;
  }
end

module Handlers = struct
  (* Mandatory *)

  let gdb_path = "/nix/store/wxsng4diyqp5ndxavm7rd47q3m20irvm-user-environment/bin/gdb"

  let error ~id fmt =
    let open Dp in
    Format.kasprintf (fun s ->
      let msg = Message.make ~id ~format:s ~show_user:(Some true) () in
      raise (Debug_rpc.Error_with_message msg)) fmt

  let handle_launch_ex server session (args : Dp.Launch_command_ex.Arguments.t) =
    Log.debug (fun k -> k "Extra arguments: %a" Yojson.Safe.pp args.extra);
    match Gdb.LaunchArguments.of_yojson args.extra with
    | Error s ->
      Log.err (fun k -> k "Cannot read gdb arguments: %s" s);
      Lwt.return ()
    | Ok gdb_args ->
      Log.debug (fun k -> k "Gdb arguments: %a" Gdb.LaunchArguments.pp gdb_args);
      (* Normalize GDB arguments *)
      let no_debug = Option.value ~default:false args.no_debug in
      let cwd =
        Option.value ~default:(Filename.dirname gdb_args.target) gdb_args.cwd
      in
      let env = Mi2.Dict.of_seq @@ List.to_seq gdb_args.env in
      (* FIXME: support other values. *)
      let tty = Mi2.GdbTty.Bool gdb_args.tty in
      (* Initialize Mi2 module *)
      Mi2.init
        session.Session.debugger
        gdb_path
        env
        no_debug
        tty
        gdb_args.cobcrunPath
        gdb_args.targetWrapperPath
        gdb_args.useCobcrun
        gdb_args.sourceDirs;
      let%lwt () =
        Mi2.load session.debugger
          cwd gdb_args.target gdb_args.arguments gdb_args.group
      in
      let%lwt () =
        Debug_rpc.send_event server (module Dp.Initialized_event) ()
      in
      let%lwt _started = Mi2.start session.debugger () in
      session.started <- true; (* TODO: review this *)
      (* TODO: handle GDB crash *)
      Lwt.return ()

  let handle_attach_ex server session (arg : Dp.Attach_command_ex.Arguments.t) =
    Log.debug (fun k -> k "Extra arguments: %a" Yojson.Safe.pp arg.extra);
    match Gdb.AttachArguments.of_yojson arg.extra with
    | Error s ->
      Log.err (fun k -> k "Cannot read gdb arguments: %s" s);
      Lwt.return ()
    | Ok gdb_args ->
      Log.debug (fun k -> k "Gdb arguments: %a" Gdb.AttachArguments.pp gdb_args);
      let attachTarget =
        match gdb_args.pid with
        | None -> gdb_args.pid
        | a -> a
      in
      if Option.is_none attachTarget then
        error ~id:100
        "Failed to start MI Debugger: PID or remote-debugger argument required"
      else (
        (* Normalize GDB arguments *)
        let cwd =
          Option.value ~default:(Filename.dirname gdb_args.target) gdb_args.cwd
        in
        let env = Mi2.Dict.of_seq @@ List.to_seq gdb_args.env in
        (* FIXME: support other values. *)
        let tty = Mi2.GdbTty.Bool false in
        let no_debug = false in
        (* Initialize Mi2 module *)
        (* TODO: factorize this code with the code in handle_launch_ex *)
        Mi2.init
          session.Session.debugger
          gdb_path
          env
          no_debug
          tty
          gdb_args.cobcrunPath
          gdb_args.targetWrapperPath
          gdb_args.useCobcrun
          gdb_args.sourceDirs;
        let%lwt () =
          Mi2.attach
            session.debugger
            cwd
            gdb_args.target
            gdb_args.group
        in
        let%lwt () = Debug_rpc.send_event server (module Dp.Initialized_event) () in
        let%lwt _started = Mi2.start session.debugger () in
        session.needContinue <- true; (* TODO: not sure should be here *)
        session.attached <- true; (* TODO: review this *)
        (* TODO: handle crash *)
        Lwt.return ())

  let handle_disconnect _server (session : Session.t) (args : Dp.Disconnect_command.Arguments.t) =
    if session.attached then
      let%lwt _done = Mi2.detach session.debugger in
      Lwt.return_unit
    else
      let%lwt _done = Mi2.stop session.debugger in
      Lwt.return_unit

  let handle_set_breakpoints _server _session (_args : Dp.Set_breakpoints_command.Arguments.t) =
    let dummy = Dp.Set_breakpoints_command.Result.make ~breakpoints:[] () in
    Lwt.return dummy

  let handle_threads _server session (_args : Dp.Threads_command.Arguments.t) =
    let%lwt threads = Mi2.getThreads session.Session.debugger in
    let threads =
      List.map (fun (t : Types.Thread.t) ->
        let name =
          string_of_int t.id ^ ":" ^ Option.value ~default:"" t.name
        in
        Dp.Thread.make ~id:t.id ~name)
        threads
    in
    Lwt.return @@ Dp.Threads_command.Result.make ~threads ()

  let encode_frame_id thread_id frame_level =
    frame_level lsl 8 lor thread_id

  let decode_frame_id frame_id =
    frame_id land 0xff, frame_id lsr 8

  let handle_stack_trace _server session (args : Dp.Stack_trace_command.Arguments.t) =
    let levels = Option.value ~default:0 args.levels in
    let%lwt stack = Mi2.getStack session.Session.debugger args.thread_id levels in
    let stack_frames =
      List.map (fun (elem : Types.Stack.t) ->
        let name =
          String.trim (elem.function_ ^ " " ^ elem.lineContents) in
        let source =
          Dp.Source.make
            ~name:(Some (Filename.basename elem.file))
            ~path:(Some elem.file)
            ~source_reference:(Some 0) ()
        in
        Dp.Stack_frame.make
          ~id:(encode_frame_id args.thread_id elem.level)
          ~name
          ~source:(Some source)
          ~line:elem.line
          ~column:0 ()
      ) stack
    in
    Lwt.return @@ Dp.Stack_trace_command.Result.make ~stack_frames ()

  let handle_scopes _server session (args : Dp.Scopes_command.Arguments.t) =
    let scopes = [
      Dp.Scope.make ~name:"Local"
        ~variables_reference:(stackHandlesStart + args.frame_id)
        ~expensive:false ()
    ] in
    let%lwt fsl = Mi2.getGlobalStorageSymbols session.Session.debugger in
    let (scopes, gv) =
      List.fold_left (fun (scopes, gv) (fs : Types.FileSymbols.t) ->
        if fs.symbols <> [] then
          let base = Util.stripPathExtension fs.filename in
          let scope_id = List.length scopes in
          let scopes_variables =
            (* NOTE: evalGlobalSymbol might return rejected promises *)
            Types.LocalizedSymbol.ofFileSymbols fs |>
            List.map (fun s -> Mi2.evalGlobalSymbol session.debugger s)
          in
          let gv =
            Types.IntMap.add scope_id (Lwt.all scopes_variables) gv in
          let scope =
            Dp.Scope.make ~name:("Globals " ^ base)
              ~variables_reference:scope_id ~expensive:false () in
          (scope :: scopes, gv)
        else
          (scopes, gv)
        ) (scopes, Types.IntMap.empty) fsl
    in
    session.globalVariables <- gv;
    let scopes = List.rev scopes in
    Lwt.return @@ Dp.Scopes_command.Result.make ~scopes ()

  (* Retrieve a scope/frame id or variable by its VSCode reference.
     This can retrive either:
     - a scope id (global variables)
     - a frame id (local variables)
     - a variable name *)
  let lookup_variable session ref_ =
    if ref_ < varHandlesStart then (* < 132072 *)
      if ref_ < stackHandlesStart then (* < 1000 *)
        Session.Int (ref_), Session.Global (* Id: 1 - 999 *)
      else
        Int (ref_ - stackHandlesStart), Local (* Id: 1000 - 132071 *)
    else
      Handles.get ref_ session.Session.variableHandles (* Id: 132072 - * *)

  let make_var_ref session (dv : DebuggerVariable.t) cat =
    (* NOTE: for locals, dv.cName is b_n,
       for globals it is '/path/to/prog.c.h'::b_n *)
    if session.Session.showDetails || not (StrMap.is_empty dv.children) then
      Handles.set session.variableHandles (String dv.cName, cat)
    else
      0

  let make_var (dv : DebuggerVariable.t) value var_ref =
    Dp.Variable.make ~name:dv.cobName
      ~evaluate_name:(Some dv.cobName) ~type_:(Some dv.displayableType)
      ~value:value ~variables_reference:var_ref ()

  let to_debug_protocol_variable session (dv : DebuggerVariable.t) cat =
    let reference = make_var_ref session dv cat in
    let value =
      if session.showDetails then
        dv.value ^ " " ^ dv.displayableType
      else
        dv.value
    in
    make_var dv value reference

  let to_debug_protocol_variable_with_details session (dv : DebuggerVariable.t) =
    let vars =
      if session.Session.showDetails then
        List.fold_left (fun vars (detail : VariableDetail.t) ->
            let v =
              Dp.Variable.make
                ~name:detail.name
                ~value:detail.value
                ~evaluate_name:(Some dv.cobName)
                ~type_:(Some detail.type_)
                ~variables_reference:0 ()
            in
            v :: vars
          ) [] dv.details
      else
        []
    in
    let v =
      Dp.Variable.make
        ~name:"value"
        ~value:dv.value
        ~evaluate_name:(Some dv.cobName)
        ~variables_reference:0 ()
    in
    v :: vars

  (* Retrieve the global variables for a given scope *)
  let global_storage_variable_request session scope_id =
    (* NOTE: should always be found, if scopesRequest is correct *)
    let%lwt scope_variables =
      Types.IntMap.find scope_id session.Session.globalVariables
    in
    let variables =
      List.map (fun dv ->
          to_debug_protocol_variable session dv Global) scope_variables
    in
    Lwt.return @@ Dp.Variables_command.Result.make ~variables ()

  (* Retrieve the local/stack variables for a given frame *)
  let local_storage_variable_request session frame_id =
    let thread_id, frame_level = decode_frame_id frame_id in
    let%lwt local_variables =
      Mi2.getLocalStorageVariables session.Session.debugger thread_id frame_level
    in
    let variables =
      List.map (fun dv ->
          to_debug_protocol_variable session dv Local) local_variables
    in
    Lwt.return @@ Dp.Variables_command.Result.make ~variables ()

  let child_variable_request (session : Session.t) var_name cat =
    let eval_func =
      match cat with
      | Session.Local ->
          (fun mi2 cName -> Mi2.evalLocalCVariable mi2 cName ())
      | Global ->
          Mi2.evalGlobalCVariable
    in
    let%lwt dv_opt = eval_func session.debugger var_name in
    Lwt.return @@
    match dv_opt with
    | None -> []
    | Some dv ->
      let vars_p =
        (* if session.showDetails then *)
        (*   List.map Promise.resolve *)
        (*     (toDebugProtocolVariableWithDetails gdb dv) *)
        (* else *)
          []
      in
      []

    (* Option.fold ~none:[] ~some:(fun dv -> *)
    (*     StrMap.fold (fun _n (child : DebuggerVariable.t) vars_p -> *)
    (*         let reference = makeVarRef gdb child cat in *)
    (*         let value_p = *)
    (*           if gdb.showDetails then *)
    (*             Promise.return child.displayableType *)
    (*           else *)
    (*             evalFunc gdb.miDebugger child.cName |> *)
    (*             Promise.then_ ~fulfilled:(fun evaluatedChild_opt -> *)
    (*                 Option.fold ~none:"?" ~some:( *)
    (*                   fun (evaluatedChild: DebuggerVariable.t) -> *)
    (*                     evaluatedChild.value) evaluatedChild_opt |> *)
    (*                 Promise.return) *)
    (*         in *)
    (*         let v_p = *)
    (*           Promise.then_ ~fulfilled:(fun value -> *)
    (*               Promise.return (makeVar child value reference) *)
    (*             ) value_p *)
    (*         in *)
    (*         (v_p :: vars_p) *)
    (*       ) dv.children vars_p *)
    (*   ) dv_opt |> *)
    (* Promise.all_list |> *)
    (* Promise.then_ ~fulfilled:(fun variables -> *)
    (*     let variables = List.rev variables in *)
    (*     let body = VariablesResponse.M.create ~variables in *)
    (*     VariablesResponse.set_body vResponse body; *)
    (*     sendResponse gdb.session ~response; *)
    (*     Promise.return () *)
    (*   ) *)
    (*   ) ~rejected:(fun err -> *)
    (*     sendErrorResponse gdb.session ~response ~codeOrMessage:1 *)
    (*       ~format:("Could not expand variable: " ^ *)
    (*                (Node.JsError.message err)) (); *)
    (*     Promise.return () *)
    (*   ) |> ignore *)

  let handle_variables _server session (args : Dp.Variables_command.Arguments.t) =
    match lookup_variable session args.variables_reference with
    | Int (scope_id), Global ->
        global_storage_variable_request session scope_id
    | Int (frame_id), Local ->
        local_storage_variable_request session frame_id
    | String (var_name), _ -> assert false
    (* let dummy = Dp.Variables_command.Result.make ~variables:[] () in *)
    (* Lwt.return dummy *)

  let handle_pause _server session (_args : Dp.Pause_command.Arguments.t) =
    let%lwt _done = Mi2.interrupt session.Session.debugger in
    (* TODO: handle error of continue. *)
    Lwt.return ()

  let handle_continue _server session (_args : Dp.Continue_command.Arguments.t) =
    let%lwt _done = Mi2.continue session.Session.debugger in
    (* TODO: handle error of continue. *)
    Lwt.return @@ Dp.Continue_command.Result.make ()

  let handle_next _server session (_args : Dp.Next_command.Arguments.t) =
    let%lwt _done = Mi2.stepOver session.Session.debugger in
    (* TODO: handle error of step in *)
    Lwt.return ()

  let handle_step_in _server session (_args : Dp.Step_in_command.Arguments.t) =
    let%lwt _done = Mi2.stepInto session.Session.debugger in
    (* TODO: handle error of step in *)
    Lwt.return ()

  let handle_step_out _server session (_args : Dp.Step_out_command.Arguments.t) =
    let%lwt _done = Mi2.stepOut session.Session.debugger in
    (* TODO: handle error of step in *)
    Lwt.return ()

  (* TODO: optional? *)
  let handle_evaluate _server (session : Session.t) (args : Dp.Evaluate_command.Arguments.t) =
    let frame_id = Option.value ~default:0 args.frame_id in
    let thread_id, frame_level = decode_frame_id frame_id in
    match args.context with
    | Some (Watch | Variables | Hover) ->
        let%lwt res_opt =
          Mi2.evalExpression session.debugger args.expression thread_id frame_level
        in
        let result = Option.value ~default:"not available" res_opt in
        let presentation_hint =
          Dp.Variable_presentation_hint.make ~kind:(Some Data) ()
        in
        Lwt.return @@ Dp.Evaluate_command.Result.make
         ~variables_reference:0
         ~presentation_hint:(Some presentation_hint)
         ~result
         ()
    | Some (Repl | Clipboard | Custom (_)) | None ->
        let%lwt result =
          match%lwt
            Mi2.sendUserInput session.debugger args.expression
              ~threadId:thread_id ~frameLevel:frame_level ();
          with
          | _ -> Lwt.return ""
          | exception (Mi2.Error s) -> Lwt.return s
        in
        Lwt.return @@ Dp.Evaluate_command.Result.make
          ~variables_reference:0
          ~result
          ()

  let handle_goto_targets _server (session : Session.t)
    (args : Dp.Goto_targets_command.Arguments.t) =
    let name = Option.get args.source.name in
    let path = Option.get args.source.path in
    let line = Obj.magic 0 in
    let column = Obj.magic 0 in
    let%lwt _done = Mi2.goto session.debugger path line in
    let targets = [
      Dp.Goto_target.make ~id:1 ~label:name ?column ~line ()
    ] in
    Lwt.return @@ Dp.Goto_targets_command.Result.make ~targets ()

  (* NOTE: goto actually performed in gotoTargetsRequest *)
  let handle_goto _server _session (_args : Dp.Goto_command.Arguments.t) =
    Lwt.return ()

  (* Optionals *)

  let handle_configuration_done _server (session : Session.t)
    (_args : Dp.Configuration_done_command.Arguments.t) =
    Mi2.Event.emit session.debugger Mi2.Event.DebugReady ();
    if session.needContinue then
      let%lwt _done = Mi2.continue session.debugger in
      Lwt.return_unit
    else Lwt.return_unit

  let handle_set_variable server (session : Session.t)
    (args : Dp.Set_variable_command.Arguments.t) =
    if session.showDetails && args.name <> "value" then
      error ~id:14 "%s cannot be changed" args.name
    else
      let%lwt invalidated_areas =
        match lookup_variable session args.variables_reference with
        | Int _, Local -> (* top / raw storage *)
            (* NOTE: only happens when session.showDetails is false *)
            (* name is COBOL name *)
            Mi2.setLocalCobolVariable session.debugger args.name args.value
        | String c_name, Local when session.showDetails (* sub / field *) ->
            Mi2.setLocalCVariable session.debugger c_name args.value
        | String _c_name, Local (* not session.showDetails *) (* sub / field *) ->
            (* name is COBOL name, c_name is short C name (b_n) *)
            Mi2.setLocalCobolVariable session.debugger args.name args.value
        | Int scope_id, Global -> (* top / raw storage *) (
            (* NOTE: only happens when session.showDetails is false *)
            match Types.IntMap.find scope_id session.globalVariables with
            | exception Not_found ->
                (* NOTE: should always be found if [handle_scopes] is correct. *)
                assert false
            | scope_variables -> (
                let%lwt scope_variables in
                (* name is COBOL name *)
                let p (dv : DebuggerVariable.t) = dv.cobName = args.name in
                match List.find p scope_variables with
                | exception Not_found -> Lwt.return []
                | dv ->
                    (* cName is full C name
                       (ie '/path/to/prog.c.h'::b_n) *)
                    Mi2.setGlobalCVariable session.debugger dv.cName args.value))
        | String c_name, Global when session.showDetails -> (* sub / field *)
            (* name is "value", c_name is full C name
               (ie '/path/to/prog.c.h'::b_n / f_n) *)
            Mi2.setGlobalCVariable session.debugger c_name args.value
        | String c_name, Global -> (* not session.showDetails *) (* sub / field *) (
            (* name is COBOL name, c_name is full C name
               (ie '/path/to/prog.c.h'::b_n) *)
            match Mi2.lookupGlobalCobolWithCName session.debugger args.name c_name with
            | None -> Lwt.return []
            | Some (dv : DebuggerVariable.t) ->
                Mi2.setGlobalCVariable session.debugger dv.cName args.value)
      in
      match invalidated_areas with
      | [] ->
        Lwt.return @@ Dp.Set_variable_command.Result.make ~value:args.value ()
      | _ ->
        let areas = List.map (fun a -> Dp.Invalidated_areas.Custom a) invalidated_areas in
        let%lwt () =
          let payload = Dp.Invalidated_event.Payload.make ~areas:(Some areas) () in
          Debug_rpc.send_event server (module Dp.Invalidated_event) payload
        in
        Lwt.return @@ Dp.Set_variable_command.Result.make ~value:args.value ()

  let all server session =
    let add m h = Capability_set.add m (h server session) in
    Capability_set.empty
    |> add (module Dp.Launch_command_ex) handle_launch_ex
    |> add (module Dp.Attach_command_ex) handle_attach_ex
    |> add (module Dp.Disconnect_command) handle_disconnect
    |> add (module Dp.Set_breakpoints_command) handle_set_breakpoints
    |> add (module Dp.Threads_command) handle_threads
    |> add (module Dp.Stack_trace_command) handle_stack_trace
    |> add (module Dp.Scopes_command) handle_scopes
    |> add (module Dp.Variables_command) handle_variables
    |> add (module Dp.Pause_command) handle_pause
    |> add (module Dp.Continue_command) handle_continue
    |> add (module Dp.Next_command) handle_next
    |> add (module Dp.Step_in_command) handle_step_in
    |> add (module Dp.Step_out_command) handle_step_out
    |> add (module Dp.Evaluate_command) handle_evaluate
    |> add (module Dp.Goto_targets_command) handle_goto_targets
    |> add (module Dp.Goto_command) handle_goto
    |> add (module Dp.Configuration_done_command) handle_configuration_done
    |> add (module Dp.Set_variable_command) handle_set_variable
    |> Capability_set.seal
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
  Log.info (fun k -> k "listening on: %a" pp_sockaddr sockaddr);
  let%lwt _server =
    Lwt_io.establish_server_with_client_address ~fd ~no_close:true sockaddr
    @@ fun _sockaddr (in_, out) ->
      let server = Debug_rpc.create ~in_ ~out () in
      let session = Session.create () in
      Capability_set.set_commands server Handlers.(all server session);
      Debug_rpc.start server
  in
  Lwt.return_unit

let () =
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
      let%lwt _ = run_server fd sockaddr in
      forever
