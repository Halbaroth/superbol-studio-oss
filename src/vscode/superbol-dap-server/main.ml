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
      let write () = match level with
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

let with_socket_fd ~temp_dir k =
  let path = temp_dir // "server.sock" in
  Unix.close @@ Unix.openfile path [ O_RDWR; O_CREAT; O_EXCL; O_CLOEXEC ] 0o600;
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let finally () = try Unix.close fd with _ -> () in
  Fun.protect ~finally @@ fun () -> k (path, Lwt_unix.of_unix_file_descr fd)

let pp_sockaddr ppf sockaddr =
  match sockaddr with
  | Unix.ADDR_UNIX s -> Fmt.string ppf s
  | ADDR_INET (s, p) -> Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr s) p

let () =
  (* let open Lwt.Infix.Let_syntax in *)
  Logs.set_reporter @@ lwt_reporter ();
  let temp_dir = Compat.Filename.temp_dir "superbol-dap-server" "" in
  with_socket_fd ~temp_dir @@ fun (path, fd) ->
  let sockaddr = Unix.ADDR_UNIX path in
  let _server =
    Lwt_io.establish_server_with_client_address ~fd sockaddr
    @@ fun _sockaddr (in_, out) ->
      Logs.info (fun k -> k "listening on %a" pp_sockaddr sockaddr);
      Debug_rpc.(start @@ create ~in_ ~out ())
  in
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
