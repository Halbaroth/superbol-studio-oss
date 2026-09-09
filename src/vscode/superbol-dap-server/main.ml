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

module Filename = struct
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

let (//) = Filename.concat

let with_fresh_socket ~temp_dir k =
  let path = temp_dir // "server.sock" in
  Unix.close @@ Unix.openfile path [ O_RDWR; O_CREAT; O_EXCL; O_CLOEXEC ] 0o600;
  let finally () = try Unix.unlink path with _ -> () in
  let sockaddr = Unix.ADDR_UNIX path in
  Fun.protect ~finally @@ fun () -> k sockaddr

let () =
  let server =
    let temp_dir = Filename.temp_dir "superbol-dap-server" "" in
    with_fresh_socket ~temp_dir @@ fun sockaddr ->
      Lwt_io.with_connection sockaddr @@ fun (in_, out) ->
        Debug_rpc.(start @@ create ~in_ ~out ())
  in
  Lwt_main.run server
