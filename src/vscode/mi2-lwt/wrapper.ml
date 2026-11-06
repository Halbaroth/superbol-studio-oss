external pipe_buf : unit -> int = "mi2_lwt_pipe_buf"

let pipe_buf = pipe_buf ()
let buf_cap = (pipe_buf / 2) - 4
let escaped_cap = 4 + (2 * buf_cap)

let escaped_chars =
  let arr = Array.make 256 '0' in
  let set c ec = arr.(Char.code c) <- ec in
  set '\b' 'b';
  set '\t' 't';
  set '\r' 'r';
  set '\n' 'n';
  set '"' '"';
  set '\\' '\\';
  arr

let to_c_string buf r escaped =
  let rec loop i j =
    if j >= r then (
      Bytes.set escaped i '\"';
      Bytes.set escaped (i + 1) '\n';
      i + 2)
    else
      let i =
        match Bytes.get buf j with
        | ('\b' | '\t' | '\r' | '\n' | '"' | '\\') as c ->
            Bytes.set escaped i '\\';
            Bytes.set escaped (i + 1) escaped_chars.(Char.code c);
            i + 2
        | c ->
            Bytes.set escaped i c;
            i + 1
      in
      loop i (j + 1)
  in
  Bytes.set escaped 0 '@';
  Bytes.set escaped 1 '\"';
  loop 2 0

let read_output () =
  let buf = Bytes.create buf_cap in
  let escaped = Bytes.create escaped_cap in
  let rec loop () =
    match Unix.read Unix.stdin buf 0 buf_cap with
    | 0 -> ()
    | r ->
        let e = to_c_string buf r escaped in
        let written = Unix.write Unix.stdout escaped 0 e in
        assert (written = e);
        loop ()
  in
  loop ()

let () =
  let fd_in, fd_out = Unix.pipe ~cloexec:false () in
  match Unix.fork () with
  | 0 ->
      Unix.close fd_out;
      Unix.dup2 ~cloexec:false fd_in Unix.stdin;
      Unix.close fd_in;
      read_output ()
  | _ ->
      Unix.close fd_in;
      Unix.dup2 ~cloexec:false fd_out Unix.stdout;
      Unix.close fd_out;
      let argc = Array.length Sys.argv - 1 in
      let argv = Array.init argc (fun i -> Sys.argv.(i + 1)) in
      Unix.execv argv.(0) argv
