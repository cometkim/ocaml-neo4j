open Lwt.Syntax

exception Unsupported_version of Neo4j.Protocol.version

let handshake sock ~id =
  print_endline (Printf.sprintf "+conn(%d): trying handshake..." id);
  let send_data = Neo4j.Protocol.hello in
  let* _ = Lwt_unix.send sock send_data 0 (Bytes.length send_data) [] in
  Lwt_unix.shutdown sock Unix.SHUTDOWN_SEND;
  let recv_data = Bytes.create 32 in
  let* _ = Lwt_unix.recv sock recv_data 0 (Bytes.length recv_data) [] in
  let version = Neo4j.Protocol.parse_version recv_data in
  match Neo4j.Protocol.supports version with
  | true -> Lwt.return version
  | false -> Lwt.fail (Unsupported_version version)

let connect ~id ~addr =
  print_endline (Printf.sprintf "+client: connecting to server with id=%d" id);
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect sock addr in
  let* version = handshake ~id sock in
  print_endline (Printf.sprintf "+conn(%d): version %d.%d" id version.major version.minor);
  print_endline (Printf.sprintf "+conn(%d): closing" id);
  let* () = Lwt_unix.close sock in
  Lwt.return ()

let () = Lwt_main.run begin
  let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 7687) in
  print_endline "+client: connect 100 times concurrently";
  Array.to_list (Array.init 100 (fun i -> i))
  |> Lwt_list.iter_p (fun id ->
      connect ~id ~addr
  )
end
