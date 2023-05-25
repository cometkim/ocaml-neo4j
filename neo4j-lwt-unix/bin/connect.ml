open Lwt.Syntax

exception Unsupported_version of Neo4j.Protocol.version

let handshake sock =
  print_endline "+Client: trying handshake...";
  let send_data = Neo4j.Protocol.hello in
  let* _ = Lwt_unix.send sock send_data 0 (Bytes.length send_data) [] in
  Lwt_unix.shutdown sock Unix.SHUTDOWN_SEND;
  let recv_data = Bytes.create 32 in
  let* _ = Lwt_unix.recv sock recv_data 0 (Bytes.length recv_data) [] in
  let version = Neo4j.Protocol.parse_version recv_data in
  match Neo4j.Protocol.supports version with
  | true -> Lwt.return version
  | false -> Lwt.fail (Unsupported_version version)

let connect ~sock ~addr =
  print_endline "+Client: connecting to server";
  let* () = Lwt_unix.connect sock addr in
  let* version = handshake sock in
  print_endline (Printf.sprintf "+Version %d.%d" version.major version.minor);
  print_endline "+Client: closing connection";
  let* () = Lwt_unix.close sock in
  Lwt.return ()

let () =
  Lwt_main.run
    (let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 7687) in
     connect ~sock ~addr)
