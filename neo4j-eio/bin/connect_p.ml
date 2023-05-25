open Eio.Std

module Read = Eio.Buf_read
module Write = Eio.Buf_write

exception Unsupported_version of Neo4j.Protocol.version

let handshake flow ~id =
  traceln "conn(%d): trying handshake..." id;
  Write.with_flow flow (fun to_server -> Write.bytes to_server Neo4j.Protocol.hello);
  Eio.Flow.shutdown flow `Send;
  let buffer = Buffer.create 32 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink buffer);
  let version = Neo4j.Protocol.parse_version (Buffer.to_bytes buffer) in
  match Neo4j.Protocol.supports version with
  | true -> version
  | false -> raise (Unsupported_version version)

let connect ~id ~net ~addr =
  traceln "client: connecting to server with id=%d" id;
  Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  let version = handshake ~id flow in
  traceln "conn(%d): version %d.%d" id version.major version.minor;
  traceln "conn(%d): closing" id;
  Eio.Net.close flow

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7687) in
  traceln "client: connect 100 times concurrently";
  Array.to_list (Array.init 100 (fun i -> i))
  |> Fiber.List.iter (fun id -> 
      connect ~id ~net ~addr
  )
