open Eio.Std

module Read = Eio.Buf_read
module Write = Eio.Buf_write

exception Unsupported_version of Neo4j.Protocol.version

let handshake flow =
  traceln "Client: trying handshake...";
  Write.with_flow flow (fun to_server -> Write.bytes to_server Neo4j.Protocol.hello);
  Eio.Flow.shutdown flow `Send;
  let buffer = Buffer.create 32 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink buffer);
  let version = Neo4j.Protocol.parse_version (Buffer.to_bytes buffer) in
  match Neo4j.Protocol.supports version with
  | true -> version
  | false -> raise (Unsupported_version version)

let connect ~net ~addr =
  traceln "Client: connecting to server";
  Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  let version = handshake flow in
  traceln "Version %d.%d" version.major version.minor;
  traceln "Client: closing connection";
  Eio.Net.close flow

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 7687) in
  connect ~net ~addr
