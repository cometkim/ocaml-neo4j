type version = {
  major: int;
  minor: int;
}

let hello =
  let header = Bytes.of_string "\x60\x60\xb0\x17" in
  let versions = Bytes.of_string "\x00\x00\x00\x05\x00\x02\x04\x04\x00\x00\x00\x00\x00\x00\x00\x00" in
  Bytes.cat header versions

let parse_version bytes =
  let minor = Bytes.get_uint8 (Bytes.sub bytes 2 2) 0 in
  let major = Bytes.get_uint8 (Bytes.sub bytes 2 2) 1 in
  { major; minor }

let supports version =
  match version with
  | { major = 5; _ } -> true
  | { major = 4; minor = 3 | 2 | 1 } -> true
  | _ -> false
