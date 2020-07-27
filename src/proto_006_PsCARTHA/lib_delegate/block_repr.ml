(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type contents = {header : Block_header.t; operations : Operation.t list list}

type metadata = {
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
  block_metadata : Bytes.t;
  operations_metadata : Bytes.t list list;
}

type block = {
  hash : Block_hash.t;
  contents : contents;
  mutable metadata : metadata option;
      (* allows updating metadata field when loading cemented metadata *)
}

type t = block

let contents_encoding =
  let open Data_encoding in
  conv
    (fun {header; operations} -> (header, operations))
    (fun (header, operations) -> {header; operations})
    (obj2
       (req "header" (dynamic_size Block_header.encoding))
       (req "operations" (list (list (dynamic_size Operation.encoding)))))

let metadata_encoding : metadata Data_encoding.t =
  let open Data_encoding in
  conv
    (fun { message;
           max_operations_ttl;
           last_allowed_fork_level;
           block_metadata;
           operations_metadata } ->
      ( message,
        max_operations_ttl,
        last_allowed_fork_level,
        block_metadata,
        operations_metadata ))
    (fun ( message,
           max_operations_ttl,
           last_allowed_fork_level,
           block_metadata,
           operations_metadata ) ->
      {
        message;
        max_operations_ttl;
        last_allowed_fork_level;
        block_metadata;
        operations_metadata;
      })
    (obj5
       (opt "message" string)
       (req "max_operations_ttl" uint16)
       (req "last_allowed_fork_level" int32)
       (req "block_metadata" bytes)
       (req "operations_metadata" (list (list bytes))))

let encoding =
  let open Data_encoding in
  conv
    (fun {hash; contents; metadata} -> (hash, contents, metadata))
    (fun (hash, contents, metadata) -> {hash; contents; metadata})
    (dynamic_size
       ~kind:`Uint30
       (obj3
          (req "hash" Block_hash.encoding)
          (req "contents" contents_encoding)
          (varopt "metadata" metadata_encoding)))

let pp_json fmt b =
  let json = Data_encoding.Json.construct encoding b in
  Data_encoding.Json.pp fmt json

(* Contents accessors *)

let hash blk = blk.hash

let header blk = blk.contents.header

let operations blk = blk.contents.operations

let shell_header blk = blk.contents.header.Block_header.shell

let level blk = blk.contents.header.Block_header.shell.level

let proto_level blk = blk.contents.header.Block_header.shell.proto_level

let predecessor blk = blk.contents.header.Block_header.shell.predecessor

let timestamp blk = blk.contents.header.Block_header.shell.timestamp

let validation_passes blk =
  blk.contents.header.Block_header.shell.validation_passes

let fitness blk = blk.contents.header.Block_header.shell.fitness

let context blk = blk.contents.header.Block_header.shell.context

let protocol_data blk = blk.contents.header.Block_header.protocol_data

(* Metadata accessors *)

let metadata blk = blk.metadata

let message metadata = metadata.message

let max_operations_ttl metadata = metadata.max_operations_ttl

let last_allowed_fork_level metadata = metadata.last_allowed_fork_level

let block_metadata metadata = metadata.block_metadata

let operations_metadata metadata = metadata.operations_metadata

let check_block_consistency ?genesis_hash ?pred_block block =
  (* TODO add proper errors *)
  let block_header = header block in
  let block_hash = hash block in
  let result_hash = Block_header.hash block_header in
  fail_unless
    ( Block_hash.equal block_hash result_hash
    || Option.fold
         ~some:(fun genesis_hash -> Block_hash.equal block_hash genesis_hash)
         ~none:false
         genesis_hash )
    (Exn
       (Failure
          (Format.asprintf
             "Inconsistent block: inconsistent hash found for block %ld. \
              Expected %a, got %a"
             (level block)
             Block_hash.pp
             block_hash
             Block_hash.pp
             result_hash)))
  >>=? fun () ->
  Option.fold pred_block ~none:return_unit ~some:(fun pred_block ->
      fail_unless
        ( Block_hash.equal (hash pred_block) (predecessor block)
        && Compare.Int32.(level block = Int32.succ (level pred_block)) )
        (Exn
           (Failure
              (Format.asprintf
                 "Inconsistent block: inconsistent predecessor found for \
                  block %a (%ld) - expected: %a vs got: %a"
                 Block_hash.pp
                 block_hash
                 (level block)
                 Block_hash.pp
                 (hash pred_block)
                 Block_hash.pp
                 (predecessor block)))))
  >>=? fun () -> return_unit

let read_next_block fd =
  (* Read length *)
  let length_bytes = Bytes.create 4 in
  Lwt_utils_unix.read_bytes ~pos:0 ~len:4 fd length_bytes
  >>= fun () ->
  let block_length_int32 = Bytes.get_int32_be length_bytes 0 in
  let block_length = Int32.to_int block_length_int32 in
  let block_bytes = Bytes.create (4 + block_length) in
  Lwt_utils_unix.read_bytes ~pos:4 ~len:block_length fd block_bytes
  >>= fun () ->
  Bytes.set_int32_be block_bytes 0 block_length_int32 ;
  Lwt.return
    (Data_encoding.Binary.of_bytes_exn encoding block_bytes, 4 + block_length)

let read_next_block_opt fd =
  Lwt.catch
    (fun () -> read_next_block fd >>= fun b -> Lwt.return_some b)
    (fun _exn -> Lwt.return_none)
