(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

include Internal_event.Simple

let section = ["node"; "chain_validator"]

let updated_to_checkpoint =
  declare_2
    ~section
    ~name:"updated_to_checkpoint"
    ~msg:"updated to checkpoint {block_hash} (running in mode {history_mode})"
    ~level:Notice
    ("block_hash", Block_hash.encoding)
    ("history_mode", History_mode.encoding)

let prevalidator_filter_not_found =
  declare_1
    ~section
    ~name:"prevalidator_filter_not_found"
    ~msg:"no prevalidator filter found for protocol {protocol_hash}"
    ~level:Warning
    ("protocol_hash", Protocol_hash.encoding)

let prevalidator_reinstantiation_failure =
  declare_1
    ~section
    ~name:"prevalidator_reinstantiation_failure"
    ~msg:"failed to reinstantiate prevalidator error {trace}"
    ~level:Error
    ~pp1:pp_print_error_first
    ("trace", trace_encoding)

let prevalidator_instantiation_failure =
  declare_1
    ~section
    ~name:"prevalidator_instantiation_failure"
    ~msg:"failed to instantiate the prevalidator: {trace}"
    ~level:Error
    ~pp1:pp_print_error_first
    ("trace", trace_encoding)

let loading_protocol =
  declare_1
    ~section
    ~name:"loading_protocol"
    ~level:Notice
    ~msg:"loading non-embedded protocol {protocol} from disk"
    ~pp1:Protocol_hash.pp
    ("protocol", Protocol_hash.encoding)

module Bootstrapper = struct
  let section = ["node"; "chain_validator"; "bootstrapper"]

  let unable_to_fetch =
    declare_1
      ~section
      ~name:"bootstrapper_unable_to_fetch"
      ~level:Info
      ~msg:"Unable to fetch some ressources. Try again in {delay} seconds"
      ("delay", Time.System.Span.encoding)

  let not_enough_peers =
    declare_1
      ~section
      ~name:"bootstrapper_not_enough_peers"
      ~level:Warning
      ~msg:
        "Could not fetch the next range of {ressource}. Not enough peer. \
         Maybe a configuration issue?"
      ("ressource", Data_encoding.string)

  let fetching_header_timeout =
    declare_2
      ~section
      ~name:"bootstrapper_fetching_header_timeout"
      ~level:Warning
      ~msg:
        "Unable to fetch header of block level {level} with {peer}, the delay \
         has expired."
      ("level", Data_encoding.int32)
      ("peer", P2p_peer.Id.encoding)

  let fetching_operations_timeout =
    declare_2
      ~section
      ~name:"bootstrapper_fetching_operations_timeout"
      ~level:Warning
      ~msg:
        "Unable to fetch operations of block level {level} with {peer}, the \
         delay has expired."
      ("level", Data_encoding.int32)
      ("peer", P2p_peer.Id.encoding)
end
