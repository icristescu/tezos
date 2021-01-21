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

module Checkpoint_heuristic = struct
  let section = ["node"; "chain_validator"; "checkpoint_heuristic"]

  let started =
    declare_0
      ~section
      ~name:"checkpoint_heuristic_started"
      ~level:Notice
      ~msg:"Checkpoint heuristic started"
      ()

  let need_more_checkpoints =
    declare_2
      ~section
      ~name:"checkpoint_heuristic_need_more_checkpoints"
      ~level:Notice
      ~msg:
        "too few candidates to get a consensus. Rcv/Exp: {received}/{expected}."
      ("received", Data_encoding.int31)
      ("expected", Data_encoding.int31)

  let need_more_checkpoints_no_consensus =
    declare_3
      ~section
      ~name:"checkpoint_heuristic_need_more_checkpoints_no_consensus"
      ~level:Notice
      ~msg:
        "did not receive {threshold} candidates with the same value. Rcv/Exp: \
         {received}/{expected}"
      ("threshold", Data_encoding.int31)
      ("received", Data_encoding.int31)
      ("expected", Data_encoding.int31)

  let no_consensus =
    declare_0
      ~section
      ~name:"checkpoint_heuristic_no_consensus"
      ~level:Notice
      ~msg:"no consensus found"
      ()

  let consensus =
    declare_1
      ~section
      ~name:"checkpoint_heuristic_consensus"
      ~level:Notice
      ~msg:"consensus found on {hash}"
      ("hash", Block_hash.encoding)

  let retry =
    declare_1
      ~section
      ~name:"checkpoint_heuristic_retry"
      ~level:Notice
      ~msg:"retry consensus heuristic in {delay} seconds"
      ("delay", Time.System.Span.encoding)
end
