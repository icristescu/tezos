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

(** {2 Bootstraper configuration} *)

(** This module aims to provide an easy configuration for the
   bootstrapper which ensures the invariants.

    - For the validation we use [Block_validator.validate], a callback
   [notify_new_block] can be registered.

   - We use a header requester to fetch headers from the network

   - We use an operations request to fetch block from the network

   - The bootstrapper directory is [data_dir // "bootstrap"]

   - The temporary directory is [data_dir // "bootstrap" // "tmp"]

   Both requesters ensure that the peer is punished if it sends
   inconsistent data.  *)
type parameters = {
  data_dir : string;
  chain_state : State.Chain.t;
  chain_db : Distributed_db.chain_db;
  operations_requester : Block_header.t Distributed_db.Request_worker.t;
  header_requester : Int32.t Distributed_db.Request_worker.t;
  notify_new_block : State.Block.t -> unit;
  block_validator : Block_validator.t;
}

type limits = {
  fetching_headers_parallel_jobs : int;
  fetching_headers_timeout : Time.System.Span.t;
  fetching_operations_parallel_jobs : int;
  fetching_operations_timeout : Time.System.Span.t;
  delay_when_fetching_failed : Time.System.Span.t;
  range_size : int;
}

val configuration : limits -> parameters -> Bootstrapper.configuration
