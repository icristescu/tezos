(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** {2 Peer validator} *)

(** Peer validator is in charge to validate new branches or new heads
   along one peer when the current head of the node is above the
   target (see [Chain_validator]). There is one peer validator worker
   by peer and by chain.

   The peer validator can handle two asynchronous request:

   - A new head advertised by the peer: [notify_head]

   - A new branch advertised by the peer: [notify_branch]

   The peer validator handles one asynchronous request at a
   time. Also, at most one request can be on hold. When a new request
   is coming while there is one on hold, the peer validator prioritizes
   the last advertised branch not handled if possible, otherwise it
   will take the last advertised head.

   Moreover, if it handles two synchronous requests:

   - Notify that the current head is equal or above the target:
   [notify_target_reached]

   - Notify that the current head is behind the target:
   [notify_behind_target]

   The peer validator can validate a branch only when the target is
   reached. Otherwise, the last advertised branch is memoized. Once
   the target is reached, if a branch was advertised, it will start to
   validate this branch.

   The peer validator sends a heartbeat if we did not receive any news
   from the peer for 90 seconds.  *)

type t

type limits = {
  new_head_request_timeout : Time.System.Span.t;
  block_header_timeout : Time.System.Span.t;
  block_operations_timeout : Time.System.Span.t;
  protocol_timeout : Time.System.Span.t;
  worker_limits : Worker_types.limits;
}

val peer_id : t -> P2p_peer.Id.t

val create :
  ?notify_new_block:(State.Block.t -> unit) ->
  ?notify_termination:(unit -> unit) ->
  target_reached:bool ->
  limits ->
  Block_validator.t ->
  Distributed_db.chain_db ->
  P2p_peer.Id.t ->
  t tzresult Lwt.t

val shutdown : t -> unit Lwt.t

val notify_branch : t -> Block_locator.t -> unit

val notify_head : t -> Block_header.t -> unit

val running_workers : unit -> ((Chain_id.t * P2p_peer.Id.t) * t) list

val status : t -> Worker_types.worker_status

val information : t -> Worker_types.worker_information

val current_request :
  t ->
  (Time.System.t * Time.System.t * Peer_validator_worker_state.Request.view)
  option

val last_events :
  t -> (Internal_event.level * Peer_validator_worker_state.Event.t list) list

val pipeline_length :
  t -> Peer_validator_worker_state.Worker_state.pipeline_length

val notify_target_reached : t -> unit

val notify_behind_target : t -> unit
