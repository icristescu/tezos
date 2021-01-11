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

(** {2 Consensus heuristic} *)

(** The consensus heuristic handles a heuristic to decide a consensus
   over one data. This data being asked to a remote peer. The set of a
   data and a peer is called a [candidate].

    The consensus heuristic is parameterized by two values:

    - [expected] which is the number of [expected] updates associated
   to [expected] different peers.

    - [threshold] which is required number of candidates to achieve a
   consensus.

   It is expected that [0 <= threshold <= expected < 2 * threshold] to
   ensure a consensus.  *)

type 'a t

type 'a state =
  | Consensus of 'a
  | No_consensus of 'a list
  | Need_more_candidates

(** The semantics of the heuristic is the following (assuming each
   update using a different peer):

   - If the heuristic received less than [threshold] updates then its
   state is [Need_more_candidates]

   - If the heuristic received more than [expected] updates and one
   candidate did not reach the [threshold] it returns [No_consensus]
   with the different candidates received. Otherwise, it returns
   [Consensus] with the candidate.

   - Otherwise, Either a candidate has reached the [threshold] and the
   state of the heuristic is [Consensus] with the candidate, or the
   state is [Need_more_candidates].

 *)

(** [create compare expected threshold ()] initializes the consensus
   heuristic. *)
val create :
  ?compare:('a -> 'a -> int) -> expected:int -> threshold:int -> unit -> 'a t

(** [get_state heuristic] returns the current state of the heuristic
   according to the semantics given above. *)
val get_state : 'a t -> 'a state

(** [update heuristic (peer, candidate)] updates the heuristic with
   the candidate [(peer, data)]. If a data was already registered for
   [peer], then it is erased by the update. *)
val update : 'a t -> P2p_peer.Id.t * 'a -> unit

(** {2 Consensus heuristic worker} *)

(** This worker aims to be used in the context where a consensus is
   required once in a while and where a consensus has to be
   reached. *)
module Worker : sig
  type 'a t

  (** [create name age_limit job restart_delay] creates the internal
     state of the worker.

   - [name] is the name of the worker.

   - [age_limit] is a promise which aims to be fulfilled when the
     current value of the consensus heuristic is too old (for example:
     [fun () -> Lwt_unix.sleep 150.]).

   - [job] is the task to be run and returns the state of the
     heuristic when the task is over.

   - [restart_delay] is a promise which is called if the state returns
     by the [job] is either [Need_more_candidates] or
     [No_consensus]. Then the promise [job ()] is run again. *)
  val create :
    name:string ->
    age_limit:(unit -> unit Lwt.t) ->
    job:(unit -> 'a state Lwt.t) ->
    restart_delay:(unit -> unit Lwt.t) ->
    'a t

  (** [wait worker] wait for a value for the consensus heuristic
     associated to the worker. If no consensus is found, the
     heuristic is run again. *)
  val wait : 'a t -> 'a Lwt.t

  (** [on_next_consensus worker hook] registers a hook to be executed
     on the next consensus value or the current one if there is a
     valid consensus ([Lwt.state (wait worker)] = [Lwt.Return hash]).

     Hooks are executed in the same order they are registered. *)
  val on_next_consensus : 'a t -> ('a -> unit) -> unit

  (** [on_all_consensus worker hook] registers a hook to be executed
     on every consensus value or the current one if there is a valid
     consensus ([Lwt.state (wait worker)] = [Lwt.Return hash]).

     It is guaranteed that a hook is executed exactly one time for
     each time that a consensus is reached. More precisely, between
     two executions of the same hook, the promise [age_limit ()] was
     run and fulfilled exactly once.  *)
  val on_all_consensus : 'a t -> ('a -> unit) -> unit

  (** [cancel worker] cancels the current task of the worker *)
  val cancel : 'a t -> unit
end
