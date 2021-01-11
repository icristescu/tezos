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

(** {2 Ranger} *)

(** The ranger module defines a generic worker to process abstract
   ranges. An abstract range aims to encode an interval [from; upto]
   . It is created from the two bounds of the interval.

   The ranger worker processes a sequence of ranges. For each range,
   the task is cut in two:

   - A task that can be parallelised called [parallel_task]

   - A task that needs to be run sequentially called
   [sequential_task].

   Each task may result into three different outcomes:

   - [task range = Some result]: The task operated successfully

   - [task range = Error err]: Such error is not recoverable from the
   worker, the worker is fulfilled with [Error err].

   - [task range = None]: The task failed but this is
   recoverable. Such recovery is delegated to the [recovery_task].  If
   [recovery_task = Error err]: No recovery is possible, the worker is
   fulfilled with [Error err]. If [discovery_task = Ok ()] then both
   the [parallel_task] and the [sequential_task] are run again.

   If one of these tasks is rejected, then the worker is rejected too.

   The worker splits the [range] into smaller ranges of size at most
   [max_range_size] (in reverse order if [reverse] is set to
   true). This list of ranges is computed lazily as a [range Seq.t].
   Then it folds over the sequence. For each [range], the worker
   allocates [tasks_in_parallel] slots. The worker feeds each
   available slot with a [parallel_task] with the next range
   available. The [sequential_task] is processed only when the
   [sequential_task] of the previous range is over. For the first
   range, the previous [sequential_task] is given by the [starts_with]
   parameter. The worker guarantees that there is at most one
   [sequential_task] which is run at the same time. *)

module type RANGE = sig
  (** An abstract range. A range is defined by two bounds. *)

  type bound

  type t = {from : bound; upto : bound}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val is_empty : t -> bool

  val length : t -> int

  (** Split a range into a sequence of ranges of at most size
     [max_size]. Ranges can be sequenced in the reversed order if
     [reverse] is true. All the ranges are guaranteed to be non-empty.

     Precondition: [max_range_size] > 0.
*)
  val split : reverse:bool -> max_range_size:int -> t -> t Seq.t
end

module type S = sig
  type range

  (** Task which can be parallelised. *)
  type ('output, 'error) parallel_task =
    range -> ('output option, 'error) result Lwt.t

  (** Task which has to be run sequentially. It takes a [trigger] as
     parameter which is the output of the previous sequential
     task. The output is a trigger for the next sequential task. *)
  type ('trigger, 'input, 'error) sequential_task =
    range -> 'trigger -> 'input -> ('trigger option, 'error) result Lwt.t

  (** A recovery task is executed when either the parallel_task or the
     sequential_task returned [Ok None]. *)
  type 'error recovery_task = range -> (unit, 'error) result Lwt.t

  type 'error parameters =
    | E : {
        tasks_in_parallel : int;
        starts_with : 'trigger Lwt.t;
        range : range;
        max_range_size : int;
        reverse : bool;
        parallel_task : ('data, 'error) parallel_task;
        sequential_task : ('trigger, 'data, 'error) sequential_task;
        recovery_task : 'error recovery_task;
        filter : range -> 'trigger option Lwt.t;
      }
        -> 'error parameters

  (** Abstract type of worker *)
  type 'error t

  (** [create parameters] create a worker with [parameters].

      Precondition:

      - [parameters.max_range_size] > 0,

      - [parameters.tasks_in_parallel] > 0. *)
  val create : 'error parameters -> 'error t

  (** [wait worker] waits that the task of the worker is over.

      Because several tasks are run in parallel, the worker may raise
     several errors at the same time. These errors are returned as a
     list. *)
  val wait : 'error t -> (unit, 'error) result Lwt.t

  (** [cancel worker] cancels the current task of the worker. *)
  val cancel : 'error t -> unit
end

module Make (Range : RANGE) : S with type range = Range.t
