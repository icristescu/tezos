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

module type RANGE = sig
  type bound

  type t = {from : bound; upto : bound}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val is_empty : t -> bool

  val length : t -> int

  val split : reverse:bool -> max_range_size:int -> t -> t Seq.t
end

let rec unfold f u () =
  match f u with None -> Seq.Nil | Some (x, u') -> Cons (x, unfold f u')

module type S = sig
  type range

  type ('output, 'error) parallel_task =
    range -> ('output option, 'error) result Lwt.t

  type ('trigger, 'input, 'error) sequential_task =
    range -> 'trigger -> 'input -> ('trigger option, 'error) result Lwt.t

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

  type 'error t

  val create : 'error parameters -> 'error t

  val wait : 'error t -> (unit, 'error) result Lwt.t

  val cancel : 'error t -> unit
end

module Make (Range : RANGE) : S with type range = Range.t = struct
  type range = Range.t

  type 'a linked_range = {range : Range.t; wait : 'a Lwt.t; trigger : 'a Lwt.u}

  type ('output, 'error) parallel_task =
    range -> ('output option, 'error) result Lwt.t

  type ('trigger, 'input, 'error) sequential_task =
    range -> 'trigger -> 'input -> ('trigger option, 'error) result Lwt.t

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

  (* Lazily, we link ranges one to ther other. The range [n] may
     trigger the wait variable of range [n+1]. *)
  let make_linked_seq ~starts_with seq =
    let next (wait, seq) =
      match seq () with
      | Seq.Nil ->
          None
      | Seq.Cons (range, seq') ->
          let (new_wait, trigger) = Lwt.task () in
          Some ({range; wait; trigger}, (new_wait, seq'))
    in
    unfold next (starts_with, seq)

  (* First check whether the task is filtered out. If it is the case,
     triggers the next range. Otherwise, first process the parallel
     task and then, when the previous range has finished its
     sequential, start the sequential task of this range. Finally
     trigger the sequential_task of the next range. If any of the task
     returns [Ok None], call the [recovery_task] and start again the
     application. *)
  let rec apply filter recovery_task parallel_task sequential_task linked_range
      =
    filter linked_range.range
    >>= function
    | Some trigger ->
        Lwt.wakeup_later linked_range.trigger trigger ;
        return_unit
    | None -> (
        parallel_task linked_range.range
        >>=? function
        | None ->
            recovery_task linked_range.range
            >>=? fun () ->
            apply
              filter
              recovery_task
              parallel_task
              sequential_task
              linked_range
        | Some res -> (
            linked_range.wait
            >>= fun trigger ->
            sequential_task linked_range.range trigger res
            >>=? function
            | None ->
                recovery_task linked_range.range
                >>=? fun () ->
                apply
                  filter
                  recovery_task
                  parallel_task
                  sequential_task
                  linked_range
            | Some trigger ->
                Lwt.wakeup_later linked_range.trigger trigger ;
                return_unit ) )

  type 'error t = (unit, 'error) result Lwt.t

  let iter_n ?(max_concurrency = 1) f seq =
    let (has_failed, on_failure) = Lwt.task () in
    let f elt =
      f elt
      >>= function
      | Ok () ->
          Lwt.return_unit
      | Error err ->
          Lwt.wakeup on_failure err ; Lwt.return_unit
    in
    let stream = Lwt_stream.of_seq seq in
    let p = Lwt_stream.iter_n ~max_concurrency f stream in
    Lwt.on_success has_failed (fun _err -> Lwt.cancel p) ;
    Lwt.pick
      [ (p >>= fun () -> return_unit);
        (has_failed >>= fun err -> Lwt.return (Error err)) ]

  let create
      (E
        { tasks_in_parallel;
          starts_with;
          range;
          max_range_size;
          reverse;
          parallel_task;
          sequential_task;
          recovery_task;
          filter }) =
    (* Use to capture invalid arguments *)
    try
      let range_seq = Range.split ~reverse ~max_range_size range in
      let linked_range_seq = make_linked_seq ~starts_with range_seq in
      iter_n
        ~max_concurrency:tasks_in_parallel
        (apply filter recovery_task parallel_task sequential_task)
        linked_range_seq
    with exn -> Lwt.fail exn

  let wait task = Lwt.protected task

  let cancel task = Lwt.cancel task
end
