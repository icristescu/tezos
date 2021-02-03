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

(** {2 Bootstrapper } *)

(** This module is used to fetch and validate a long branch. For the
   bootstrapper, a branch is encoded as an interval delimited by two
   bounds. Below such interval will be called a [range].

    The bootstrapper proceeds in two steps:

    1. All the [block headers] of the branch are fetched and stored
   onto the hard-drive.

   2. Once all the [block headers] have been fetched and stored, it
   starts to fetch the operations and validate the blocks.

   The first step is done top to bottom while the second step is done
   bottom to top.

   Because the initial [range] may be contains branch with several
   millions of blocks, the [range] is cut into smaller ranges of size
   [range_size].

   Both steps described above are implemented using the functor
   [Ranger.Make] where the parameter is [Ranger.Int32].

   For the first step, the parallel task is fetching the headers via
   the function [get_headers]. The sequential task is to check there
   consistency and store them onto the disk.

   For the second step, the parallel task is fetching operations via
   the function [get_operations]. The sequential task is to validate
   the blocks one by one with the function [validate].

   The recovery tasks (given to the [ranger]) for both steps is given
   by [when_unable_to_fetch]. As the name suggests, the only
   recoverable failure for the [bootstrapper] is when the fetching of
   one data failed. All other errors are fatal.

   The bootstrapper maintains several invariants:

   - There is at most one job at the time

   - The memory space is linear with respect to
   [parallel_ranges_header_fetched] for the first step and
   [parallel_ranges_operations_fetched] for the second.

   This last invariant does not take into account the memory space
   used for the introspection.

   {3 Recovery mode}

   The recovery mode remembers [headers] fetched when the [node] is
   killed. First the [root_path] is renamed to [temporary_path] and a
   fresh [root_path] is created. Then for each [range], if there is
   the associated file in [temporary_path], it checks whether the file
   is corrupted. If the file can be decoded, then it marks the range
   as skippable and continues and the file is copied to [root_path].

   Because the directory [root_path] is renamed to [temporary_path],
   if the node is killed at the wrong time, i.e. valid files are still
   in [temporary_path], then these files will be erased once the node
   will be restarted. In practice, because headers are fetched in the
   same order between two runs, and because the time to check whether
   a file can be decoded is fast, such behavior seems quite difficult
   to trigger. Indeed, between two runs, if everything went well, at
   most one range needs to be fetched it the [target] changed.

   Notice also that if the [range_size] is changed between two runs,
   all the headers needs to be fetched gain since the name of the file
   depends on the [range_size].

 *)

(** The internal state of the bootstrapper. *)
type t

(** Definition of a block for the bootstrapper *)
type block = Block_header.t * Operation.t list list

(** The consistency check of the header cannot be done in
   parallel. This callback is given by the [get_headers] function and
   is called by the [bootstrapper] when it starts the sequential task
   associated to the parallel task which called [get_headers]. *)
type consistency_checker = Block_hash.t -> bool Lwt.t

type getters = {
  get_current_head : unit -> Block_header.t Lwt.t;
      (** Use to get the current head of the chain. *)
  get_headers :
    target:Block_header.t ->
    from:Int32.t ->
    upto:Int32.t ->
    (consistency_checker * Block_header.t list) option Lwt.t;
      (** Use to fetch a batch of headers from level [bottom] (excluded)
     to level [upto] (included).  The size of the list should be equal
     to [top - bottom]. Returns [None] if it was unable to fetch one
     of the headers. [target] is a reference block whose level should
     be above [upto]. The bootstrapper always call this function with
     [from] <= [upto]. *)
  get_operations : Block_header.t list -> block list option Lwt.t;
      (** Use to fetch all the operations of a batch of blocks. Returns
     [None] if it was unable to fetch one of the operations. Otherwise
     returns a list of blocks which has the same size than the initial
     list of [block headers]. *)
  when_unable_to_fetch : unit -> unit tzresult Lwt.t;
      (** This function is called when either [get_headers] or
     [get_operations] returned [None]. If the promise returns by this
     function fulfilled with an error then the whole [bootstrapper]
     current task fails with this error. Otherwise if it fulfills with
     [()] then it tries again to fetch the corresponding data for
     the given range.  *)
  validate : block -> unit tzresult Lwt.t;
      (** This function is called when an entire block was fetched and the
     predecessor of this block is known as valid. If the function
     returns a promise which is fulfilled with an error then the whole
     [bootstrapper] current tasks fails also with this error. *)
}

type configuration = {
  root_path : string;
      (** root_path where the bootstrapper will store its files *)
  temporary_path : string;
      (** temporary_path where the bootstrapper store intermediate
     representations of a file. It is expected that these both paths
     are on the same partition but one should not be a subpath of the
     other. *)
  range_size : int;  (** Size of ranges fetched. *)
  parallel_ranges_header_fetched : int;
      (** How many ranges are fetched at the same time. *)
  parallel_ranges_operations_fetched : int;
      (** How many ranges are fetched at the same time *)
  getters : getters;
      (** Functions used by the bootstrapper to communicate with the external world. *)
}

(** [create configuration] creates an [Inactive] bootstrapper.  *)
val create : configuration -> t tzresult Lwt.t

(** [notify_target bootstrapper target] notifies a new [target] for
   the bootstrapper. If the [bootstrapper] is inactive, it starts a
   new task with this target. If the [bootstrapper] is active then the
   target is cached and will be executed once the [bootstrapper]
   finished successfuly its current task. Previous cached target are
   erased. *)
val notify_target : t -> target:Block_header.t -> unit

(** [wait bootstrapper] is a noop if the bootstrapper is
     inactive. Otherwise, it waits that the current job of the
     bootstrapper is over. *)
val wait : t -> unit tzresult Lwt.t

(** [cancel bootstrapper] cancels the current task of the
     bootstrapper. Is a noop if the bootstrapper was inactive. *)
val cancel : t -> unit

(** [shutdown bootstrapper] cancels the current task of the
     bootstrapper and log a shutdown event. *)
val shutdown : t -> unit Lwt.t

(** [state bootstrapper] returns the current state of the
   bootstrapper. *)
val state : t -> Bootstrapper_services.Introspection.state
