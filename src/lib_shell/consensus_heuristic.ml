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

type 'a t = {
  threshold : int;
  expected : int;
  candidates : 'a P2p_peer.Id.Table.t;
  compare : 'a -> 'a -> int;
}

let update t (peer_id, data) =
  P2p_peer.Id.Table.replace t.candidates peer_id data

type 'a state =
  | Consensus of 'a
  | No_consensus of 'a list
  | Need_more_candidates

let create ?(compare = compare) ~expected ~threshold () =
  if threshold < 0 then invalid_arg "Hash_heuristic.create: threshold negative" ;
  if expected < threshold then
    invalid_arg "Hash_heuristic.create: expected lower than threshold" ;
  if expected >= threshold * 2 then
    invalid_arg
      "Hash_heuristic.create: expected greater than twice the threshold" ;
  {expected; threshold; candidates = P2p_peer.Id.Table.create 11; compare}

let get_state (type a) (t : a t) =
  let module Table = Hashtbl.Make (struct
    type t = a

    let equal x y = compare x y = 0

    let hash = Hashtbl.hash
  end) in
  if P2p_peer_id.Table.length t.candidates < t.threshold then
    Need_more_candidates
  else
    let table = Table.create 11 in
    let found = ref None in
    let candidates = ref [] in
    P2p_peer_id.Table.iter
      (fun _ value ->
        candidates := value :: !candidates ;
        let n =
          match Table.find table value with
          | None ->
              Table.add table value 1 ; 1
          | Some n ->
              Table.replace table value (n + 1) ;
              n + 1
        in
        if n = t.threshold then found := Some value)
      t.candidates ;
    match !found with
    | None ->
        if P2p_peer_id.Table.length t.candidates >= t.expected then
          No_consensus (List.fast_sort t.compare !candidates)
        else Need_more_candidates
    | Some header ->
        Consensus header

module Worker = struct
  type 'a t = {
    name : string;
    age_limit : unit -> unit Lwt.t;
    state : 'a state;
    mutable is_too_old : unit Lwt.t;
    job : unit -> 'a state Lwt.t;
    mutable result : 'a Lwt.t;
    restart_delay : unit -> unit Lwt.t;
    mutable all_consensus_hooks : ('a -> unit) list;
    mutable next_consensus_hooks : ('a -> unit) list;
  }

  let create ~name ~age_limit ~job ~restart_delay =
    {
      name;
      age_limit;
      state = Need_more_candidates;
      is_too_old = Lwt.return ();
      job;
      result = Lwt.fail Lwt.Canceled (* only for initialization *);
      restart_delay;
      all_consensus_hooks = [];
      next_consensus_hooks = [];
    }

  let rec loop t () =
    t.job ()
    >>= fun res ->
    t.is_too_old <- t.age_limit () ;
    match res with
    | Need_more_candidates | No_consensus _ ->
        t.restart_delay () >>= fun () -> loop t ()
    | Consensus data ->
        (* We call [List.rev] to ensure hooks are called in the same
          order they were registered. *)
        List.iter (fun hook -> hook data) (List.rev t.next_consensus_hooks) ;
        t.next_consensus_hooks <- [] ;
        List.iter (fun hook -> hook data) t.all_consensus_hooks ;
        Lwt.return data

  let wait t =
    match Lwt.state t.is_too_old with
    | Lwt.Sleep ->
        Lwt.protected t.result
    (* Lwt.Fail is not supposed to happen except on cancellation *)
    | Lwt.Fail exn ->
        t.result <- Lwt.fail exn ;
        t.result
    | Lwt.Return () ->
        t.result <- loop t () ;
        t.is_too_old <- t.age_limit () ;
        Lwt.protected t.result

  let on_next_consensus t hook =
    match Lwt.state t.result with
    | Lwt.Return data ->
        hook data
    | _ ->
        t.next_consensus_hooks <- hook :: t.next_consensus_hooks

  let on_all_consensus t hook =
    t.all_consensus_hooks <- hook :: t.all_consensus_hooks ;
    match Lwt.state t.result with Lwt.Return data -> hook data | _ -> ()

  let cancel t = Lwt.cancel t.is_too_old ; Lwt.cancel t.result
end
