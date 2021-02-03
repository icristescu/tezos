(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Shell
   Invocation:   dune exec src/lib_shell/test/test.exe test "consensus heuristic"
   Subject:      Test the hash heuristic
 *)

open Consensus_heuristic

let prn = function
  | Consensus hash ->
      Format.asprintf "Consensus with %a" Block_hash.pp hash
  | No_consensus _ ->
      "No consensus"
  | Need_more_candidates ->
      "Synchronised (not stuck)"

let forge_peer_id () =
  let identity = P2p_identity.generate_with_pow_target_0 () in
  identity.peer_id

let forge_hash () =
  Block_hash.hash_bytes [Bytes.init 32 (fun _ -> Char.chr (Random.int 256))]

let create_state_init_1 () =
  let h = create ~expected:1 ~threshold:1 () in
  Assert.equal (get_state h) Need_more_candidates

let create_state_bad_threshold () =
  try
    let _ = create ~expected:1 ~threshold:(-1) () in
    Assert.fail_msg ~expected:"Invalid_argument" ""
  with Invalid_argument _ -> ()

let create_state_bad_expected () =
  try
    let _ = create ~expected:0 ~threshold:1 () in
    Assert.fail_msg ~expected:"Invalid_argument" ""
  with Invalid_argument _ -> ()

let create_consensus () =
  let h = create ~expected:1 ~threshold:1 () in
  let peer_id = forge_peer_id () in
  let zero = Block_hash.zero in
  update h (peer_id, zero) ;
  Assert.equal ~prn (get_state h) (Consensus zero)

let create_no_consensus () =
  let h = create ~expected:2 ~threshold:2 () in
  let peer_1 = forge_peer_id () in
  let peer_2 = forge_peer_id () in
  let hash_1 = Block_hash.zero in
  let hash_2 = forge_hash () in
  update h (peer_1, hash_1) ;
  update h (peer_2, hash_2) ;
  Assert.equal
    ~prn
    (get_state h)
    (No_consensus (List.sort Block_hash.compare [hash_1; hash_2]))

let tests_raw : (string * (unit -> unit)) list =
  [ ("create_state_init_1", create_state_init_1);
    ("create_state_bad_threshold", create_state_bad_threshold);
    ("create_state_bad_expected", create_state_bad_expected);
    ("create_consensus", create_consensus);
    ("create_no_consensus", create_no_consensus) ]

let prn = function
  | Lwt.Fail exn ->
      Format.asprintf "fail: %s@." (Printexc.to_string exn)
  | Lwt.Sleep ->
      "sleep"
  | Lwt.Return _ ->
      "return"

let job_failed () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.return_unit)
      ~job:(fun () -> Lwt.fail_with "failed")
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail_with "failed"))

let job_canceled () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.return_unit)
      ~job:(fun () -> Lwt.fail Lwt.Canceled)
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let job_return_hash () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.return_unit)
      ~job:(fun () -> Lwt.return (Consensus Block_hash.zero))
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.return Block_hash.zero))

let job_return_hash_2 () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.fail Lwt.Canceled)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> Lwt.return (Consensus Block_hash.zero))
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.return Block_hash.zero)) ;
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let job_return_need_more_hashes () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> Lwt.return Need_more_candidates)
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let job_return_no_consensus () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> Lwt.return (No_consensus []))
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let job_return_no_consensus_2 () =
  let restart_delay = ref false in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () ->
        restart_delay := true ;
        Lwt.return_unit)
      ~job:(fun () ->
        if not !restart_delay then Lwt.return (No_consensus [])
        else Lwt.return (Consensus Block_hash.zero))
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.return Block_hash.zero))

let job_return_no_consensus_3 () =
  let restart_delay = ref false in
  let age_limit = ref false in
  let hash_1 = Block_hash.zero in
  let hash_2 = forge_hash () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () ->
        if !restart_delay then age_limit := true ;
        Lwt.return_unit)
      ~restart_delay:(fun () ->
        restart_delay := true ;
        Lwt.return_unit)
      ~job:(fun () ->
        if not !restart_delay then Lwt.return (No_consensus [])
        else if not !age_limit then Lwt.return (Consensus hash_1)
        else Lwt.return (Consensus hash_2))
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.return hash_1)) ;
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.return hash_2))

let job_sleep () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> fst @@ Lwt.task ())
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) Lwt.Sleep ;
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) Lwt.Sleep

let job_sleep_2 () =
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.fail Lwt.Canceled)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> fst @@ Lwt.task ())
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) Lwt.Sleep ;
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let job_protected () =
  let (t, u) = Lwt.task () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> t)
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) Lwt.Sleep ;
  Lwt.cancel p ;
  Lwt.wakeup u (Consensus Block_hash.zero) ;
  let p' = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p') (Lwt.state (Lwt.return Block_hash.zero))

let worker_canceled () =
  let (t, _) = Lwt.task () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> Lwt.return_unit)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> t)
  in
  let p = Worker.wait worker in
  Assert.equal ~prn (Lwt.state p) Lwt.Sleep ;
  Worker.cancel worker ;
  Assert.equal ~prn (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let tests_worker_raw : (string * (unit -> unit)) list =
  [ ("job_fail", job_failed);
    ("job_canceled", job_canceled);
    ("job_return_hash", job_return_hash);
    ("job_return_hash_2", job_return_hash_2);
    ("job_return_need_more_hashes", job_return_need_more_hashes);
    ("job_return_no_consensus", job_return_no_consensus);
    ("job_return_no_consensus_2", job_return_no_consensus_2);
    ("job_return_no_consensus_3", job_return_no_consensus_3);
    ("job_sleep", job_sleep);
    ("job_sleep_2", job_sleep_2);
    ("job_protected", job_protected);
    ("worker_canceled", job_canceled) ]

let job_age_limit_twice () =
  let cpt = ref 0 in
  let condition = Lwt_condition.create () in
  let p = Lwt_condition.wait condition in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> p)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () ->
        incr cpt ;
        Lwt_condition.broadcast condition () ;
        Lwt.return (Consensus ()))
  in
  Worker.wait worker
  >>= fun () ->
  Assert.equal !cpt 1 ;
  p
  >>= fun () ->
  Worker.wait worker >>= fun () -> Assert.equal !cpt 2 ; Lwt.return_unit

let job_on_next_consensus_1 () =
  let cpt = ref 0 in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> fst (Lwt.wait ()))
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_next_consensus worker (fun () -> incr cpt) ;
  Worker.wait worker >>= fun () -> Assert.equal !cpt 1 ; Lwt.return_unit

let job_on_next_consensus_2 () =
  let cpt = ref 0 in
  let (t, u) = Lwt.task () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> fst (Lwt.wait ()))
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> t >>= fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_next_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  p >>= fun () -> Assert.equal !cpt 1 ; Lwt.return_unit

let job_on_all_consensus_1 () =
  let cpt = ref 0 in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> fst (Lwt.wait ()))
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_all_consensus worker (fun () -> incr cpt) ;
  Worker.wait worker >>= fun () -> Assert.equal !cpt 1 ; Lwt.return_unit

let job_on_all_consensus_2 () =
  let cpt = ref 0 in
  let (t, u) = Lwt.task () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> fst (Lwt.wait ()))
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> t >>= fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_all_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  p >>= fun () -> Assert.equal !cpt 1 ; Lwt.return_unit

let job_on_all_consensus_3 () =
  let cpt = ref 0 in
  let (t, u) = Lwt.task () in
  let (ta, ua) = Lwt.task () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> ta)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> t >>= fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_all_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  p
  >>= fun () ->
  Assert.equal !cpt 1 ;
  Lwt.wakeup_later ua () ;
  Worker.wait worker >>= fun () -> Assert.equal !cpt 2 ; Lwt.return_unit

let job_on_next_consensus_3 () =
  let cpt = ref 0 in
  let (t, u) = Lwt.task () in
  let (ta, ua) = Lwt.task () in
  let worker =
    Worker.create
      ~name:"job"
      ~age_limit:(fun () -> ta)
      ~restart_delay:(fun () -> Lwt.fail Lwt.Canceled)
      ~job:(fun () -> t >>= fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_next_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  p
  >>= fun () ->
  Assert.equal !cpt 1 ;
  Lwt.wakeup_later ua () ;
  Worker.wait worker >>= fun () -> Assert.equal !cpt 1 ; Lwt.return_unit

let wrap f _switch () = f ()

let tests_worker_lwt_raw : (string * (Lwt_switch.t -> unit -> unit Lwt.t)) list
    =
  [ ("job age limit twice", wrap job_age_limit_twice);
    ("job_on_next_consensus_1", wrap job_on_next_consensus_1);
    ("job_on_next_consensus_2", wrap job_on_next_consensus_2);
    ("job_on_next_consensus_3", wrap job_on_next_consensus_3);
    ("job_on_all_consensus_1", wrap job_on_all_consensus_1);
    ("job_on_all_consensus_2", wrap job_on_all_consensus_2);
    ("job_on_all_consensus_3", wrap job_on_all_consensus_3) ]

let tests =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case_sync s `Quick f)
    (tests_raw @ tests_worker_raw)

let tests_lwt =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case s `Quick f)
    tests_worker_lwt_raw
