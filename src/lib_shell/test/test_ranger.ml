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
   Invocation:   dune exec src/lib_shell/test/test.exe test "ranger"
   Subject:      Test the ranger module
 *)

module Range = Ranger.Int32
module Ranger = Ranger.Make (Range)

let make_parameters ?(tasks_in_parallel = 1) ?(starts_with = Lwt.return_unit)
    ?(max_range_size = 10) ?(reverse = false)
    ?(parallel_task = fun _ -> return_some ())
    ?(sequential_task = fun _ _ _ -> return_some ())
    ?(recovery_task = fun _ -> fail Error_monad.Canceled)
    ?(range = Range.{from = 0l; upto = 0l})
    ?(filter = fun _ -> Lwt.return_none) () =
  Ranger.E
    {
      tasks_in_parallel;
      starts_with;
      max_range_size;
      reverse;
      parallel_task;
      sequential_task;
      recovery_task;
      range;
      filter;
    }

let empty_range () =
  Ranger.wait (Ranger.create (make_parameters ())) >>=? fun () -> return_unit

let empty_range_is_noop () =
  Ranger.wait
    (Ranger.create
       (make_parameters
          ~parallel_task:(fun _ -> Lwt.fail_with "should not be executed")
          ()))
  >>=? fun () -> return_unit

let empty_range_is_noop_2 () =
  Ranger.wait
    (Ranger.create
       (make_parameters
          ~parallel_task:(fun _ -> Lwt.fail_with "should not be executed")
          ~reverse:true
          ()))
  >>=? fun () -> return_unit

let empty_range_is_noop_3 () =
  Ranger.wait
    (Ranger.create
       (make_parameters
          ~sequential_task:(fun _ _ _ ->
            Lwt.fail_with "should not be executed")
          ()))
  >>=? fun () -> return_unit

let max_range_size_is_zero () =
  let p =
    Ranger.wait
      (Ranger.create
         (make_parameters ~max_range_size:0 ~range:{from = 0l; upto = 10l} ()))
  in
  match Lwt.state p with
  | Fail (Invalid_argument _) ->
      return_unit
  | _ ->
      Assert.fail_msg "Expected an invalid argument error"

let max_range_size_is_non_positive () =
  let p =
    Ranger.wait
      (Ranger.create
         (make_parameters
            ~max_range_size:(-5)
            ~range:{from = 0l; upto = 10l}
            ()))
  in
  match Lwt.state p with
  | Fail (Invalid_argument _) ->
      return_unit
  | _ ->
      Assert.fail_msg "Expected an invalid argument error"

let task_in_parallel_is_zero () =
  let p =
    Ranger.wait (Ranger.create (make_parameters ~tasks_in_parallel:0 ()))
  in
  match Lwt.state p with
  | Fail (Invalid_argument _) ->
      return_unit
  | _ ->
      Assert.fail_msg "Expected an invalid argument error"

let task_in_parallel_is_non_positive () =
  let p =
    Ranger.wait (Ranger.create (make_parameters ~tasks_in_parallel:(-5) ()))
  in
  match Lwt.state p with
  | Fail (Invalid_argument _) ->
      return_unit
  | _ ->
      Assert.fail_msg "Expected an invalid argument error"

let starts_with_rejected_empty_sequence () =
  Ranger.wait
    (Ranger.create (make_parameters ~starts_with:(Lwt.fail_with "oops") ()))
  >>=? fun () -> return_unit

let starts_with_rejected_non_empty_sequence () =
  let exception Oops in
  let p =
    Ranger.wait
      (Ranger.create
         (make_parameters
            ~starts_with:(Lwt.fail Oops)
            ~range:{from = 0l; upto = 10l}
            ()))
  in
  Lwt.on_termination p (fun () ->
      match Lwt.state p with
      | Fail Oops ->
          ()
      | _ ->
          Assert.fail_msg "Expected to fail with Oops exception") ;
  return_unit

let task_protected () =
  let (t, u) = Lwt.task () in
  let task =
    Ranger.create
      (make_parameters ~starts_with:t ~range:{from = 0l; upto = 10l} ())
  in
  let p = Ranger.wait task in
  Assert.equal (Lwt.state p) Lwt.Sleep ;
  Lwt.cancel p ;
  Lwt.wakeup u () ;
  let p' = Ranger.wait task in
  Assert.equal (Lwt.state p') (Lwt.Return (Ok ())) ;
  return_unit

let task_canceled () =
  let (t, _) = Lwt.task () in
  let task =
    Ranger.create
      (make_parameters ~starts_with:t ~range:{from = 0l; upto = 10l} ())
  in
  let p = Ranger.wait task in
  Assert.equal (Lwt.state p) Lwt.Sleep ;
  Ranger.cancel task ;
  Assert.equal (Lwt.state p) (Lwt.Fail Lwt.Canceled) ;
  return_unit

let task_canceled_two_tasks () =
  let (t, _) = Lwt.task () in
  let task =
    Ranger.create
      (make_parameters
         ~tasks_in_parallel:2
         ~starts_with:t
         ~range:{from = 0l; upto = 10l}
         ())
  in
  let p = Ranger.wait task in
  Assert.equal (Lwt.state p) Lwt.Sleep ;
  Ranger.cancel task ;
  Assert.equal (Lwt.state p) (Lwt.Fail Lwt.Canceled) ;
  return_unit

let task_with_one_range () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let parameters =
    make_parameters
      ~starts_with:Lwt.return_unit
      ~range:{from = 0l; upto = 10l}
      ~max_range_size:10
      ~parallel_task:(fun Range.{from; upto} ->
        Assert.equal from 0l ;
        Assert.equal upto 10l ;
        incr parallel_task ;
        Lwt.return (Ok (Some ())))
      ~sequential_task:(fun Range.{from; upto} _ _ ->
        Assert.equal from 0l ;
        Assert.equal upto 10l ;
        incr sequential_task ;
        Lwt.return (Ok (Some ())))
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task 1 ;
  Assert.equal !sequential_task 1 ;
  return_unit

let task_with_splitted_range () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let parameters =
    make_parameters
      ~starts_with:Lwt.return_unit
      ~range:{from = 5l; upto = 15l}
      ~max_range_size:10
      ~parallel_task:(fun Range.{from; upto} ->
        Assert.is_true (from = 5l || from = 10l) ;
        Assert.is_true (upto = 10l || upto = 15l) ;
        incr parallel_task ;
        Lwt.return (Ok (Some ())))
      ~sequential_task:(fun Range.{from; upto} _ _ ->
        Assert.is_true (from = 5l || from = 10l) ;
        Assert.is_true (upto = 10l || upto = 15l) ;
        incr sequential_task ;
        Lwt.return (Ok (Some ())))
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task 2 ;
  Assert.equal !sequential_task 2 ;
  return_unit

let task_with_recovery () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let recovery_task = ref 0 in
  let parameters =
    make_parameters
      ~starts_with:Lwt.return_unit
      ~range:{from = 5l; upto = 15l}
      ~max_range_size:10
      ~parallel_task:(fun _ ->
        incr parallel_task ;
        if !parallel_task = 1 then Lwt.return (Ok None)
        else Lwt.return (Ok (Some ())))
      ~sequential_task:(fun _ _ _ ->
        incr sequential_task ; Lwt.return (Ok (Some ())))
      ~recovery_task:(fun _ -> incr recovery_task ; return_unit)
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task 3 ;
  Assert.equal !sequential_task 2 ;
  Assert.equal !recovery_task 1 ;
  return_unit

let task_with_recovery_2 () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let recovery_task = ref 0 in
  let parameters =
    make_parameters
      ~starts_with:Lwt.return_unit
      ~range:{from = 5l; upto = 15l}
      ~max_range_size:10
      ~parallel_task:(fun _ ->
        incr parallel_task ;
        if !parallel_task = 1 then Lwt.return (Ok None)
        else Lwt.return (Ok (Some ())))
      ~sequential_task:(fun _ _ _ ->
        incr sequential_task ;
        if !sequential_task = 1 then Lwt.return (Ok None)
        else Lwt.return (Ok (Some ())))
      ~recovery_task:(fun _ -> incr recovery_task ; return_unit)
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task 4 ;
  Assert.equal !sequential_task 3 ;
  Assert.equal !recovery_task 2 ;
  return_unit

let task_with_recovery_3 () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let recovery_task = ref 0 in
  let parameters =
    make_parameters
      ~tasks_in_parallel:2
      ~starts_with:Lwt.return_unit
      ~range:{from = 5l; upto = 15l}
      ~max_range_size:10
      ~parallel_task:(fun _ ->
        incr parallel_task ;
        if !parallel_task = 1 then Lwt.return (Ok None)
        else Lwt.return (Ok (Some ())))
      ~sequential_task:(fun _ _ _ ->
        incr sequential_task ;
        if !sequential_task = 1 then Lwt.return (Ok None)
        else Lwt.return (Ok (Some ())))
      ~recovery_task:(fun _ -> incr recovery_task ; return_unit)
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task 4 ;
  Assert.equal !sequential_task 3 ;
  Assert.equal !recovery_task 2 ;
  return_unit

let task_with_splitted_ranges () =
  List.iter_es
    (fun i ->
      let parallel_task = ref 0 in
      let sequential_task = ref 0 in
      let parameters =
        make_parameters
          ~tasks_in_parallel:i
          ~starts_with:Lwt.return_unit
          ~range:{from = 5l; upto = 100l}
          ~max_range_size:10
          ~parallel_task:(fun _ ->
            incr parallel_task ; Lwt.return (Ok (Some ())))
          ~sequential_task:(fun _ _ _ ->
            incr sequential_task ; Lwt.return (Ok (Some ())))
          ()
      in
      Ranger.wait (Ranger.create parameters)
      >>=? fun () ->
      Assert.equal !parallel_task (((100 - 5) / 10) + 1) ;
      Assert.equal !sequential_task (((100 - 5) / 10) + 1) ;
      return_unit)
    (1 -- 10)

let task_with_splitted_ranges_2 () =
  (* This test check differente splits of the range [5;100]. For each
     split, we check that the parallel_task and sequential_taks are
     called in correct order and correct values. *)
  List.iter_es
    (fun reverse ->
      List.iter_es
        (fun max_range_size ->
          let parallel_task = ref 0 in
          let sequential_task = ref 0 in
          let current_from = ref (if reverse then 101l else 4l) in
          let current_upto = ref (if reverse then 101l else 4l) in
          let check a b = if reverse then a > b else a < b in
          let parameters =
            make_parameters
              ~reverse
              ~starts_with:Lwt.return_unit
              ~range:{from = 5l; upto = 100l}
              ~max_range_size
              ~parallel_task:(fun Range.{from; upto} ->
                Assert.is_true
                  ( Int32.rem from (Int32.of_int max_range_size) = 0l
                  || from = 5l ) ;
                Assert.is_true
                  ( Int32.rem upto (Int32.of_int max_range_size) = 0l
                  || upto = 100l ) ;
                Assert.is_true (check !current_from from) ;
                Assert.is_true (check !current_upto upto) ;
                current_from := from ;
                current_upto := upto ;
                incr parallel_task ;
                Lwt.return (Ok (Some ())))
              ~sequential_task:(fun Range.{from; upto} _ _ ->
                Assert.equal !current_from from ;
                Assert.equal !current_upto upto ;
                incr sequential_task ;
                Lwt.return (Ok (Some ())))
              ()
          in
          Ranger.wait (Ranger.create parameters)
          >>=? fun () ->
          let leftovers =
            (if 5 mod max_range_size <> 0 then 1 else 0)
            + if 100 mod max_range_size <> 0 then 1 else 0
          in
          if reverse then Assert.equal !current_from 5l
          else Assert.equal !current_upto 100l ;
          Assert.equal !parallel_task (((100 - 5) / max_range_size) + leftovers) ;
          Assert.equal
            !sequential_task
            (((100 - 5) / max_range_size) + leftovers) ;
          return_unit)
        [2; 3; 6; 7; 11])
    [true; false]

let parallel_tasks () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let (t, u) = Lwt.task () in
  let parameters =
    make_parameters
      ~tasks_in_parallel:5
      ~starts_with:Lwt.return_unit
      ~range:{from = 5l; upto = 100l}
      ~max_range_size:10
      ~parallel_task:(fun _ ->
        incr parallel_task ;
        if !parallel_task = 5 then Lwt.wakeup_later u (Ok (Some ())) ;
        t)
      ~sequential_task:(fun _ _ _ ->
        incr sequential_task ; Lwt.return (Ok (Some ())))
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task (((100 - 5) / 10) + 1) ;
  Assert.equal !sequential_task (((100 - 5) / 10) + 1) ;
  return_unit

let sequential_tasks () =
  let parallel_task = ref 0 in
  let sequential_task = ref 0 in
  let running_sequential_task = ref 0 in
  let (t, u) = Lwt.task () in
  let parameters =
    make_parameters
      ~tasks_in_parallel:5
      ~starts_with:Lwt.return_unit
      ~range:{from = 5l; upto = 100l}
      ~max_range_size:10
      ~parallel_task:(fun _ ->
        incr parallel_task ;
        if !parallel_task = 5 then Lwt.wakeup_later u (Ok (Some ())) ;
        t)
      ~sequential_task:(fun _ _ _ ->
        Assert.equal !running_sequential_task 0 ;
        incr sequential_task ;
        incr running_sequential_task ;
        Lwt_unix.sleep 0.01
        >>= fun () ->
        decr running_sequential_task ;
        Lwt.return (Ok (Some ())))
      ()
  in
  Ranger.wait (Ranger.create parameters)
  >>=? fun () ->
  Assert.equal !parallel_task (((100 - 5) / 10) + 1) ;
  Assert.equal !sequential_task (((100 - 5) / 10) + 1) ;
  return_unit

let check_linking () =
  let sequential_task = ref 0 in
  let parameters =
    Ranger.E
      {
        tasks_in_parallel = 1;
        starts_with = Lwt.return 0;
        max_range_size = 10;
        reverse = false;
        parallel_task = (fun _ -> Lwt.return (Ok (Some ())));
        sequential_task =
          (fun _ link _ ->
            Assert.equal !sequential_task link ;
            incr sequential_task ;
            Lwt.return (Ok (Some (link + 1))));
        recovery_task = (fun _ -> return_unit);
        range = {from = 10l; upto = 100l};
        filter = (fun _ -> Lwt.return_none);
      }
  in
  Ranger.wait (Ranger.create parameters) >>=? fun () -> return_unit

let check_filtering () =
  let parameters =
    Ranger.E
      {
        tasks_in_parallel = 1;
        starts_with = Lwt.return_unit;
        max_range_size = 10;
        reverse = false;
        parallel_task = (fun _ -> Lwt.fail_with "Should not be executed");
        sequential_task = (fun _ _ _ -> Lwt.fail_with "Should not be executed");
        recovery_task = (fun _ -> Lwt.fail_with "Should not be executed");
        range = {from = 10l; upto = 100l};
        filter = (fun _ -> Lwt.return_some ());
      }
  in
  Ranger.wait (Ranger.create parameters) >>=? fun () -> return_unit

let check_filtering_2 () =
  let parameters =
    Ranger.E
      {
        tasks_in_parallel = 2;
        starts_with = Lwt.return_unit;
        max_range_size = 3;
        reverse = false;
        parallel_task =
          (fun r ->
            if Int32.to_int r.from mod 2 = 0 then return_some ()
            else Lwt.fail_with "Should not be executed");
        sequential_task =
          (fun r _ _ ->
            if Int32.to_int r.from mod 2 = 0 then return_some ()
            else Lwt.fail_with "Should not be executed");
        recovery_task = (fun _ -> Lwt.fail_with "Should not be executed");
        range = {from = 10l; upto = 100l};
        filter =
          (fun r ->
            if Int32.to_int r.from mod 2 = 0 then Lwt.return_none
            else Lwt.return_some ());
      }
  in
  Ranger.wait (Ranger.create parameters) >>=? fun () -> return_unit

let check_filtering_with_linking () =
  let parameters =
    Ranger.E
      {
        tasks_in_parallel = 2;
        starts_with = Lwt.return 10;
        max_range_size = 10;
        reverse = false;
        parallel_task =
          (fun r ->
            if Int32.to_int r.from mod 3 = 0 then return_some ()
            else Lwt.fail_with "Should not be executed");
        sequential_task =
          (fun r trigger _ ->
            Assert.equal trigger (Int32.to_int r.from) ;
            if Int32.to_int r.from mod 3 = 0 then return_some (trigger + 10)
            else Lwt.fail_with "Should not be executed");
        recovery_task = (fun _ -> Lwt.fail_with "Should not be executed");
        range = {from = 10l; upto = 100l};
        filter =
          (fun r ->
            if Int32.to_int r.from mod 3 = 0 then Lwt.return_none
            else Lwt.return_some (Int32.to_int r.from + 10));
      }
  in
  Ranger.wait (Ranger.create parameters) >>=? fun () -> return_unit

let wrap f _switch () =
  f ()
  >>= function
  | Error errs ->
      Assert.fail_msg "Test failed: %a@." Error_monad.pp_print_error errs
  | Ok _ ->
      Lwt.return_unit

let tests_raw : (string * (Lwt_switch.t -> unit -> unit Lwt.t)) list =
  [ ("empty range", wrap empty_range);
    ("empty range is noop", wrap empty_range_is_noop);
    ("empty range is noop 2", wrap empty_range_is_noop_2);
    ("empty range is noop 3", wrap empty_range_is_noop_3);
    ("max range size is 0", wrap max_range_size_is_zero);
    ("max range size is non positive", wrap max_range_size_is_non_positive);
    ("task in parallel is 0", wrap task_in_parallel_is_zero);
    ("task in parallel is non positive", wrap task_in_parallel_is_non_positive);
    ( "starts_with rejected on empty sequence",
      wrap starts_with_rejected_empty_sequence );
    ( "starts_with rejected on non empty sequence",
      wrap starts_with_rejected_non_empty_sequence );
    ("task protected", wrap task_protected);
    ("task canceled", wrap task_canceled);
    ("task canceled two tasks", wrap task_canceled_two_tasks);
    ("task with one range", wrap task_with_one_range);
    ("task with one splitted range", wrap task_with_splitted_range);
    ("task with splitted ranges", wrap task_with_splitted_ranges);
    ("task with splitted ranges 2", wrap task_with_splitted_ranges_2);
    ("task with recovery", wrap task_with_recovery);
    ("task with recovery 2", wrap task_with_recovery_2);
    ("task with recovery 3", wrap task_with_recovery_3);
    ("parallel tasks", wrap parallel_tasks);
    ("sequential tasks", wrap sequential_tasks);
    ("check linking", wrap check_linking);
    ("check filtering", wrap check_filtering);
    ("check filtering 2", wrap check_filtering_2);
    ("check filtering with linking", wrap check_filtering_with_linking) ]

let tests =
  List.map (fun (s, f) -> Alcotest_lwt.test_case s `Quick f) tests_raw
