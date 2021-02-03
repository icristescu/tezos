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
   Invocation:   dune exec src/lib_shell/test/test.exe test "bootstrapper"
   Subject:      Test the bootstrapper
 *)

let genesis_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

(* Given a n generate a string of characters as an operation *)
let generate_operation (hash : Block_hash.t) n =
  let len = n mod 10 in
  let bytes = Bytes.create len in
  let v = ref 0 in
  for i = 0 to len - 1 do
    v := ((!v + n) mod 26) + 65 ;
    Bytes.set bytes i (Char.chr !v)
  done ;
  {Operation.shell = {branch = hash}; proto = bytes}

(* Given a level, generate a fixed number of operations (at most 7) *)
let generate_operations hash =
  let nb = Stdlib.Hashtbl.hash hash mod 7 in
  List.fold_left
    (fun ops _ -> generate_operation hash (Stdlib.Hashtbl.hash ops) :: ops)
    []
    (0 -- nb)

let generate_block_operations hash =
  let nb = Stdlib.Hashtbl.hash hash mod 5 in
  ( nb,
    List.fold_left (fun ops _ -> generate_operations hash :: ops) [] (0 -- nb)
  )

(* This chain produces block hashes which are constants. This way,
   given a hash, and knowing the ordering list of hashes, it is easy
   to reconstruct a header. *)
let forge_block ?(level = 0l) ?(predecessor = genesis_hash) () =
  let (validation_passes, operations) =
    generate_block_operations predecessor
  in
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map
         (fun ops -> Operation_list_hash.compute (List.map Operation.hash ops))
         operations)
  in
  {
    Block_header.shell =
      {
        timestamp = Time.Protocol.epoch;
        level;
        (* dummy *)
        proto_level = 0;
        (* dummy *)
        validation_passes;
        predecessor;
        operations_hash;
        fitness =
          [ Bytes.of_string @@ string_of_int @@ 0;
            Bytes.of_string @@ string_of_int @@ 12 ];
        context = Context_hash.zero;
      };
    protocol_data = Bytes.empty;
  }

let max_level = 100_000

module H = Stdlib.Hashtbl

(* Map a hash of the block at level n to the block at level (n -
   1). This way, given a hash we can reproduce a header in O(1). *)
let predecessor = H.create max_level

(* Populate predecessor. This function should run quickly. *)
let populate max_level =
  let current_predecessor = ref genesis_hash in
  for level = 1 to max_level do
    H.add predecessor level !current_predecessor ;
    let level = Int32.of_int level in
    let block = forge_block ~level ~predecessor:!current_predecessor () in
    current_predecessor := Block_header.hash block
  done

let _ = populate max_level

(* This function uses the predecessor relation to construct a block
   quickly. *)
let forge_cached_block ~level =
  let predecessor = H.find predecessor level in
  forge_block ~level:(Int32.of_int level) ~predecessor ()

let forge_cached_operations header =
  let level = header.Block_header.shell.level in
  let pred = H.find predecessor (Int32.to_int level) in
  snd @@ generate_block_operations pred

open Bootstrapper

(* The bootstrapper module may save files in a directory. We use a
   counter to avoid mixing up directories between two tests.These
   directoies are in `temp_dir_name` to ensure that they will be
   destroy by the OS. *)
let temp_directory =
  let counter = ref 0 in
  fun () ->
    incr counter ;
    Format.asprintf
      "%s/%s-%s-%d"
      (Filename.get_temp_dir_name ())
      "bootstrapped-alcotest"
      (string_of_int (Stdlib.Hashtbl.hash (Systime_os.now ())))
      !counter

let make_configuration ?(root_path = temp_directory ()) ?(range_size = 10)
    ?(parallel_ranges_header_fetched = 5)
    ?(parallel_ranges_operations_fetched = 2)
    ?(get_current_head = fun () -> Lwt.return (forge_cached_block ~level:1))
    ?(get_headers = fun ~target:_ ~from:_ ~upto:_ -> Lwt.return_none)
    ?(get_operations =
      fun headers ->
        Lwt.return_some (List.map (fun header -> (header, [[]])) headers))
    ?(validate = fun _ -> return_unit)
    ?(when_unable_to_fetch =
      fun () -> Lwt.return (Error_monad.error Error_monad.Canceled)) () =
  {
    root_path;
    temporary_path = temp_directory () ^ "tmp";
    range_size;
    parallel_ranges_header_fetched;
    parallel_ranges_operations_fetched;
    getters =
      {
        get_current_head;
        get_headers;
        get_operations;
        when_unable_to_fetch;
        validate;
      };
  }

let is_active b =
  match state b with
  | Active _ ->
      ()
  | Inactive _ ->
      Assert.fail_msg "Should be active"

let is_inactive b =
  match state b with
  | Inactive _ ->
      ()
  | Active _ ->
      Assert.fail_msg "Should be inactive"

let create_simple_configuration () =
  create (make_configuration ())
  >>=? fun bootstrapper -> is_inactive bootstrapper ; return_unit

let create_simple_configuration_and_wait () =
  create (make_configuration ())
  >>=? fun bootstrapper ->
  is_inactive bootstrapper ;
  wait bootstrapper >>=? fun () -> return_unit

let create_simple_configuration_and_cancel () =
  create (make_configuration ())
  >>=? fun bootstrapper ->
  is_inactive bootstrapper ;
  cancel bootstrapper ;
  is_inactive bootstrapper ;
  return_unit

let create_notify_and_cancel () =
  (* we want to check the job starts and does not terminate. This test
     relies on an internal behavior: When notified with a better
     target than the current one (default is None), the bootstrapper
     will call get_current_head *)
  let configuration =
    make_configuration ~get_current_head:(fun _ -> fst (Lwt.task ())) ()
  in
  create configuration
  >>=? fun bootstrapper ->
  let header = forge_block ~level:1l () in
  notify_target bootstrapper ~target:header ;
  is_active bootstrapper ;
  cancel bootstrapper ;
  is_inactive bootstrapper ;
  return_unit

let create_bad_configuration () =
  let configuration = make_configuration ~range_size:0 () in
  create configuration
  >>= function
  | Ok _ -> Assert.fail_msg "The test should fail" | Error _err -> return_unit

let create_bad_configuration_2 () =
  (* This root path is not accessible from a normal user. *)
  let configuration = make_configuration ~root_path:"/root/whatever" () in
  create configuration
  >>= function
  | Ok _ -> Assert.fail_msg "The test should fail" | Error _err -> return_unit

let create_notify_one_block_to_fetch () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:1) in
  let get_headers ~target:_ ~from ~upto =
    Assert.equal from 1l ;
    Assert.equal upto 2l ;
    Lwt.return
      (Some ((fun _ -> Lwt.return true), [forge_cached_block ~level:2]))
  in
  let configuration = make_configuration ~get_current_head ~get_headers () in
  create configuration
  >>=? fun bootstrapper ->
  let header = forge_cached_block ~level:2 in
  notify_target bootstrapper ~target:header ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error err ->
      Assert.fail_msg
        "The test should pass.@.%a@."
        Error_monad.pp_print_error
        err
  | Ok () ->
      return_unit

let unable_to_fetch_then_retry () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:1) in
  let cpt = ref 0 in
  let get_headers ~target:_ ~from ~upto =
    Assert.equal from 1l ;
    Assert.equal upto 2l ;
    if !cpt = 0 then Lwt.return_none
    else
      Lwt.return
        (Some ((fun _ -> Lwt.return true), [forge_cached_block ~level:2]))
  in
  let when_unable_to_fetch () = incr cpt ; return_unit in
  let configuration =
    make_configuration ~get_current_head ~get_headers ~when_unable_to_fetch ()
  in
  create configuration
  >>=? fun bootstrapper ->
  let header = forge_cached_block ~level:2 in
  notify_target bootstrapper ~target:header ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error err ->
      Assert.fail_msg
        "The test should pass.@.%a@."
        Error_monad.pp_print_error
        err
  | Ok () ->
      Assert.equal !cpt 1 ; return_unit

let bad_fetch_headers_function () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:1) in
  let get_headers ~target:_ ~from:_ ~upto:_ =
    Lwt.return
      (Some ((fun _ -> Lwt.return true), [forge_cached_block ~level:1]))
  in
  let configuration = make_configuration ~get_current_head ~get_headers () in
  create configuration
  >>=? fun bootstrapper ->
  let header = forge_cached_block ~level:10 in
  notify_target bootstrapper ~target:header ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error _err ->
      return_unit
  | Ok () ->
      Assert.fail_msg "The test should fail.@."

let get_headers ~target:_ ~from ~upto =
  let upto = Int32.to_int upto in
  let from = Int32.to_int from in
  Lwt.return
    (Some
       ( (fun _ -> Lwt.return true),
         List.fold_right
           (fun level l -> forge_cached_block ~level :: l)
           (from + 1 -- upto)
           [] ))

let get_operations headers =
  Lwt.return_some
    (List.map (fun header -> (header, forge_cached_operations header)) headers)

let fetch_headers_less_than_a_range () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:1) in
  let configuration = make_configuration ~get_current_head ~get_headers () in
  create configuration
  >>=? fun bootstrapper ->
  let header = forge_cached_block ~level:10 in
  notify_target bootstrapper ~target:header ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error err ->
      Assert.fail_msg
        "The test should pass.@.%a@."
        Error_monad.pp_print_error
        err
  | Ok () ->
      return_unit

let fetch_headers_split_one_range () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:1) in
  let configuration = make_configuration ~get_current_head ~get_headers () in
  create configuration
  >>=? fun bootstrapper ->
  let header = forge_cached_block ~level:11 in
  notify_target bootstrapper ~target:header ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error err ->
      Assert.fail_msg
        "The test should pass.@.%a@."
        Error_monad.pp_print_error
        err
  | Ok () ->
      return_unit

let fetch_headers_exactly_one_range () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:10) in
  let configuration = make_configuration ~get_current_head ~get_headers () in
  create configuration
  >>=? fun bootstrapper ->
  let target = forge_cached_block ~level:20 in
  notify_target bootstrapper ~target ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error _err ->
      Assert.fail_msg "The test should pass.@."
  | Ok () ->
      return_unit

let fetch_headers_several_ranges () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:10) in
  let block_validated = ref 0 in
  let validate _ = incr block_validated ; return_unit in
  let configuration =
    make_configuration ~get_current_head ~get_headers ~validate ()
  in
  create configuration
  >>=? fun bootstrapper ->
  let target = forge_cached_block ~level:2000 in
  notify_target bootstrapper ~target ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error _err ->
      Assert.fail_msg "The test should pass.@."
  | Ok () ->
      Assert.equal !block_validated (2000 - 10) ;
      return_unit

let fetch_blocks_several_ranges () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:10) in
  let block_validated = ref 0 in
  let validate (header, ops) =
    let level = Int32.to_int header.Block_header.shell.level in
    let got_hash = Block_header.hash header in
    let expected_hash =
      (forge_cached_block ~level:(level + 1)).shell.predecessor
    in
    let pred_hash = H.find predecessor level in
    let (_, expected_ops) = generate_block_operations pred_hash in
    Assert.equal got_hash expected_hash ;
    Assert.equal ops expected_ops ;
    incr block_validated ;
    return_unit
  in
  let configuration =
    make_configuration
      ~get_current_head
      ~get_headers
      ~get_operations
      ~validate
      ()
  in
  create configuration
  >>=? fun bootstrapper ->
  let target = forge_cached_block ~level:2000 in
  notify_target bootstrapper ~target ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error _err ->
      Assert.fail_msg "The test should pass.@."
  | Ok () ->
      Assert.equal !block_validated (2000 - 10) ;
      return_unit

let empty_range () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:5) in
  let configuration =
    make_configuration
      ~get_current_head
      ~get_headers:(fun ~target:_ ~from:_ ~upto:_ ->
        Lwt.fail_with "Should not be called")
      ()
  in
  create configuration
  >>=? fun bootstrapper ->
  let target = forge_cached_block ~level:4 in
  notify_target bootstrapper ~target ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error _err ->
      Assert.fail_msg "The test should pass.@."
  | Ok () ->
      return_unit

let more_tasks_than_ranges () =
  let get_current_head () = Lwt.return (forge_cached_block ~level:100) in
  let configuration =
    make_configuration
      ~parallel_ranges_header_fetched:4
      ~parallel_ranges_operations_fetched:4
      ~range_size:50
      ~get_current_head
      ~get_headers
      ~when_unable_to_fetch:(fun () -> Lwt.fail_with "Should not be called")
      ()
  in
  create configuration
  >>=? fun bootstrapper ->
  let target = forge_cached_block ~level:200 in
  notify_target bootstrapper ~target ;
  is_active bootstrapper ;
  wait bootstrapper
  >>= function
  | Error _err ->
      Assert.fail_msg "The test should pass.@."
  | Ok () ->
      return_unit

let wrap f _switch () =
  f ()
  >>= function
  | Error err ->
      Assert.fail_msg "Test failed: %a@." Error_monad.pp_print_error err
  | Ok _ ->
      Lwt.return_unit

let tests_raw : (string * (Lwt_switch.t -> unit -> unit Lwt.t)) list =
  [ ("create simple configuration", wrap create_simple_configuration);
    ("create and wait", wrap create_simple_configuration_and_wait);
    ("create and cancel", wrap create_simple_configuration_and_cancel);
    ("create, notify and cancel", wrap create_notify_and_cancel);
    ("create bad configuration (bad range size)", wrap create_bad_configuration);
    ( "create bad configuration (bad root path)",
      wrap create_bad_configuration_2 );
    ("create and notify one block", wrap create_notify_one_block_to_fetch);
    ("unable to fetch then retry", wrap unable_to_fetch_then_retry);
    ("wrong fetch header function", wrap bad_fetch_headers_function);
    ("fetch headers", wrap fetch_headers_less_than_a_range);
    ("Split one range", wrap fetch_headers_split_one_range);
    ("empty range", wrap empty_range);
    ("more tasks than ranges", wrap more_tasks_than_ranges);
    ("fetch exactly one range", wrap fetch_headers_exactly_one_range);
    ("fetch several ranges", wrap fetch_headers_several_ranges);
    ("fetch blocks ranges", wrap fetch_blocks_several_ranges) ]

let tests =
  List.map (fun (s, f) -> Alcotest_lwt.test_case s `Quick f) tests_raw
