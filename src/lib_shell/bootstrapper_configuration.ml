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

let root_path parameters = Filename.concat parameters.data_dir "bootstrap"

let get_current_head parameters () =
  State.read_chain_data parameters.chain_state (fun _ chain_data ->
      Lwt.return chain_data.State.current_head)
  >>= fun header -> Lwt.return (State.Block.header header)

let validate parameters (header, operations) =
  Block_validator.validate
    ~notify_new_block:parameters.notify_new_block
    parameters.block_validator
    parameters.chain_db
    (Block_header.hash header)
    header
    operations

(* The function Distributed_db.Operations.fetch ensures that a peer is
   punished if it sens inconsistent data. *)
let get_operations limits parameters headers =
  let request peer header =
    let hash = Block_header.hash header in
    List.map_ep
      (fun i ->
        Distributed_db.Operations.fetch
          ~timeout:limits.fetching_operations_timeout
          ~peer
          parameters.chain_db
          (hash, i)
          header.Block_header.shell.operations_hash)
      (0 -- (header.shell.validation_passes - 1))
    >>= function
    | Error _err ->
        (* The error can only be a timeout *)
        Lwt.return_none
    | Ok ops ->
        Lwt.return_some ops
  in
  let handle_opt =
    Distributed_db.Request_worker.worker
      parameters.operations_requester
      request
      headers
  in
  match handle_opt with
  | None ->
      Lwt.return_none
  | Some handle -> (
      Distributed_db.Request_worker.wait handle
      >>= function
      | None ->
          Lwt.return_none
      | Some opss -> (
          assert (List.length headers = List.length opss) ;
          let exception Not_supposed_to_happen_but_lets_make_it_type in
          List.map2
            ~when_different_lengths:
              (Error Not_supposed_to_happen_but_lets_make_it_type)
            (fun header ops -> (header, ops))
            headers
            opss
          |> function
          | Ok blocks -> Lwt.return_some blocks | Error _ -> Lwt.return_none )
      )

let int32_range from upto =
  let rec range from upto acc =
    if upto < from then acc else range from (Int32.sub upto 1l) (upto :: acc)
  in
  range from upto []

let get_headers limits parameters ~target ~from ~upto =
  let request peer level =
    let offset = Int32.sub target.Block_header.shell.level level in
    Distributed_db.Request.get_predecessor_header
      ~timeout:limits.fetching_headers_timeout
      parameters.chain_db
      ~peer
      (Block_header.hash target)
      offset
  in
  let handle_opt =
    Distributed_db.Request_worker.worker
      parameters.header_requester
      request
      (int32_range (Int32.add from 1l) upto)
  in
  match handle_opt with
  | None ->
      Lwt.return_none
  | Some handle -> (
      Distributed_db.Request_worker.wait handle
      >>= function
      | None ->
          Lwt.return_none
      | Some headers ->
          let consistency_checker target =
            let current_target = ref target in
            let consistent =
              List.for_all
                (fun block ->
                  let consistent = Block_header.hash block = !current_target in
                  current_target := block.Block_header.shell.predecessor ;
                  consistent)
                (List.rev headers)
            in
            if consistent then Lwt.return true
            else
              Distributed_db.Request_worker.punish handle
              >>= fun () -> Lwt.return false
          in
          Lwt.return_some (consistency_checker, headers) )

let getters limits parameters =
  {
    Bootstrapper.get_current_head = get_current_head parameters;
    get_headers = get_headers limits parameters;
    get_operations = get_operations limits parameters;
    when_unable_to_fetch =
      (fun _ -> Systime_os.sleep limits.delay_when_fetching_failed >>= return);
    validate = validate parameters;
  }

let temporary_path parameters =
  Filename.concat parameters.data_dir "bootstrapper_tmp"

let configuration limits parameters =
  {
    Bootstrapper.root_path = root_path parameters;
    temporary_path = temporary_path parameters;
    range_size = limits.range_size;
    parallel_ranges_header_fetched = limits.fetching_headers_parallel_jobs;
    parallel_ranges_operations_fetched =
      limits.fetching_operations_parallel_jobs;
    getters = getters limits parameters;
  }
