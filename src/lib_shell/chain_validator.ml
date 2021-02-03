(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

open Chain_validator_worker_state

module Name = struct
  type t = Chain_id.t

  let encoding = Chain_id.encoding

  let base = ["validator"; "chain"]

  let pp = Chain_id.pp_short

  let equal = Chain_id.equal
end

module Request = struct
  include Request

  type _ t = Validated : State.Block.t -> Event.update t

  let view (type a) (Validated block : a t) : view = State.Block.hash block
end

type synchronisation_limits = {latency : int; threshold : int}

type checkpoint_limits = {
  threshold : int;
  expected : int;
  request_checkpoint_timeout : Time.System.Span.t;
  checkpoint_too_old : Time.System.Span.t;
  delay_on_failure : Time.System.Span.t;
}

type limits = {
  synchronisation : synchronisation_limits;
  checkpoint : checkpoint_limits;
  bootstrapper : Bootstrapper_configuration.limits;
  worker_limits : Worker_types.limits;
}

module Types = struct
  include Worker_state

  type parameters = {
    data_dir : string;
    target : (Block_hash.t * int32) option;
    parent : Name.t option;
    db : Distributed_db.t;
    chain_state : State.Chain.t;
    chain_db : Distributed_db.chain_db;
    block_validator : Block_validator.t;
    block_validator_process : Block_validator_process.t;
    global_valid_block_input : State.Block.t Lwt_watcher.input;
    global_chains_input : (Chain_id.t * bool) Lwt_watcher.input;
    start_prevalidator : bool;
    prevalidator_limits : Prevalidator.limits;
    peer_validator_limits : Peer_validator.limits;
    limits : limits;
  }

  type bootstrapper_state = {
    bootstrapper : Bootstrapper.t;
    header_requester : Int32.t Distributed_db.Request_worker.t;
    operations_requester : Block_header.t Distributed_db.Request_worker.t;
  }

  type state = {
    parameters : parameters;
    (* This state should be updated everytime a block is validated or
       when we receive a message [Current_head] or [Current_branch]
       with a known head. Because the chain validator does not handle
       directly these messages, this is done through callbacks. *)
    synchronisation_state : Synchronisation_heuristic.t;
    mutable current_status : Synchronisation_heuristic.status option;
    (* This should be true if after updating the synchronisatio_state,
       the status was [Synchronized] at least once. When this flag is
       true, the node starts its mempool. *)
    mutable bootstrapped : bool;
    mutable above_target : bool;
    mutable last_incremented_head : Block_header.t;
    checkpoint_consensus : Block_header.t Consensus_heuristic.Worker.t;
    bootstrapped_waiter : unit Lwt.t;
    bootstrapped_wakener : unit Lwt.u;
    valid_block_input : State.Block.t Lwt_watcher.input;
    new_head_input : State.Block.t Lwt_watcher.input;
    mutable child : (state * (unit -> unit Lwt.t (* shutdown *))) option;
    mutable prevalidator : Prevalidator.t option;
    bootstrapper : bootstrapper_state;
    active_peers :
      (Peer_validator.t, Error_monad.tztrace) P2p_peer.Error_table.t;
  }

  let view (state : state) _ : view =
    let {bootstrapped; active_peers; _} = state in
    {
      bootstrapped;
      active_peers =
        P2p_peer.Error_table.fold_keys (fun id l -> id :: l) active_peers [];
    }
end

module Logger =
  Worker_logger.Make (Event) (Request)
    (struct
      let worker_name = "node_chain_validator"
    end)

module Worker = Worker.Make (Name) (Event) (Request) (Types) (Logger)
open Types

type t = Worker.infinite Worker.queue Worker.t

let table = Worker.create_table Queue

let shutdown w = Worker.shutdown w

let shutdown_child nv active_chains =
  Option.iter_s
    (fun ({parameters = {chain_state; global_chains_input; _}; _}, shutdown) ->
      Lwt_watcher.notify global_chains_input (State.Chain.id chain_state, false) ;
      Chain_id.Table.remove active_chains (State.Chain.id chain_state) ;
      State.update_chain_data nv.parameters.chain_state (fun _ chain_data ->
          Lwt.return (Some {chain_data with test_chain = None}, ()))
      >>= fun () ->
      shutdown ()
      >>= fun () ->
      nv.child <- None ;
      Lwt.return_unit)
    nv.child

let with_consensus_checkpoint nv f =
  (* Because this function may be used inside `Error_monad.dont_wait`
     we catch here the Canceled event so that it is not caught by the
     `Error_monad.dont_wait` function. *)
  Lwt.catch
    (fun () ->
      Consensus_heuristic.Worker.wait nv.checkpoint_consensus
      >>= fun checkpoint -> f checkpoint)
    (function Lwt.Canceled -> return_unit | exn -> Lwt.fail exn)

let may_notify_pv pv f =
  match Peer_validator.status pv with
  | Worker_types.Running _ ->
      f pv
  | Worker_types.Closing (_, _)
  | Worker_types.Closed (_, _, _)
  | Worker_types.Launching _ ->
      ()

let activate_events_when_above_target () =
  Prevalidator_worker_state.activate_events_when_above_target () ;
  Chain_validator_worker_state.activate_events_when_above_target () ;
  Block_validator_worker_state.activate_events_when_above_target ()

let deactivate_events_when_behind_target () =
  Prevalidator_worker_state.deactivate_events_when_behind_target () ;
  Chain_validator_worker_state.deactivate_events_when_behind_target () ;
  Block_validator_worker_state.deactivate_events_when_behind_target ()

let update_above_target nv (head : Block_header.t) (target : Block_header.t) =
  if head.shell.level >= target.shell.level && not nv.above_target then (
    activate_events_when_above_target () ;
    nv.above_target <- true ;
    P2p_peer.Error_table.fold_resolved
      (fun _ pv () -> may_notify_pv pv Peer_validator.notify_target_reached)
      nv.active_peers
      () )
  else if head.shell.level < target.shell.level && nv.above_target then (
    deactivate_events_when_behind_target () ;
    nv.above_target <- false ;
    P2p_peer.Error_table.fold_resolved
      (fun _ pv () -> may_notify_pv pv Peer_validator.notify_behind_target)
      nv.active_peers
      () )

(* Update the synchronisation state and if it is relevant, set the
   bootstrapped flag to true. Assume:

   - [peer_id] is not us.

   - [block] is known as valid. *)

let update_synchronisation_state w
    ((block, peer_id) : Block_header.t * P2p_peer.Id.t) =
  let nv = Worker.state w in
  Synchronisation_heuristic.update
    nv.synchronisation_state
    (block.shell.timestamp, peer_id) ;
  let status = Synchronisation_heuristic.get_status nv.synchronisation_state in
  ( match status with
  | Synchronised _ when nv.bootstrapped = false ->
      nv.bootstrapped <- true ;
      Lwt.wakeup_later nv.bootstrapped_wakener () ;
      Worker.record_event w Bootstrapped
  | _ ->
      () ) ;
  if nv.current_status <> Some status then (
    nv.current_status <- Some status ;
    Worker.record_event w (Sync_status status) )

(* The synchronisation state is updated only for blocks known as
   valid. Assume:

   - [peer_id] is not us *)
let check_and_update_synchronisation_state w (block, peer_id) : unit Lwt.t =
  let nv = Worker.state w in
  let hash = Block_header.hash block in
  let chain_state = nv.parameters.chain_state in
  State.Block.known_valid chain_state hash
  >>= fun known_valid ->
  let known_valid_or_genesis =
    known_valid
    || Block_hash.equal (State.Chain.faked_genesis_hash chain_state) hash
  in
  if known_valid_or_genesis then (
    update_synchronisation_state w (block, peer_id) ;
    Lwt.return_unit )
  else Lwt.return_unit

(* Called for every validated block. *)
let notify_new_block w block =
  let nv = Worker.state w in
  Option.iter
    (fun id ->
      List.assoc id (Worker.list table)
      |> Option.iter (fun w ->
             let nv = Worker.state w in
             Lwt_watcher.notify nv.valid_block_input block))
    nv.parameters.parent ;
  Lwt_watcher.notify nv.valid_block_input block ;
  Lwt_watcher.notify nv.parameters.global_valid_block_input block ;
  Worker.Queue.push_request_now w (Validated block)

(* Called for every validated block coming from a remote peer_id. *)
let notify_new_foreign_block w peer_id block =
  notify_new_block w block ;
  update_synchronisation_state w (State.Block.header block, peer_id)

let init_checkpoint_consensus parameters peers_table request_checkpoint =
  let threshold = parameters.limits.checkpoint.threshold in
  let expected = parameters.limits.checkpoint.expected in
  let job () =
    let update_received = ref 0 in
    let checkpoint = Consensus_heuristic.create ~threshold ~expected () in
    Chain_validator_event.(emit Checkpoint_heuristic.started) ()
    >>= fun () ->
    P2p_peer.Error_table.fold_keys
      (fun peer _ ->
        let timeout =
          parameters.limits.checkpoint.request_checkpoint_timeout
        in
        request_checkpoint timeout peer (function
            | None ->
                Lwt.return_unit
            | Some header ->
                incr update_received ;
                Consensus_heuristic.update checkpoint (peer, header) ;
                Lwt.return_unit))
      peers_table
      Lwt.return_unit
    >>= fun () ->
    let state = Consensus_heuristic.get_state checkpoint in
    ( match state with
    | Consensus_heuristic.Need_more_candidates ->
        if !update_received < threshold then
          Chain_validator_event.(
            emit Checkpoint_heuristic.need_more_checkpoints)
            (!update_received, expected)
        else
          Chain_validator_event.(
            emit Checkpoint_heuristic.need_more_checkpoints_no_consensus)
            (threshold, !update_received, expected)
    | Consensus_heuristic.No_consensus _ ->
        Chain_validator_event.(emit Checkpoint_heuristic.no_consensus) ()
    | Consensus_heuristic.Consensus head ->
        Chain_validator_event.(emit Checkpoint_heuristic.consensus)
          (Block_header.hash head) )
    >>= fun () -> Lwt.return state
  in
  Consensus_heuristic.Worker.create
    ~name:"Checkpoint heuristic"
    ~age_limit:(fun () ->
      Systime_os.sleep parameters.limits.checkpoint.checkpoint_too_old)
    ~job
    ~restart_delay:(fun () ->
      Systime_os.sleep parameters.limits.checkpoint.delay_on_failure)

let with_activated_peer_validator w peer_id f =
  let nv = Worker.state w in
  P2p_peer.Error_table.find_or_make nv.active_peers peer_id (fun () ->
      Peer_validator.create
        ~notify_new_block:(notify_new_foreign_block w peer_id)
        ~notify_termination:(fun _pv ->
          P2p_peer.Error_table.remove nv.active_peers peer_id)
        ~target_reached:nv.above_target
        nv.parameters.peer_validator_limits
        nv.parameters.block_validator
        nv.parameters.chain_db
        peer_id)
  >>=? fun pv ->
  match Peer_validator.status pv with
  | Worker_types.Running _ ->
      f pv
  | Worker_types.Closing (_, _)
  | Worker_types.Closed (_, _, _)
  | Worker_types.Launching _ ->
      return_unit

let may_update_checkpoint chain_state new_head =
  State.Chain.checkpoint chain_state
  >>= fun checkpoint ->
  State.Block.last_allowed_fork_level new_head
  >>=? fun new_level ->
  if new_level <= checkpoint.shell.level then return_unit
  else
    let state = State.Chain.global_state chain_state in
    State.history_mode state
    >>= fun history_mode ->
    let head_level = State.Block.level new_head in
    State.Block.predecessor_n
      new_head
      (Int32.to_int (Int32.sub head_level new_level))
    >>= function
    | None ->
        assert false (* should not happen *)
    | Some new_checkpoint -> (
        State.Block.read_opt chain_state new_checkpoint
        >>= function
        | None ->
            assert false (* should not happen *)
        | Some new_checkpoint -> (
            Chain_validator_event.(emit updated_to_checkpoint)
              (State.Block.hash new_checkpoint, history_mode)
            >>= fun () ->
            let new_checkpoint = State.Block.header new_checkpoint in
            match history_mode with
            | History_mode.Archive ->
                State.Chain.set_checkpoint chain_state new_checkpoint
                >>= fun () -> return_unit
            | Full ->
                State.Chain.set_checkpoint_then_purge_full
                  chain_state
                  new_checkpoint
            | Rolling ->
                State.Chain.set_checkpoint_then_purge_rolling
                  chain_state
                  new_checkpoint ) )

let may_update_protocol_levels chain_state ~prev ~block =
  let prev_proto_level = State.Block.protocol_level prev in
  let new_proto_level = State.Block.protocol_level block in
  if Compare.Int.(prev_proto_level < new_proto_level) then
    State.Block.protocol_hash block
    >>=? fun new_protocol ->
    State.Chain.update_level_indexed_protocol_store
      chain_state
      (State.Chain.id chain_state)
      new_proto_level
      new_protocol
      (State.Block.header block)
    >>= fun () -> return_unit
  else return_unit

let may_switch_test_chain w active_chains spawn_child block =
  let nv = Worker.state w in
  let create_child block protocol expiration forking_block =
    let block_header = State.Block.header block in
    let genesis =
      Context.compute_testchain_genesis (State.Block.hash forking_block)
    in
    let chain_id = Context.compute_testchain_chain_id genesis in
    let activated =
      match nv.child with
      | None ->
          false
      | Some (child, _) ->
          Block_hash.equal
            (State.Chain.genesis child.parameters.chain_state).block
            genesis
    in
    let expired = expiration < block_header.shell.timestamp in
    if expired && activated then
      shutdown_child nv active_chains >>= fun () -> return_unit
    else if
      activated || expired
      || not (State.Chain.allow_forked_chain nv.parameters.chain_state)
    then return_unit
    else
      State.Chain.get_opt
        (State.Chain.global_state nv.parameters.chain_state)
        chain_id
      >>= (function
            | Some chain_state ->
                State.update_testchain block ~testchain_state:chain_state
                >>= fun () -> return chain_state
            | None ->
                let try_init_test_chain cont =
                  let bvp = nv.parameters.block_validator_process in
                  Block_validator_process.init_test_chain bvp forking_block
                  >>= function
                  | Ok genesis_header ->
                      State.fork_testchain
                        block
                        chain_id
                        genesis
                        genesis_header
                        protocol
                        expiration
                      >>=? fun chain_state ->
                      Chain.head chain_state
                      >>= fun new_genesis_block ->
                      Lwt_watcher.notify
                        nv.parameters.global_valid_block_input
                        new_genesis_block ;
                      Lwt_watcher.notify nv.valid_block_input new_genesis_block ;
                      return chain_state
                  | Error
                      (Block_validator_errors.Missing_test_protocol
                         missing_protocol
                      :: _) ->
                      Block_validator.fetch_and_compile_protocol
                        nv.parameters.block_validator
                        missing_protocol
                      >>=? fun _ -> cont ()
                  | Error _ as error ->
                      Lwt.return error
                in
                try_init_test_chain
                @@ fun () ->
                try_init_test_chain
                @@ fun () -> failwith "Could not retrieve test protocol")
      >>=? fun chain_state ->
      (* [spawn_child] is a callback to [create_node]. Thus, it takes care of
         global initialization boilerplate (e.g. notifying [global_chains_input],
         adding the chain to the correct tables, ...) *)
      spawn_child
        ~parent:(State.Chain.id chain_state)
        nv.parameters.start_prevalidator
        nv.parameters.peer_validator_limits
        nv.parameters.prevalidator_limits
        nv.parameters.block_validator
        nv.parameters.global_valid_block_input
        nv.parameters.global_chains_input
        nv.parameters.db
        chain_state
        nv.parameters.limits
      (* TODO: different limits main/test ? *)
      >>=? fun child ->
      nv.child <- Some child ;
      return_unit
  in
  State.Block.test_chain block
  >>= (function
        | (Not_running, _) ->
            shutdown_child nv active_chains >>= fun () -> return_unit
        | ((Forking _ | Running _), None) ->
            return_unit (* only for snapshots *)
        | ( ( Forking {protocol; expiration; _}
            | Running {protocol; expiration; _} ),
            Some forking_block ) ->
            create_child block protocol expiration forking_block)
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      Worker.record_event w (Could_not_switch_testchain err) ;
      Lwt.return_unit

let broadcast_head w ~previous block =
  let nv = Worker.state w in
  if not nv.bootstrapped then Lwt.return_unit
  else
    State.Block.predecessor block
    >>= (function
          | None ->
              Lwt.return_true
          | Some predecessor ->
              Lwt.return (State.Block.equal predecessor previous))
    >>= fun successor ->
    if successor then (
      Distributed_db.Advertise.current_head nv.parameters.chain_db block ;
      Lwt.return_unit )
    else Distributed_db.Advertise.current_branch nv.parameters.chain_db

let safe_get_prevalidator_filter hash =
  match Prevalidator_filters.find hash with
  | Some filter ->
      return filter
  | None -> (
    match Registered_protocol.get hash with
    | None ->
        (* FIXME. *)
        (* This should not happen: it should be handled in the validator. *)
        failwith
          "chain_validator: missing protocol '%a' for the current block."
          Protocol_hash.pp_short
          hash
    | Some protocol ->
        Chain_validator_event.(emit prevalidator_filter_not_found) hash
        >>= fun () ->
        let (module Proto) = protocol in
        let module Filter = Prevalidator_filters.No_filter (Proto) in
        return (module Filter : Prevalidator_filters.FILTER) )

let instantiate_prevalidator nv block (limits, chain_db) =
  State.Block.protocol_hash block
  >>=? (fun new_protocol ->
         safe_get_prevalidator_filter new_protocol
         >>=? fun (module Filter) ->
         Prevalidator.create limits (module Filter) chain_db)
  >>= function
  | Error errs ->
      Chain_validator_event.(emit prevalidator_reinstantiation_failure) errs
      >>= fun () ->
      nv.prevalidator <- None ;
      Lwt.return_unit
  | Ok prevalidator ->
      nv.prevalidator <- Some prevalidator ;
      Lwt.return_unit

let may_flush_or_update_prevalidator nv ~prev ~block =
  match nv.prevalidator with
  | None ->
      return_unit
  | Some old_prevalidator ->
      let prev_proto_level = State.Block.protocol_level prev in
      let new_proto_level = State.Block.protocol_level block in
      if Compare.Int.(prev_proto_level < new_proto_level) then
        (* TODO inject in the new prevalidator the operation
           from the previous one. *)
        let parameters = Prevalidator.parameters old_prevalidator in
        instantiate_prevalidator nv block parameters
        >>= fun () ->
        Prevalidator.shutdown old_prevalidator >>= fun () -> return_unit
      else Prevalidator.flush old_prevalidator (State.Block.hash block)

let may_instantiate_prevalidator nv ~head =
  if nv.parameters.start_prevalidator && nv.bootstrapped then
    instantiate_prevalidator
      nv
      head
      (nv.parameters.prevalidator_limits, nv.parameters.chain_db)
  else Lwt.return_unit

let on_request (type a) w start_testchain active_chains spawn_child
    (req : a Request.t) : a tzresult Lwt.t =
  let (Request.Validated block) = req in
  let nv = Worker.state w in
  Chain.head nv.parameters.chain_state
  >>= fun head ->
  let head_header = State.Block.header head
  and head_hash = State.Block.hash head
  and block_header = State.Block.header block in
  ( match nv.prevalidator with
  | None ->
      Lwt.return head_header.shell.fitness
  | Some pv ->
      Prevalidator.fitness pv )
  >>= fun context_fitness ->
  let head_fitness = head_header.shell.fitness in
  let new_fitness = block_header.shell.fitness in
  let accepted_head =
    if Fitness.(context_fitness = head_fitness) then
      Fitness.(new_fitness > head_fitness)
    else Fitness.(new_fitness >= context_fitness)
  in
  if not accepted_head then return Event.Ignored_head
  else
    Chain.set_head nv.parameters.chain_state block
    >>=? fun previous ->
    may_update_checkpoint nv.parameters.chain_state block
    >>=? fun () ->
    may_update_protocol_levels nv.parameters.chain_state ~prev:previous ~block
    >>=? fun () ->
    broadcast_head w ~previous block
    >>= fun () ->
    may_flush_or_update_prevalidator nv ~prev:previous ~block
    >>=? fun () ->
    ( if start_testchain then
      may_switch_test_chain w active_chains spawn_child block
    else Lwt.return_unit )
    >>= fun () ->
    nv.last_incremented_head <- State.Block.header block ;
    Consensus_heuristic.Worker.on_next_consensus
      nv.checkpoint_consensus
      (fun consensus_checkpoint ->
        update_above_target nv nv.last_incremented_head consensus_checkpoint) ;
    Lwt_watcher.notify nv.new_head_input block ;
    if Block_hash.equal head_hash block_header.shell.predecessor then
      return Event.Head_increment
    else return Event.Branch_switch

let on_completion (type a) w (req : a Request.t) (update : a) request_status =
  let (Request.Validated block) = req in
  let fitness = State.Block.fitness block in
  let request = State.Block.hash block in
  let level = State.Block.level block in
  let timestamp = State.Block.timestamp block in
  Worker.record_event
    w
    (Processed_block
       {request; request_status; update; fitness; level; timestamp}) ;
  Lwt.return_unit

let on_close w =
  let nv = Worker.state w in
  Distributed_db.deactivate nv.parameters.chain_db
  >>= fun () ->
  let pvs =
    P2p_peer.Error_table.fold_promises
      (fun _ pv acc ->
        ( pv
        >>= function
        | Error _ -> Lwt.return_unit | Ok pv -> Peer_validator.shutdown pv )
        :: acc)
      nv.active_peers
      []
  in
  Consensus_heuristic.Worker.cancel nv.checkpoint_consensus ;
  Bootstrapper.shutdown nv.bootstrapper.bootstrapper
  >>= fun () ->
  Lwt.join
    ( Option.iter_s Prevalidator.shutdown nv.prevalidator
    :: Option.iter_s (fun (_, shutdown) -> shutdown ()) nv.child
    :: pvs )

let may_load_protocols parameters =
  let chain_state = Distributed_db.chain_state parameters.chain_db in
  let state = Distributed_db.state parameters.db in
  State.Chain.all_indexed_protocols chain_state
  >>= fun indexed_protocols ->
  List.iter_es
    (fun (_proto_level, (proto_hash, _activation_block)) ->
      if Registered_protocol.mem proto_hash then return_unit
      else
        State.Protocol.known state proto_hash
        >>= function
        | false ->
            return_unit
        | true ->
            (* Only compile protocols that are on-disk *)
            Chain_validator_event.(emit loading_protocol proto_hash)
            >>= fun () ->
            trace
              (Validation_errors.Cannot_load_protocol proto_hash)
              ( Block_validator.fetch_and_compile_protocol
                  parameters.block_validator
                  proto_hash
              >>=? fun _ -> return_unit ))
    indexed_protocols

let on_launch w _ parameters =
  may_load_protocols parameters
  >>=? fun () ->
  let valid_block_input = Lwt_watcher.create_input () in
  let new_head_input = Lwt_watcher.create_input () in
  let (bootstrapped_waiter, bootstrapped_wakener) = Lwt.wait () in
  let synchronisation_state =
    Synchronisation_heuristic.create
      ~threshold:parameters.limits.synchronisation.threshold
      ~latency:parameters.limits.synchronisation.latency
  in
  let bootstrapped =
    match Synchronisation_heuristic.get_status synchronisation_state with
    | Synchronised _ ->
        true
    | Not_synchronised ->
        false
  in
  let active_peers = P2p_peer.Error_table.create 50 in
  let checkpoint_requester timeout peer f =
    Distributed_db.Request.checkpoint ~timeout parameters.chain_db ~peer f
  in
  let checkpoint_consensus =
    init_checkpoint_consensus parameters active_peers checkpoint_requester
  in
  let header_requester =
    Distributed_db.Request_worker.create
      parameters.chain_db
      ~on_failure:(fun _ _ ->
        (*FIXME: implemented in a next commit *) Lwt.return_unit)
  in
  let operations_requester =
    Distributed_db.Request_worker.create
      parameters.chain_db
      ~on_failure:(fun _ _ ->
        (*FIXME: implemented in a next commit *) Lwt.return_unit)
  in
  let bootstrapper_parameters =
    {
      Bootstrapper_configuration.data_dir = parameters.data_dir;
      chain_state = parameters.chain_state;
      chain_db = parameters.chain_db;
      operations_requester;
      header_requester;
      notify_new_block = notify_new_block w;
      block_validator = parameters.block_validator;
    }
  in
  let bootstrapper_configuration =
    Bootstrapper_configuration.configuration
      parameters.limits.bootstrapper
      bootstrapper_parameters
  in
  Bootstrapper.create bootstrapper_configuration
  >>=? fun bootstrapper ->
  State.read_chain_data
    parameters.chain_state
    (fun _ {State.current_head; _} -> Lwt.return current_head)
  >>= fun head ->
  let nv =
    {
      parameters;
      valid_block_input;
      new_head_input;
      bootstrapped_wakener;
      bootstrapped_waiter;
      bootstrapped;
      above_target = false;
      synchronisation_state;
      current_status = None;
      checkpoint_consensus;
      active_peers;
      (* TODO use [2 * max_connection] *)
      child = None;
      prevalidator = None (* the prevalidator may be instantiated next *);
      bootstrapper = {bootstrapper; header_requester; operations_requester};
      last_incremented_head = State.Block.header head;
    }
  in
  (* Start the prevalidator when the chain becomes bootstrapped *)
  Lwt.on_success bootstrapped_waiter (fun () ->
      (* ignore errors *)
      Lwt.dont_wait
        (fun () ->
          Chain.head parameters.chain_state
          >>= fun head -> may_instantiate_prevalidator nv ~head)
        (fun exc -> ignore exc)) ;
  Consensus_heuristic.Worker.on_all_consensus
    checkpoint_consensus
    (fun target -> update_above_target nv nv.last_incremented_head target) ;
  if nv.bootstrapped then Lwt.wakeup_later nv.bootstrapped_wakener () ;
  let initialize_target =
    let initialized = ref false in
    fun peer ->
      if !initialized then Lwt.return_unit
      else
        match parameters.target with
        | None ->
            Lwt.return_unit
        | Some (hash, _lvl) -> (
            Distributed_db.Block_header.fetch
              ~peer
              parameters.chain_db
              ~timeout:(Ptime.Span.of_int_s 5)
              hash
              ()
            >>= function
            | Ok header ->
                initialized := true ;
                Bootstrapper.notify_target bootstrapper ~target:header ;
                Lwt.return_unit
            | Error _ ->
                Lwt.return_unit )
  in
  Distributed_db.set_callback
    parameters.chain_db
    {
      notify_branch =
        (fun peer_id locator ->
          Error_monad.dont_wait
            (fun exc ->
              Format.eprintf
                "Uncaught exception: %s\n%!"
                (Printexc.to_string exc))
            (fun trace ->
              Format.eprintf
                "Uncaught error: %a\n%!"
                Error_monad.pp_print_error
                trace)
            (fun () ->
              let (block, _) = (locator : Block_locator.t :> _ * _) in
              check_and_update_synchronisation_state w (block, peer_id)
              >>= fun () ->
              initialize_target peer_id
              >>= fun () ->
              with_activated_peer_validator w peer_id (fun pv ->
                  Peer_validator.notify_branch pv locator ;
                  with_consensus_checkpoint nv (fun consensus_checkpoint ->
                      if not nv.above_target then
                        Bootstrapper.notify_target
                          bootstrapper
                          ~target:consensus_checkpoint ;
                      return_unit))));
      notify_head =
        (fun peer_id block ops ->
          Error_monad.dont_wait
            (fun exc ->
              Format.eprintf
                "Uncaught exception: %s\n%!"
                (Printexc.to_string exc))
            (fun trace ->
              Format.eprintf
                "Uncaught error: %a\n%!"
                Error_monad.pp_print_error
                trace)
            (fun () ->
              check_and_update_synchronisation_state w (block, peer_id)
              >>= fun () ->
              with_activated_peer_validator w peer_id (fun pv ->
                  Peer_validator.notify_head pv block ;
                  with_consensus_checkpoint nv (fun consensus_checkpoint ->
                      if not nv.above_target then
                        Bootstrapper.notify_target
                          bootstrapper
                          ~target:consensus_checkpoint ;
                      (* TODO notify prevalidator only if head is known ??? *)
                      match nv.prevalidator with
                      | Some prevalidator ->
                          Prevalidator.notify_operations
                            prevalidator
                            peer_id
                            ops
                          >>= fun () -> return_unit
                      | None ->
                          return_unit))));
      disconnection =
        (fun peer_id ->
          Error_monad.dont_wait
            (fun exc ->
              Format.eprintf
                "Uncaught exception: %s\n%!"
                (Printexc.to_string exc))
            (fun trace ->
              Format.eprintf
                "Uncaught error: %a\n%!"
                Error_monad.pp_print_error
                trace)
            (fun () ->
              let nv = Worker.state w in
              match P2p_peer.Error_table.find nv.active_peers peer_id with
              | None ->
                  return_unit
              | Some pv ->
                  pv >>=? fun pv -> Peer_validator.shutdown pv >>= return));
    } ;
  return nv

let rec create ?target ~data_dir ~start_testchain ~active_chains ?parent
    ~block_validator_process start_prevalidator peer_validator_limits
    prevalidator_limits block_validator global_valid_block_input
    global_chains_input db chain_state limits =
  let spawn_child ~parent epv pvl pl bl gvbi gci db n l =
    create
      ?target
      ~data_dir
      ~start_testchain
      ~active_chains
      ~parent
      ~block_validator_process
      epv
      pvl
      pl
      bl
      gvbi
      gci
      db
      n
      l
    >>=? fun w -> return (Worker.state w, fun () -> Worker.shutdown w)
  in
  let module Handlers = struct
    type self = t

    let on_launch = on_launch

    let on_request w = on_request w start_testchain active_chains spawn_child

    let on_close = on_close

    let on_error _ _ _ errs = Lwt.return_error errs

    let on_completion = on_completion

    let on_no_request _ = return_unit
  end in
  let parameters =
    {
      data_dir;
      target;
      parent;
      peer_validator_limits;
      start_prevalidator;
      prevalidator_limits;
      block_validator;
      block_validator_process;
      global_valid_block_input;
      global_chains_input;
      db;
      chain_db = Distributed_db.activate db chain_state;
      chain_state;
      limits;
    }
  in
  Chain.init_head chain_state
  >>=? fun () ->
  Worker.launch
    table
    prevalidator_limits.worker_limits
    (State.Chain.id chain_state)
    parameters
    (module Handlers)
  >>=? fun w ->
  Chain_id.Table.add active_chains (State.Chain.id chain_state) w ;
  Lwt_watcher.notify global_chains_input (State.Chain.id chain_state, true) ;
  return w

(** Current block computation *)

let create ?target ~data_dir ~start_prevalidator ~start_testchain
    ~active_chains ~block_validator_process peer_validator_limits
    prevalidator_limits block_validator global_valid_block_input
    global_chains_input global_db state limits =
  (* hide the optional ?parent *)
  create
    ?target
    ~data_dir
    ~start_testchain
    ~active_chains
    ~block_validator_process
    start_prevalidator
    peer_validator_limits
    prevalidator_limits
    block_validator
    global_valid_block_input
    global_chains_input
    global_db
    state
    limits

let chain_id w =
  let {parameters = {chain_state; _}; _} = Worker.state w in
  State.Chain.id chain_state

let chain_state w =
  let {parameters = {chain_state; _}; _} = Worker.state w in
  chain_state

let prevalidator w =
  let {prevalidator; _} = Worker.state w in
  prevalidator

let chain_db w =
  let {parameters = {chain_db; _}; _} = Worker.state w in
  chain_db

let child w =
  Option.bind
    (Worker.state w).child
    (fun ({parameters = {chain_state; _}; _}, _) ->
      List.assoc (State.Chain.id chain_state) (Worker.list table))

let assert_fitness_increases ?(force = false) w distant_header =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  Chain.head chain_state
  >>= fun local_header ->
  fail_when
    ( (not force)
    && Fitness.compare
         distant_header.Block_header.shell.fitness
         (State.Block.fitness local_header)
       <= 0 )
    (failure "Fitness too low")

let assert_checkpoint w (header : Block_header.t) =
  let pv = Worker.state w in
  let chain_state = Distributed_db.chain_state pv.parameters.chain_db in
  State.Chain.acceptable_block chain_state header
  >>= fun acceptable ->
  fail_unless
    acceptable
    (Validation_errors.Checkpoint_error (Block_header.hash header, None))

let validate_block w ?force hash block operations =
  let nv = Worker.state w in
  assert (Block_hash.equal hash (Block_header.hash block)) ;
  assert_fitness_increases ?force w block
  >>=? fun () ->
  assert_checkpoint w block
  >>=? fun () ->
  Block_validator.validate
    ~canceler:(Worker.canceler w)
    ~notify_new_block:(notify_new_block w)
    nv.parameters.block_validator
    nv.parameters.chain_db
    hash
    block
    operations

let bootstrapped w =
  let {bootstrapped_waiter; _} = Worker.state w in
  Lwt.protected bootstrapped_waiter

let is_bootstrapped w = (Worker.state w).bootstrapped

let force_bootstrapped w b =
  let state = Worker.state w in
  if b then Worker.record_event w Event.Bootstrapped ;
  state.bootstrapped <- b

let valid_block_watcher w =
  let {valid_block_input; _} = Worker.state w in
  Lwt_watcher.create_stream valid_block_input

let new_head_watcher w =
  let {new_head_input; _} = Worker.state w in
  Lwt_watcher.create_stream new_head_input

let status = Worker.status

let information = Worker.information

let running_workers () = Worker.list table

let pending_requests t = Worker.Queue.pending_requests t

let pending_requests_length t = Worker.Queue.pending_requests_length t

let current_request t = Worker.current_request t

let last_events = Worker.last_events

let ddb_information t =
  let state = Worker.state t in
  let ddb = state.parameters.chain_db in
  Distributed_db.information ddb

let sync_status w =
  let nv = Worker.state w in
  Synchronisation_heuristic.get_status nv.synchronisation_state

let get_bootstrapper_state w =
  let nv = Worker.state w in
  let t = nv.bootstrapper.bootstrapper in
  Bootstrapper.state t
