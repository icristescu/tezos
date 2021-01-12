(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let protocol = Protocol.Alpha

let data_dir_env = "TEZT_BOOTSTRAP"

let data_dir = Sys.getenv_opt data_dir_env

let blocks_to_bake = 100

let bootstrap_nodes =
  match Sys.getenv_opt "BOOTSTRAP_PEERS" with
  | None ->
      1
  | Some x ->
      int_of_string x

let rec list_of_int from upto =
  if upto < from then [] else from :: list_of_int (from + 1) upto

let hash_of_block block_json = JSON.(block_json |-> "hash" |> as_string)

let initialize data_dir protocol =
  match data_dir with
  | None ->
      let* node =
        Node.init
          [Synchronisation_threshold 0; Network "sandbox"; Connections 1]
      in
      let* client = Client.init ~node () in
      let* () = Client.activate_protocol client ~protocol in
      Log.info
        "No data-dir provided. Initializing context with %d empty blocks."
        (blocks_to_bake + 1) ;
      let* () = repeat blocks_to_bake (fun () -> Client.bake_for client) in
      let* _ = Node.wait_for_level node (blocks_to_bake + 1) in
      let* block = RPC.get_block client in
      let hash = hash_of_block block in
      Log.info "Initialization completed." ;
      Lwt.return ([client], hash, [node], "sandbox")
  | Some path ->
      Log.info "Copying data-dir into a temporary directory." ;
      let home = Sys.getenv "HOME" in
      Filename.set_temp_dir_name (Format.asprintf "%s/tmp" home) ;
      let* nodes =
        Lwt_list.map_p
          (fun i ->
            let data_dir = Temp.dir (Format.asprintf "tezos-node-test-%d" i) in
            let* () = Process.run "cp" ["-R"; path ^ "."; data_dir] in
            Node.init
              ~data_dir
              [ Network "mainnet";
                Private_mode;
                No_bootstrap_peers;
                Connections 1 ])
          (list_of_int 1 bootstrap_nodes)
      in
      let* clients = Lwt_list.map_p (fun node -> Client.init ~node ()) nodes in
      let hash = Sys.getenv "TARGET" in
      Lwt.return (clients, hash, nodes, "mainnet")

let target_reached target resolver event =
  if event.Node.name = "node_chain_validator.v0" then
    match
      JSON.(event.Node.value |=> 1 |-> "event" |-> "request" |> as_string_opt)
    with
    | None ->
        ()
    | Some hash ->
        if target = hash then Lwt.wakeup resolver ()

let bootstrap data_dir protocol =
  Test.register ~__FILE__ ~title:"Bootstrap test" ~tags:["node"; "bootstrap"]
  @@ fun () ->
  let* (clients, target_hash, _bootstrap_nodes, network) =
    initialize data_dir protocol
  in
  let* node =
    Node.init [Connections bootstrap_nodes; Network network; No_bootstrap_peers]
  in
  let (target, resolver) = Lwt.task () in
  Node.on_event node (target_reached target_hash resolver) ;
  let* _ =
    Lwt_list.map_p
      (fun client -> Client.Admin.trust_address client ~peer:node)
      clients
  in
  let* _ =
    Lwt_list.map_p
      (fun client -> Client.Admin.connect_address client ~peer:node)
      clients
  in
  let bstart = Unix.gettimeofday () in
  Log.info "Start bootstrapping up to %s." target_hash ;
  let* _ = target in
  let bend = Unix.gettimeofday () in
  Log.info "End bootstrapping in %d seconds." (int_of_float (bend -. bstart)) ;
  Lwt.return_unit

let register () = bootstrap data_dir protocol
