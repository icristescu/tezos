module Format' = Format

let mainnet_genesis =
  {
    Genesis.time = Time.Protocol.of_notation_exn "2018-06-30T16:07:32Z";
    block =
      Block_hash.of_b58check_exn
        "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2";
    protocol =
      Protocol_hash.of_b58check_exn
        "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
  }

let () =
  Lwt_main.run
    ( Lwt.return @@ ignore
    @@
    let data_dir = Sys.argv.(1) in
    Tezos_shell.State.init
      ~store_root:(Filename.concat data_dir "store")
      ~context_root:(Filename.concat data_dir "context")
      mainnet_genesis
    >>=? fun (_state, chain, _ctxt, _) ->
    Tezos_shell.Chain.head chain
    >>= fun head ->
    Tezos_shell.State.Block.context head
    >>=? fun ctxt ->
    let ctxt = Tezos_shell_context.Shell_context.wrap_disk_context ctxt in
    let open Tezos_protocol_006_PsCARTHA.Protocol in
    let predecessor_timestamp = Tezos_shell.State.Block.timestamp head in
    let fitness = Tezos_shell.State.Block.fitness head in
    let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
    Raw_context.prepare
      ~level:(Tezos_shell.State.Block.level head)
      ~predecessor_timestamp
      ~timestamp
      ~fitness
      ctxt
    >>= (fun res -> Lwt.return (Environment.wrap_error res))
    >>=? fun raw_ctxt ->
    Storage.Contract.fold raw_ctxt ~init:() ~f:(fun contract () ->
        let open Tezos_protocol_environment_006_PsCARTHA.Environment in
        let open Tezos_protocol_environment_006_PsCARTHA.Environment
                 .Error_monad in
        Storage.Contract.Code.get_option raw_ctxt contract
        >>= function
        | Ok (_, None) ->
            Lwt.return ()
        | Ok (_, Some code) ->
            let code = Data_encoding.force_decode code in
            let code =
              match code with Some code -> code | None -> assert false
            in
            Format'.printf "%a" Michelson_v1_printer.print_expr code ;
            Lwt.return ()
        | Error _ ->
            Lwt.return ())
    >>= fun () -> return_unit )
