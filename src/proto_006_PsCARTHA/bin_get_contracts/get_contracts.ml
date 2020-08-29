module Format' = Format
module P = Tezos_raw_protocol_006_PsCARTHA

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

module ScriptOrd : Map.OrderedType with type t = P.Script_expr_hash.t = struct
  type t = P.Script_expr_hash.t

  let compare = P.Script_expr_hash.compare
end

module Scripts = Map.Make (ScriptOrd)

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
    Storage.Contract.list raw_ctxt
    >>=? fun contract_list ->
    let total_contract_number = List.length contract_list in
    print_endline "Listing addresses" ;
    Storage.Contract.fold
      raw_ctxt
      ~init:(Scripts.empty, 0)
      ~f:(fun contract (m, i) ->
        let i = i + 1 in
        Format'.printf "%d/%d\n" i total_contract_number ;
        flush stdout ;
        let open Tezos_protocol_environment_006_PsCARTHA.Environment in
        let open Tezos_protocol_environment_006_PsCARTHA.Environment
                 .Error_monad in
        Storage.Contract.Code.get_option raw_ctxt contract
        >>= function
        | Ok (_, None) ->
            Lwt.return (m, i)
        | Ok (_, Some code) ->
            let code = Data_encoding.force_decode code in
            let code =
              match code with Some code -> code | None -> assert false
            in
            let bytes =
              Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding code
            in
            let key = Script_expr_hash.hash_bytes [bytes] in
            Lwt.return
              ( Scripts.update
                  key
                  (function
                    | Some (code, contracts) ->
                        Some (code, contract :: contracts)
                    | None ->
                        Some (code, [contract]))
                  m,
                i )
        | Error _ ->
            Lwt.return (m, i))
    >>= fun (m, _) ->
    print_endline "Listing addresses done" ;
    Scripts.fold
      (fun hash (script, contracts) () ->
        let filename = P.Script_expr_hash.to_b58check hash ^ ".tz" in
        let chan = open_out filename in
        let fmt = Format'.formatter_of_out_channel chan in
        ( try Format'.fprintf fmt "%a\n" Michelson_v1_printer.print_expr script
          with _ ->
            Format'.eprintf
              "Could not print script for %s from contracts %a"
              filename
              (Format'.pp_print_list
                 ~pp_sep:Format'.pp_print_space
                 P.Contract_repr.pp)
              contracts ) ;
        flush chan ; close_out chan)
      m
      () ;
    return_unit )
