open Test_utils

(* open Tezos_context *)

let test_freeze store =
  let _index = Store.context_index store in
  let chain_store = Store.main_chain_store store in
  Store.Chain.genesis_block chain_store
  >>= fun genesis ->
  let pred = genesis in
  (* Create 2 cycles of blocks *)
  Alpha_utils.bake_until_n_cycle_end
    ~synchronous_merge:false
    chain_store
    2
    pred
  >>=? fun (_, last_block) ->
  (* First checkpoint will be the latest of the 2 cycles *)
  let checkpoint_ctxt = Store.Block.context_hash last_block in
  (* Loop and freeze *)
  let rec loop pred _pred_checkpoint = function
    | 0 ->
        return_unit
    | n ->
        Alpha_utils.bake_until_cycle_end
          ~synchronous_merge:false
          chain_store
          pred
        >>=? fun (_, last_block) ->
        let new_checkpoint = Store.Block.context_hash last_block in
        (* Context.freeze ~max:pred_checkpoint ~heads:[new_checkpoint] index
         * >>= fun () -> *)
        loop last_block new_checkpoint (n - 1)
  in
  loop last_block checkpoint_ctxt 10

let wrap_test (name, g) =
  let test _ store = g store in
  wrap_test
    ~keep_dir:false
    ~patch_context:Alpha_utils.default_patch_context
    (name, test)

let tests =
  let test_cases = List.map wrap_test [("test freeze", test_freeze)] in
  ("freeze", test_cases)
