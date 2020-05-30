This is a mirror of vbot@bench_context on nomadic's remote.
Here are the instructions :

- Retrieve the data-dirs located at `data/ioana` on comanche ({full,archive}_store_BLAktkWruUqXNgHAiR7kLh4dMP96mGmQANDGagdHAsTXfqgvfiR_933914.tar.gz) and their md5 checks;
- Also, copy the blocks you'll validate locally using the patched baker (/data/ioana/blocks_above_933913 on comanche);
- Extract the data-dir somewhere, this will be your bench's initial state;
- Checkout vbot@bench_context on nomadic's remote;
- Generate a p2p identity in the data-dir: `./tezos-node identity generate --data-dir <data-dir>` (this can be your real "ready-to-use" initial state backup);
- Start a node with no connections : `./tezos-node run --rpc-addr :8732 --no-bootstrap-peers --connections 0 --data-dir <data-dir>`;
- Start the baker : `./tezos-baker-006-PsCARTHA run with local node <data-dir> <path_to/blocks_above_933913>`.

This should simulate 2 RO (node + baker) and 1 RW (validator process) allowing you to bench. I also logged in the baker the time spent trying to validate the next block.

A short explanation of the fake baker: instead of producing blocks out of the blue, the fake baker reads already produced blocks on mainnet (in the provided file), try to simulate the operations as it would have done if it was normally producing blocks (thus using the RO instance), and inject it to the node that will also validate it in its RW instance.
