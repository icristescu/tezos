.. _validation:

The validation subsystem
========================

This document explains the inner working of the validation subsystem
of the Tezos shell, that sits between the peer-to-peer layer and the
economic protocol. This part is in charge of validating chains, blocks
and operations that come from the network (or from an RPC), and
deciding whether they are worthy to propagate. The global schema of
validation is summarized in the diagram below.

|Tezos validation diagram|

Validation
----------

The *validation* subsytem in Tezos has several objectives:

- checking that blocks coming from the network or a baker are valid,
  w.r.t. the rules defined by the economic protocol
- selecting the block that it considers to be the current head of the
  blockchain.
- activating/deactivating the *test chain* when required
- fetching and validating a new protocol from the network

The validation system is designed as a collection of workers: local
event-processing loops communicating with each other via message
passing. Workers are spawned and killed dynamically, according to
connected peers, incoming blocks to validate, and active (test)chains.

.. _chain_validator:

Chain validator
---------------

The *chain validator* component is the conductor of the validation
subsysten in Tezos. There is exactly one chain validator worker by
active chain. Hence, currently, there is at most two chain validators
worker active: One for the main chain and one for the test chain
during the third voting phase of the economic protocol. The chain
validator worker of the main chain is spawned by the node during its
initialization and remains alive while the node is.  The main chain
validator worker can also spawn a new chain validator worker for the
test chain. The choice of forking a test chain is decided by the
economic protocol. In version Alpha, this is only used to try new
protocols before self-amending the main chain.

The chain validator may receive three kinds of requests:

- A new block: Either from an RPC (sent by a baker for
  example) or from the network
- A new branch: Only from the network (this request is sent most of
  the time during the hand-shaking phase when a connection is
  initialized but it can be sent during a reorganisation too)
- A new mempool: Only from the network (generally at the same time a
  new block is received)


These messages are not handled directly by the chain validator. As a
conductor, it delegates these messages to other workers:

- The *peer validator* (see :ref:`Peer validator<peer_validator>`) is
  in charge for validating new blocks or short branches

- The *bootstrapper* (see :ref:`Bootstrapper<bootstrapper>`) is in
  charge to validate new (long) branches. Validating a branch is not
  that simple. This is described in the *branch validation* section
  (see :ref:`branch validation<branch_validation>`)

- The *prevalidator* is in charge to prevalidate new mempools

The latter is active only when the chain validator considers itself as
being *bootstrapped* (otherwise the *mempool* is just dropped). This
value is decided by the synchronisation heuristic (see
:ref:`Synchronisation heuristic<synchronisation_heuristic>`).

.. _peer_validator:

Peer validator
--------------

The chain validator spawns one *peer validator* worker per connected
peer. The set of peer validators is updated, grown, or shrunk on the
fly, according to the connection and disconnection signals from the
*distributed DB* component (see :ref:`distributed DB<DDB_component>`).

Each peer validator will treat new head proposals from the associated
peer, one at a time, in a loop. Five cases may happen:

1. The block is already known as `valid`
2. The block is already known as `invalid`
3. The block is not known but its predecessor block is known as
   `valid`
4. The block is not known but its predecessor block is known as
   `invalid`
5. The block is not known as well as its predecessor block

In cases 2 and 4, the block is refused (and declared as `invalid` in
case 4). The peer who sent the block is consequently punished.

In case 1, nothing happens.

In case 3, the block is fed to the *block validator* module (see
:ref:`Block validator<block_validator>`).

Finally, in case 5, a new branch is requested from the remote
peer. Indirectly, this will trigger a branch validation (see
:ref:`branch validation<branch_validation>`).

.. _branch_validation:

Branch validation
-----------------

Validating a branch means validating a list of continuous blocks. One
case where the node needs to validate a branch is when a node receives
a new block for which it does not know its predecessor. The
bootstrapping phase is another case where of a node requires to
validate a long branch to catch up one of the heads of the blockchain.

As explained in more details in the :ref:`bootstrapper
documentation<bootstrapper>`, the chain validator worker has a notion
of *target*. A *target* is a block for which the main chain has to go
through. Currently, this target is defined via a checkpoint
heuristic. The checkpoint heuristic is defined via the generic
consensus heuristic (see :ref:`Consensus
heuristic<consensus_heuristic>`). The checkpoint heuristic asks the
current checkpoint to the remote peers and if there is a consensus on
this value, then this block is being defined as a target. The current
checkpoint is interesting because of the two following properties:

- It is not too far from one of the heads of the chain
- It does not change often, hence it should not be too hard to get a
  consensus on it

The checkpoint heuristic instantiates the parameters of the consensus
heuristic as follows:

- If the option ``--connection`` is set on the command-line, then
  values are defined as follows: ``threshold`` is defined as ``max(1,
  (min(((connections + 1) / 2), 10)))`` and ``expected`` is defined as
  ``max(1, min(connections, 15))``.


- Otherwise, by default, ``threshold`` is ``6`` and ``expected`` is
  ``10``

Once a target is defined via the checkpoint heuristic, there are
two possible scenarios:

- The current head of the node is behind the target

- The current head is the target or above the target

The *bootstrapper* (see :ref:`Bootstrapper<bootstrapper>`) is in
charge of validating branches when the current head is behind the
target. In practice, the target is quite close to one of the heads of
the chain. Hence, as the current head of the node might be far away,
there might be a huge number of blocks to validate until the target.
The *bootstrapper* is specialised to validate long branches. However,
it can validate at most one branch at the time.

In the second case, the branch received may be an alternative branch
and may be different from the main branch. Indeed, the target, as it
is defined, also means that past this block, alternative branches are
possible. But such branches should be also short since the target is
closed from the heads of the chain.

From this point, the node should be able to validate several branches
at the same time and this is why the *chain validator* delegates the
validation of such branches to the *peer validator* (see :ref:`Peer
validator<peer_validator>`). Using the *peer validator*, the node can
now validate in parallel (up to) one branch per peer.

In both cases, the branch validator delegates the validation of blocks
to the *block validator* (see :ref:`block
validator<block_validator>`).

Block validator
---------------
.. _block_validator:

The *block validator* validates blocks assuming that all the necessary
data have already been retrieved. When a block is valid, it will
notify the chain validator in case the latter needs to increment its
head. In this case, the chain validator will propagate this
information to its associated *prevalidator*, and may decide to kill
or spawn the test chain according to the economic protocol's decision.

For efficiency reasons, validation of a block is done in an external
process. This external process communicates via the node using a
socket.

There is at most one block validator worker which means at any given
time, the node can validate at most one block.

Prevalidator
------------
.. _prevalidator_component:

To each chain validator is associated a *prevalidator* (this may become
optional in the future, to allow running nodes on machines with less
RAM), that is responsible for the transmission of operations for this
chain over the peer-to-peer network.

To prevent spam, this prevalidator must select the set of operations
that it considers valid and the ones that it chooses to broadcast.
This is done by constantly baking a dummy block, floating over the
current head, and growing as new operations are received.

Operations that get included can be broadcast unconditionally.

Operations that are included are classified in several
categories. Some (such as bad signatures or garbage byte sequences)
are dismissed. They are put in a temporary bounded set for quick
rejection, and the peer that sent it is kicked. Some other operations
are temporarily refused: they come too soon or too late. For instance,
in Alpha, contracts have counters, and operations with counters in the
future are classified as temporarily refused. A malicious peer could
easily flood the mempool with such operations, so they are put in a
bounded set. Another bounded set is also kept for a third kind of
non-inclusion: operations that could be valid in another branch.

Distributed DB
--------------
.. _DDB_component:

The gathering of data needed for validation is centralized in the
*distributed DB*. This component allocates a slot per requested
piece of data, whose priority depends on the number of peer validators
requesting it. This component is an abstraction of the *peer-to-peer*
component for the shell. All requests of the shell which requires to
communicate with the peer-to-peer layer goes through the distributed
DB component.

.. |Tezos validation diagram| image:: validation.svg
