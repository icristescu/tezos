.. _bootstrapper:

Bootstrapper
============

The bootstrapper is a worker which is in charge of validating (very
long) branches received from the network. This worker is instantiated
by the *chain validator* (see
:ref:`chain_validator<chain_validator>`). Its role in the validation
subsystem is described in the
:ref:`branch_validation<branch_validation>`). At most one branch can
be validated at the same time. Moreover, the last block of the branch
(the more recent one) is given as a *target* by the *chain
validator*. An assumption is made that the chain has to go through
this target, hence all the blocks between the current head and this
target are expected to be valid. While the bootstrapper is active, the
target can be updated. However, the bootstrapper will try to fetch the
branch associated to this new target only once it has done with its
current task.

When the bootstrapper receives a target and is currently *inactive*,
it starts a new task. First, this task does some preliminary work to
ensure that the bootstrapper is in a consistent state. Then, the task
proceeds in two sequential phases:

- All the headers are fetched from the top of the branch to the bottom
  of the chain and stored onto the disk. It also ensures that headers
  fetched are consistent.

- Once the last header has been fetched, and we have ensured its
  consistency, we start fetching operations, also from bottom to
  top. Once all the first block's operations have been fetched, we can
  start the validation of this block.


In average, given a block, we observe the following: ``
time_to_check_consistency < time_to_fetch_block_header <
time_to_fetch_operations < time_to_validate``. Hence, to accelerate
the bootstrap:

- The first phase should be as fast as possible

- The fetching of data over the network should be parallelised

Notice that the second point have obviously an impact on the first
one.

Because we are bootstrapping long branches (the order of magnitude is
millions of blocks) when joining a network for the first time, it may
happen that the administrator of a node interrupts its node during the
validation of the branch. In that case, once the node is restarted, it
would be a waste of time to fetch the headers one more time.

To overcome this issue, block headers are stored onto persistent
storage. This way, when the node is restarted, it does not need to
fetch them again (see recovery section). The bootstrapper ensures that
only consistent headers are stored onto the persistent storage.

Storing headers onto the disk also has the advantage that the memory
consumption of the bootstrapper is constant (modulo the introspection
of the bootstraper).

Fetching in parallel
--------------------

The bootstrapper allows fetching headers and operations in
parallel. To do so, the initial range ``[level of the current head;
level of the target]`` is split into smaller ranges of limited
size. For each phase (1 or 2) and for each range, the bootstrapper
attributes a slot. A task is assigned for each slot, and this task is
also split in two parts:

- A parallel part, which can be run in parallel

- A sequential part, which needs to wait the
  parallel part as well as the sequential part of the previous range
  to be over

During the first phase of the bootstrapper, ranges are processed in
the reversed order (to go top to bottom). For example, assume the
initial range is ``[0;1000]`` and we split the range into smaller
ranges of size 100. Then the first phase will process ranges this way:
``[900;1000], [800;900], [700;800], ...`` while the second phase will
process ranges ``[0;100], [100;200], [200;300], ...``

For the first phase of each range, the parallel part is defined as
fetching the headers over the network while the sequential part is
defined as ensuring the consistency of the headers fetched and then
storing them onto the disk.

Because we **trust** the target, the consistency check can also be
done *top* to *bottom* by checking that the hash of the predecessor
block is the one in the *predecessor field* of the current block.


For the second phase, the parallel part is defined as fetching the
operations once we have the headers. The sequential part is simply
defined as validating blocks.


Generally the parallelization is really about fetching several ranges
of data at the same time.  Once a slot is done with its current task
(both parallel and sequential), it proceeds to the next range which
does not have a slot assigned yet, until all the ranges (the initial
branch) have been processed.

Parallelisation of the two phases can be controlled via a parameter
``ranges_in_parallel``.

During the first phase, the optimial value for the number of
``ranges`` to fetch in parallel depends on the time it takes to check
the consistency of the block headers. In practice, this check is
fast. The optimal time would be achieved when checking the consistency
of ``range_size * ranges_in_parallel`` is longer than to fetch
``ranges_in_parallel ranges`` of size ``range_size``.

Recovery mode
-------------

Because headers are stored onto the disk, when the node is restarted,
it does not need to fetch all the headers again through the
network. It only needs to look for them in the storage, this is the
*recovery mode*.

The *recovery mode* only works for *headers* fetched through the
network. Indeed, operations are fetched during the second phase, and
since the validation takes much more time than fetching operations, we
do not need to fetch the operations in advance (even if it is possible
at the expense of memory consumption).

The recovery mode also is efficient if the ``range_size``
parameter was not changed between two runs. Otherwise, the recovery
mode will not be able to detect that a range of headers has been
fetched.

The recovery mode uses two directories:

- A ``root directory`` (by default ``.tezos-node/bootstrapper``)
  directory which contains only valid files for the current run of the
  bootstrapper
- A ``temporary directory`` (by default ``.tezos-node/bootstrap_tmp``)
  directory which contains files that may be invalid or which come
  from previous runs

When the bootstrapper starts a new task, it first transfers all the
file from the *root directory* to the *temporary
directory*. Then, when a range is processed during the first phase, it
checks whether there is a valid file associated in the temporary
directory. If so, and if this file is valid it is copied into the root
directory and the headers associated to this range will not be fetched
over the network and can be reused directly by the second phase.

Such policy also ensures that both the *root directory* and the
*temporary directory* will not contain leftovers files from previous
runs.

In practice, even though all the headers have been fetched once, it
may happen that the recovery mode still needs to fetch some new headers
through the network after a restart, when:

- The target has changed and new blocks need to be fetched. In that
  case, the whole last range is fetched

- The validation process has started and the first range to validate
  is different. Only this range will be fetched again through the
  network

- A file is corrupted and needs to be fetched again
