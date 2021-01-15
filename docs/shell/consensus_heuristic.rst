.. _consensus_heuristic:

Consensus heuristic
====================

The consensus heuristic is a heuristic which can be used to get a
consensus over some possible necessary data fetched from remote peers
over the network.

Consensus heuristic state
--------------------------

Similarly to the *synchronisation heuristic* (see
:ref:`synchronisation heuristic<synchronisation_heuristic>`), the
consensus heuristic relies on a notion of **candidate**: a pair of a
value with a peer. The current consensus heuristic uses a **consensus
state** as follows:

- ``Need_more_candidates``: the heuristic did not receive enough
  candidates to decide whether there is a consensus

- ``Consensus data-value``: the heuristic find a consensus on ``data-value``

- ``Not_consensus data-values``: the heuristic did not find a
  consensus. ``data-values`` are the different data received by
  the heuristic.

Basic description of the heuristic
----------------------------------

The heuristic can be tuned using two parameters:

- ``threshold``: The number of candidates which should agree on the
  same data value

- ``expected``: The number of candidates we expected

As long as there is no ``threshold`` candidates, the heuristic will
only answer ``Need_more_candidates``.

If there are between ``threshold`` and ``expected`` candidates, the
heuristic will either answer ``Consensus data-value`` if there are
``threshold`` candidates which agree on the same data, and
``Need_more_candidates`` otherwise.

Beyond ``expected`` candidates, the heuristic answers ``No_consensus
data-values`` instead of ``Need_more_candidates`` if no consensus was
found.

.. _acceptable_values_consensus:

Acceptable values for parameters
--------------------------------

The heuristic accepts any vaue for the ``threshold`` and ``expected``
parameters as long as ``0 <= threshold <= expected < 2 *
threshold``. The last inequality is to ensure that only one consensus
is possible. A value of ``0`` for ``threshold`` is special and the
heuristic will answers ``No_consensus nil``.

Having a too low value for ``threshold`` and ``expected`` is dangerous
since the consensus does not rely on enough candidates. However, this
value should not be too high otherwise the heuristic is likely to
fail, especially when there is a change of cycle. Also, we do not
recommand having a value of ``threshold`` too close to the value of
``expected`` since a node may be connected to peers which are not up
to date.

If the heuristic fails
----------------------

The heuristic may fail and find no consensus. In that case, the
heuristic comes with a worker which runs again the heuristic if no
consensus was found. Moreover, it may happen, depending on the context
the heuristic is used in, that the consensus found at time ``t`` could
be invalidated. For this reason, the worker also ensures that the
consensus is run again if the last time a consensus was found is too
old.
