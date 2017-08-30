Multi-core IFC runtime system
=============================

The purpose of this document is to outline the algorithms used by a multi-core
IFC runtime system.

For starters, why do we need to design a custom runtime system? Well, IFC
systems are designed to run untrusted code on sensitive data. In doing so, the
system needs to guarentee the non-interference property. This can easily prevent
developer mistakes through labeling systems, but labels do not guard against
covert channels. Covert channels present a challenge in designing IFC systems
because they allow sensitive and non-sensitive computations to share resources.
Specifically the heap and the scheduler. The algorithms described later will
isolate resources on a per-thread basis so that a sensitive computation cannot
affect a non-sensitive computation.

The Heap
========

Resource Containers
-------------------

A resource container (RC) is a slice of the programs heap. They are hierarchical:
meaning that the first RC owns all of the heap. In order to create another, it
must be given space from another RC.