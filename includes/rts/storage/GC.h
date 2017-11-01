/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STORAGE_GC_H
#define RTS_STORAGE_GC_H

#include <stddef.h>
#include "rts/OSThreads.h"

/* -----------------------------------------------------------------------------
 * Generational GC
 *
 * We support an arbitrary number of generations, with an arbitrary number
 * of steps per generation.  Notes (in no particular order):
 *
 *       - all generations except the oldest should have the same
 *         number of steps.  Multiple steps gives objects a decent
 *         chance to age before being promoted, and helps ensure that
 *         we don't end up with too many thunks being updated in older
 *         generations.
 *
 *       - the oldest generation has one step.  There's no point in aging
 *         objects in the oldest generation.
 *
 *       - generation 0, step 0 (G0S0) is the allocation area.  It is given
 *         a fixed set of blocks during initialisation, and these blocks
 *         normally stay in G0S0.  In parallel execution, each
 *         Capability has its own nursery.
 *
 *       - during garbage collection, each step which is an evacuation
 *         destination (i.e. all steps except G0S0) is allocated a to-space.
 *         evacuated objects are allocated into the step's to-space until
 *         GC is finished, when the original step's contents may be freed
 *         and replaced by the to-space.
 *
 *       - the mutable-list is per-generation (not per-step).  G0 doesn't
 *         have one (since every garbage collection collects at least G0).
 *
 *       - block descriptors contain pointers to both the step and the
 *         generation that the block belongs to, for convenience.
 *
 *       - static objects are stored in per-generation lists.  See GC.c for
 *         details of how we collect CAFs in the generational scheme.
 *
 *       - large objects are per-step, and are promoted in the same way
 *         as small objects, except that we may allocate large objects into
 *         generation 1 initially.
 *
 * ------------------------------------------------------------------------- */

// A count of blocks needs to store anything up to the size of memory
// divided by the block size.  The safest thing is therefore to use a
// type that can store the full range of memory addresses,
// ie. StgWord.  Note that we have had some tricky int overflows in a
// couple of cases caused by using ints rather than longs (e.g. #5086)

typedef StgWord memcount;

typedef struct WSDeque_ {
  // Size of elements array. Used for modulo calculation: we round up
  // to powers of 2 and use the dyadic log (modulo == bitwise &)
  StgWord size;
  StgWord moduloSize; /* bitmask for modulo */

  // top, index where multiple readers steal() (protected by a cas)
  volatile StgWord top;

  // bottom, index of next free place where one writer can push
  // elements. This happens unsynchronised.
  volatile StgWord bottom;

  // both top and bottom are continuously incremented, and used as
  // an index modulo the current array size.

  // lower bound on the current top value. This is an internal
  // optimisation to avoid unnecessarily accessing the top field
  // inside pushBottom
  volatile StgWord topBound;

  // The elements array
  void ** elements;

  //  Please note: the dataspace cannot follow the admin fields
  //  immediately, as it should be possible to enlarge it without
  //  disposing the old one automatically (as realloc would)!

} WSDeque;

/* Stats on spark creation/conversion */
typedef struct {
  StgWord created;
  StgWord dud;
  StgWord overflowed;
  StgWord converted;
  StgWord gcd;
  StgWord fizzled;
} SparkCounters;

typedef WSDeque SparkPool;

typedef struct nursery_ nursery;

typedef struct gen_workspace_ gen_workspace;
typedef struct gc_thread_ gc_thread;

typedef struct generation_ generation;

typedef struct RCChildren_ {
  struct ResourceContainer_ *child;
  struct RCChildren_ *next;
} RCChildren;

typedef struct ResourceContainer_ {

  StgTSO *ownerTSO; // the thread that allocates into this RC. Should not ever change
  StgTSO *parentTSO; // the thread that will reclaim this RC when owner dies. This
                  // can change dynamically

  // All the pointers
  struct ResourceContainer_ *link;
  struct ResourceContainer_ *parent;
  RCChildren *children;
  nat id;

  rtsBool isDead; // This is used to say if an RC is dead. A dead RC cannot
                  // receive resources, but it wont be freed from memory. It
                  // will continue


  memcount max_blocks;
  memcount used_blocks;

  StgWord status;

  HashTable *block_record;

  // One mutable list per generation, so we don't need to take any
  // locks when updating an old-generation thunk.  This also lets us
  // keep track of which closures this CPU has been mutating, so we
  // can traverse them using the right thread during GC and avoid
  // unnecessarily moving the data from one cache to another.
  bdescr **mut_lists;
  bdescr **saved_mut_lists; // tmp use during GC

  // block for allocating pinned objects into
  bdescr *pinned_object_block;
  // full pinned object blocks allocated since the last GC
  bdescr *pinned_object_blocks;

  bdescr *       free_blocks; // free blocks that can be used anywhere
                              // Allocation should not occur into any of these
                              // until they are moved into a nursery, large_obj,
                              // pinned_object, or gc list

  // rthread goes here but do this when working on the GC

  // per-capability weak pointer list associated with nursery (older
  // lists stored in generation object)
  StgWeak *weak_ptr_list_hd;
  StgWeak *weak_ptr_list_tl;

  nursery *nursery;

  bdescr *currentAlloc;

  gc_thread *gc_thread;

  generation **generations;

  // Sparks are a part of RCs now!
  SparkPool *sparks;
  
  // Stats on spark creation/conversion
  SparkCounters spark_stats;

  // Used for debugging the GC. We want to know how many things were allocated
  // and how many the GC found through the roots
  int allocationCount;
  int foundCount;

} ResourceContainer;

struct nursery_ {
    bdescr *       blocks;
    memcount       n_blocks;
    ResourceContainer *rc;
};

// Nursery invariants:
//
//  - cap->r.rNursery points to the nursery for this capability
//
//  - cap->r.rCurrentNursery points to the block in the nursery that we are
//    currently allocating into.  While in Haskell the current heap pointer is
//    in Hp, outside Haskell it is stored in cap->r.rCurrentNursery->free.
//
//  - the blocks *after* cap->rCurrentNursery in the chain are empty
//    (although their bd->free pointers have not been updated to
//    reflect that)
//
//  - the blocks *before* cap->rCurrentNursery have been used.  Except
//    for rCurrentAlloc.
//
//  - cap->r.rCurrentAlloc is either NULL, or it points to a block in
//    the nursery *before* cap->r.rCurrentNursery.
//
// See also Note [allocation accounting] to understand how total
// memory allocation is tracked.

struct generation_ {
    nat            no;                  // generation number

    memcount       max_blocks;          // max blocks

    StgTSO *       threads;             // threads in this gen
                                        // linked via global_link
    StgWeak *      weak_ptr_list;       // weak pointers in this gen

    struct generation_ *to;             // destination gen for live objects

    // stats information
    nat collections;
    nat par_collections;
    nat failed_promotions;

    // ------------------------------------
    // Fields below are used during GC only

#if defined(THREADED_RTS)
    char pad[128];                      // make sure the following is
                                        // on a separate cache line.
    SpinLock     sync;                  // lock for large_objects
                                        //    and scavenged_large_objects
#endif

    int          mark;                  // mark (not copy)? (old gen only)
    int          compact;               // compact (not sweep)? (old gen only)

    // During GC, if we are collecting this gen, blocks and n_blocks
    // are copied into the following two fields.  After GC, these blocks
    // are freed.
    bdescr *     old_blocks;            // bdescr of first from-space block
    memcount     n_old_blocks;         // number of blocks in from-space
    memcount     live_estimate;         // for sweeping: estimate of live data

    bdescr *       blocks;              // blocks in this RC
    memcount       n_blocks;            // number of blocks
    memcount       n_words;             // number of used words

    bdescr *       large_objects;       // large objects (doubly linked)
    memcount       n_large_blocks;      // no. of blocks used by large objs
    memcount       n_large_words;       // no. of words used by large objs
    memcount       n_new_large_words;   // words of new large objects
                                        // (for doYouWantToGC())

    bdescr *     scavenged_large_objects;  // live large objs after GC (d-link)
    memcount     n_scavenged_large_blocks; // size (not count) of above

    bdescr *     bitmap;                // bitmap for compacting collection

    StgTSO *     old_threads;
    StgWeak *    old_weak_ptr_list;
};

extern generation * generations;
extern generation * g0;
extern generation * oldest_gen;

/* -----------------------------------------------------------------------------
   Generic allocation
   StgPtr allocate(Capability *cap, W_ n)
                                Allocates memory from the nursery in
                                the current Capability.
   StgPtr allocatePinned(Capability *cap, W_ n)
                                Allocates a chunk of contiguous store
                                n words long, which is at a fixed
                                address (won't be moved by GC).
                                Returns a pointer to the first word.
                                Always succeeds.
                                NOTE: the GC can't in general handle
                                pinned objects, so allocatePinned()
                                can only be used for ByteArrays at the
                                moment.
                                Don't forget to TICK_ALLOC_XXX(...)
                                after calling allocate or
                                allocatePinned, for the
                                benefit of the ticky-ticky profiler.
   -------------------------------------------------------------------------- */

StgPtr  allocate        ( Capability *cap, W_ n );
StgPtr  allocatePinned  ( Capability *cap, W_ n );

/* memory allocator for executable memory */
typedef void* AdjustorWritable;
typedef void* AdjustorExecutable;

AdjustorWritable allocateExec(W_ len, AdjustorExecutable *exec_addr);
void flushExec(W_ len, AdjustorExecutable exec_addr);
#if defined(ios_HOST_OS)
AdjustorWritable execToWritable(AdjustorExecutable exec);
#endif
void             freeExec (AdjustorExecutable p);

// Used by GC checks in external .cmm code:
extern W_ large_alloc_lim;

/* -----------------------------------------------------------------------------
   Performing Garbage Collection
   -------------------------------------------------------------------------- */

void performGC(void);
void performMajorGC(void);

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs in GHCi
   -------------------------------------------------------------------------- */

StgInd *newCAF         (StgRegTable *reg, StgIndStatic *caf);
StgInd *newRetainedCAF (StgRegTable *reg, StgIndStatic *caf);
StgInd *newGCdCAF      (StgRegTable *reg, StgIndStatic *caf);
void revertCAFs (void);

// Request that all CAFs are retained indefinitely.
// (preferably use RtsConfig.keep_cafs instead)
void setKeepCAFs (void);

/* -----------------------------------------------------------------------------
   Stats
   -------------------------------------------------------------------------- */

typedef struct _GCStats {
  StgWord64 bytes_allocated;
  StgWord64 num_gcs;
  StgWord64 num_byte_usage_samples;
  StgWord64 max_bytes_used;
  StgWord64 cumulative_bytes_used;
  StgWord64 bytes_copied;
  StgWord64 current_bytes_used;
  StgWord64 current_bytes_slop;
  StgWord64 max_bytes_slop;
  StgWord64 peak_megabytes_allocated;
  StgWord64 par_tot_bytes_copied;
  StgWord64 par_max_bytes_copied;
  StgDouble mutator_cpu_seconds;
  StgDouble mutator_wall_seconds;
  StgDouble gc_cpu_seconds;
  StgDouble gc_wall_seconds;
  StgDouble cpu_seconds;
  StgDouble wall_seconds;
} GCStats;
void getGCStats (GCStats *s);
rtsBool getGCStatsEnabled (void);

// These don't change over execution, so do them elsewhere
//  StgDouble init_cpu_seconds;
//  StgDouble init_wall_seconds;

typedef struct _ParGCStats {
  StgWord64 tot_copied;
  StgWord64 max_copied;
} ParGCStats;
void getParGCStats (ParGCStats *s);

/*
typedef struct _TaskStats {
  StgWord64 mut_time;
  StgWord64 mut_etime;
  StgWord64 gc_time;
  StgWord64 gc_etime;
} TaskStats;
// would need to allocate arbitrarily large amount of memory
// because it's a linked list of results
void getTaskStats (TaskStats **s);
// Need to stuff SparkCounters in a public header file...
void getSparkStats (SparkCounters *s);
*/

// Returns the total number of bytes allocated since the start of the program.
HsInt64 getAllocations (void);

/* -----------------------------------------------------------------------------
   This is the write barrier for MUT_VARs, a.k.a. IORefs.  A
   MUT_VAR_CLEAN object is not on the mutable list; a MUT_VAR_DIRTY
   is.  When written to, a MUT_VAR_CLEAN turns into a MUT_VAR_DIRTY
   and is put on the mutable list.
   -------------------------------------------------------------------------- */

void dirty_MUT_VAR(StgRegTable *reg, StgClosure *p);

/* set to disable CAF garbage collection in GHCi. */
/* (needed when dynamic libraries are used). */
extern rtsBool keepCAFs;

INLINE_HEADER void initBdescr(bdescr *bd, generation *gen, generation *dest, ResourceContainer *rc)
{
    bd->gen     = gen;
    bd->gen_no  = gen->no;
    bd->dest_no = dest->no;
    bd->rc = rc;
}

#endif /* RTS_STORAGE_GC_H */
