/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_GC_H
#define SM_GC_H

#include "BeginPrivate.h"

#include "HeapAlloc.h"

int derp();

void GarbageCollect (rtsBool force_major_gc,
                     rtsBool do_heap_census,
                     nat gc_type,  ResourceContainer *rc);

typedef void (*evac_fn)(void *user, StgClosure **root);
typedef void (*evac_fn_rc)(void *user, StgClosure **root, ResourceContainer *rc,
    nat genNumber, bdescr *mark_stack_bd, bdescr *mark_stack_top_bd,
    StgPtr mark_sp, gc_thread *gt);

StgClosure * isAlive      ( StgClosure *p );
void         markCAFs     ( evac_fn_rc evac, ResourceContainer *rc,
    bdescr *mark_stack_bd, bdescr *mark_stack_top_bd, StgPtr mark_sp, gc_thread *gt );

extern nat N;
extern rtsBool major_gc;

extern bdescr *mark_stack_bd;
extern bdescr *mark_stack_top_bd;
extern StgPtr mark_sp;

extern long copied;

extern rtsBool work_stealing;

#ifdef DEBUG
extern nat mutlist_MUTVARS, mutlist_MUTARRS, mutlist_MVARS, mutlist_OTHERS,
    mutlist_TVAR,
    mutlist_TVAR_WATCH_QUEUE,
    mutlist_TREC_CHUNK,
    mutlist_TREC_HEADER,
    mutlist_ATOMIC_INVARIANT,
    mutlist_INVARIANT_CHECK_QUEUE;
#endif

#if defined(PROF_SPIN) && defined(THREADED_RTS)
extern StgWord64 whitehole_spin;
#endif

void gcWorkerThread (Capability *cap);
void initGcThreads (nat from, nat to);
void freeGcThreads (void);

#if defined(THREADED_RTS)
void waitForGcThreads (Capability *cap);
void releaseGCThreads (Capability *cap);
#endif

#define WORK_UNIT_WORDS 128

#include "EndPrivate.h"

#endif /* SM_GC_H */
