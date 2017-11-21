#ifndef RESOURCE_LIMITS_H
#define RESOURCE_LIMITS_H

#include "sm/GCThread.h"
#include "sm/GC.h"
#include "Sparks.h"

#include "BeginPrivate.h"

#define RC_NORMAL       0
#define RC_KILLED       1

void pinpointSegfault();
void pinpointSegfaultCap(Capability *cap);

bdescr *allocGroupFor(W_ n, ResourceContainer *rc);
bdescr *allocBlockFor(ResourceContainer *rc);
bdescr *allocBlocksFor(ResourceContainer *rc, W_ n);

ResourceContainer *newRC(ResourceContainer *parent);
void killRC(ResourceContainer *rc);

void initRC(void);

W_ countRCBlocks(ResourceContainer *rc);

void setThreadParent(Capability *cap, StgTSO *parent);

void releaseSpaceAndTime(ResourceContainer *rc);

//void removeTSOFromSleepingQueue(ResourceContainer *rc, StgTSO *sleeping_queue);

// For the GC
void markRC(evac_fn_rc evac, ResourceContainer *rc, rtsBool dontMarkSparks,
  bdescr *mark_stack_bd, bdescr *mark_stack_top_bd, StgPtr mark_sp, gc_thread *gt);
void traverseSparkQueues (evac_fn evac, void *user);

#if defined(THREADED_RTS)
rtsBool checkSparkCountInvariant (void);
#endif

// Try to find a spark to run
//
StgClosure *findSpark (Capability *cap);

// True if any capabilities have sparks
//
rtsBool anySparks (void);

INLINE_HEADER rtsBool emptySparkPoolRC (ResourceContainer *rc);
INLINE_HEADER nat     sparkPoolSizeRC  (ResourceContainer *rc);
INLINE_HEADER void    discardSparksRC  (ResourceContainer *rc);

#if defined(THREADED_RTS)
INLINE_HEADER rtsBool
emptySparkPoolRC (ResourceContainer *rc)
{ return looksEmpty(rc->sparks); }

INLINE_HEADER nat
sparkPoolSizeRC (ResourceContainer *rc)
{ return sparkPoolSize(rc->sparks); }

INLINE_HEADER void
discardSparksRC (ResourceContainer *rc)
{ discardSparks(rc->sparks); }
#endif


#include "EndPrivate.h"

#endif /* RESOURCE_LIMITS_H */
