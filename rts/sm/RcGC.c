
#include "PosixSource.h"
#include "Rts.h"
#include "GC.h"
#include "Trace.h"
#include "ResourceLimits.h"
#include "Snapshot.h"
#include "Evac.h"
#include "Scav.h"
#include "WSDeque.h"




void
GarbageCollect_rc (ResourceContainer *rc)
{
  debugTrace(DEBUG_gc, "Starting GC for rc %p", rc);

  rc->currentCopy = rc->generations[0]->old_blocks;

  //evacuate_rc((StgClosure **)(void *)&rc->ownerTSO, rc);
  evacuate_rc((StgClosure **)(void *)&rc->ownerTSO->cap->run_queue_hd, rc);
  evacuate_rc((StgClosure **)(void *)&rc->ownerTSO->cap->run_queue_tl, rc);
  

  pushWSDeque(rc->scavd_list, rc->currentCopy);

  bdescr *curr, *prev;

  while(1) {
    curr = popWSDeque(rc->scavd_list);
    if (curr == NULL) {
      break;
    }
    debugTrace(DEBUG_gc, "Aboyt to scavenge %p", curr);
    scavenge_block(rc, curr);
    prev = curr;
  }

  debugTrace(DEBUG_gc, "Done scavenging");
  rc->currentAlloc = curr;
  rc->nursery->blocks = curr;

  bdescr *swap = rc->generations[0]->blocks;
  rc->generations[0]->blocks = rc->generations[0]->old_blocks;
  rc->generations[0]->old_blocks = allocBlocksFor(rc, 100);


  // Update the run_queue_hd and run_queue_tl forwarding ptrs
  // This will only fix the case where there is one thread running
  Capability *cap = rc->ownerTSO->cap;

  StgTSO *oldTSO = rc->ownerTSO;

  StgTSO **hd = &rc->ownerTSO;
  StgTSO *h = *hd;
  StgInfoTable *info = (StgInfoTable *)h->header.info;
  if (IS_FORWARDING_PTR(info)) {
      debugTrace(DEBUG_gc, "TSO HEAD IS A FORWARDING PTR");
      cap->run_queue_hd = (StgTSO *)UN_FORWARDING_PTR(info);
      debugTrace(DEBUG_gc, "FIXME: Setting the hd and tl of the run queue will fail with multiple threads!");
      cap->run_queue_tl = (StgTSO *)UN_FORWARDING_PTR(info);
      cap->r.rCurrentTSO = (StgTSO *)UN_FORWARDING_PTR(info);
      rc->ownerTSO = (StgTSO *)UN_FORWARDING_PTR(info);
      rc->nursery->blocks = rc->generations[0]->blocks;
      cap->r.rCurrentNursery = rc->nursery->blocks;
      cap->r.rCurrentAlloc = rc->currentAlloc;
      //rc->currentAlloc = rc->nursery->blocks;

      cap->running_task->incall->tso = rc->ownerTSO;

      InCall *ic;
      debugTrace(DEBUG_gc, "FIXME: Unsafe iteration over suspended calls");
      for(ic = cap->suspended_ccalls; ic != NULL; ic = ic->next) {
        if (ic->tso == oldTSO) {
          debugTrace(DEBUG_gc, "Updating InCall tso pointer");
          ic->tso = rc->ownerTSO;
        }
      }
      //info = *hd->header.info;
  } else {
      debugTrace(DEBUG_gc, "TSO HEAD IS NOT A FORWARDING PTR");
  }

  debugTrace(DEBUG_gc, "GC has completed");
  //takeSnapshot("/tmp/derp.txt");
  /*
    RC Garbage collection:

    1. Evacuate the roots
      - Start at the TSO and copy the TSO to old space
      - Recursively copy each field of the TSO to old space
    2. Scavenge the old space
      - Scavenge each block in the old space
        - Recursively update all of the forwarding pointers
    3. Switch old/to space

  */
}
