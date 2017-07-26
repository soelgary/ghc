#include "Rts.h"
#include "RtsUtils.h"
#include "ResourceLimits.h"
#include "sm/GC.h"
#include "sm/Storage.h"
#include "Hash.h"
#include "Schedule.h"
#include "sm/BlockAlloc.h"
#include "Hash.h"
#include "Capability.h"

ResourceContainer *RC_MAIN = NULL;
ResourceContainer *RC_LIST = NULL;
nat RC_COUNT = 0;

nat unreclaimable_count = 10;
nat min_reclaimable_count = 1;

extern nat numGenerations = 2;

extern nat nurserySize = 5;

bdescr *
allocGroupFor(W_ n, ResourceContainer *rc)
{
  W_ real = 0;
  if (rc->status == RC_NORMAL) {
    if (rc->max_blocks != 0) {
      real = neededBlocks(n);
      if (rc->used_blocks + real > rc->max_blocks) {
        // Kill dis fucker
        IF_DEBUG(gc, debugBelch("rc (%p) out of memory (requested %ld at %ld/%ld)\n", rc, (long)real, (long)rc->used_blocks, (long)rc->max_blocks));
        killRC(rc);
        return NULL;
      }
    }
  } else {
    ASSERT(rc->status == RC_KILLED);
    return NULL;
  }
  bdescr *bd = allocGroup(n);
  ASSERT(real == 0 || bd->blocks == real);
  bd->rc = rc;
  return bd;
}

bdescr *
allocBlockFor(ResourceContainer *rc)
{
  bdescr *bd = allocGroupFor(1, rc);
  if(bd == NULL) {
    barf("fukc");
  }
  return bd;
}

void
killRC(ResourceContainer *rc)
{
  rc->status = RC_KILLED;
  // TODO....
  // Reclaim memory and give it to the parent
}

static void
addChild(ResourceContainer *parent, ResourceContainer *child)
{
  RCChildren *childLink = stgMallocBytes(sizeof(RCChildren), "createRCChildren");
  childLink->child = child;
  childLink->next = parent->children;
  parent->children = childLink;
}

void
initRCGeneration(generation *gen, ResourceContainer *rc, nat genNumber)
{
  gen->no = genNumber;
  gen->collections = 0;
  gen->par_collections = 0;
  gen->failed_promotions = 0;
  gen->max_blocks = 0;
  gen->live_estimate = 0;
  gen->old_blocks = NULL;
  gen->n_old_blocks = 0;
  gen->scavenged_large_objects = NULL;
  gen->n_scavenged_large_blocks = 0;
  gen->mark = 0;
  gen->compact = 0;
  gen->bitmap = NULL;
//#ifdef THREADED_RTS
//  initSpinLock(&gen->sync);
//#endif
  gen->threads = END_TSO_QUEUE;
  gen->old_threads = END_TSO_QUEUE;
  gen->weak_ptr_list = NULL;
  gen->old_weak_ptr_list = NULL;
}

void
initGenerations(ResourceContainer *rc)
{
  nat g;
  for(g = 0; g < numGenerations; g++) {
    initRCGeneration(&rc->generations[g], rc, g);
  }
}

void
initGCThread(ResourceContainer *rc)
{

  gc_thread *gct = stgMallocBytes(sizeof(gc_thread) +
                                  (sizeof(gen_workspace) * 2),
                                 "newGCThread");
  rc->gc_thread = gct;

  gct->thread_index = 0;
  gct->idle = rtsFalse;
  gct->free_blocks = NULL;
  gct->static_objects = END_OF_STATIC_OBJECT_LIST;
  gct->scavenged_static_objects = END_OF_STATIC_OBJECT_LIST;
  gct->gc_count = 0;
  gct->scan_bd = NULL;
  gct->mut_lists = rc->mut_lists;
  gct->evac_gen_no = 0;
  gct->failed_to_evac = rtsFalse;
  gct->eager_promotion = rtsTrue;
  gct->thunk_selector_depth = 0;
  gct->rc = rc;
  
  gct->copied = 0;
  gct->scanned = 0;
  gct->any_work = 0;
  gct->no_work = 0;
  gct->scav_find_work = 0;

  nat g;
  gen_workspace *ws;
  for(g = 0; g < numGenerations; g++) {
    ws = &gct->gens[g];
    ws->gen = &rc->generations[g];
    ws->my_gct = gct;
    
    bdescr *bd = allocBlockFor(rc);
    bd->flags = BF_EVACUATED;
    bd->u.scan = bd->free = bd->start;

    ws->todo_bd = bd;
    ws->todo_free = bd->free;
    ws->todo_lim = bd->start + BLOCK_SIZE_W;

    ws->todo_q = newWSDeque(128);
    ws->todo_overflow = NULL;
    ws->n_todo_overflow = 0;
    ws->todo_large_objects = NULL;

    ws->part_list = NULL;
    ws->n_part_blocks = 0;
    ws->n_part_words = 0;

    ws->scavd_list = NULL;
    ws->n_scavd_blocks = 0;
    ws->n_scavd_words = 0;
  }

}


ResourceContainer *
newRC(ResourceContainer *parent)
{
  // TODO: Allocate generations here!

  // TODO: Think about sane defaults for number of children. Is it a constant
  // number? Is it based on the number of blocks?
  // We need this becuase there are two separate memory partitions: for haskell
  // and the runtime system. The RTS has "unlimited" size. The haskell does not.

  //StgRC *stgRC = stgMallocBytes(sizeof(StgRC), "newStgRC");
  //SET_HDR(stgRC, &stg_RC_info, CCS_SYSTEM);
  ResourceContainer *rc = stgMallocBytes(sizeof(ResourceContainer), "newRC");
  //stgRC->rc = rc;
  rc->status = RC_NORMAL;
  rc->used_blocks = 0;
  rc->block_record = NULL;
#ifdef DEBUG
    IF_DEBUG(sanity, rc->block_record = allocHashTable());
#endif
  rc->pinned_object_block = NULL;

  memcount max_blocks;

  if (parent != NULL) {
    if (parent->max_blocks - parent->n_blocks > unreclaimable_count + min_reclaimable_count) {
      max_blocks = parent->max_blocks / 2; // TODO: Need to take unused blocks
      debugTrace(DEBUG_gc, "Resource container is created from a parent with %d blocks. This RC is stealing %d", parent->max_blocks, max_blocks);
      parent->max_blocks = parent->max_blocks - max_blocks;
    } else {
      barf("Cannot create a resource container when the parent has %d free blocks", parent->max_blocks - parent->n_blocks);
    }
  } else if (RtsFlags.GcFlags.maxHeapSize) {
    max_blocks = RtsFlags.GcFlags.maxHeapSize; // Divide by the size of a block
    debugTrace(DEBUG_gc, "Resource container does not have a parent. Setting number of blocks to be %d", max_blocks);
  } else {
      barf("Unknown heap size");
  }

  generation *generations = stgMallocBytes(numGenerations * sizeof(generation),
                                           "createGenerations");
  rc->generations = generations;


  nursery *nurse = stgMallocBytes(sizeof(struct nursery_), "rcCreateNursery");
  memcount initialNurserySize = 1;
  bdescr *bd = allocNursery(NULL, initialNurserySize, rc);
  
  rc->isDead = rtsFalse;

  nurse->blocks = bd;
  nurse->n_blocks = initialNurserySize; //TODO: Decrement used blocks by n_blocks?
  nurse->rc = rc;

  rc->nursery = nurse;
  rc->currentAlloc = bd;

  rc->pinned_object_block = NULL;
  rc->pinned_object_blocks = NULL;

  rc->large_objects = NULL;
  rc->n_large_blocks = 0;
  rc->n_large_words = 0;
  rc->n_new_large_words = 0;

  rc->blocks = NULL;
  rc->n_blocks = 0;
  rc->n_words = 0;
  rc->max_blocks = max_blocks;

  rc->weak_ptr_list_hd = NULL;
  rc->weak_ptr_list_tl = NULL;

  rc->parent = parent;
  rc->free_blocks = NULL;

  rc->mut_lists  = stgMallocBytes(sizeof(bdescr *) *
                                     RtsFlags.GcFlags.generations,
                                     "newRC");
  rc->saved_mut_lists = stgMallocBytes(sizeof(bdescr *) *
                                        RtsFlags.GcFlags.generations,
                                        "newRC");
  nat g;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    rc->mut_lists[g] = allocBlockFor(rc);
  }

  rc->link = RC_LIST;
  rc->children = NULL;

  if(parent != NULL) {
    addChild(parent, rc);
  }

  rc->id = RC_COUNT;

  initGenerations(rc);
  initGCThread(rc);

  RC_LIST = rc;
  RC_COUNT++;
  return rc;
}

void
initRC()
{

  // Set up Resource Containers
  ResourceContainer *rc = newRC(NULL);
  RC_MAIN = rc;

  nat i;

  newNurseryBlock(RC_MAIN->nursery->blocks);

  for (i = 0; i < n_capabilities; i++) {
      capabilities[i]->r.rNursery = RC_MAIN->nursery;
      capabilities[i]->r.rCurrentNursery = RC_MAIN->nursery->blocks;
      capabilities[i]->r.rCurrentAlloc = NULL;
  }

}

/*
  Count the number of blocks allocated and owned by this RC. This is used for
  accounting in debug to find memory leaks (See Sanity.c:memInventory).
*/
W_
countRCBlocks(ResourceContainer *rc)
{
  ASSERT(countBlocks(rc->blocks) == rc->n_blocks);
  ASSERT(countBlocks(rc->large_objects) == rc->n_large_blocks);

  W_ numBlocks = 0;

  bdescr *bd;

  for (bd = rc->nursery->blocks; bd != NULL;) {
    numBlocks++;
    bd = bd->link;
  }

  numBlocks += countBlocks(rc->large_objects) + rc->n_blocks;

  /*
    TODO: RCs only track the nursery. We need to also keep track of blocks in
          the generations.
  */

  return numBlocks;
}

void
setThreadParent(Capability *cap, StgTSO *parent)
{
  ResourceContainer *rc;

  rc = cap->r.rCurrentTSO->rc;
  rc->parentTSO = parent;
  debugTrace(DEBUG_gc, "Set the current thread (%d) parent to %d", cap->r.rCurrentTSO->id, parent->id);
}

void appendToFreeList(ResourceContainer *rc, bdescr *head)
{
  if (rc != NULL) {
    bdescr *tail = head;
    while(tail != NULL) {
      if (tail->link == NULL) {
        tail->link = rc->free_blocks;
        break;
      }
      tail = tail->link;
    }
    rc->free_blocks = head;
  }
}

void
releaseSpaceAndTime(ResourceContainer *rc)
{
  debugTrace(DEBUG_gc, "Releasing resources for thread %d", rc->ownerTSO->id);

  // TODO: Check if there are any pointers into this RC before releasing all
  //       the blocks. Maybe have a remembered set for this?

  nat n = rc->max_blocks;
  nat actual = 0;
  bdescr *start;
  for(start = rc->nursery->blocks; start != NULL; start = start->link) {
    actual++;
  }
  if(actual != n) {
    debugTrace(DEBUG_gc, "Found %d blocks to free in RC", actual);
    debugTrace(DEBUG_gc, "%d blocks are supposed to be freed", n);
  }

  nat released = rc->max_blocks;
  if(rc->parent != NULL) {
    rc->parent->max_blocks += released;
    rc->max_blocks = 0;

    appendToFreeList(rc->parent, rc->nursery->blocks);
    appendToFreeList(rc->parent, rc->pinned_object_block);
    appendToFreeList(rc->parent, rc->pinned_object_blocks);
    appendToFreeList(rc->parent, rc->large_objects);
    appendToFreeList(rc->parent, rc->free_blocks);
    debugTrace(DEBUG_gc, "Released %d blocks to parent", released);
  } else {
    debugTrace(DEBUG_gc, "Parent is NULL. Not realeasing resources");
  }

  if(rc->parent != NULL && rc->parent->isDead) {
    releaseSpaceAndTime(rc->parent);
  }
  rc->isDead = rtsTrue;
}

