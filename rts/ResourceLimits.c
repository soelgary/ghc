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
int RC_COUNT = 0;

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
  return allocGroupFor(1, rc);
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

StgRC *
newRC(ResourceContainer *parent, nat max_blocks)
{
  // TODO: Think about sane defaults for number of children. Is it a constant
  // number? Is it based on the number of blocks?
  // We need this becuase there are two separate memory partitions: for haskell
  // and the runtime system. The RTS has "unlimited" size. The haskell does not.

  StgRC *stgRC = stgMallocBytes(sizeof(StgRC), "newStgRC");
  SET_HDR(stgRC, &stg_RC_info, CCS_SYSTEM);
  ResourceContainer *rc = stgMallocBytes(sizeof(ResourceContainer), "newRC");
  stgRC->rc = rc;
  rc->status = RC_NORMAL;
  rc->max_blocks = max_blocks;
  rc->used_blocks = 0;
  rc->block_record = NULL;
#ifdef DEBUG
    IF_DEBUG(sanity, rc->block_record = allocHashTable());
#endif
  rc->pinned_object_block = NULL;

  memcount n_blocks;

  if (RtsFlags.GcFlags.nurseryChunkSize) {
      n_blocks = RtsFlags.GcFlags.nurseryChunkSize;
  } else {
      n_blocks = RtsFlags.GcFlags.minAllocAreaSize;
  }

  nursery *nurse = stgMallocBytes(sizeof(struct nursery_), "rcCreateNursery");
  bdescr *bd = allocNursery(NULL, n_blocks, rc);
  
  nurse->blocks = bd;
  nurse->n_blocks = n_blocks; //TODO: Decrement used blocks by n_blocks?
  nurse->rc = rc;

  rc->nursery = nurse;
  rc->currentAlloc = bd;

  rc->pinned_object_block = NULL;
  rc->pinned_object_blocks = NULL;

  rc->large_objects = NULL;
  rc->n_large_blocks = 0;
  rc->n_large_words = 0;
  rc->n_new_large_words = 0;

  rc->mut_lists  = stgMallocBytes(sizeof(bdescr *) *
                                     RtsFlags.GcFlags.generations,
                                     "newRC");
  rc->saved_mut_lists = stgMallocBytes(sizeof(bdescr *) *
                                        RtsFlags.GcFlags.generations,
                                        "newRC");
  nat g;
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    rc->mut_lists[g] = NULL;
  }

  rc->link = RC_LIST;
  rc->children = NULL;

  if(parent != NULL) {
    addChild(parent, rc);
  }

  RC_LIST = rc;
  RC_COUNT++;
  return stgRC;
}

void
initRC()
{

  // Set up Resource Containers
  StgRC *stgRC = newRC(NULL, 0);
  RC_MAIN = stgRC->rc;

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
  W_ numBlocks = 0;

  bdescr *bd;

  for(bd = rc->nursery->blocks; bd != NULL;) {
    numBlocks++;
    bd = bd->link;
  }

  /*
    TODO: RCs only track the nursery. We need to also keep track of blocks in
          the generations.
  */

  return numBlocks;
}