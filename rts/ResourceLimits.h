#ifndef RESOURCE_LIMITS_H
#define RESOURCE_LIMITS_H

#include "sm/GCThread.h"
#include "sm/GC.h"

#include "BeginPrivate.h"

#define RC_NORMAL       0
#define RC_KILLED       1

bdescr *allocGroupFor(W_ n, ResourceContainer *rc);
bdescr *allocBlockFor(ResourceContainer *rc);

ResourceContainer *newRC(ResourceContainer *parent);
void killRC(ResourceContainer *rc);

void initRC(void);

W_ countRCBlocks(ResourceContainer *rc);

void setThreadParent(Capability *cap, StgTSO *parent);

void releaseSpaceAndTime(ResourceContainer *rc);
//void removeTSOFromSleepingQueue(ResourceContainer *rc, StgTSO *sleeping_queue);

void markRC(evac_fn_rc evac, ResourceContainer *rc, rtsBool dontMarkSparks);

#include "EndPrivate.h"

#endif /* RESOURCE_LIMITS_H */
