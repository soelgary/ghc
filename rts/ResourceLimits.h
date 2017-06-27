#ifndef RESOURCE_LIMITS_H
#define RESOURCE_LIMITS_H

#include "sm/GCThread.h"
#include "sm/GC.h"

#include "BeginPrivate.h"

#define RC_NORMAL       0
#define RC_KILLED       1

bdescr *allocGroupFor(W_ n, ResourceContainer *rc);
bdescr *allocBlockFor(ResourceContainer *rc);

StgRC *newRC(ResourceContainer *parent, nat max_blocks);
void killRC(ResourceContainer *rc);

void initRC(void);

W_ countRCBlocks(ResourceContainer *rc);

#include "EndPrivate.h"

#endif /* RESOURCE_LIMITS_H */
