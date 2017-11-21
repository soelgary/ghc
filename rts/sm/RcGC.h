#ifndef SM_RCGC_H
#define SM_RCGC_H

#include "BeginPrivate.h"

#include "HeapAlloc.h"

//typedef void (*evac_fn_rc)(StgClosure **root, ResourceContainer *rc);

void GarbageCollect_rc(ResourceContainer *rc);

#include "EndPrivate.h"

#endif /* SM_RCGC_H */
