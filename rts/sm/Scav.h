/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector: scavenging functions
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_SCAV_H
#define SM_SCAV_H

#include "BeginPrivate.h"

void    scavenge_loop (ResourceContainer *rc);
void    scavenge_mutable_list (ResourceContainer *rc, bdescr *bd, generation *gen);
void    scavenge_capability_mut_lists (Capability *cap);
void    scavenge_rc_mut_lists(ResourceContainer *rc);
void    scavenge_block(ResourceContainer *rc, bdescr *bd);

#ifdef THREADED_RTS
void    scavenge_loop1 (ResourceContainer *rc);
void    scavenge_mutable_list1 (ResourceContainer *rc, bdescr *bd, generation *gen);
void    scavenge_capability_mut_Lists1 (Capability *cap);
void    scavenge_rc_mut_lists1(ResourceContainer *rc);
void    scavenge_block1(ResourceContainer *rc, bdescr *bd);
#endif

#include "EndPrivate.h"

#endif /* SM_SCAV_H */

