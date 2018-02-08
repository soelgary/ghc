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

#include "PosixSource.h"
#include "Rts.h"

#include "Storage.h"
#include "GC.h"
#include "GCThread.h"
#include "GCUtils.h"
#include "Compact.h"
#include "MarkStack.h"
#include "Evac.h"
#include "Scav.h"
#include "Apply.h"
#include "Trace.h"
#include "Sanity.h"
#include "Capability.h"
#include "LdvProfile.h"

#include "sm/MarkWeak.h"

static void scavenge_stack (StgPtr p, StgPtr stack_end, ResourceContainer *rc);

static void scavenge_large_bitmap (StgPtr p,
                                   StgLargeBitmap *large_bitmap,
                                   StgWord size, ResourceContainer *rc);

#if defined(THREADED_RTS) && !defined(PARALLEL_GC)
# define evacuate(a) evacuate1(a)
# define scavenge_loop(rc) scavenge_loop1(rc)
# define scavenge_block(rc,a) scavenge_block1(rc,a)
# define scavenge_mutable_list(rc,bd,g) scavenge_mutable_list1(rc,bd,g)
# define scavenge_capability_mut_lists(cap) scavenge_capability_mut_Lists1(cap)
# define scavenge_rc_mut_lists(rc) scavenge_rc_mut_lists1(rc)
#endif

/* -----------------------------------------------------------------------------
   Scavenge a TSO.
   -------------------------------------------------------------------------- */

static void
scavengeTSO (StgTSO *tso, ResourceContainer *rc)
{
    rtsBool saved_eager;

    debugTrace(DEBUG_gc,"scavenging thread %d",(int)tso->id);

    // update the pointer from the InCall.
    if (tso->bound != NULL) {
      // NB. We can't just set tso->bound->tso = tso, because this
      // might be an invalid copy the TSO resulting from multiple
      // threads evacuating the TSO simultaneously (see
      // Evac.c:copy_tag()).  Calling evacuate() on this pointer
      // will ensure that we update it to point to the correct copy.
      evacuate_rc((StgClosure **)&tso->bound->tso, rc);
    }

    saved_eager = rc->gc_thread->eager_promotion;
    rc->gc_thread->eager_promotion = rtsFalse;

    evacuate_rc((StgClosure **)&tso->blocked_exceptions, rc);
    evacuate_rc((StgClosure **)&tso->bq, rc);

    // scavange current transaction record
    evacuate_rc((StgClosure **)&tso->trec, rc);

    evacuate_rc((StgClosure **)&tso->stackobj, rc);

    // TODO RC: DONT DO DIS!!!
    evacuate_rc((StgClosure **)&tso->_link, rc);

    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
        || tso->why_blocked == BlockedOnBlackHole
        || tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
        ) {
        evacuate_rc((StgClosure **)&tso->block_info.closure, rc);
    }
#ifdef THREADED_RTS
    // in the THREADED_RTS, block_info.closure must always point to a
    // valid closure, because we assume this in throwTo().  In the
    // non-threaded RTS it might be a FD (for
    // BlockedOnRead/BlockedOnWrite) or a time value (BlockedOnDelay)
    else {
        tso->block_info.closure = (StgClosure *)END_TSO_QUEUE;
    }
#endif

    tso->dirty = rc->gc_thread->failed_to_evac;

    rc->gc_thread->eager_promotion = saved_eager;
}

/* -----------------------------------------------------------------------------
   Mutable arrays of pointers
   -------------------------------------------------------------------------- */

static StgPtr scavenge_mut_arr_ptrs (StgMutArrPtrs *a, ResourceContainer *rc)
{
    W_ m;
    rtsBool any_failed;
    StgPtr p, q;

    any_failed = rtsFalse;
    p = (StgPtr)&a->payload[0];
    for (m = 0; (int)m < (int)mutArrPtrsCards(a->ptrs) - 1; m++)
    {
        q = p + (1 << MUT_ARR_PTRS_CARD_BITS);
        for (; p < q; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }
        if (rc->gc_thread->failed_to_evac) {
            any_failed = rtsTrue;
            *mutArrPtrsCard(a,m) = 1;
            rc->gc_thread->failed_to_evac = rtsFalse;
        } else {
            *mutArrPtrsCard(a,m) = 0;
        }
    }

    q = (StgPtr)&a->payload[a->ptrs];
    if (p < q) {
        for (; p < q; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }
        if (rc->gc_thread->failed_to_evac) {
            any_failed = rtsTrue;
            *mutArrPtrsCard(a,m) = 1;
            rc->gc_thread->failed_to_evac = rtsFalse;
        } else {
            *mutArrPtrsCard(a,m) = 0;
        }
    }

    rc->gc_thread->failed_to_evac = any_failed;
    return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}

// scavenge only the marked areas of a MUT_ARR_PTRS
static StgPtr scavenge_mut_arr_ptrs_marked (StgMutArrPtrs *a, ResourceContainer *rc)
{
    W_ m;
    StgPtr p, q;
    rtsBool any_failed;

    any_failed = rtsFalse;
    for (m = 0; m < mutArrPtrsCards(a->ptrs); m++)
    {
        if (*mutArrPtrsCard(a,m) != 0) {
            p = (StgPtr)&a->payload[m << MUT_ARR_PTRS_CARD_BITS];
            q = stg_min(p + (1 << MUT_ARR_PTRS_CARD_BITS),
                        (StgPtr)&a->payload[a->ptrs]);
            for (; p < q; p++) {
                evacuate_rc((StgClosure **)p, rc);
            }
            if (rc->gc_thread->failed_to_evac) {
                any_failed = rtsTrue;
                rc->gc_thread->failed_to_evac = rtsFalse;
            } else {
                *mutArrPtrsCard(a,m) = 0;
            }
        }
    }

    rc->gc_thread->failed_to_evac = any_failed;
    return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}

STATIC_INLINE StgPtr
scavenge_small_bitmap (StgPtr p, StgWord size, StgWord bitmap, ResourceContainer *rc)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            evacuate_rc((StgClosure **)p, rc);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

/* -----------------------------------------------------------------------------
   Blocks of function args occur on the stack (at the top) and
   in PAPs.
   -------------------------------------------------------------------------- */

STATIC_INLINE StgPtr
scavenge_arg_block (StgFunInfoTable *fun_info, StgClosure **args, ResourceContainer *rc)
{
    StgPtr p;
    StgWord bitmap;
    StgWord size;

    p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, rc);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = scavenge_small_bitmap(p, size, bitmap, rc);
        break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
scavenge_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size, ResourceContainer *rc)
{
    StgPtr p;
    StgWord bitmap;
    StgFunInfoTable *fun_info;

    fun_info = get_fun_itbl(UNTAG_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        scavenge_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, rc);
        p += size;
        break;
    case ARG_BCO:
        scavenge_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size, rc);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = scavenge_small_bitmap(p, size, bitmap, rc);
        break;
    }
    return p;
}

STATIC_INLINE GNUC_ATTR_HOT StgPtr
scavenge_PAP (StgPAP *pap, ResourceContainer *rc)
{
    evacuate_rc(&pap->fun, rc);
    return scavenge_PAP_payload (pap->fun, pap->payload, pap->n_args, rc);
}

STATIC_INLINE StgPtr
scavenge_AP (StgAP *ap, ResourceContainer *rc)
{
    evacuate_rc(&ap->fun, rc);
    return scavenge_PAP_payload (ap->fun, ap->payload, ap->n_args, rc);
}

/* -----------------------------------------------------------------------------
   Scavenge SRTs
   -------------------------------------------------------------------------- */

/* Similar to scavenge_large_bitmap(), but we don't write back the
 * pointers we get back from evacuate().
 */
static void
scavenge_large_srt_bitmap( StgLargeSRT *large_srt, ResourceContainer *rc)
{
    nat i, j, size;
    StgWord bitmap;
    StgClosure **p;

    size   = (nat)large_srt->l.size;
    p      = (StgClosure **)large_srt->srt;

    for (i = 0; i < size / BITS_IN(W_); i++) {
        bitmap = large_srt->l.bitmap[i];
        // skip zero words: bitmaps can be very sparse, and this helps
        // performance a lot in some cases.
        if (bitmap != 0) {
            for (j = 0; j < BITS_IN(W_); j++) {
                if ((bitmap & 1) != 0) {
                    evacuate_rc(p, rc);
                }
                p++;
                bitmap = bitmap >> 1;
            }
        } else {
            p += BITS_IN(W_);
        }
    }
    if (size % BITS_IN(W_) != 0) {
        bitmap = large_srt->l.bitmap[i];
        for (j = 0; j < size % BITS_IN(W_); j++) {
            if ((bitmap & 1) != 0) {
                evacuate_rc(p, rc);
            }
            p++;
            bitmap = bitmap >> 1;
        }
    }
}

/* evacuate the SRT.  If srt_bitmap is zero, then there isn't an
 * srt field in the info table.  That's ok, because we'll
 * never dereference it.
 */
STATIC_INLINE GNUC_ATTR_HOT void
scavenge_srt (StgClosure **srt, nat srt_bitmap, ResourceContainer *rc)
{
  nat bitmap;
  StgClosure **p;

  bitmap = srt_bitmap;
  p = srt;

  if (bitmap == (StgHalfWord)(-1)) {
      scavenge_large_srt_bitmap( (StgLargeSRT *)srt, rc);
      return;
  }

  while (bitmap != 0) {
      if ((bitmap & 1) != 0) {
#if defined(COMPILING_WINDOWS_DLL)
          // Special-case to handle references to closures hiding out in DLLs, since
          // double indirections required to get at those. The code generator knows
          // which is which when generating the SRT, so it stores the (indirect)
          // reference to the DLL closure in the table by first adding one to it.
          // We check for this here, and undo the addition before evacuating it.
          //
          // If the SRT entry hasn't got bit 0 set, the SRT entry points to a
          // closure that's fixed at link-time, and no extra magic is required.
          if ( (W_)(*srt) & 0x1 ) {
              evacuate_rc((StgClosure**) ((W_) (*srt) & ~0x1), rc);
          } else {
            evacuate_rc(p, rc);
          }
#else
    evacuate_rc(p, rc);
#endif
      }
      p++;
      bitmap = bitmap >> 1;
  }
}


STATIC_INLINE GNUC_ATTR_HOT void
scavenge_thunk_srt(const StgInfoTable *info, ResourceContainer *rc)
{
    StgThunkInfoTable *thunk_info;
    nat bitmap;

    //if (!major_gc) return;

    thunk_info = itbl_to_thunk_itbl(info);
    bitmap = thunk_info->i.srt_bitmap;
    if (bitmap) {
        // don't read srt_offset if bitmap==0, because it doesn't exist
        // and so the memory might not be readable.
        scavenge_srt((StgClosure **)GET_SRT(thunk_info), bitmap, rc);
    }
}

STATIC_INLINE GNUC_ATTR_HOT void
scavenge_fun_srt(const StgInfoTable *info, ResourceContainer *rc)
{
    StgFunInfoTable *fun_info;
    nat bitmap;

    //if (!major_gc) return;

    fun_info = itbl_to_fun_itbl(info);
    bitmap = fun_info->i.srt_bitmap;
    if (bitmap) {
        // don't read srt_offset if bitmap==0, because it doesn't exist
        // and so the memory might not be readable.
        scavenge_srt((StgClosure **)GET_FUN_SRT(fun_info), bitmap, rc);
    }
}

/* -----------------------------------------------------------------------------
   Scavenge a block from the given scan pointer up to bd->free.

   evac_gen_no is set by the caller to be either zero (for a step in a
   generation < N) or G where G is the generation of the step being
   scavenged.

   We sometimes temporarily change evac_gen_no back to zero if we're
   scavenging a mutable object where eager promotion isn't such a good
   idea.
   -------------------------------------------------------------------------- */

void
scavenge_block (ResourceContainer *rc, bdescr *bd)
{
  StgPtr p, q;
  StgInfoTable *info;
  rtsBool saved_eager_promotion, forwardPtr;
  //gen_workspace *ws;

  forwardPtr = rtsFalse;

  debugTrace(DEBUG_gc, "scavenging block %p (gen %d) @ %p",
             bd->start, bd->gen_no, bd->u.scan);

  rc->gc_thread->scan_bd = bd;
  rc->gc_thread->evac_gen_no = bd->gen_no;
  saved_eager_promotion = rc->gc_thread->eager_promotion;
  rc->gc_thread->failed_to_evac = rtsFalse;

  //ws = &rc->gc_thread->gens[bd->gen->no];

  p = bd->start;//bd->u.scan;

  // we might be evacuating into the very object that we're
  // scavenging, so we have to check the real bd->free pointer each
  // time around the loop.
  while (p < bd->free /*|| (bd == ws->todo_bd && p < ws->todo_free)*/) {

    debugTrace(DEBUG_gc, "Scavenging object in block %p", Bdescr(p));
    if(p == 0x420009d080) {
        debugTrace(DEBUG_gc, "Found the bad closure");
    }
    //ASSERT(bd->link == NULL);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);

    if (IS_FORWARDING_PTR(info)) {
        forwardPtr = rtsTrue;
        debugTrace(DEBUG_gc, "Well fuck. Cant scavenge a fucking forwarding ptr %p", p);
    }

    ASSERT(rc->gc_thread->thunk_selector_depth == 0);

    q = p;
    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = ((StgMVar *)p);
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc((StgClosure **)&mvar->head, rc);
        evacuate_rc((StgClosure **)&mvar->tail, rc);
        evacuate_rc((StgClosure **)&mvar->value, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            mvar->header.info = &stg_MVAR_CLEAN_info;
        } else {
            mvar->header.info = &stg_MVAR_CLEAN_info;
        }
        p += sizeofW(StgMVar);
        break;
    }

    case TVAR:
    {
        StgTVar *tvar = ((StgTVar *)p);
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc((StgClosure **)&tvar->current_value, rc);
        evacuate_rc((StgClosure **)&tvar->first_watch_queue_entry, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            tvar->header.info = &stg_TVAR_DIRTY_info;
        } else {
            tvar->header.info = &stg_TVAR_CLEAN_info;
        }
        p += sizeofW(StgTVar);
        break;
    }

    case FUN_2_0:
        scavenge_fun_srt(info, rc);
        evacuate_rc(&((StgClosure *)p)->payload[1], rc);
        evacuate_rc(&((StgClosure *)p)->payload[0], rc);
        p += sizeofW(StgHeader) + 2;
        break;

    case THUNK_2_0:
        scavenge_thunk_srt(info, rc);
        evacuate_rc(&((StgThunk *)p)->payload[1], rc);
        evacuate_rc(&((StgThunk *)p)->payload[0], rc);
        p += sizeofW(StgThunk) + 2;
        break;

    case CONSTR_2_0:
        evacuate_rc(&((StgClosure *)p)->payload[1], rc);
        evacuate_rc(&((StgClosure *)p)->payload[0], rc);
        p += sizeofW(StgHeader) + 2;
        break;

    case THUNK_1_0:
        scavenge_thunk_srt(info, rc);
        evacuate_rc(&((StgThunk *)p)->payload[0], rc);
        p += sizeofW(StgThunk) + 1;
        break;

    case FUN_1_0:
        scavenge_fun_srt(info, rc);
    case CONSTR_1_0:
        evacuate_rc(&((StgClosure *)p)->payload[0], rc);
        p += sizeofW(StgHeader) + 1;
        break;

    case THUNK_0_1:
        scavenge_thunk_srt(info, rc);
        p += sizeofW(StgThunk) + 1;
        break;

    case FUN_0_1:
        scavenge_fun_srt(info, rc);
    case CONSTR_0_1:
        p += sizeofW(StgHeader) + 1;
        break;

    case THUNK_0_2:
        scavenge_thunk_srt(info, rc);
        p += sizeofW(StgThunk) + 2;
        break;

    case FUN_0_2:
        scavenge_fun_srt(info, rc);
    case CONSTR_0_2:
        p += sizeofW(StgHeader) + 2;
        break;

    case THUNK_1_1:
        scavenge_thunk_srt(info, rc);
        evacuate_rc(&((StgThunk *)p)->payload[0], rc);
        p += sizeofW(StgThunk) + 2;
        break;

    case FUN_1_1:
        scavenge_fun_srt(info, rc);
    case CONSTR_1_1:
        evacuate_rc(&((StgClosure *)p)->payload[0], rc);
        p += sizeofW(StgHeader) + 2;
        break;

    case FUN:
        scavenge_fun_srt(info, rc);
        goto gen_obj;

    case THUNK:
    {
        StgPtr end;

        scavenge_thunk_srt(info, rc);
        end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }
        p += info->layout.payload.nptrs;
        break;
    }

    gen_obj:
    case CONSTR:
    case WEAK:
    case PRIM:
    {
        StgPtr end;

        end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }
        p += info->layout.payload.nptrs;
        break;
    }

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        evacuate_rc((StgClosure **)&bco->instrs, rc);
        evacuate_rc((StgClosure **)&bco->literals, rc);
        evacuate_rc((StgClosure **)&bco->ptrs, rc);
        p += bco_sizeW(bco);
        break;
    }

    case IND_PERM:
    case BLACKHOLE:
        evacuate_rc(&((StgInd *)p)->indirectee, rc);
        p += sizeofW(StgInd);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc(&((StgMutVar *)p)->var, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
        }
        p += sizeofW(StgMutVar);
        break;

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc(&bq->bh, rc);
        evacuate_rc((StgClosure**)&bq->owner, rc);
        evacuate_rc((StgClosure**)&bq->queue, rc);
        evacuate_rc((StgClosure**)&bq->link, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
        } else {
            bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
        }
        p += sizeofW(StgBlockingQueue);
        break;
    }

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        evacuate_rc(&s->selectee, rc);
        p += THUNK_SELECTOR_sizeW();
        break;
    }

    // A chunk of stack saved in a heap object
    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)p;

        evacuate_rc(&ap->fun, rc);
        scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, rc);
        p = (StgPtr)ap->payload + ap->size;
        break;
    }

    case PAP:
        p = scavenge_PAP((StgPAP *)p, rc);
        break;

    case AP:
        p = scavenge_AP((StgAP *)p, rc);
        break;

    case ARR_WORDS:
        // nothing to follow
        p += arr_words_sizeW((StgArrBytes *)p);
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        rc->gc_thread->eager_promotion = rtsFalse;

        p = scavenge_mut_arr_ptrs((StgMutArrPtrs*)p, rc);

        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
        }

        rc->gc_thread->eager_promotion = saved_eager_promotion;
        rc->gc_thread->failed_to_evac = rtsTrue; // always put it on the mutable list.
        break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
        // follow everything
    {
        p = scavenge_mut_arr_ptrs((StgMutArrPtrs*)p, rc);

        // If we're going to put this object on the mutable list, then
        // set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
        }
        break;
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
        // follow everything
    {
        StgPtr next;

        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        rc->gc_thread->eager_promotion = rtsFalse;
        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_CLEAN_info;
        }

        rc->gc_thread->failed_to_evac = rtsTrue; // always put it on the mutable list.
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
        // follow everything
    {
        StgPtr next;

        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }

        // If we're going to put this object on the mutable list, then
        // set its info ptr to SMALL_MUT_ARR_PTRS_FROZEN0 to indicate that.
        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN0_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_info;
        }
        break;
    }

    case TSO:
    {
        scavengeTSO((StgTSO *)p, rc);
        p += sizeofW(StgTSO);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        rc->gc_thread->eager_promotion = rtsFalse;

        scavenge_stack(stack->sp, stack->stack + stack->stack_size, rc);
        stack->dirty = rc->gc_thread->failed_to_evac;
        p += stack_sizeW(stack);

        rc->gc_thread->eager_promotion = saved_eager_promotion;
        break;
    }

    case MUT_PRIM:
      {
        StgPtr end;

        rc->gc_thread->eager_promotion = rtsFalse;

        end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }
        p += info->layout.payload.nptrs;

        rc->gc_thread->eager_promotion = saved_eager_promotion;
        rc->gc_thread->failed_to_evac = rtsTrue; // mutable
        break;
      }

    case TREC_CHUNK:
      {
        StgWord i;
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        TRecEntry *e = &(tc -> entries[0]);
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc((StgClosure **)&tc->prev_chunk, rc);
        for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          evacuate_rc((StgClosure **)&e->tvar, rc);
          evacuate_rc((StgClosure **)&e->expected_value, rc);
          evacuate_rc((StgClosure **)&e->new_value, rc);
        }
        rc->gc_thread->eager_promotion = saved_eager_promotion;
        rc->gc_thread->failed_to_evac = rtsTrue; // mutable
        p += sizeofW(StgTRecChunk);
        break;
      }

    default:
        barf("scavenge: unimplemented/strange closure type %d @ %p",
             info->type, p);
    }

    /*
     * We need to record the current object on the mutable list if
     *  (a) It is actually mutable, or
     *  (b) It contains pointers to a younger generation.
     * Case (b) arises if we didn't manage to promote everything that
     * the current object points to into the current generation.
     */
    if (rc->gc_thread->failed_to_evac) {
        rc->gc_thread->failed_to_evac = rtsFalse;
        if (bd->gen_no > 0) {
            recordMutableGen_GC((StgClosure *)q, bd->gen_no, rc->gc_thread);
        }
    }
  }

  if (p > bd->free)  {
      //rc->gc_thread->copied += ws->todo_free - bd->free;
      bd->free = p;
  }

  debugTrace(DEBUG_gc, "   scavenged %ld bytes",
             (unsigned long)((bd->free - bd->u.scan) * sizeof(W_)));

  // update stats: this is a block that has been scavenged
  rc->gc_thread->scanned += bd->free - bd->u.scan;
  bd->u.scan = bd->free;

  //if (bd != ws->todo_bd) {
      // we're not going to evac any more objects into
      // this block, so push it now.
  //    push_scanned_block(bd, ws);
  //}

  rc->gc_thread->scan_bd = NULL;
}
/* -----------------------------------------------------------------------------
   Scavenge everything on the mark stack.

   This is slightly different from scavenge():
      - we don't walk linearly through the objects, so the scavenger
        doesn't need to advance the pointer on to the next object.
   -------------------------------------------------------------------------- */
/*
static void
scavenge_mark_stack(ResourceContainer *rc, bdescr *mark_stack_bd, bdescr *mark_stack_top_bd, StgPtr mark_sp, gc_thread *rc->gc_thread)
{
    StgPtr p, q;
    StgInfoTable *info;
    rtsBool saved_eager_promotion;

    rc->gc_thread->evac_gen_no = oldest_gen->no;
    saved_eager_promotion = rc->gc_thread->eager_promotion;

    while ((p = pop_mark_stack(mark_stack_bd, mark_sp))) {

        ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure *)p);

        q = p;
        switch (info->type) {

        case MVAR_CLEAN:
        case MVAR_DIRTY:
        {
            StgMVar *mvar = ((StgMVar *)p);
            rc->gc_thread->eager_promotion = rtsFalse;
            evacuate_rc((StgClosure **)&mvar->head, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc((StgClosure **)&mvar->tail, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc((StgClosure **)&mvar->value, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            rc->gc_thread->eager_promotion = saved_eager_promotion;

            if (rc->gc_thread->failed_to_evac) {
                mvar->header.info = &stg_MVAR_DIRTY_info;
            } else {
                mvar->header.info = &stg_MVAR_CLEAN_info;
            }
            break;
        }

        case TVAR:
        {
            StgTVar *tvar = ((StgTVar *)p);
            rc->gc_thread->eager_promotion = rtsFalse;
            evacuate_rc((StgClosure **)&tvar->current_value, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc((StgClosure **)&tvar->first_watch_queue_entry, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            rc->gc_thread->eager_promotion = saved_eager_promotion;

            if (rc->gc_thread->failed_to_evac) {
                tvar->header.info = &stg_TVAR_DIRTY_info;
            } else {
                tvar->header.info = &stg_TVAR_CLEAN_info;
            }
            break;
        }

        case FUN_2_0:
            scavenge_fun_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgClosure *)p)->payload[1], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgClosure *)p)->payload[0], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case THUNK_2_0:
            scavenge_thunk_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgThunk *)p)->payload[1], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgThunk *)p)->payload[0], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case CONSTR_2_0:
            evacuate_rc(&((StgClosure *)p)->payload[1], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgClosure *)p)->payload[0], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case FUN_1_0:
        case FUN_1_1:
            scavenge_fun_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgClosure *)p)->payload[0], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case THUNK_1_0:
        case THUNK_1_1:
            scavenge_thunk_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&((StgThunk *)p)->payload[0], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case CONSTR_1_0:
        case CONSTR_1_1:
            evacuate_rc(&((StgClosure *)p)->payload[0], rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case FUN_0_1:
        case FUN_0_2:
            scavenge_fun_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case THUNK_0_1:
        case THUNK_0_2:
            scavenge_thunk_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case CONSTR_0_1:
        case CONSTR_0_2:
            break;

        case FUN:
            scavenge_fun_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            goto gen_obj;

        case THUNK:
        {
            StgPtr end;

            scavenge_thunk_srt(info, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgThunk *)p)->payload; p < end; p++) {
                evacuate_rc((StgClosure **)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            }
            break;
        }

        gen_obj:
        case CONSTR:
        case WEAK:
        case PRIM:
        {
            StgPtr end;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                evacuate_rc((StgClosure **)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            }
            break;
        }

        case BCO: {
            StgBCO *bco = (StgBCO *)p;
            evacuate_rc((StgClosure **)&bco->instrs, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc((StgClosure **)&bco->literals, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc((StgClosure **)&bco->ptrs, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;
        }

        case IND_PERM:
            // don't need to do anything here: the only possible case
            // is that we're in a 1-space compacting collector, with
            // no "old" generation.
            break;

        case IND:
        case BLACKHOLE:
            evacuate_rc(&((StgInd *)p)->indirectee, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case MUT_VAR_CLEAN:
        case MUT_VAR_DIRTY: {
            rc->gc_thread->eager_promotion = rtsFalse;
            evacuate_rc(&((StgMutVar *)p)->var, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            rc->gc_thread->eager_promotion = saved_eager_promotion;

            if (rc->gc_thread->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
            }
            break;
        }

        case BLOCKING_QUEUE:
        {
            StgBlockingQueue *bq = (StgBlockingQueue *)p;

            rc->gc_thread->eager_promotion = rtsFalse;
            evacuate_rc(&bq->bh, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&bq->owner, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&bq->queue, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            evacuate_rc(&bq->link, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            rc->gc_thread->eager_promotion = saved_eager_promotion;

            if (rc->gc_thread->failed_to_evac) {
                bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
            } else {
                bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
            }
            break;
        }

        case ARR_WORDS:
            break;

        case THUNK_SELECTOR:
        {
            StgSelector *s = (StgSelector *)p;
            evacuate_rc(&s->selectee, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;
        }

        // A chunk of stack saved in a heap object
        case AP_STACK:
        {
            StgAP_STACK *ap = (StgAP_STACK *)p;

            evacuate_rc(&ap->fun, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, rc,
                mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;
        }

        case PAP:
            scavenge_PAP((StgPAP *)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case AP:
            scavenge_AP((StgAP *)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;

        case MUT_ARR_PTRS_CLEAN:
        case MUT_ARR_PTRS_DIRTY:
            // follow everything
        {
            // We don't eagerly promote objects pointed to by a mutable
            // array, but if we find the array only points to objects in
            // the same or an older generation, we mark it "clean" and
            // avoid traversing it during minor GCs.
            rc->gc_thread->eager_promotion = rtsFalse;

            scavenge_mut_arr_ptrs((StgMutArrPtrs *)p, rc, mark_stack_bd,
                mark_stack_top_bd, mark_sp, rc->gc_thread);

            if (rc->gc_thread->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
            }

            rc->gc_thread->eager_promotion = saved_eager_promotion;
            rc->gc_thread->failed_to_evac = rtsTrue; // mutable anyhow.
            break;
        }

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            // follow everything
        {
            StgPtr q = p;

            scavenge_mut_arr_ptrs((StgMutArrPtrs *)p, rc, mark_stack_bd,
            mark_stack_top_bd, mark_sp, rc->gc_thread);

            // If we're going to put this object on the mutable list, then
            // set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
            if (rc->gc_thread->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
            }
            break;
        }

        case SMALL_MUT_ARR_PTRS_CLEAN:
        case SMALL_MUT_ARR_PTRS_DIRTY:
            // follow everything
        {
            StgPtr next;
            rtsBool saved_eager;

            // We don't eagerly promote objects pointed to by a mutable
            // array, but if we find the array only points to objects in
            // the same or an older generation, we mark it "clean" and
            // avoid traversing it during minor GCs.
            saved_eager = rc->gc_thread->eager_promotion;
            rc->gc_thread->eager_promotion = rtsFalse;
            next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
            for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
                evacuate_rc((StgClosure **)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            }
            rc->gc_thread->eager_promotion = saved_eager;

            if (rc->gc_thread->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_DIRTY_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_CLEAN_info;
            }

            rc->gc_thread->failed_to_evac = rtsTrue; // mutable anyhow.
            break;
        }

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
            // follow everything
        {
            StgPtr next, q = p;

            next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
            for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
                evacuate_rc((StgClosure **)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            }

            // If we're going to put this object on the mutable list, then
            // set its info ptr to SMALL_MUT_ARR_PTRS_FROZEN0 to indicate that.
            if (rc->gc_thread->failed_to_evac) {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN0_info;
            } else {
                ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_info;
            }
            break;
        }

        case TSO:
        {
            scavengeTSO((StgTSO*)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            break;
        }

        case STACK:
        {
            StgStack *stack = (StgStack*)p;

            rc->gc_thread->eager_promotion = rtsFalse;

            scavenge_stack(stack->sp, stack->stack + stack->stack_size, rc,
                mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            stack->dirty = rc->gc_thread->failed_to_evac;

            rc->gc_thread->eager_promotion = saved_eager_promotion;
            break;
        }

        case MUT_PRIM:
        {
            StgPtr end;

            rc->gc_thread->eager_promotion = rtsFalse;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                evacuate_rc((StgClosure **)p, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            }

            rc->gc_thread->eager_promotion = saved_eager_promotion;
            rc->gc_thread->failed_to_evac = rtsTrue; // mutable
            break;
        }

        case TREC_CHUNK:
          {
            StgWord i;
            StgTRecChunk *tc = ((StgTRecChunk *) p);
            TRecEntry *e = &(tc -> entries[0]);
            rc->gc_thread->eager_promotion = rtsFalse;
            evacuate_rc((StgClosure **)&tc->prev_chunk, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
              evacuate_rc((StgClosure **)&e->tvar, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
              evacuate_rc((StgClosure **)&e->expected_value, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
              evacuate_rc((StgClosure **)&e->new_value, rc, mark_stack_bd, mark_stack_top_bd, mark_sp, rc->gc_thread);
            }
            rc->gc_thread->eager_promotion = saved_eager_promotion;
            rc->gc_thread->failed_to_evac = rtsTrue; // mutable
            break;
          }

        default:
            barf("scavenge_mark_stack: unimplemented/strange closure type %d @ %p",
                 info->type, p);
        }

        if (rc->gc_thread->failed_to_evac) {
            rc->gc_thread->failed_to_evac = rtsFalse;
            if (rc->gc_thread->evac_gen_no) {
                recordMutableGen_GC((StgClosure *)q, rc->gc_thread->evac_gen_no, rc->gc_thread);
            }
        }
    } // while (p = pop_mark_stack())
}
*/

/* -----------------------------------------------------------------------------
   Scavenge one object.

   This is used for objects that are temporarily marked as mutable
   because they contain old-to-new generation pointers.  Only certain
   objects can have this property.
   -------------------------------------------------------------------------- */

static rtsBool
scavenge_one(StgPtr p, ResourceContainer *rc)
{
    const StgInfoTable *info;
    rtsBool no_luck;
    rtsBool saved_eager_promotion;

    saved_eager_promotion = rc->gc_thread->eager_promotion;

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl((StgClosure *)p);

    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
        StgMVar *mvar = ((StgMVar *)p);
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc((StgClosure **)&mvar->head, rc);
        evacuate_rc((StgClosure **)&mvar->tail, rc);
        evacuate_rc((StgClosure **)&mvar->value, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            mvar->header.info = &stg_MVAR_DIRTY_info;
        } else {
            mvar->header.info = &stg_MVAR_CLEAN_info;
        }
        break;
    }

    case TVAR:
    {
        StgTVar *tvar = ((StgTVar *)p);
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc((StgClosure **)&tvar->current_value, rc);
        evacuate_rc((StgClosure **)&tvar->first_watch_queue_entry, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            tvar->header.info = &stg_TVAR_DIRTY_info;
        } else {
            tvar->header.info = &stg_TVAR_CLEAN_info;
        }
        break;
    }

    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    {
        StgPtr q, end;

        end = (StgPtr)((StgThunk *)p)->payload + info->layout.payload.ptrs;
        for (q = (StgPtr)((StgThunk *)p)->payload; q < end; q++) {
            evacuate_rc((StgClosure **)q, rc);
        }
        break;
    }

    case FUN:
    case FUN_1_0:                       // hardly worth specialising these guys
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case PRIM:
    case IND_PERM:
    {
        StgPtr q, end;

        end = (StgPtr)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (q = (StgPtr)((StgClosure *)p)->payload; q < end; q++) {
            evacuate_rc((StgClosure **)q, rc);
        }
        break;
    }

    case WEAK:
        scavengeLiveWeak((StgWeak *)p);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
        StgPtr q = p;

        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc(&((StgMutVar *)p)->var, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
        }
        break;
    }

    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;

        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc(&bq->bh, rc);
        evacuate_rc((StgClosure**)&bq->owner, rc);
        evacuate_rc((StgClosure**)&bq->queue, rc);
        evacuate_rc((StgClosure**)&bq->link, rc);
        rc->gc_thread->eager_promotion = saved_eager_promotion;

        if (rc->gc_thread->failed_to_evac) {
            bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
        } else {
            bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
        }
        break;
    }

    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)p;
        evacuate_rc(&s->selectee, rc);
        break;
    }

    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)p;

        evacuate_rc(&ap->fun, rc);
        scavenge_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, rc);
        p = (StgPtr)ap->payload + ap->size;
        break;
    }

    case PAP:
        p = scavenge_PAP((StgPAP *)p, rc);
        break;

    case AP:
        p = scavenge_AP((StgAP *)p, rc);
        break;

    case ARR_WORDS:
        // nothing to follow
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        rc->gc_thread->eager_promotion = rtsFalse;

        scavenge_mut_arr_ptrs((StgMutArrPtrs *)p, rc);

        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
        }

        rc->gc_thread->eager_promotion = saved_eager_promotion;
        rc->gc_thread->failed_to_evac = rtsTrue;
        break;
    }

    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
    {
        // follow everything
        scavenge_mut_arr_ptrs((StgMutArrPtrs *)p, rc);

        // If we're going to put this object on the mutable list, then
        // set its info ptr to MUT_ARR_PTRS_FROZEN0 to indicate that.
        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
        } else {
            ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
        }
        break;
    }

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    {
        StgPtr next, q;
        rtsBool saved_eager;

        // We don't eagerly promote objects pointed to by a mutable
        // array, but if we find the array only points to objects in
        // the same or an older generation, we mark it "clean" and
        // avoid traversing it during minor GCs.
        saved_eager = rc->gc_thread->eager_promotion;
        rc->gc_thread->eager_promotion = rtsFalse;
        q = p;
        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate_rc((StgClosure**)p, rc);
        }
        rc->gc_thread->eager_promotion = saved_eager;

        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_DIRTY_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_CLEAN_info;
        }

        rc->gc_thread->failed_to_evac = rtsTrue;
        break;
    }

    case SMALL_MUT_ARR_PTRS_FROZEN:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
    {
        // follow everything
        StgPtr next, q=p;

        next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
        for (p = (P_)((StgSmallMutArrPtrs *)p)->payload; p < next; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }

        // If we're going to put this object on the mutable list, then
        // set its info ptr to SMALL_MUT_ARR_PTRS_FROZEN0 to indicate that.
        if (rc->gc_thread->failed_to_evac) {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN0_info;
        } else {
            ((StgClosure *)q)->header.info = &stg_SMALL_MUT_ARR_PTRS_FROZEN_info;
        }
        break;
    }

    case TSO:
    {
        scavengeTSO((StgTSO*)p, rc);
        break;
    }

    case STACK:
    {
        StgStack *stack = (StgStack*)p;

        rc->gc_thread->eager_promotion = rtsFalse;

        scavenge_stack(stack->sp, stack->stack + stack->stack_size, rc);
        stack->dirty = rc->gc_thread->failed_to_evac;

        rc->gc_thread->eager_promotion = saved_eager_promotion;
        break;
    }

    case MUT_PRIM:
    {
        StgPtr end;

        rc->gc_thread->eager_promotion = rtsFalse;

        end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
        for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
            evacuate_rc((StgClosure **)p, rc);
        }

        rc->gc_thread->eager_promotion = saved_eager_promotion;
        rc->gc_thread->failed_to_evac = rtsTrue; // mutable
        break;

    }

    case TREC_CHUNK:
      {
        StgWord i;
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        TRecEntry *e = &(tc -> entries[0]);
        rc->gc_thread->eager_promotion = rtsFalse;
        evacuate_rc((StgClosure **)&tc->prev_chunk, rc);
        for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
          evacuate_rc((StgClosure **)&e->tvar, rc);
          evacuate_rc((StgClosure **)&e->expected_value, rc);
          evacuate_rc((StgClosure **)&e->new_value, rc);
        }
        rc->gc_thread->eager_promotion = saved_eager_promotion;
        rc->gc_thread->failed_to_evac = rtsTrue; // mutable
        break;
      }

    case IND:
        // IND can happen, for example, when the interpreter allocates
        // a gigantic AP closure (more than one block), which ends up
        // on the large-object list and then gets updated.  See #3424.
    case BLACKHOLE:
    case IND_STATIC:
        evacuate_rc(&((StgInd *)p)->indirectee, rc);

#if 0 && defined(DEBUG)
      if (RtsFlags.DebugFlags.gc)
      /* Debugging code to print out the size of the thing we just
       * promoted
       */
      {
        StgPtr start = gen->scan;
        bdescr *start_bd = gen->scan_bd;
        StgWord size = 0;
        scavenge(&gen);
        if (start_bd != gen->scan_bd) {
          size += (P_)BLOCK_ROUND_UP(start) - start;
          start_bd = start_bd->link;
          while (start_bd != gen->scan_bd) {
            size += BLOCK_SIZE_W;
            start_bd = start_bd->link;
          }
          size += gen->scan -
            (P_)BLOCK_ROUND_DOWN(gen->scan);
        } else {
          size = gen->scan - start;
        }
        debugBelch("evac IND_OLDGEN: %ld bytes", size * sizeof(W_));
      }
#endif
      break;

    default:
        barf("scavenge_one: strange object %d", (int)(info->type));
    }

    no_luck = rc->gc_thread->failed_to_evac;
    rc->gc_thread->failed_to_evac = rtsFalse;
    return (no_luck);
}

/* -----------------------------------------------------------------------------
   Scavenging mutable lists.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

void
scavenge_mutable_list(ResourceContainer *rc, bdescr *bd, generation *gen)
{
    StgPtr p, q;
    nat gen_no;

    gen_no = gen->no;
    rc->gc_thread->evac_gen_no = gen_no;
    for (; bd != NULL; bd = bd->link) {
        for (q = bd->start; q < bd->free; q++) {
            p = (StgPtr)*q;
            ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));

#ifdef DEBUG
            // RC TODO: Get rid of these???
            switch (get_itbl((StgClosure *)p)->type) {
            case MUT_VAR_CLEAN:
                // can happen due to concurrent writeMutVars
            case MUT_VAR_DIRTY:
                mutlist_MUTVARS++; break;
            case MUT_ARR_PTRS_CLEAN:
            case MUT_ARR_PTRS_DIRTY:
            case MUT_ARR_PTRS_FROZEN:
            case MUT_ARR_PTRS_FROZEN0:
                mutlist_MUTARRS++; break;
            case MVAR_CLEAN:
                barf("MVAR_CLEAN on mutable list");
            case MVAR_DIRTY:
                mutlist_MVARS++; break;
            case TVAR:
                mutlist_TVAR++; break;
            case TREC_CHUNK:
                mutlist_TREC_CHUNK++; break;
            case MUT_PRIM:
                if (((StgClosure*)p)->header.info == &stg_TVAR_WATCH_QUEUE_info)
                    mutlist_TVAR_WATCH_QUEUE++;
                else if (((StgClosure*)p)->header.info == &stg_TREC_HEADER_info)
                    mutlist_TREC_HEADER++;
                else if (((StgClosure*)p)->header.info == &stg_ATOMIC_INVARIANT_info)
                    mutlist_ATOMIC_INVARIANT++;
                else if (((StgClosure*)p)->header.info == &stg_INVARIANT_CHECK_QUEUE_info)
                    mutlist_INVARIANT_CHECK_QUEUE++;
                else
                    mutlist_OTHERS++;
                break;
            default:
                mutlist_OTHERS++; break;
            }
#endif

            // Check whether this object is "clean", that is it
            // definitely doesn't point into a young generation.
            // Clean objects don't need to be scavenged.  Some clean
            // objects (MUT_VAR_CLEAN) are not kept on the mutable
            // list at all; others, such as MUT_ARR_PTRS
            // are always on the mutable list.
            //
            switch (get_itbl((StgClosure *)p)->type) {
            case MUT_ARR_PTRS_CLEAN:
                recordMutableGen_GC((StgClosure *)p,gen_no,rc->gc_thread);
                continue;
            case MUT_ARR_PTRS_DIRTY:
            {
                rtsBool saved_eager_promotion;
                saved_eager_promotion = rc->gc_thread->eager_promotion;
                rc->gc_thread->eager_promotion = rtsFalse;

                scavenge_mut_arr_ptrs_marked((StgMutArrPtrs *)p, rc);

                if (rc->gc_thread->failed_to_evac) {
                    ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
                } else {
                    ((StgClosure *)p)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
                }

                rc->gc_thread->eager_promotion = saved_eager_promotion;
                rc->gc_thread->failed_to_evac = rtsFalse;
                recordMutableGen_GC((StgClosure *)p,gen_no,rc->gc_thread);
                continue;
            }
            default:
                ;
            }

            if (scavenge_one(p, rc)) {
                // didn't manage to promote everything, so put the
                // object back on the list.
                recordMutableGen_GC((StgClosure *)p,gen_no,rc->gc_thread);
            }
        }
    }
}
/*
void
scavenge_capability_mut_lists (Capability *cap)
{
    nat g;

    ResourceContainer *rc = cap->r.rCurrentTSO->rc;*/
    /* Mutable lists from each generation > N
     * we want to *scavenge* these roots, not evacuate them: they're not
     * going to move in this GC.
     * Also do them in reverse generation order, for the usual reason:
     * namely to reduce the likelihood of spurious old->new pointers.
     *//*
    for (g = RtsFlags.GcFlags.generations-1; g > N; g--) {
        scavenge_mutable_list(rc->saved_mut_lists[g], &generations[g]);
        freeChain_sync(rc->saved_mut_lists[g]);
        rc->saved_mut_lists[g] = NULL;
    }
}
*/

void
scavenge_rc_mut_lists (ResourceContainer *rc)
{
    nat g;
    /* Mutable lists from each generation > N
     * we want to *scavenge* these roots, not evacuate them: they're not
     * going to move in this GC.
     * Also do them in reverse generation order, for the usual reason:
     * namely to reduce the likelihood of spurious old->new pointers.
     */
    for (g = numGenerations-1; g > N; g--) {
        scavenge_mutable_list(rc, rc->saved_mut_lists[g], rc->generations[g]);
        freeChain_sync(rc->saved_mut_lists[g]);
        rc->saved_mut_lists[g] = NULL;
    }
}

/* -----------------------------------------------------------------------------
   Scavenging the static objects.

   We treat the mutable list of each generation > N (i.e. all the
   generations older than the one being collected) as roots.  We also
   remove non-mutable objects from the mutable list at this point.
   -------------------------------------------------------------------------- */

static void
scavenge_static(ResourceContainer *rc)
{
  StgClosure *flagged_p, *p;
  const StgInfoTable *info;

  debugTrace(DEBUG_gc, "scavenging static objects");

  /* Always evacuate straight to the oldest generation for static
   * objects */
  rc->gc_thread->evac_gen_no = rc->generations[1]->no;

  /* keep going until we've scavenged all the objects on the linked
     list... */

  while (1) {

    /* get the next static object from the list.  Remember, there might
     * be more stuff on this list after each evacuation...
     * (static_objects is a global)
     */
    flagged_p = rc->gc_thread->static_objects;
    if (flagged_p == END_OF_STATIC_OBJECT_LIST) {
          break;
    }
    p = UNTAG_STATIC_LIST_PTR(flagged_p);

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    info = get_itbl(p);
    /*
        if (info->type==RBH)
        info = REVERT_INFOPTR(info); // if it's an RBH, look at the orig closure
    */
    // make sure the info pointer is into text space

    /* Take this object *off* the static_objects list,
     * and put it on the scavenged_static_objects list.
     */
    rc->gc_thread->static_objects = *STATIC_LINK(info,p);
    *STATIC_LINK(info,p) = rc->gc_thread->scavenged_static_objects;
    rc->gc_thread->scavenged_static_objects = flagged_p;

    switch (info -> type) {

    case IND_STATIC:
      {
        StgInd *ind = (StgInd *)p;
        evacuate_rc(&ind->indirectee, rc);

        /* might fail to evacuate it, in which case we have to pop it
         * back on the mutable list of the oldest generation.  We
         * leave it *on* the scavenged_static_objects list, though,
         * in case we visit this object again.
         */
        if (rc->gc_thread->failed_to_evac) {
          rc->gc_thread->failed_to_evac = rtsFalse;
          recordMutableGen_GC((StgClosure *)p,rc->generations[1],rc->gc_thread);
        }
        break;
      }

    case THUNK_STATIC:
      scavenge_thunk_srt(info, rc);
      break;

    case FUN_STATIC:
      scavenge_fun_srt(info, rc);
      break;

    case CONSTR_STATIC:
      {
        StgPtr q, next;

        next = (P_)p->payload + info->layout.payload.ptrs;
        // evacuate the pointers
        for (q = (P_)p->payload; q < next; q++) {
            evacuate_rc((StgClosure **)q, rc);
        }
        break;
      }

    default:
      barf("scavenge_static: strange closure %d", (int)(info->type));
    }

    ASSERT(rc->gc_thread->failed_to_evac == rtsFalse);
  }
}

/* -----------------------------------------------------------------------------
   scavenge a chunk of memory described by a bitmap
   -------------------------------------------------------------------------- */

static void
scavenge_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, StgWord size,
    ResourceContainer *rc)
{
    nat i, j, b;
    StgWord bitmap;

    b = 0;

    for (i = 0; i < size; b++) {
        bitmap = large_bitmap->bitmap[b];
        j = stg_min(size-i, BITS_IN(W_));
        i += j;
        for (; j > 0; j--, p++) {
            if ((bitmap & 1) == 0) {
                evacuate_rc((StgClosure **)p, rc);
            }
            bitmap = bitmap >> 1;
        }
    }
}


/* -----------------------------------------------------------------------------
   scavenge_stack walks over a section of stack and evacuates all the
   objects pointed to by it.  We can use the same code for walking
   AP_STACK_UPDs, since these are just sections of copied stack.
   -------------------------------------------------------------------------- */

static void
scavenge_stack(StgPtr p, StgPtr stack_end, ResourceContainer *rc)
{
  const StgRetInfoTable* info;
  StgWord bitmap;
  StgWord size;

  /*
   * Each time around this loop, we are looking at a chunk of stack
   * that starts with an activation record.
   */

  while (p < stack_end) {
    info  = get_ret_itbl((StgClosure *)p);

    switch (info->i.type) {

    case UPDATE_FRAME:
        // Note [upd-black-hole]
        // In SMP, we can get update frames that point to indirections
        // when two threads evaluate the same thunk.  We do attempt to
        // discover this situation in threadPaused(), but it's
        // possible that the following sequence occurs:
        //
        //        A             B
        //                  enter T
        //     enter T
        //     blackhole T
        //                  update T
        //     GC
        //
        // Now T is an indirection, and the update frame is already
        // marked on A's stack, so we won't traverse it again in
        // threadPaused().  We could traverse the whole stack again
        // before GC, but that seems like overkill.
        //
        // Scavenging this update frame as normal would be disastrous;
        // the updatee would end up pointing to the value.  So we
        // check whether the value after evacuation is a BLACKHOLE,
        // and if not, we change the update frame to an stg_enter
        // frame that simply returns the value.  Hence, blackholing is
        // compulsory (otherwise we would have to check for thunks
        // too).
        //
        // One slight hiccup is that the THUNK_SELECTOR machinery can
        // overwrite the updatee with an IND.  In parallel GC, this
        // could even be happening concurrently, so we can't check for
        // the IND.  Fortunately if we assume that blackholing is
        // happening (either lazy or eager), then we can be sure that
        // the updatee is never a THUNK_SELECTOR and we're ok.
        // NB. this is a new invariant: blackholing is not optional.
    {
        StgUpdateFrame *frame = (StgUpdateFrame *)p;
        StgClosure *v;

        evacuate_rc(&frame->updatee, rc);
        v = frame->updatee;
        if (GET_CLOSURE_TAG(v) != 0 ||
            (get_itbl(v)->type != BLACKHOLE)) {
            // blackholing is compulsory, see above.
            frame->header.info = (const StgInfoTable*)&stg_enter_checkbh_info;
        }
        ASSERT(v->header.info != &stg_TSO_info);
        p += sizeofW(StgUpdateFrame);
        continue;
    }

      // small bitmap (< 32 entries, or 64 on a 64-bit machine)
    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case CATCH_FRAME:
    case RET_SMALL:
        bitmap = BITMAP_BITS(info->i.layout.bitmap);
        size   = BITMAP_SIZE(info->i.layout.bitmap);
        // NOTE: the payload starts immediately after the info-ptr, we
        // don't have an StgHeader in the same sense as a heap closure.
        p++;
        p = scavenge_small_bitmap(p, size, bitmap, rc);

    follow_srt:
        if (rtsTrue)
            scavenge_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap, rc);
        continue;

    case RET_BCO: {
        StgBCO *bco;
        StgWord size;

        p++;
        evacuate_rc((StgClosure **)p, rc);
        bco = (StgBCO *)*p;
        p++;
        size = BCO_BITMAP_SIZE(bco);
        scavenge_large_bitmap(p, BCO_BITMAP(bco), size, rc);
        p += size;
        continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
    case RET_BIG:
    {
        StgWord size;

        size = GET_LARGE_BITMAP(&info->i)->size;
        p++;
        scavenge_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size, rc);
        p += size;
        // and don't forget to follow the SRT
        goto follow_srt;
    }

    case RET_FUN:
    {
        StgRetFun *ret_fun = (StgRetFun *)p;
        StgFunInfoTable *fun_info;

        evacuate_rc(&ret_fun->fun, rc);
        fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
        p = scavenge_arg_block(fun_info, ret_fun->payload, rc);
        goto follow_srt;
    }

    default:
        barf("scavenge_stack: weird activation record found on stack: %d", (int)(info->i.type));
    }
  }
}

/*-----------------------------------------------------------------------------
  scavenge the large object list.

  evac_gen set by caller; similar games played with evac_gen as with
  scavenge() - see comment at the top of scavenge().  Most large
  objects are (repeatedly) mutable, so most of the time evac_gen will
  be zero.
  --------------------------------------------------------------------------- */

static void
scavenge_large (gen_workspace *ws, ResourceContainer *rc)
{
    bdescr *bd;
    StgPtr p;

    rc->gc_thread->evac_gen_no = ws->gen->no;

    bd = ws->todo_large_objects;

    for (; bd != NULL; bd = ws->todo_large_objects) {

        // take this object *off* the large objects list and put it on
        // the scavenged large objects list.  This is so that we can
        // treat new_large_objects as a stack and push new objects on
        // the front when evacuating.
        ws->todo_large_objects = bd->link;

        ACQUIRE_SPIN_LOCK(&ws->gen->sync);
        dbl_link_onto(bd, &ws->gen->scavenged_large_objects);
        ws->gen->n_scavenged_large_blocks += bd->blocks;
        RELEASE_SPIN_LOCK(&ws->gen->sync);

        p = bd->start;
        if (scavenge_one(p, rc)) {
            if (ws->gen->no > 0) {
                recordMutableGen_GC((StgClosure *)p, ws->gen->no, rc->gc_thread);
            }
        }

        // stats
        rc->gc_thread->scanned += closure_sizeW((StgClosure*)p);
    }
}

/* ----------------------------------------------------------------------------
   Look for work to do.

   We look for the oldest gen that has either a todo block that can
   be scanned, or a block of work on the global queue that we can
   scan.

   It is important to take work from the *oldest* generation that we
   has work available, because that minimizes the likelihood of
   evacuating objects into a young generation when they should have
   been eagerly promoted.  This really does make a difference (the
   cacheprof benchmark is one that is affected).

   We also want to scan the todo block if possible before grabbing
   work from the global queue, the reason being that we don't want to
   steal work from the global queue and starve other threads if there
   is other work we can usefully be doing.
   ------------------------------------------------------------------------- */

static rtsBool
scavenge_find_work (ResourceContainer *rc)
{
    int g;
    gen_workspace *ws;
    rtsBool did_something, did_anything;
    bdescr *bd;

    rc->gc_thread->scav_find_work++;

    did_anything = rtsFalse;

loop:
    did_something = rtsFalse;
    for (g = RtsFlags.GcFlags.generations-1; g >= 0; g--) {
        ws = &rc->gc_thread->gens[g];

        rc->gc_thread->scan_bd = NULL;

        // If we have a scan block with some work to do,
        // scavenge everything up to the free pointer.
        if (ws->todo_bd->u.scan < ws->todo_free)
        {
            scavenge_block(rc, ws->todo_bd);
            did_something = rtsTrue;
            break;
        }

        // If we have any large objects to scavenge, do them now.
        if (ws->todo_large_objects) {
            scavenge_large(ws, rc);
            did_something = rtsTrue;
            break;
        }

        if ((bd = grab_local_todo_block(ws)) != NULL) {
            scavenge_block(rc, bd);
            did_something = rtsTrue;
            break;
        }
    }

    if (did_something) {
        did_anything = rtsTrue;
        goto loop;
    }

#if defined(THREADED_RTS)
    if (work_stealing) {
        // look for work to steal
        for (g = RtsFlags.GcFlags.generations-1; g >= 0; g--) {
            if ((bd = steal_todo_block(rc, g)) != NULL) {
                scavenge_block(rc, bd);
                did_something = rtsTrue;
                break;
            }
        }

        if (did_something) {
            did_anything = rtsTrue;
            goto loop;
        }
    }
#endif

    // only return when there is no more work to do

    return did_anything;
}

/* ----------------------------------------------------------------------------
   Scavenge until we can't find anything more to scavenge.
   ------------------------------------------------------------------------- */

void
scavenge_loop(ResourceContainer *rc)
{
    rtsBool work_to_do;

loop:
    work_to_do = rtsFalse;

    // scavenge static objects
    if (major_gc && rc->gc_thread->static_objects != END_OF_STATIC_OBJECT_LIST) {
        IF_DEBUG(sanity, checkStaticObjects(rc->gc_thread->static_objects));
        scavenge_static(rc);
    }

    // scavenge objects in compacted generation
    //if (mark_stack_bd != NULL && !mark_stack_empty(mark_stack_bd, mark_sp)) {
    //    scavenge_mark_stack(rc);
    //    work_to_do = rtsTrue;
    //}

    // Order is important here: we want to deal in full blocks as
    // much as possible, so go for global work in preference to
    // local work.  Only if all the global work has been exhausted
    // do we start scavenging the fragments of blocks in the local
    // workspaces.
    if (scavenge_find_work(rc)) goto loop;

    if (work_to_do) goto loop;
}
