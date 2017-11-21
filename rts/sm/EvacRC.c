#include "PosixSource.h"
#include "Rts.h"

#include "EvacRC.h"
#include "Storage.h"
#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"
#include "GCUtils.h"
#include "Compact.h"
#include "MarkStack.h"
#include "Prelude.h"
#include "Trace.h"
#include "LdvProfile.h"
#include "ResourceLimits.h"

void evac_PAP_payload(StgClosure *fun, StgClosure **payload, StgWord size, ResourceContainer *rc);
void evac_stack(StgPtr p, StgPtr stack_end, ResourceContainer *rc);
StgPtr evac_mut_arr_ptrs(StgMutArrPtrs *ptrs, ResourceContainer *rc);
void evacTSO (StgTSO *tso, ResourceContainer *rc);
void evac_static_object (StgClosure **link_field, StgClosure *q, ResourceContainer *rc);
void evac_fun_srt(const StgInfoTable *info, ResourceContainer *rc);
void evac_thunk_srt(const StgInfoTable *info, ResourceContainer *rc);
void evac_srt (StgClosure **srt, nat srt_bitmap, ResourceContainer *rc);




void
copy_meh(StgClosure **p, const StgInfoTable *info,
     StgClosure *src, nat size, StgWord tag, ResourceContainer *rc)
{
  StgPtr to, from, lim;
  nat i;

  bdescr *bdescr_to = rc->currentCopy;

  lim = bdescr_to->start + BLOCK_SIZE_W;

  if (size + bdescr_to->free > lim) {
    // We need to grab the next block in the to-space to copy_meh into
    if (bdescr_to->link == NULL) {
      barf("GC ran out of space!");
    }
    // set the block and reset its starting point
    bdescr_to = bdescr_to->link;
    bdescr_to->free = bdescr_to->start;
  }

  // Now we have our destination block and the point to copy_meh to
  // Lets actually do the copy_meh
  from = (StgPtr)src;
  to = bdescr_to->free;

  to[0] = (W_)info;
  for (i = 1; i < size; i++) { // unroll for small i
    to[i] = from[i];
  }

  // Now everthing is copied
  // Update the from space with a forwarding ptr
  src->header.info = (const StgInfoTable *)MK_FORWARDING_PTR(to);
  if(tag != NULL) {
    *p = TAG_CLOSURE(tag,(StgClosure*)to);
  }
}

void
evac_rc(StgClosure **p, ResourceContainer *rc)
{
  bdescr *bd;
  StgClosure *q;
  const StgInfoTable *info;
  StgWord tag;

  q = *p;

loop:
  tag = GET_CLOSURE_TAG(q);
  q = UNTAG_CLOSURE(q);

  info = q->header.info;

  if(IS_FORWARDING_PTR(info)) {
    debugTrace(DEBUG_gc, "Not copy_ing a forwarding pointer");
    return;
  }

  ASSERTM(LOOKS_LIKE_CLOSURE_PTR(q), "invalid closure, info=%p", q->header.info);

  if (!HEAP_ALLOCED_GC(q)) {

    info = get_itbl(q);

    switch (info->type) {

      case THUNK_STATIC:
      {
        if (info->srt_bitmap != 0) {
          evac_static_object(THUNK_STATIC_LINK((StgClosure *)q), q, rc);
        }
        return;
      }
      case FUN_STATIC:
      {
        if (info->srt_bitmap != 0) {
          evac_static_object(FUN_STATIC_LINK((StgClosure *)q), q, rc);
        }
        return;
      }
      case IND_STATIC:
        evac_static_object(IND_STATIC_LINK((StgClosure *)q), q, rc);
        return;
      case CONSTR_STATIC:
        evac_static_object(STATIC_LINK(info,(StgClosure *)q), q, rc);
        return;
      case CONSTR_NOCAF_STATIC:
        return;
      default:
        barf("evacuate(static): strange closure type %d", (int)(info->type));
    }
  }

  bd = Bdescr((P_)q);
  
  // We dont want to follow cross references yet.
  // Lets just think about the single threaded problem for now
  if (bd->rc != rc) {
    barf("Cross references not supported yet");
  }

  switch (INFO_PTR_TO_STRUCT(info)->type) {

    case WHITEHOLE:
      goto loop;

    // 0_1's
    case CONSTR_0_1:
    case FUN_0_1:
      copy_meh(p,info,q,sizeofW(StgHeader)+1,tag,rc);
      return;
    case THUNK_0_1:
      copy_meh(p,info,q,sizeofW(StgThunk)+1,NULL,rc);
      return;

    // 1_0's
    case CONSTR_1_0:
      evac_rc(&((StgClosure *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgHeader)+1,tag,rc);
      return;
    case THUNK_1_0:
      evac_rc(&((StgThunk *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgThunk)+1,NULL,rc);
      return;
    case FUN_1_0:
      evac_rc(&((StgClosure *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgHeader)+1,tag,rc);
      return;

    // 2_0's
    case CONSTR_2_0:
      evac_rc(&((StgClosure *)q)->payload[1],rc);
      evac_rc(&((StgClosure *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgHeader)+2,tag,rc);
      return;
    case THUNK_2_0:
      evac_rc(&((StgThunk *)q)->payload[0],rc);
      evac_rc(&((StgThunk *)q)->payload[1],rc);
      copy_meh(p,info,q,sizeofW(StgThunk)+2,NULL,rc);
      return;
    case FUN_2_0:
      evac_rc(&((StgClosure *)q)->payload[0],rc);
      evac_rc(&((StgClosure *)q)->payload[1],rc);
      copy_meh(p,info,q,sizeofW(StgHeader)+2,tag,rc);
      return;

    // 1_1's
    case CONSTR_1_1:
      evac_rc(&((StgClosure *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgHeader)+2,tag,rc);
      return;
    case THUNK_1_1:
      evac_rc(&((StgThunk *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgThunk)+2,NULL,rc);
      return;
    case FUN_1_1:
      evac_rc(&((StgClosure *)q)->payload[0],rc);
      copy_meh(p,info,q,sizeofW(StgHeader)+2,tag,rc);
      return;

    // 0_2's
    case CONSTR_0_2:
      copy_meh(p,info,q,sizeofW(StgHeader)+2,tag,rc);
      return;
    case THUNK_0_2:
      copy_meh(p,info,q,sizeofW(StgThunk)+2,NULL,rc);
      return;
    case FUN_0_2:
      copy_meh(p,info,q,sizeofW(StgHeader)+2,tag,rc);
      return;

    // bland
    case CONSTR:
    {
      W_ size = sizeW_fromITBL(INFO_PTR_TO_STRUCT(info));
      StgPtr end, pp;

      end = (P_)((StgClosure *)q)->payload + info->layout.payload.ptrs;
      for (pp = (P_)((StgClosure *)q)->payload; pp < end; pp++) {
          evac_rc((StgClosure **)pp,rc);
      }
      copy_meh(p,info,q,size,tag,rc);
      return;
    }
    case FUN:
    {
      W_ size = sizeW_fromITBL(INFO_PTR_TO_STRUCT(info));
      evac_fun_srt(info, rc);
      copy_meh(p,info,q,size,tag,rc);
      return;
    }
    case THUNK:
    {
      W_ size = thunk_sizeW_fromITBL(INFO_PTR_TO_STRUCT(info));

      StgPtr end, pp;

      evac_thunk_srt(info, rc);
      end = (P_)((StgThunk *)q)->payload + info->layout.payload.ptrs;
      for (pp = (P_)((StgThunk *)q)->payload; pp < end; pp++) {
          evac_rc((StgClosure **)pp,rc);
      }
      copy_meh(p,info,q,size,NULL,rc);
      return;
    }
    case BCO:
    {
      StgBCO *bco = (StgBCO *)q;
      evac_rc((StgClosure **)&bco->instrs,rc);
      evac_rc((StgClosure **)&bco->literals,rc);
      evac_rc((StgClosure **)&bco->ptrs,rc);
      copy_meh(p,info,q,bco_sizeW((StgBCO *)q),NULL,rc);
      return;
    }

    case AP:
    {
      StgAP *ap = (StgAP *)q;
      evac_rc(&ap->fun, rc);
      evac_PAP_payload(ap->fun, ap->payload, ap->n_args, rc);
      copy_meh(p,info,q,ap_sizeW(ap),NULL,rc);
      return;
    }
    case PAP:
    {
      StgPAP *pap = (StgPAP *)q;
      evac_rc(&pap->fun, rc);
      evac_PAP_payload(pap->fun, pap->payload, pap->n_args, rc);
      copy_meh(p,info,q,pap_sizeW(pap),NULL,rc);
      return;
    }
    case AP_STACK:
    {
      StgAP_STACK *ap = (StgAP_STACK *)q;
      evac_rc(&ap->fun,rc);
      evac_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, rc);
      copy_meh(p,info,q,ap_stack_sizeW(ap),NULL,rc);
      return;
    }
    case IND:
    {
      q = ((StgInd *)q)->indirectee;
      *p = q;
      goto loop;
    }
    case IND_PERM:
    {
      evac_rc(&((StgInd *)q)->indirectee, rc);
      copy_meh(p,info,q,sizeW_fromITBL(INFO_PTR_TO_STRUCT(info)),NULL,rc);
      return;
    }
    case RET_BCO:
    case RET_SMALL:
    case RET_BIG:
    case UPDATE_FRAME:
    case CATCH_FRAME:
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case CATCH_RETRY_FRAME:
    case CATCH_STM_FRAME:
    case ATOMICALLY_FRAME:
      barf("Bad stack frames in evacuate resource container at %p\n", q);
    case BLOCKING_QUEUE:
    {
      StgBlockingQueue *bq = (StgBlockingQueue *)q;
      evac_rc(&bq->bh,rc);
      evac_rc((StgClosure**)&bq->owner,rc);
      evac_rc((StgClosure**)&bq->queue,rc);
      evac_rc((StgClosure**)&bq->link,rc);
      if (rc->failed_to_evac) {
        bq->header.info = &stg_BLOCKING_QUEUE_DIRTY_info;
      } else {
        bq->header.info = &stg_BLOCKING_QUEUE_CLEAN_info;
      }
      copy_meh(p,info,q,sizeW_fromITBL(info),NULL,rc);
      return;
    }
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    {
      evac_rc(&((StgMutVar *)q)->var,rc);
      if (rc->failed_to_evac) {
        ((StgClosure *)q)->header.info = &stg_MUT_VAR_DIRTY_info;
      } else {
        ((StgClosure *)q)->header.info = &stg_MUT_VAR_CLEAN_info;
      }
      copy_meh(p,info,q,sizeW_fromITBL(info),NULL,rc);
      return;
    }
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
      StgMVar *mvar = ((StgMVar *)q);
      evac_rc((StgClosure **)&mvar->head,rc);
      evac_rc((StgClosure **)&mvar->tail,rc);
      evac_rc((StgClosure **)&mvar->value,rc);
      if (rc->failed_to_evac) {
        mvar->header.info = &stg_MVAR_DIRTY_info;
      } else {
        mvar->header.info = &stg_MVAR_CLEAN_info;
      }
      copy_meh(p,info,q,sizeW_fromITBL(info),NULL,rc);
      return;
    }
    case TVAR:
    {
      StgTVar *tvar = ((StgTVar *)q);
      evac_rc((StgClosure **)&tvar->current_value,rc);
      evac_rc((StgClosure **)&tvar->first_watch_queue_entry,rc);
      if (rc->failed_to_evac) {
        tvar->header.info = &stg_TVAR_DIRTY_info;
      } else {
        tvar->header.info = &stg_TVAR_CLEAN_info;
      }
      copy_meh(p,info,q,sizeW_fromITBL(info),NULL,rc);
      return;
    }
    case WEAK:
    case PRIM:
    {
      StgPtr end, pp;

      end = (P_)((StgClosure *)q)->payload + info->layout.payload.ptrs;
      for (pp = (P_)((StgClosure *)q)->payload; pp < end; pp++) {
        evac_rc((StgClosure **)pp, rc);
      }
      copy_meh(p,info,q,sizeW_fromITBL(info),NULL,rc);
      return;
    }
    case MUT_PRIM:
    {
      StgPtr end, pp;

      end = (P_)((StgClosure *)q)->payload + info->layout.payload.ptrs;
      for (pp = (P_)((StgClosure *)q)->payload; pp < end; pp++) {
          evac_rc((StgClosure **)pp, rc);
      }
      rc->failed_to_evac = rtsTrue; // mutable
      copy_meh(p,info,q,sizeW_fromITBL(info),NULL,rc);
      return;
    }
    case BLACKHOLE:
    {
      StgClosure *r;
      const StgInfoTable *i;
      r = ((StgInd*)q)->indirectee;
      if (GET_CLOSURE_TAG(r) == 0) {
        i = r->header.info;
        if (IS_FORWARDING_PTR(i)) {
          r = (StgClosure *)UN_FORWARDING_PTR(i);
          i = r->header.info;
        }
        if (i == &stg_TSO_info
          || i == &stg_WHITEHOLE_info
          || i == &stg_BLOCKING_QUEUE_CLEAN_info
          || i == &stg_BLOCKING_QUEUE_DIRTY_info) {
          copy_meh(p,info,q,sizeofW(StgInd),NULL,rc);
          return;
        }
        ASSERT(i != &stg_IND_info);
      }
      q = r;
      *p = r;
      goto loop;
    }
    case ARR_WORDS:
    {
      copy_meh(p,info,q,arr_words_sizeW((StgArrBytes *)q),NULL,rc);
      return;
    }
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
      evac_mut_arr_ptrs((StgMutArrPtrs*)q,rc);
      if (rc->failed_to_evac) {
          ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_DIRTY_info;
      } else {
          ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_CLEAN_info;
      }
      rc->failed_to_evac = rtsTrue; // always put it on the mutable list.
      copy_meh(p,info,q,mut_arr_ptrs_sizeW((StgMutArrPtrs *)q),NULL,rc);
      return;
    }
    case MUT_ARR_PTRS_FROZEN0:
    case MUT_ARR_PTRS_FROZEN:
    {
      evac_mut_arr_ptrs((StgMutArrPtrs*)q,rc);
      if (rc->failed_to_evac) {
          ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN0_info;
      } else {
          ((StgClosure *)q)->header.info = &stg_MUT_ARR_PTRS_FROZEN_info;
      }
      copy_meh(p,info,q,mut_arr_ptrs_sizeW((StgMutArrPtrs *)q),NULL,rc);
      return;
    }
    case TSO:
      evacTSO((StgTSO *)q,rc);
      copy_meh(p,info,q,sizeofW(StgTSO),NULL,rc);
      return;
    case STACK:
    {
      StgStack *stack = (StgStack *)q;
      evac_stack(stack->sp,stack->stack + stack->stack_size, rc);
      StgStack *new_stack;
      StgPtr r, s;
      new_stack = (StgStack *)*p;
      move_STACK(stack, new_stack);
      for (r = stack->sp, s = new_stack->sp;
            r < stack->stack + stack->stack_size;) {
        *s++ = *r++;
      }
      copy_meh(p,info,q,sizeofW(StgStack),NULL,rc);
      return;
    }
    case TREC_CHUNK:
    {
      StgWord i;
      StgTRecChunk *tc = ((StgTRecChunk *) q);
      TRecEntry *e = &(tc->entries[0]);
      evac_rc((StgClosure **)&tc->prev_chunk,rc);
      for (i = 0; i < tc->next_entry_idx; i++, e++) {
        evac_rc((StgClosure **)&e->tvar,rc);
        evac_rc((StgClosure **)&e->expected_value,rc);
        evac_rc((StgClosure **)&e->new_value,rc);
      }
      copy_meh(p,info,q,sizeofW(StgTRecChunk),NULL,rc);
      return;
    }
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
    case SMALL_MUT_ARR_PTRS_FROZEN:
    {
      StgPtr next, size, start, pp;

      // We don't eagerly promote objects pointed to by a mutable
      // array, but if we find the array only points to objects in
      // the same or an older generation, we mark it "clean" and
      // avoid traversing it during minor GCs.
      next = small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)q);
      for (start = 0 ,pp = (P_)((StgSmallMutArrPtrs *)q)->payload; start < next; pp++) {
          evac_rc((StgClosure **)pp, rc);
      }
      copy_meh(p,info,q,small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs *)q),NULL,rc);
      return;
    }
    default:
      barf("Evac for weird closure not implemented");
  }


}

void
evacTSO (StgTSO *tso, ResourceContainer *rc)
{
    debugTrace(DEBUG_gc, "evacuating thread %d",(int)tso->id);

    // update the pointer from the InCall.
    if (tso->bound != NULL) {
        // NB. We can't just set tso->bound->tso = tso, because this
        // might be an invalid copy_meh the TSO resulting from multiple
        // threads evacuating the TSO simultaneously (see
        // Evac.c:copy__tag()).  Calling evacuate() on this pointer
        // will ensure that we update it to point to the correct copy_meh.
        //evac_rc((StgClosure **)&tso->bound->tso, rc);
    }

    evac_rc((StgClosure **)&tso->blocked_exceptions, rc);
    evac_rc((StgClosure **)&tso->bq, rc);

    // scavange current transaction record
    evac_rc((StgClosure **)&tso->trec, rc);

    evac_rc((StgClosure **)&tso->stackobj, rc);

    evac_rc((StgClosure **)&tso->_link, rc);
    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
        || tso->why_blocked == BlockedOnBlackHole
        || tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
        ) {
        evac_rc(&tso->block_info.closure, rc);
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
}

StgPtr
evac_mut_arr_ptrs(StgMutArrPtrs *a, ResourceContainer *rc)
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
          evac_rc((StgClosure**)p,rc);
      }
      if (rc->failed_to_evac) {
          any_failed = rtsTrue;
          *mutArrPtrsCard(a,m) = 1;
          rc->failed_to_evac = rtsFalse;
      } else {
          *mutArrPtrsCard(a,m) = 0;
      }
  }

  q = (StgPtr)&a->payload[a->ptrs];
  if (p < q) {
    for (; p < q; p++) {
      evac_rc((StgClosure**)p,rc);
    }
    if (rc->failed_to_evac) {
      any_failed = rtsTrue;
      *mutArrPtrsCard(a,m) = 1;
      rc->failed_to_evac = rtsFalse;
    } else {
      *mutArrPtrsCard(a,m) = 0;
    }
  }

  rc->failed_to_evac = any_failed;
  return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}

StgPtr
evac_small_bitmap(StgPtr p, StgWord size, StgWord bitmap, ResourceContainer *rc)
{
  while (size > 0) {
    if ((bitmap & 1) == 0) {
      evac_rc((StgClosure **)p, rc);
    }
    p++;
    bitmap = bitmap >> 1;
    size--;
  }
  return p;
}

void
evac_large_bitmap(StgPtr p, StgWord size, StgLargeBitmap *large_bitmap, ResourceContainer *rc)
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
        evac_rc((StgClosure **)p, rc);
      }
      bitmap = bitmap >> 1;
    }
  }
}

void
evac_PAP_payload(StgClosure *fun, StgClosure **payload, StgWord size, ResourceContainer *rc)
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
      evac_small_bitmap(p, size, bitmap, rc);
  case ARG_GEN_BIG:
      evac_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, rc);
      break;
  case ARG_BCO:
      evac_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size, rc);
      break;
  default:
      bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
      evac_small_bitmap(p, size, bitmap, rc);
      break;
  }
}

StgPtr
evac_arg_block(StgFunInfoTable *fun_info, StgClosure **args, ResourceContainer *rc)
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
      evac_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, rc);
      p += size;
      break;
  default:
      bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
      size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
  small_bitmap:
      evac_small_bitmap(p, size, bitmap, rc);
      break;
  }
  return p;
}

void
evac_stack(StgPtr p, StgPtr stack_end, ResourceContainer *rc)
{
  const StgRetInfoTable* info;
  StgWord bitmap;
  StgWord size;

   while (p < stack_end) {
    info  = get_ret_itbl((StgClosure *)p);

    switch (info->i.type) {

    case UPDATE_FRAME:
    {
        StgUpdateFrame *frame = (StgUpdateFrame *)p;
        StgClosure *v;

        evac_rc(&frame->updatee,rc);
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
        p = evac_small_bitmap(p, size, bitmap, rc);
        continue;
    // TODO: implement SRT
    follow_srt:
      evac_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap, rc);
      continue;

    case RET_BCO: {
        StgBCO *bco;
        StgWord size;

        p++;
        evac_rc((StgClosure **)p, rc);
        bco = (StgBCO *)*p;
        p++;
        size = BCO_BITMAP_SIZE(bco);
        evac_large_bitmap(p, BCO_BITMAP(bco), size, rc);
        p += size;
        continue;
    }

      // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
    case RET_BIG:
    {
        StgWord size;

        size = GET_LARGE_BITMAP(&info->i)->size;
        p++;
        evac_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size, rc);
        p += size;
        // and don't forget to follow the SRT
        goto follow_srt;
    }

    case RET_FUN:
    {
        StgRetFun *ret_fun = (StgRetFun *)p;
        StgFunInfoTable *fun_info;

        evac_rc(&ret_fun->fun, rc);
        fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
        p = evac_arg_block(fun_info, ret_fun->payload, rc);
        goto follow_srt;
    }

    default:
        barf("scavenge_stack: weird activation record found on stack: %d", (int)(info->i.type));
    }
    break; // TODO --- ughhhhhh no
  }
}

/* ----------------------------------------------------------------------------
   Evacuate static objects

   When a static object is visited for the first time in this GC, it
   is chained on to the gct->static_objects list.

   evacuate_static_object (link_field, q)
     - link_field must be STATIC_LINK(q)
   ------------------------------------------------------------------------- */

void
evac_static_object (StgClosure **link_field, StgClosure *q, ResourceContainer *rc)
{
    StgWord link = (StgWord)*link_field;

    // See Note [STATIC_LINK fields] for how the link field bits work
    if ((((StgWord)(link)&STATIC_BITS) | prev_static_flag) != 3) {
        StgWord new_list_head = (StgWord)q | static_flag;
#ifndef THREADED_RTS
        *link_field = rc->static_objects;
        rc->static_objects = (StgClosure *)new_list_head;
#else
        StgWord prev;
        prev = cas((StgVolatilePtr)link_field, link,
                  (StgWord)rc->static_objects);
        if (prev == link) {
            rc->static_objects = (StgClosure *)new_list_head;
        }
#endif
    }
}

/* Similar to scavenge_large_bitmap(), but we don't write back the
 * pointers we get back from evacuate().
 */
void
evac_large_srt_bitmap(StgLargeSRT *large_srt, ResourceContainer *rc)
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
                    evac_rc(p, rc);
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
                evac_rc(p, rc);
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
void
evac_srt (StgClosure **srt, nat srt_bitmap, ResourceContainer *rc)
{
  nat bitmap;
  StgClosure **p;

  bitmap = srt_bitmap;
  p = srt;

  if (bitmap == (StgHalfWord)(-1)) {
      evac_large_srt_bitmap((StgLargeSRT *)srt, rc);
      return;
  }

  while (bitmap != 0) {
      if ((bitmap & 1) != 0) {
        // TODO: well this is gonna break stuff
        //evac_rc(p, rc);
      }
      p++;
      bitmap = bitmap >> 1;
  }
}

void
evac_thunk_srt(const StgInfoTable *info, ResourceContainer *rc)
{
    StgThunkInfoTable *thunk_info;
    nat bitmap;

    thunk_info = itbl_to_thunk_itbl(info);
    bitmap = thunk_info->i.srt_bitmap;
    if (bitmap) {
        // don't read srt_offset if bitmap==0, because it doesn't exist
        // and so the memory might not be readable.
        evac_srt((StgClosure **)GET_SRT(thunk_info), bitmap, rc);
    }
}

void
evac_fun_srt(const StgInfoTable *info, ResourceContainer *rc)
{
    StgFunInfoTable *fun_info;
    nat bitmap;

    fun_info = itbl_to_fun_itbl(info);
    bitmap = fun_info->i.srt_bitmap;
    if (bitmap) {
        // don't read srt_offset if bitmap==0, because it doesn't exist
        // and so the memory might not be readable.
        evac_srt((StgClosure **)GET_FUN_SRT(fun_info), bitmap, rc);
    }
}

void
evac_static(ResourceContainer *rc)
{
  StgClosure *flagged_p, *p;
  const StgInfoTable *info;

  debugTrace(DEBUG_gc, "evacing static objects");


  /* keep going until we've scavenged all the objects on the linked
     list... */
  while (1) {

    /* get the next static object from the list.  Remember, there might
     * be more stuff on this list after each evacuation...
     * (static_objects is a global)
     */
    flagged_p = rc->static_objects;
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
    rc->static_objects = *STATIC_LINK(info,p);
    *STATIC_LINK(info,p) = rc->scavenged_static_objects;
    gct->scavenged_static_objects = flagged_p;

    switch (info -> type) {

    case IND_STATIC:
      {
        StgInd *ind = (StgInd *)p;
        evac_rc(&ind->indirectee, rc);

        /* might fail to evacuate it, in which case we have to pop it
         * back on the mutable list of the oldest generation.  We
         * leave it *on* the scavenged_static_objects list, though,
         * in case we visit this object again.
         */
        if (rc->failed_to_evac) {
          rc->failed_to_evac = rtsFalse;
          // TODO RC: recordMutableGen_GC((StgClosure *)p,oldest_gen->no);
          barf("Cannot handle this yet");
        }
        break;
      }

    case THUNK_STATIC:
      evac_thunk_srt(info, rc);
      break;

    case FUN_STATIC:
      evac_fun_srt(info, rc);
      break;

    case CONSTR_STATIC:
      {
        StgPtr q, next;

        next = (P_)p->payload + info->layout.payload.ptrs;
        // evacuate the pointers
        for (q = (P_)p->payload; q < next; q++) {
            evac_rc((StgClosure **)q, rc);
        }
        break;
      }

    default:
      barf("scavenge_static: strange closure %d", (int)(info->type));
    }

    ASSERT(rc->failed_to_evac == rtsFalse);
  }
}