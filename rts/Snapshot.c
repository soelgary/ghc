#include <stdio.h>
#include "Rts.h"

#include "Trace.h"
#include "ResourceLimits.h"
#include "Capability.h"

void takeSnapshotFrom(StgClosure **p, void *link, FILE *fd, char *source_closure);
void snapshot_srt (StgClosure **srt, nat srt_bitmap, void *link, FILE *fd, char *source_closure);


void
printLine(FILE *fd, void *source, void *target, char *closure_type, char *source_closure, int last) 
{
  fprintf(fd, "{\"source\": \"%p\", \"target\": \"%p\", \"source_closure_type\": \"%s\", \"target_closure_type\": \"%s\"}", source, target, source_closure, closure_type);
  if (!last) {
    fputs(",", fd);
  }
  fputs("\n", fd);
}
/*
void
printLine(FILE *fd, void *source, void *target, char *closure_type, char *source_closure, int last) 
{
  if(source != NULL) {
    fprintf(fd, "p%p_%s -> p%p_%s\n", source, source_closure, target, closure_type);
  }
}
*/
void
snapshot_large_bitmap( StgPtr p, StgLargeBitmap *large_bitmap, StgWord size, void *link, FILE *fd, char *source_closure)
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
                debugTrace(DEBUG_gc, "Ignoring large bitmap");
                //takeSnapshotFrom((StgClosure **)p, link, fd, source_closure);
            }
            bitmap = bitmap >> 1;
        }
    }
}

StgPtr
snapshot_small_bitmap (StgPtr p, StgWord size, StgWord bitmap, void *link, FILE *fd, char *source_closure)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
          debugTrace(DEBUG_gc, "Ignoring small bitmap");
          //takeSnapshotFrom((StgClosure **)p, link, fd, source_closure);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
    return p;
}

StgPtr
snapshot_PAP_payload (StgClosure *fun, StgClosure **payload, StgWord size, FILE *fd, char *source_closure)
{
    StgPtr p;
    StgWord bitmap;
    StgFunInfoTable *fun_info;

    fun_info = get_fun_itbl(UNTAG_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    p = (StgPtr)payload;

    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        printLine(fd, link, p, "PAP_ARG_GEN", source_closure, 0);
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        printLine(fd, link, p, "PAP_ARG_GEN_BIG", source_closure, 0);
        snapshot_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, fun, fd, source_closure);
        p += size;
        break;
    case ARG_BCO:
        printLine(fd, link, p, "PAP_ARG_BCO", source_closure, 0);
        snapshot_large_bitmap((StgPtr)payload, BCO_BITMAP(fun), size, fun, fd, source_closure);
        p += size;
        break;
    default:
        printLine(fd, link, p, "DEFAULT_PAP_PAYLOAD", source_closure, 0);
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = snapshot_small_bitmap(p, size, bitmap, fun, fd, source_closure);
        break;
    }
    return p;
}

StgPtr
snapshot_PAP (StgPAP *pap, FILE *fd, char *source_closure)
{
    takeSnapshotFrom(&pap->fun, pap, fd, source_closure);
    return snapshot_PAP_payload (pap->fun, pap->payload, pap->n_args, fd, source_closure);
}


StgPtr
snapshot_arg_block (StgFunInfoTable *fun_info, StgClosure **args, void *link, FILE *fd, char *source_closure)
{
    StgPtr p;
    StgWord bitmap;
    StgWord size;

    p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        printLine(fd, link, p, "ARG_ARG_GEN", source_closure, 0);
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        printLine(fd, link, p, "ARG_ARG_GEN_BIG", source_closure, 0);
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        snapshot_large_bitmap(p, GET_FUN_LARGE_BITMAP(fun_info), size, link, fd, source_closure);
        p += size;
        break;
    default:
        printLine(fd, link, p, "DEFUALT_ARG", source_closure, 0);
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        p = snapshot_small_bitmap(p, size, bitmap, link, fd, source_closure);
        break;
    }
    return p;
}

void
snapshotTSO (StgTSO *tso, FILE *fd, char *source_closure)
{
    rtsBool saved_eager;

    if (tso->bound != NULL && tso->bound->tso != tso) {
      takeSnapshotFrom((StgClosure **)&tso->bound->tso, tso, fd, source_closure);
    }

    takeSnapshotFrom((StgClosure **)&tso->blocked_exceptions, tso, fd, source_closure);
    takeSnapshotFrom((StgClosure **)&tso->bq, tso, fd, source_closure);

    takeSnapshotFrom((StgClosure **)&tso->trec, tso, fd, source_closure);

    takeSnapshotFrom((StgClosure **)&tso->stackobj, tso, fd, source_closure);

    takeSnapshotFrom((StgClosure **)&tso->_link, tso, fd, source_closure);
    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
        || tso->why_blocked == BlockedOnBlackHole
        || tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
        ) {
        takeSnapshotFrom((StgClosure **)&tso->block_info.closure, tso, fd, source_closure);
    }
}

void
snapshot_stack(StgPtr p, StgPtr stack_end, void *link, FILE *fd, char *source_closure)
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

        debugTrace(DEBUG_gc, "Ignoring updatee");
        // takeSnapshotFrom(&frame->updatee, link, fd, source_closure);
        v = frame->updatee;
        printLine(fd, link, v, "UPDATE_FRAME", source_closure, 0);
        if (GET_CLOSURE_TAG(v) != 0 ||
            (get_itbl(v)->type != BLACKHOLE)) {
        }
        p += sizeofW(StgUpdateFrame);
        continue;
    }

    case CATCH_STM_FRAME:
    case CATCH_RETRY_FRAME:
    case ATOMICALLY_FRAME:
    case UNDERFLOW_FRAME:
    case STOP_FRAME:
    case CATCH_FRAME:
    case RET_SMALL:
        printLine(fd, link, p, "STACK_FRAME", source_closure, 0);
        bitmap = BITMAP_BITS(info->i.layout.bitmap);
        size   = BITMAP_SIZE(info->i.layout.bitmap);
        // NOTE: the payload starts immediately after the info-ptr, we
        // don't have an StgHeader in the same sense as a heap closure.
        p++;
        p = snapshot_small_bitmap(p, size, bitmap, link, fd, source_closure);

    follow_srt:
        snapshot_srt((StgClosure **)GET_SRT(info), info->i.srt_bitmap, link, fd, source_closure);
        continue;

    case RET_BCO: {
        StgBCO *bco;
        StgWord size;
        printLine(fd, link, p, "RET_BCO", source_closure, 0);
        p++;
        takeSnapshotFrom((StgClosure **)p, link, fd, source_closure);
        bco = (StgBCO *)*p;
        p++;
        size = BCO_BITMAP_SIZE(bco);
        snapshot_large_bitmap(p, BCO_BITMAP(bco), size, link, fd, source_closure);
        p += size;
        continue;
    }

    case RET_BIG:
    {
        StgWord size;
        printLine(fd, link, p, "RET_BIG", source_closure, 0);
        size = GET_LARGE_BITMAP(&info->i)->size;
        p++;
        snapshot_large_bitmap(p, GET_LARGE_BITMAP(&info->i), size, link, fd, source_closure);
        p += size;
        goto follow_srt;
    }

    case RET_FUN:
    {
        StgRetFun *ret_fun = (StgRetFun *)p;
        StgFunInfoTable *fun_info;
        printLine(fd, link, ret_fun, "RET_FUN", source_closure, 0);
        takeSnapshotFrom(&ret_fun->fun, link, fd, source_closure);
        fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
        p = snapshot_arg_block(fun_info, ret_fun->payload, link, fd, source_closure);
        goto follow_srt;
    }

    default:
        debugTrace(DEBUG_gc, "snapshot_stack: weird activation record found on stack: %d", (int)(info->i.type));
        return;
    }
  }
}

void
snapshot_large_srt_bitmap(StgLargeSRT *large_srt, FILE *fd, char *source_closure)
{
    nat i, j, size;
    StgWord bitmap;
    StgClosure **p;

    size   = (nat)large_srt->l.size;
    p      = (StgClosure **)large_srt->srt;

    for (i = 0; i < size / BITS_IN(W_); i++) {
        bitmap = large_srt->l.bitmap[i];
        if (bitmap != 0) {
            for (j = 0; j < BITS_IN(W_); j++) {
                if ((bitmap & 1) != 0) {
                    takeSnapshotFrom(p, large_srt, fd, source_closure);
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
                takeSnapshotFrom(p, large_srt, fd, source_closure);
            }
            p++;
            bitmap = bitmap >> 1;
        }
    }
}

void
snapshot_srt (StgClosure **srt, nat srt_bitmap, void *link, FILE *fd, char *source_closure)
{
  nat bitmap;
  StgClosure **p;

  bitmap = srt_bitmap;
  p = srt;

  if (bitmap == (StgHalfWord)(-1)) {
      snapshot_large_srt_bitmap((StgLargeSRT *)srt, fd, source_closure);
      return;
  }

  while (bitmap != 0) {
      if ((bitmap & 1) != 0) {
        takeSnapshotFrom(p, link, fd, source_closure);
      }
      p++;
      bitmap = bitmap >> 1;
  }
}

void
snapshot_fun_srt(const StgInfoTable *info, void *link, FILE *fd, char *source_closure)
{
  StgFunInfoTable *fun_info;
  nat bitmap;

  fun_info = itbl_to_fun_itbl(info);
  bitmap = fun_info->i.srt_bitmap;
  if (bitmap) {
      // don't read srt_offset if bitmap==0, because it doesn't exist
      // and so the memory might not be readable.
      snapshot_srt((StgClosure **)GET_FUN_SRT(fun_info), bitmap, link, fd, source_closure);
  }
}

void
snapshot_thunk_srt(const StgInfoTable *info, void *link, FILE *fd, char *source_closure)
{
    StgThunkInfoTable *thunk_info;
    nat bitmap;

    thunk_info = itbl_to_thunk_itbl(info);
    bitmap = thunk_info->i.srt_bitmap;
    if (bitmap) {
        snapshot_srt((StgClosure **)GET_SRT(thunk_info), bitmap, link, fd, source_closure);
    }
}

StgPtr
snapshot_AP (StgAP *ap, FILE *fd, char *source_closure)
{
    takeSnapshotFrom(&ap->fun, ap, fd, source_closure);
    return snapshot_PAP_payload (ap->fun, ap->payload, ap->n_args, fd, source_closure);
}

StgPtr
snapshot_mut_arr_ptrs (StgMutArrPtrs *a, FILE *fd, char *source_closure)
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
          takeSnapshotFrom((StgClosure **)p, a, fd, source_closure);
      }
  }

  q = (StgPtr)&a->payload[a->ptrs];
  if (p < q) {
      for (; p < q; p++) {
          takeSnapshotFrom((StgClosure **)p, a, fd, source_closure);
      }
  }
  return (StgPtr)a + mut_arr_ptrs_sizeW(a);
}

void
takeSnapshotFrom(StgClosure **p, void *link, FILE *fd, char *source_closure)
{
  if (p == NULL) {
    return;
  }
  const StgInfoTable *info;
  StgClosure *q;

  q = *p;

  if (q == NULL) {
    return;
  }


  info = get_itbl(q);
  debugTrace(DEBUG_gc, "info table = %hu", info->type)
  switch(info->type) {
    case MVAR_CLEAN:
    case MVAR_DIRTY:
    {
      char *ct = "MVAR CLEAN/DIRTY";
      debugTrace(DEBUG_gc, "Found an mvar clean/dirty");
      printLine(fd, link, q, ct, source_closure, 0);
      StgMVar *mvar = ((StgMVar *)q);
      takeSnapshotFrom((StgClosure **)&mvar->head, q, fd, ct);
      takeSnapshotFrom((StgClosure **)&mvar->tail, q, fd, ct);
      takeSnapshotFrom((StgClosure **)&mvar->value, q, fd, ct);
      break;
    }
    case TVAR:
    {
      char *ct = "TVAR";
      debugTrace(DEBUG_gc, "Found a tvar");
      printLine(fd, link, q, ct, source_closure, 0);
      StgTVar *tvar = ((StgTVar *)p);
      takeSnapshotFrom((StgClosure **)&tvar->current_value, q, fd, ct);
      takeSnapshotFrom((StgClosure **)&tvar->first_watch_queue_entry, q, fd, ct);
      break;
    }
    case FUN_2_0:
      {
        char *ct = "FUN_2_0";
        debugTrace(DEBUG_gc, "Found a FUN_2_0");
        printLine(fd, link, q, ct, source_closure, 0);
        snapshot_fun_srt(info, p, fd, ct);
        takeSnapshotFrom(&((StgClosure *)p)->payload[1], q, fd, ct);
        takeSnapshotFrom(&((StgClosure *)p)->payload[0], q, fd, ct);
        break;
      }
    case THUNK_2_0:
      {
        char *ct = "THUNK_2_0";
        debugTrace(DEBUG_gc, "Found a THUNK_2_0");
        printLine(fd, link, q, ct, source_closure, 0);
        snapshot_thunk_srt(info, q, fd, ct);
        takeSnapshotFrom(&((StgThunk *)p)->payload[1], q, fd, ct);
        takeSnapshotFrom(&((StgThunk *)p)->payload[0], q, fd, ct);
        break;
      }
    case CONSTR_2_0:
      {
        char *ct = "CONSTR_2_0";
        debugTrace(DEBUG_gc, "Found a CONSTR_2_0");
        printLine(fd, link, q, ct, source_closure, 0);
        takeSnapshotFrom(&((StgClosure *)p)->payload[1], q, fd, ct);
        takeSnapshotFrom(&((StgClosure *)p)->payload[0], q, fd, ct);
        break;
      }
    case THUNK_1_0:
      {
        char *ct = "THUNK_1_0";
        debugTrace(DEBUG_gc, "Found a THUNK_1_0");
        printLine(fd, link, q, ct,  source_closure, 0);
        snapshot_thunk_srt(info, q, fd, ct);
        takeSnapshotFrom(&((StgThunk *)p)->payload[0], q, fd, ct);
        break;
      }
    case FUN_1_0:
      {
        char *ct = "FUN_1_0";
        debugTrace(DEBUG_gc, "Found a FUN_1_0");
        printLine(fd, link, q, ct, source_closure, 0);
        snapshot_fun_srt(info, p, fd, ct);
        break;
      }
    case CONSTR_1_0:
      {
        char *ct = "CONSTR_1_0";
        debugTrace(DEBUG_gc, "Found a CONSTR_1_0");
        printLine(fd, link, q, ct, source_closure, 0);
        takeSnapshotFrom(&((StgClosure *)p)->payload[0], q, fd, ct);
        break;
      }
    case THUNK_0_1:
    {
      char *ct = "THUNK_0_1";
      debugTrace(DEBUG_gc, "Found a THUNK_0_1");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_thunk_srt(info, q, fd, ct);
      break;
    }
    case FUN_0_1:
    {
      char *ct = "FUN_0_1";
      debugTrace(DEBUG_gc, "Found a FUN_0_1");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_fun_srt(info, p, fd, ct);
      break;
    }
    case CONSTR_0_1:
    {
      char *ct = "CONSTR_0_1";
      debugTrace(DEBUG_gc, "Found a CONSTR_0_1");
      printLine(fd, link, q, ct, source_closure, 0);
      break;
    }
    case THUNK_0_2:
    {
      char *ct = "THUNK_0_2";
      debugTrace(DEBUG_gc, "Found a THUNK_0_2");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_thunk_srt(info, q, fd, ct);
      break;
    }
    case FUN_0_2:
    {
      char *ct = "FUN_0_2";
      debugTrace(DEBUG_gc, "Found a FUN_0_2");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_fun_srt(info, p, fd, ct);
      break;
    }
    case CONSTR_0_2:
    {
      char *ct = "CONSTR_0_2";
      debugTrace(DEBUG_gc, "Found a CONSTR_0_2");
      printLine(fd, link, q, ct, source_closure, 0);
      break;
    }
    case THUNK_1_1:
    {
      char *ct = "THUNK_1_1";
      debugTrace(DEBUG_gc, "Found a THUNK_1_1");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_thunk_srt(info, q, fd, ct);
      takeSnapshotFrom(&((StgThunk *)p)->payload[0], q, fd, ct);
      break;
    }
    case FUN_1_1:
    {
      char *ct = "FUN_1_1";
      debugTrace(DEBUG_gc, "Found a FUN_1_1");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_fun_srt(info, p, fd, ct);
      break;
    }
    case CONSTR_1_1:
    {
      char *ct = "CONSTR_1_1";
      debugTrace(DEBUG_gc, "Found a CONSTR_1_1");
      printLine(fd, link, q, ct, source_closure, 0);
      takeSnapshotFrom(&((StgClosure *)p)->payload[0], q, fd, ct);
      break;
    }
    case FUN:
    {
      char *ct = "FUN";
      debugTrace(DEBUG_gc, "Found a FUN");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_fun_srt(info, p, fd, ct);
      break;
    }
    case THUNK:
    {
      char *ct = "THUNK";
      debugTrace(DEBUG_gc, "Found a THUNK");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_thunk_srt(info, q, fd, ct);
      StgPtr end;
      end = (P_)((StgThunk *)p)->payload + info->layout.payload.ptrs;
      StgClosure **pp = p;
      for (pp = (P_)((StgThunk *)pp)->payload; pp < end; pp++) {
        takeSnapshotFrom((StgClosure **)pp, q, fd, ct);
      }
      break;
    }
    case CONSTR:
    case WEAK:
    case PRIM:
    {
      char *ct = "CONSTR/WEAK/PRIM";
      debugTrace(DEBUG_gc, "Found a CONSTR/WEAK/PRIM");
      printLine(fd, link, q, ct, source_closure, 0);
      StgPtr end;
      StgClosure **pp = p;
      end = (P_)((StgClosure *)pp)->payload + info->layout.payload.ptrs;
      for (pp = (P_)((StgClosure *)pp)->payload; pp < end; pp++) {
          takeSnapshotFrom((StgClosure **)pp, q, fd, ct);
      }
      break;
    }
    case BCO:
    {
      char *ct = "BCO";
      debugTrace(DEBUG_gc, "Found a BCO");
      printLine(fd, link, q, ct, source_closure, 0);
      StgBCO *bco = (StgBCO *)p;
      takeSnapshotFrom((StgClosure **)&bco->instrs, q, fd, ct);
      takeSnapshotFrom((StgClosure **)&bco->literals, q, fd, ct);
      takeSnapshotFrom((StgClosure **)&bco->ptrs, q, fd, ct);
      break;
    }
    case IND_PERM:
    case BLACKHOLE:
    {
      char *ct = "IND_PRIM/BLACKHOLE";
      debugTrace(DEBUG_gc, "Found a IND_PRIM/BLACKHOLE");
      printLine(fd, link, q, ct, source_closure, 0);
      takeSnapshotFrom(&((StgInd *)q)->indirectee, q, fd, ct);
      break;
    }
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    {
      char *ct = "MUT_VAR_CLEAN/DIRTY";
      debugTrace(DEBUG_gc, "Found a MUT_VAR_CLEAN/DIRTY");
      printLine(fd, link, q, ct, source_closure, 0);
      takeSnapshotFrom(&((StgMutVar *)p)->var, q, fd, ct);
      break;
    }
    case BLOCKING_QUEUE:
    {
      char *ct = "BLOCKING_QUEUE";
      debugTrace(DEBUG_gc, "Found a BLOCKING_QUEUE");
      printLine(fd, link, q, ct, source_closure, 0);
      StgBlockingQueue *bq = (StgBlockingQueue *)p;
      
      takeSnapshotFrom(&bq->bh, q, fd, ct);
      takeSnapshotFrom((StgClosure**)&bq->owner, q, fd, ct);
      takeSnapshotFrom((StgClosure**)&bq->queue, q, fd, ct);
      takeSnapshotFrom((StgClosure**)&bq->link, q, fd, ct);
      break;
    }
    case THUNK_SELECTOR:
    {
      char *ct = "THUNK_SELECTOR";
      debugTrace(DEBUG_gc, "Found a THUNK_SELECTOR");
      printLine(fd, link, q, ct, source_closure, 0);
      StgSelector *s = (StgSelector *)p;
      takeSnapshotFrom(&s->selectee, q, fd, ct);
      break;
    }
    case AP_STACK:
    {
      char *ct = "AP_STACK";
      debugTrace(DEBUG_gc, "Found a AP_STACK");
      printLine(fd, link, q, ct, source_closure, 0);
      StgAP_STACK *ap = (StgAP_STACK *)p;
      
      takeSnapshotFrom(&ap->fun, q, fd, ct);
      snapshot_stack((StgPtr)ap->payload, (StgPtr)ap->payload + ap->size, ap, fd, ct);
      break;
    }
    case PAP:
    {
      char *ct = "PAP";
      debugTrace(DEBUG_gc, "Found a PAP");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_PAP((StgPAP *)p, fd, ct);
      break;
    }
    case AP:
    {
      char *ct = "AP";
      debugTrace(DEBUG_gc, "Found a AP");
      printLine(fd, link, q, ct, source_closure, 0);
      snapshot_AP((StgAP *)p, fd, ct);
      break;
    }
    case ARR_WORDS:
    {
      char *ct = "ARR_WORDS";
      debugTrace(DEBUG_gc, "Found a ARR_WORDS");
      printLine(fd, link, q, ct, source_closure, 0);
      break;
    }
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    {
      char *ct = "MUT_ARR_PTRS CLEAN/DIRTY";
      debugTrace(DEBUG_gc, "Found a MUT_ARR_PTRS_CLEAN/DIRTY");
      printLine(fd, link, q, ct, source_closure, 0);
      StgClosure **pp = snapshot_mut_arr_ptrs((StgMutArrPtrs*)p, fd, ct);
      takeSnapshotFrom(pp, q, fd, ct);
      break;
    }
    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
    {
      char *ct = "MUT_ARR_PTRS_FROZEN/0";
      debugTrace(DEBUG_gc, "Found a MUT_ARR_PTRS_FROZEN/0");
      printLine(fd, link, q, ct, source_closure, 0);
      StgClosure **ppp = snapshot_mut_arr_ptrs((StgMutArrPtrs*)p, fd, ct);
      takeSnapshotFrom(ppp, q, fd, ct);
      break;
    }
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    {
      char *ct = "SMALL_MUT_ARR_PTRS_CLEAN/DIRTY";
      debugTrace(DEBUG_gc, "Found a SMALL_MUT_ARR_PTRS_CLEAN/DIRTY");
      printLine(fd, link, q, ct, source_closure, 0);
      StgPtr next;
      next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
      StgClosure **pp = p;
      for (pp = (P_)((StgSmallMutArrPtrs *)pp)->payload; pp < next; pp++) {
        takeSnapshotFrom((StgClosure **)pp, q, fd, ct);
      }
      break;
    }
    case SMALL_MUT_ARR_PTRS_FROZEN:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
    {
      char *ct = "SMALL_MUT_ARR_PTRS_FROZEN/0";
      debugTrace(DEBUG_gc, "Found a SMALL_MUT_ARR_PTRS_FROZEN/0");
      printLine(fd, link, q, ct, source_closure, 0);
      StgPtr next;
      StgClosure **pp = p;
      next = p + small_mut_arr_ptrs_sizeW((StgSmallMutArrPtrs*)p);
      for (pp = (P_)((StgSmallMutArrPtrs *)pp)->payload; pp < next; pp++) {
          takeSnapshotFrom((StgClosure **)pp, q, fd, ct);
      }
      break;
    }
    case TSO:
    {
      char *ct = "TSO";
      debugTrace(DEBUG_gc, "Found a TSO");
      printLine(fd, link, (StgTSO *)p, ct, source_closure, 0);
      snapshotTSO((StgTSO *)q, fd, ct);
      break;
    }
    case STACK:
    {
      char *ct = "STACK";
      debugTrace(DEBUG_gc, "Found a STACK");
      printLine(fd, link, q, ct, source_closure, 0);
      StgStack *stack = (StgStack*)q;
      snapshot_stack((StgPtr)stack->sp, stack->stack + stack->stack_size, stack, fd, ct);
      break;
    }
    case MUT_PRIM:
    {
      char *ct = "MUT_PRIM";
      debugTrace(DEBUG_gc, "Found a MUT_PRIM");
      printLine(fd, link, q, ct, source_closure, 0);
      StgPtr end;
      StgClosure **pp = p;
      end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
      for (pp = (P_)((StgClosure *)pp)->payload; pp < end; pp++) {
          takeSnapshotFrom((StgClosure **)pp, q, fd, ct);
      }
      break;
    }
    case TREC_CHUNK:
    {
      char *ct = "TREC_TRUNK";
      debugTrace(DEBUG_gc, "Found a TREC_CHUNK");
      printLine(fd, link, q, ct, source_closure, 0);
      StgWord i;
      StgTRecChunk *tc = ((StgTRecChunk *) p);
      TRecEntry *e = &(tc -> entries[0]);
      takeSnapshotFrom((StgClosure **)&tc->prev_chunk, q, fd, ct);
      for (i = 0; i < tc -> next_entry_idx; i ++, e++ ) {
        takeSnapshotFrom((StgClosure **)&e->tvar, q, fd, ct);
        takeSnapshotFrom((StgClosure **)&e->expected_value, q, fd, ct);
        takeSnapshotFrom((StgClosure **)&e->new_value, q, fd, ct);
      }
      break;
    }

    default:
      debugTrace(DEBUG_gc, "Got the info table from a root");
  }
}

/*
  Lets take a heap snapshot for a nice visual stimulant
*/
void
takeSnapshot(char *name)
{
  FILE *fd;
  StgClosure **c;
  ResourceContainer *rc = RC_MAIN;

  fd = fopen(name, "w+");
  fputs("[\n", fd);
  //fputs("digraph G {\n",fd);
  
  //printLine(fd, 0x4200005410, 0x4200005412, "blackhole", 0);
  //printLine(fd, 0x4200005410, 0x4200005415, "blackhole", 0);
  //printLine(fd, 0x4200005411, 0x4200005415, "blackhole", 1);
  
  
  c = (StgClosure **)(void *)&rc->ownerTSO;
  takeSnapshotFrom(c, NULL, fd, NULL);
  printLine(fd, NULL, NULL, NULL, NULL, 1);
  fputs("]\n", fd);
  //fputs("}", fd);
  fclose(fd);
  

  fd = fopen("/tmp/roots.txt", "w+");
  fputs("[\n", fd);
  //fputs("digraph G {\n", fd);
  Capability *cap = rc->ownerTSO->cap;
  c = (StgClosure **)(void *)&cap->run_queue_hd;
  takeSnapshotFrom(c, NULL, fd, NULL);
  c = (StgClosure **)(void *)&cap->run_queue_tl;
  takeSnapshotFrom(c, NULL, fd, NULL);

#if defined(THREADED_RTS)
  c = (StgClosure **)(void *)&cap->inbox;
  takeSnapshotFrom(c, NULL, fd, NULL);
#endif

  InCall *incall;
  for (incall = cap->suspended_ccalls; incall != NULL;
    incall=incall->next) {
    c = (StgClosure **)(void *)&incall->suspended_tso;
    takeSnapshotFrom(c, NULL, fd, NULL);
  }
  printLine(fd, NULL, NULL, NULL, NULL, 1);
  fputs("]\n", fd);
  //fputs("}", fd);
  fclose(fd);
  
}

