/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Generational garbage collector
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "HsFFI.h"

#include "GC.h"
#include "GCThread.h"
#include "GCTDecl.h"            // NB. before RtsSignals.h which
                                // clobbers REG_R1 on arm/Linux
#include "Compact.h"
#include "Evac.h"
#include "Scav.h"
#include "GCUtils.h"
#include "MarkStack.h"
#include "MarkWeak.h"
#include "Sparks.h"
#include "Sweep.h"

#include "Storage.h"
#include "RtsUtils.h"
#include "Apply.h"
#include "Updates.h"
#include "Stats.h"
#include "Schedule.h"
#include "Sanity.h"
#include "BlockAlloc.h"
#include "ProfHeap.h"
#include "Weak.h"
#include "Prelude.h"
#include "RtsSignals.h"
#include "STM.h"
#include "Trace.h"
#include "RetainerProfile.h"
#include "LdvProfile.h"
#include "RaiseAsync.h"
#include "Stable.h"
#include "CheckUnload.h"

#include <string.h> // for memset()
#include <unistd.h>

/* -----------------------------------------------------------------------------
   Global variables
   -------------------------------------------------------------------------- */

/* STATIC OBJECT LIST.
 *
 * During GC:
 * We maintain a linked list of static objects that are still live.
 * The requirements for this list are:
 *
 *  - we need to scan the list while adding to it, in order to
 *    scavenge all the static objects (in the same way that
 *    breadth-first scavenging works for dynamic objects).
 *
 *  - we need to be able to tell whether an object is already on
 *    the list, to break loops.
 *
 * Each static object has a "static link field", which we use for
 * linking objects on to the list.  We use a stack-type list, consing
 * objects on the front as they are added (this means that the
 * scavenge phase is depth-first, not breadth-first, but that
 * shouldn't matter).
 *
 * A separate list is kept for objects that have been scavenged
 * already - this is so that we can zero all the marks afterwards.
 *
 * An object is on the list if its static link field is non-zero; this
 * means that we have to mark the end of the list with '1', not NULL.
 *
 * Extra notes for generational GC:
 *
 * Each generation has a static object list associated with it.  When
 * collecting generations up to N, we treat the static object lists
 * from generations > N as roots.
 *
 * We build up a static object list while collecting generations 0..N,
 * which is then appended to the static object list of generation N+1.
 */

/* N is the oldest generation being collected, where the generations
 * are numbered starting at 0.  A major GC (indicated by the major_gc
 * flag) is when we're collecting all generations.  We only attempt to
 * deal with static objects and GC CAFs when doing a major GC.
 */
nat N;
rtsBool major_gc;

/* Data used for allocation area sizing.
 */
static W_ g0_pcnt_kept = 30; // percentage of g0 live at last minor GC

/* Mut-list stats */
#ifdef DEBUG
nat mutlist_MUTVARS,
    mutlist_MUTARRS,
    mutlist_MVARS,
    mutlist_TVAR,
    mutlist_TVAR_WATCH_QUEUE,
    mutlist_TREC_CHUNK,
    mutlist_TREC_HEADER,
    mutlist_ATOMIC_INVARIANT,
    mutlist_INVARIANT_CHECK_QUEUE,
    mutlist_OTHERS;
#endif

/* Thread-local data for each GC thread
 */
gc_thread **gc_threads = NULL;

#if !defined(THREADED_RTS)
StgWord8 the_gc_thread[sizeof(gc_thread) + 64 * sizeof(gen_workspace)];
#endif

// Number of threads running in *this* GC.  Affects how many
// step->todos[] lists we have to look in to find work.
nat n_gc_threads;

// For stats:
long copied;        // *words* copied & scavenged during this GC

rtsBool work_stealing;

nat static_flag = STATIC_FLAG_B;
nat prev_static_flag = STATIC_FLAG_A;

DECLARE_GCT

/* -----------------------------------------------------------------------------
   Static function declarations
   -------------------------------------------------------------------------- */

static void mark_root               (void *user, StgClosure **root);
static void prepare_collected_gen   (generation *gen);
static void prepare_uncollected_gen (generation *gen);
static void init_gc_thread          (gc_thread *t);
static void resize_generations      (void);
static void resize_nursery          (void);
static void start_gc_threads        (void);
static void scavenge_until_all_done (void);
static StgWord inc_running          (void);
static StgWord dec_running          (void);
static void wakeup_gc_threads       (nat me);
static void shutdown_gc_threads     (nat me);
static void collect_gct_blocks      (void);
static void collect_pinned_object_blocks (void);

#if defined(DEBUG)
static void gcCAFs                  (void);
#endif

/* -----------------------------------------------------------------------------
   The mark stack.
   -------------------------------------------------------------------------- */

bdescr *mark_stack_top_bd; // topmost block in the mark stack
bdescr *mark_stack_bd;     // current block in the mark stack
StgPtr mark_sp;            // pointer to the next unallocated mark stack entry

/* -----------------------------------------------------------------------------
   GarbageCollect: the main entry point to the garbage collector.
   The collect_gen parameter is gotten by calling calcNeeded().
   Locks held: all capabilities are held throughout GarbageCollect().
   -------------------------------------------------------------------------- */

void
GarbageCollect (nat collect_gen,
                rtsBool do_heap_census,
                nat gc_type USED_IF_THREADS,
                Capability *cap)
{ }

/* -----------------------------------------------------------------------------
   Initialise the gc_thread structures.
   -------------------------------------------------------------------------- */

#define GC_THREAD_INACTIVE             0
#define GC_THREAD_STANDING_BY          1
#define GC_THREAD_RUNNING              2
#define GC_THREAD_WAITING_TO_CONTINUE  3

static void
new_gc_thread (nat n, gc_thread *t)
{
    nat g;
    gen_workspace *ws;

    t->cap = capabilities[n];

#ifdef THREADED_RTS
    t->id = 0;
    initSpinLock(&t->gc_spin);
    initSpinLock(&t->mut_spin);
    ACQUIRE_SPIN_LOCK(&t->gc_spin);
    ACQUIRE_SPIN_LOCK(&t->mut_spin);
    t->wakeup = GC_THREAD_INACTIVE;  // starts true, so we can wait for the
                          // thread to start up, see wakeup_gc_threads
#endif

    t->thread_index = n;
    t->idle = rtsFalse;
    t->free_blocks = NULL;
    t->gc_count = 0;

    init_gc_thread(t);

    for (g = 0; g < RtsFlags.GcFlags.generations; g++)
    {
        ws = &t->gens[g];
        ws->gen = &generations[g];
        ASSERT(g == ws->gen->no);
        ws->my_gct = t;

        // We want to call
        //   alloc_todo_block(ws,0);
        // but can't, because it uses gct which isn't set up at this point.
        // Hence, allocate a block for todo_bd manually:
        {
            bdescr *bd = allocBlock(); // no lock, locks aren't initialised yet
            initBdescr(bd, ws->gen, ws->gen->to, t->r.rCurrentTSO.rc);
            bd->flags = BF_EVACUATED;
            bd->u.scan = bd->free = bd->start;

            ws->todo_bd = bd;
            ws->todo_free = bd->free;
            ws->todo_lim = bd->start + BLOCK_SIZE_W;
        }

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


void
initGcThreads (nat from USED_IF_THREADS, nat to USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;

    if (from > 0) {
        gc_threads = stgReallocBytes (gc_threads, to * sizeof(gc_thread*),
                                      "initGcThreads");
    } else {
        gc_threads = stgMallocBytes (to * sizeof(gc_thread*),
                                     "initGcThreads");
    }

    for (i = from; i < to; i++) {
        gc_threads[i] =
            stgMallocBytes(sizeof(gc_thread) +
                           RtsFlags.GcFlags.generations * sizeof(gen_workspace),
                           "alloc_gc_threads");

        new_gc_thread(i, gc_threads[i]);
    }
#else
    ASSERT(from == 0 && to == 1);
    gc_threads = stgMallocBytes (sizeof(gc_thread*),"alloc_gc_threads");
    gc_threads[0] = gct;
    new_gc_thread(0,gc_threads[0]);
#endif
}

void
freeGcThreads (void)
{
    nat g;
    if (gc_threads != NULL) {
#if defined(THREADED_RTS)
        nat i;
        for (i = 0; i < n_capabilities; i++) {
            for (g = 0; g < RtsFlags.GcFlags.generations; g++)
            {
                freeWSDeque(gc_threads[i]->gens[g].todo_q);
            }
            stgFree (gc_threads[i]);
        }
        stgFree (gc_threads);
#else
        for (g = 0; g < RtsFlags.GcFlags.generations; g++)
        {
            freeWSDeque(gc_threads[0]->gens[g].todo_q);
        }
        stgFree (gc_threads);
#endif
        gc_threads = NULL;
    }
}

/* ----------------------------------------------------------------------------
   Start GC threads
   ------------------------------------------------------------------------- */

static volatile StgWord gc_running_threads;

static StgWord
inc_running (void)
{
    StgWord new;
    new = atomic_inc(&gc_running_threads, 1);
    ASSERT(new <= n_gc_threads);
    return new;
}

static StgWord
dec_running (void)
{
    ASSERT(gc_running_threads != 0);
    return atomic_dec(&gc_running_threads);
}

static rtsBool
any_work (void)
{
    int g;
    gen_workspace *ws;

    gct->any_work++;

    write_barrier();

    // scavenge objects in compacted generation
    if (mark_stack_bd != NULL && !mark_stack_empty()) {
        return rtsTrue;
    }

    // Check for global work in any step.  We don't need to check for
    // local work, because we have already exited scavenge_loop(),
    // which means there is no local work for this thread.
    for (g = 0; g < (int)RtsFlags.GcFlags.generations; g++) {
        ws = &gct->gens[g];
        if (ws->todo_large_objects) return rtsTrue;
        if (!looksEmptyWSDeque(ws->todo_q)) return rtsTrue;
        if (ws->todo_overflow) return rtsTrue;
    }

#if defined(THREADED_RTS)
    if (work_stealing) {
        nat n;
        // look for work to steal
        for (n = 0; n < n_gc_threads; n++) {
            if (n == gct->thread_index) continue;
            for (g = RtsFlags.GcFlags.generations-1; g >= 0; g--) {
                ws = &gc_threads[n]->gens[g];
                if (!looksEmptyWSDeque(ws->todo_q)) return rtsTrue;
            }
        }
    }
#endif

    gct->no_work++;
#if defined(THREADED_RTS)
    yieldThread();
#endif

    return rtsFalse;
}

static void
scavenge_until_all_done (void)
{
    DEBUG_ONLY( nat r );


loop:
#if defined(THREADED_RTS)
    if (n_gc_threads > 1) {
        scavenge_loop();
    } else {
        scavenge_loop1();
    }
#else
    scavenge_loop();
#endif

    collect_gct_blocks();

    // scavenge_loop() only exits when there's no work to do

#ifdef DEBUG
    r = dec_running();
#else
    dec_running();
#endif

    traceEventGcIdle(gct->cap);

    debugTrace(DEBUG_gc, "%d GC threads still running", r);

    while (gc_running_threads != 0) {
        // usleep(1);
        if (any_work()) {
            inc_running();
            traceEventGcWork(gct->cap);
            goto loop;
        }
        // any_work() does not remove the work from the queue, it
        // just checks for the presence of work.  If we find any,
        // then we increment gc_running_threads and go back to
        // scavenge_loop() to perform any pending work.
    }

    traceEventGcDone(gct->cap);
}

#if defined(THREADED_RTS)

void
gcWorkerThread (Capability *cap)
{
    gc_thread *saved_gct;

    // necessary if we stole a callee-saves register for gct:
    saved_gct = gct;

    SET_GCT(gc_threads[cap->no]);
    gct->id = osThreadId();

    // Wait until we're told to wake up
    RELEASE_SPIN_LOCK(&gct->mut_spin);
    // yieldThread();
    //    Strangely, adding a yieldThread() here makes the CPU time
    //    measurements more accurate on Linux, perhaps because it syncs
    //    the CPU time across the multiple cores.  Without this, CPU time
    //    is heavily skewed towards GC rather than MUT.
    gct->wakeup = GC_THREAD_STANDING_BY;
    debugTrace(DEBUG_gc, "GC thread %d standing by...", gct->thread_index);
    ACQUIRE_SPIN_LOCK(&gct->gc_spin);

    init_gc_thread(gct);

    traceEventGcWork(gct->cap);

    // Every thread evacuates some roots.
    gct->evac_gen_no = 0;
    markCapability(mark_root, gct, cap, rtsTrue/*prune sparks*/);
    scavenge_capability_mut_lists(cap);

    scavenge_until_all_done();

#ifdef THREADED_RTS
    // Now that the whole heap is marked, we discard any sparks that
    // were found to be unreachable.  The main GC thread is currently
    // marking heap reachable via weak pointers, so it is
    // non-deterministic whether a spark will be retained if it is
    // only reachable via weak pointers.  To fix this problem would
    // require another GC barrier, which is too high a price.
    pruneSparkQueue(cap);
#endif

    // Wait until we're told to continue
    RELEASE_SPIN_LOCK(&gct->gc_spin);
    gct->wakeup = GC_THREAD_WAITING_TO_CONTINUE;
    debugTrace(DEBUG_gc, "GC thread %d waiting to continue...",
               gct->thread_index);
    ACQUIRE_SPIN_LOCK(&gct->mut_spin);
    debugTrace(DEBUG_gc, "GC thread %d on my way...", gct->thread_index);

    SET_GCT(saved_gct);
}

#endif

#if defined(THREADED_RTS)

void
waitForGcThreads (Capability *cap USED_IF_THREADS)
{
    const nat n_threads = n_capabilities;
    const nat me = cap->no;
    nat i, j;
    rtsBool retry = rtsTrue;

    while(retry) {
        for (i=0; i < n_threads; i++) {
            if (i == me || gc_threads[i]->idle) continue;
            if (gc_threads[i]->wakeup != GC_THREAD_STANDING_BY) {
                prodCapability(capabilities[i], cap->running_task);
            }
        }
        for (j=0; j < 10; j++) {
            retry = rtsFalse;
            for (i=0; i < n_threads; i++) {
                if (i == me || gc_threads[i]->idle) continue;
                write_barrier();
                interruptCapability(capabilities[i]);
                if (gc_threads[i]->wakeup != GC_THREAD_STANDING_BY) {
                    retry = rtsTrue;
                }
            }
            if (!retry) break;
            yieldThread();
        }
    }
}

#endif // THREADED_RTS

static void
start_gc_threads (void)
{
#if defined(THREADED_RTS)
    gc_running_threads = 0;
#endif
}

static void
wakeup_gc_threads (nat me USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;

    if (n_gc_threads == 1) return;

    for (i=0; i < n_gc_threads; i++) {
        if (i == me || gc_threads[i]->idle) continue;
        inc_running();
        debugTrace(DEBUG_gc, "waking up gc thread %d", i);
        if (gc_threads[i]->wakeup != GC_THREAD_STANDING_BY) barf("wakeup_gc_threads");

        gc_threads[i]->wakeup = GC_THREAD_RUNNING;
        ACQUIRE_SPIN_LOCK(&gc_threads[i]->mut_spin);
        RELEASE_SPIN_LOCK(&gc_threads[i]->gc_spin);
    }
#endif
}

// After GC is complete, we must wait for all GC threads to enter the
// standby state, otherwise they may still be executing inside
// any_work(), and may even remain awake until the next GC starts.
static void
shutdown_gc_threads (nat me USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    nat i;

    if (n_gc_threads == 1) return;

    for (i=0; i < n_gc_threads; i++) {
        if (i == me || gc_threads[i]->idle) continue;
        while (gc_threads[i]->wakeup != GC_THREAD_WAITING_TO_CONTINUE) {
            busy_wait_nop();
            write_barrier();
        }
    }
#endif
}

#if defined(THREADED_RTS)
void
releaseGCThreads (Capability *cap USED_IF_THREADS)
{
    const nat n_threads = n_capabilities;
    const nat me = cap->no;
    nat i;
    for (i=0; i < n_threads; i++) {
        if (i == me || gc_threads[i]->idle) continue;
        if (gc_threads[i]->wakeup != GC_THREAD_WAITING_TO_CONTINUE)
            barf("releaseGCThreads");

        gc_threads[i]->wakeup = GC_THREAD_INACTIVE;
        ACQUIRE_SPIN_LOCK(&gc_threads[i]->gc_spin);
        RELEASE_SPIN_LOCK(&gc_threads[i]->mut_spin);
    }
}
#endif

/* ----------------------------------------------------------------------------
   Initialise a generation that is to be collected
   ------------------------------------------------------------------------- */

static void
prepare_collected_gen (generation *gen)
{
    nat i, g, n;
    gen_workspace *ws;
    bdescr *bd, *next;

    // Throw away the current mutable list.  Invariant: the mutable
    // list always has at least one block; this means we can avoid a
    // check for NULL in recordMutable().
    g = gen->no;
    if (g != 0) {
        for (i = 0; i < n_capabilities; i++) {
            freeChain(capabilities[i]->mut_lists[g]);
            capabilities[i]->mut_lists[g] = allocBlock();
        }
    }

    gen = &generations[g];
    ASSERT(gen->no == g);

    // we'll construct a new list of threads in this step
    // during GC, throw away the current list.
    gen->old_threads = gen->threads;
    gen->threads = END_TSO_QUEUE;

    // deprecate the existing blocks
    gen->old_blocks   = gen->blocks;
    gen->n_old_blocks = gen->n_blocks;
    gen->blocks       = NULL;
    gen->n_blocks     = 0;
    gen->n_words      = 0;
    gen->live_estimate = 0;

    // initialise the large object queues.
    ASSERT(gen->scavenged_large_objects == NULL);
    ASSERT(gen->n_scavenged_large_blocks == 0);

    // grab all the partial blocks stashed in the gc_thread workspaces and
    // move them to the old_blocks list of this gen.
    for (n = 0; n < n_capabilities; n++) {
        ws = &gc_threads[n]->gens[gen->no];

        for (bd = ws->part_list; bd != NULL; bd = next) {
            next = bd->link;
            bd->link = gen->old_blocks;
            gen->old_blocks = bd;
            gen->n_old_blocks += bd->blocks;
        }
        ws->part_list = NULL;
        ws->n_part_blocks = 0;
        ws->n_part_words = 0;

        ASSERT(ws->scavd_list == NULL);
        ASSERT(ws->n_scavd_blocks == 0);
        ASSERT(ws->n_scavd_words == 0);

        if (ws->todo_free != ws->todo_bd->start) {
            ws->todo_bd->free = ws->todo_free;
            ws->todo_bd->link = gen->old_blocks;
            gen->old_blocks = ws->todo_bd;
            gen->n_old_blocks += ws->todo_bd->blocks;
            alloc_todo_block(ws,0); // always has one block.
        }
    }

    // mark the small objects as from-space
    for (bd = gen->old_blocks; bd; bd = bd->link) {
        bd->flags &= ~BF_EVACUATED;
    }

    // TODO: This function needs to know which RC is being collected
    // mark the large objects as from-space
    //for (bd = gen->large_objects; bd; bd = bd->link) {
    //    bd->flags &= ~BF_EVACUATED;
    //}

    // for a compacted generation, we need to allocate the bitmap
    if (gen->mark) {
        StgWord bitmap_size; // in bytes
        bdescr *bitmap_bdescr;
        StgWord *bitmap;

        bitmap_size = gen->n_old_blocks * BLOCK_SIZE / (sizeof(W_)*BITS_PER_BYTE);

        if (bitmap_size > 0) {
            bitmap_bdescr = allocGroup((StgWord)BLOCK_ROUND_UP(bitmap_size)
                                       / BLOCK_SIZE);
            gen->bitmap = bitmap_bdescr;
            bitmap = bitmap_bdescr->start;

            debugTrace(DEBUG_gc, "bitmap_size: %d, bitmap: %p",
                       bitmap_size, bitmap);

            // don't forget to fill it with zeros!
            memset(bitmap, 0, bitmap_size);

            // For each block in this step, point to its bitmap from the
            // block descriptor.
            for (bd=gen->old_blocks; bd != NULL; bd = bd->link) {
                bd->u.bitmap = bitmap;
                bitmap += BLOCK_SIZE_W / (sizeof(W_)*BITS_PER_BYTE);

                // Also at this point we set the BF_MARKED flag
                // for this block.  The invariant is that
                // BF_MARKED is always unset, except during GC
                // when it is set on those blocks which will be
                // compacted.
                if (!(bd->flags & BF_FRAGMENTED)) {
                    bd->flags |= BF_MARKED;
                }

                // BF_SWEPT should be marked only for blocks that are being
                // collected in sweep()
                bd->flags &= ~BF_SWEPT;
            }
        }
    }
}


/* ----------------------------------------------------------------------------
   Save the mutable lists in saved_mut_lists
   ------------------------------------------------------------------------- */

static void
stash_mut_list (Capability *cap, nat gen_no)
{
    cap->r.rCurrentTSO->rc->saved_mut_lists[gen_no] = cap->r.rCurrentTSO->rc->mut_lists[gen_no];
    cap->r.rCurrentTSO->rc->mut_lists[gen_no] = allocBlock_sync();
}

/* ----------------------------------------------------------------------------
   Initialise a generation that is *not* to be collected
   ------------------------------------------------------------------------- */

static void
prepare_uncollected_gen (generation *gen)
{
    nat i;


    ASSERT(gen->no > 0);

    // save the current mutable lists for this generation, and
    // allocate a fresh block for each one.  We'll traverse these
    // mutable lists as roots early on in the GC.
    for (i = 0; i < n_capabilities; i++) {
        stash_mut_list(capabilities[i], gen->no);
    }

    ASSERT(gen->scavenged_large_objects == NULL);
    ASSERT(gen->n_scavenged_large_blocks == 0);
}

/* -----------------------------------------------------------------------------
   Collect the completed blocks from a GC thread and attach them to
   the generation.
   -------------------------------------------------------------------------- */

static void
collect_gct_blocks (void)
{
    nat g;
    gen_workspace *ws;
    bdescr *bd, *prev;

    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        ws = &gct->gens[g];

        // there may still be a block attached to ws->todo_bd;
        // leave it there to use next time.

        if (ws->scavd_list != NULL) {
            ACQUIRE_SPIN_LOCK(&ws->gen->sync);

            ASSERT(gct->scan_bd == NULL);
            ASSERT(countBlocks(ws->scavd_list) == ws->n_scavd_blocks);

            prev = NULL;
            for (bd = ws->scavd_list; bd != NULL; bd = bd->link) {
                prev = bd;
            }
            if (prev != NULL) {
                prev->link = ws->gen->blocks;
                ws->gen->blocks = ws->scavd_list;
            }
            ws->gen->n_blocks += ws->n_scavd_blocks;
            ws->gen->n_words += ws->n_scavd_words;

            ws->scavd_list = NULL;
            ws->n_scavd_blocks = 0;
            ws->n_scavd_words = 0;

            RELEASE_SPIN_LOCK(&ws->gen->sync);
        }
    }
}

/* -----------------------------------------------------------------------------
   During mutation, any blocks that are filled by allocatePinned() are
   stashed on the local pinned_object_blocks list, to avoid needing to
   take a global lock.  Here we collect those blocks from the
   cap->pinned_object_blocks lists and put them on the
   main g0->large_object list.
   Returns: the number of words allocated this way, for stats
   purposes.
   -------------------------------------------------------------------------- */

static void
collect_pinned_object_blocks (void)
{
    // TODO: This needs to be fixed! pinned objects belong to RCs, not capabilities!
    nat n;
    bdescr *bd, *prev;

    for (n = 0; n < n_capabilities; n++) {
        prev = NULL;
        // TODO: This needs to know which RC is being collected!
        //for (bd = capabilities[n]->pinned_object_blocks; bd != NULL; bd = bd->link) {
        //    prev = bd;
        //}
        if (prev != NULL) {
            // TODO: Same as above
            //prev->link = g0->large_objects;
            //if (g0->large_objects != NULL) {
            //    g0->large_objects->u.back = prev;
            //}
            //g0->large_objects = capabilities[n]->pinned_object_blocks;
            //capabilities[n]->pinned_object_blocks = 0;
        }
    }
}

/* -----------------------------------------------------------------------------
   Initialise a gc_thread before GC
   -------------------------------------------------------------------------- */

static void
init_gc_thread (gc_thread *t)
{
    t->static_objects = END_OF_STATIC_OBJECT_LIST;
    t->scavenged_static_objects = END_OF_STATIC_OBJECT_LIST;
    t->scan_bd = NULL;
    t->mut_lists = t->cap->mut_lists;
    t->evac_gen_no = 0;
    t->failed_to_evac = rtsFalse;
    t->eager_promotion = rtsTrue;
    t->thunk_selector_depth = 0;
    t->copied = 0;
    t->scanned = 0;
    t->any_work = 0;
    t->no_work = 0;
    t->scav_find_work = 0;
}

/* -----------------------------------------------------------------------------
   Function we pass to evacuate roots.
   -------------------------------------------------------------------------- */

static void
mark_root(void *user USED_IF_THREADS, StgClosure **root)
{
    // we stole a register for gct, but this function is called from
    // *outside* the GC where the register variable is not in effect,
    // so we need to save and restore it here.  NB. only call
    // mark_root() from the main GC thread, otherwise gct will be
    // incorrect.
#if defined(THREADED_RTS)
    gc_thread *saved_gct;
    saved_gct = gct;
#endif
    SET_GCT(user);

    evacuate(root);

    SET_GCT(saved_gct);
}

/* ----------------------------------------------------------------------------
   Reset the sizes of the older generations when we do a major
   collection.
   CURRENT STRATEGY: make all generations except zero the same size.
   We have to stay within the maximum heap size, and leave a certain
   percentage of the maximum heap size available to allocate into.
   ------------------------------------------------------------------------- */

static void
resize_generations (void)
{
    nat g;

    if (major_gc && RtsFlags.GcFlags.generations > 1) {
        W_ live, size, min_alloc, words;
        const W_ max  = RtsFlags.GcFlags.maxHeapSize;
        const W_ gens = RtsFlags.GcFlags.generations;

        // live in the oldest generations
        if (oldest_gen->live_estimate != 0) {
            words = oldest_gen->live_estimate;
        } else {
            words = oldest_gen->n_words;
        }
        // TODO: fixme
        //live = (words + BLOCK_SIZE_W - 1) / BLOCK_SIZE_W +
         //   oldest_gen->n_large_blocks;

        // default max size for all generations except zero
        size = stg_max(live * RtsFlags.GcFlags.oldGenFactor,
                       RtsFlags.GcFlags.minOldGenSize);

        if (RtsFlags.GcFlags.heapSizeSuggestionAuto) {
            if (max > 0) {
                RtsFlags.GcFlags.heapSizeSuggestion = stg_min(max, size);
            } else {
                RtsFlags.GcFlags.heapSizeSuggestion = size;
            }
        }

        // minimum size for generation zero
        min_alloc = stg_max((RtsFlags.GcFlags.pcFreeHeap * max) / 200,
                            RtsFlags.GcFlags.minAllocAreaSize);

        // Auto-enable compaction when the residency reaches a
        // certain percentage of the maximum heap size (default: 30%).
        if (RtsFlags.GcFlags.compact ||
            (max > 0 &&
             oldest_gen->n_blocks >
             (RtsFlags.GcFlags.compactThreshold * max) / 100)) {
            oldest_gen->mark = 1;
            oldest_gen->compact = 1;
//        debugBelch("compaction: on\n", live);
        } else {
            oldest_gen->mark = 0;
            oldest_gen->compact = 0;
//        debugBelch("compaction: off\n", live);
        }

        if (RtsFlags.GcFlags.sweep) {
            oldest_gen->mark = 1;
        }

        // if we're going to go over the maximum heap size, reduce the
        // size of the generations accordingly.  The calculation is
        // different if compaction is turned on, because we don't need
        // to double the space required to collect the old generation.
        if (max != 0) {

            // this test is necessary to ensure that the calculations
            // below don't have any negative results - we're working
            // with unsigned values here.
            if (max < min_alloc) {
                heapOverflow();
            }

            if (oldest_gen->compact) {
                if ( (size + (size - 1) * (gens - 2) * 2) + min_alloc > max ) {
                    size = (max - min_alloc) / ((gens - 1) * 2 - 1);
                }
            } else {
                if ( (size * (gens - 1) * 2) + min_alloc > max ) {
                    size = (max - min_alloc) / ((gens - 1) * 2);
                }
            }

            if (size < live) {
                heapOverflow();
            }
        }

#if 0
        debugBelch("live: %d, min_alloc: %d, size : %d, max = %d\n", live,
                   min_alloc, size, max);
#endif

        for (g = 0; g < gens; g++) {
            generations[g].max_blocks = size;
        }
    }
}

/* -----------------------------------------------------------------------------
   Calculate the new size of the nursery, and resize it.
   -------------------------------------------------------------------------- */

static void
resize_nursery (void)
{
    const StgWord min_nursery =
      RtsFlags.GcFlags.minAllocAreaSize * (StgWord)n_capabilities;

    if (RtsFlags.GcFlags.generations == 1)
    {   // Two-space collector:
        W_ blocks;

        /* set up a new nursery.  Allocate a nursery size based on a
         * function of the amount of live data (by default a factor of 2)
         * Use the blocks from the old nursery if possible, freeing up any
         * left over blocks.
         *
         * If we get near the maximum heap size, then adjust our nursery
         * size accordingly.  If the nursery is the same size as the live
         * data (L), then we need 3L bytes.  We can reduce the size of the
         * nursery to bring the required memory down near 2L bytes.
         *
         * A normal 2-space collector would need 4L bytes to give the same
         * performance we get from 3L bytes, reducing to the same
         * performance at 2L bytes.
         */
        blocks = generations[0].n_blocks;

        if ( RtsFlags.GcFlags.maxHeapSize != 0 &&
             blocks * RtsFlags.GcFlags.oldGenFactor * 2 >
             RtsFlags.GcFlags.maxHeapSize )
        {
            long adjusted_blocks;  // signed on purpose
            int pc_free;

            adjusted_blocks = (RtsFlags.GcFlags.maxHeapSize - 2 * blocks);

            debugTrace(DEBUG_gc, "near maximum heap size of 0x%x blocks, blocks = %d, adjusted to %ld",
                       RtsFlags.GcFlags.maxHeapSize, blocks, adjusted_blocks);

            pc_free = adjusted_blocks * 100 / RtsFlags.GcFlags.maxHeapSize;
            if (pc_free < RtsFlags.GcFlags.pcFreeHeap) /* might even * be < 0 */
            {
                heapOverflow();
            }
            blocks = adjusted_blocks;
        }
        else
        {
            blocks *= RtsFlags.GcFlags.oldGenFactor;
            if (blocks < min_nursery)
            {
                blocks = min_nursery;
            }
        }
        resizeNurseries(blocks);
    }
    else  // Generational collector
    {
        /*
         * If the user has given us a suggested heap size, adjust our
         * allocation area to make best use of the memory available.
         */
        if (RtsFlags.GcFlags.heapSizeSuggestion)
        {
            long blocks;
            StgWord needed;

            calcNeeded(rtsFalse, &needed); // approx blocks needed at next GC

            /* Guess how much will be live in generation 0 step 0 next time.
             * A good approximation is obtained by finding the
             * percentage of g0 that was live at the last minor GC.
             *
             * We have an accurate figure for the amount of copied data in
             * 'copied', but we must convert this to a number of blocks, with
             * a small adjustment for estimated slop at the end of a block
             * (- 10 words).
             */
            if (N == 0)
            {
                g0_pcnt_kept = ((copied / (BLOCK_SIZE_W - 10)) * 100)
                    / countNurseryBlocks();
            }

            /* Estimate a size for the allocation area based on the
             * information available.  We might end up going slightly under
             * or over the suggested heap size, but we should be pretty
             * close on average.
             *
             * Formula:            suggested - needed
             *                ----------------------------
             *                    1 + g0_pcnt_kept/100
             *
             * where 'needed' is the amount of memory needed at the next
             * collection for collecting all gens except g0.
             */
            blocks =
                (((long)RtsFlags.GcFlags.heapSizeSuggestion - (long)needed) * 100) /
                (100 + (long)g0_pcnt_kept);

            if (blocks < (long)min_nursery) {
                blocks = min_nursery;
            }

            resizeNurseries((W_)blocks);
        }
        else
        {
            // we might have added extra blocks to the nursery, so
            // resize back to the original size again.
            //resizeNurseriesFixed();
            // TODO: Resize the nurseries used here!
            //barf("Resize the damn nurseries");
        }
    }
}

/* -----------------------------------------------------------------------------
   Sanity code for CAF garbage collection.
   With DEBUG turned on, we manage a CAF list in addition to the SRT
   mechanism.  After GC, we run down the CAF list and blackhole any
   CAFs which have been garbage collected.  This means we get an error
   whenever the program tries to enter a garbage collected CAF.
   Any garbage collected CAFs are taken off the CAF list at the same
   time.
   -------------------------------------------------------------------------- */

#if defined(DEBUG)

static void gcCAFs(void)
{
    StgIndStatic *p, *prev;

    const StgInfoTable *info;
    nat i;

    i = 0;
    p = debug_caf_list;
    prev = NULL;

    for (p = debug_caf_list; p != (StgIndStatic*)END_OF_CAF_LIST;
         p = (StgIndStatic*)p->saved_info) {

        info = get_itbl((StgClosure*)p);
        ASSERT(info->type == IND_STATIC);

        if (p->static_link == NULL) {
            debugTrace(DEBUG_gccafs, "CAF gc'd at 0x%p", p);
            SET_INFO((StgClosure*)p,&stg_GCD_CAF_info); // stub it
            if (prev == NULL) {
                debug_caf_list = (StgIndStatic*)p->saved_info;
            } else {
                prev->saved_info = p->saved_info;
            }
        } else {
            prev = p;
            i++;
        }
    }

    debugTrace(DEBUG_gccafs, "%d CAFs live", i);
}
#endif