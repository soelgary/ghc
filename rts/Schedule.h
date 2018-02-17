/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Schedule.c
 * (RTS internal scheduler interface)
 *
 * -------------------------------------------------------------------------*/

#ifndef SCHEDULE_H
#define SCHEDULE_H

#include "rts/OSThreads.h"
#include "Capability.h"
#include "Trace.h"

#include "BeginPrivate.h"

/* initScheduler(), exitScheduler()
 * Called from STG :  no
 * Locks assumed   :  none
 */
void initScheduler (void);
void exitScheduler (bool wait_foreign);
void freeScheduler (void);
void markScheduler (evac_fn evac, void *user);

// Place a new thread on the run queue of the current Capability
void scheduleThread (Capability *cap, StgTSO *tso);

// Place a new thread on the run queue of a specified Capability
// (cap is the currently owned Capability, cpu is the number of
// the desired Capability).
void scheduleThreadOn(Capability *cap, StgWord cpu, StgTSO *tso);

/* wakeUpRts()
 *
 * Causes an OS thread to wake up and run the scheduler, if necessary.
 */
#if defined(THREADED_RTS)
void wakeUpRts(void);
#endif

/* raiseExceptionHelper */
StgWord raiseExceptionHelper (StgRegTable *reg, StgTSO *tso, StgClosure *exception);

/* findRetryFrameHelper */
StgWord findRetryFrameHelper (Capability *cap, StgTSO *tso);

/* Entry point for a new worker */
void scheduleWorker (Capability *cap, Task *task);

/* The state of the scheduler.  This is used to control the sequence
 * of events during shutdown.  See Note [shutdown] in Schedule.c.
 */
#define SCHED_RUNNING       0  /* running as normal */
#define SCHED_INTERRUPTING  1  /* before threads are deleted */
#define SCHED_SHUTTING_DOWN 2  /* final shutdown */

extern volatile StgWord sched_state;

/*
 * flag that tracks whether we have done any execution in this time
 * slice, and controls the disabling of the interval timer.
 *
 * The timer interrupt transitions ACTIVITY_YES into
 * ACTIVITY_MAYBE_NO, waits for RtsFlags.GcFlags.idleGCDelayTime,
 * and then:
 *   - if idle GC is no, set ACTIVITY_INACTIVE and wakeUpRts()
 *   - if idle GC is off, set ACTIVITY_DONE_GC and stopTimer()
 *
 * If the scheduler finds ACTIVITY_INACTIVE, then it sets
 * ACTIVITY_DONE_GC, performs the GC and calls stopTimer().
 *
 * If the scheduler finds ACTIVITY_DONE_GC and it has a thread to run,
 * it enables the timer again with startTimer().
 */
#define ACTIVITY_YES      0
  // the RTS is active
#define ACTIVITY_MAYBE_NO 1
  // no activity since the last timer signal
#define ACTIVITY_INACTIVE 2
  // RtsFlags.GcFlags.idleGCDelayTime has passed with no activity
#define ACTIVITY_DONE_GC  3
  // like ACTIVITY_INACTIVE, but we've done a GC too (if idle GC is
  // enabled) and the interval timer is now turned off.

/* Recent activity flag.
 * Locks required  : Transition from MAYBE_NO to INACTIVE
 * happens in the timer signal, so it is atomic.  Trnasition from
 * INACTIVE to DONE_GC happens under sched_mutex.  No lock required
 * to set it to ACTIVITY_YES.
 */
extern volatile StgWord recent_activity;

/* Thread queues.
 * Locks required  : sched_mutex
 */
#if !defined(THREADED_RTS)
extern  StgTSO *blocked_queue_hd, *blocked_queue_tl;
extern  StgTSO *sleeping_queue;
#endif

extern bool heap_overflow;

#if defined(THREADED_RTS)
extern Mutex sched_mutex;
#endif

/* Called by shutdown_handler(). */
void interruptStgRts (void);

void resurrectThreads (StgTSO *);

/* -----------------------------------------------------------------------------
 * Some convenient macros/inline functions...
 */

#if !IN_STG_CODE

/* END_TSO_QUEUE and friends now defined in includes/stg/MiscClosures.h */

/* Add a thread to the end of the run queue.
 * NOTE: tso->link should be END_TSO_QUEUE before calling this macro.
 * ASSUMES: cap->running_task is the current task.
 */
EXTERN_INLINE void
appendToRunQueue (Capability *cap, StgTSO *tso);

EXTERN_INLINE void
appendToRunQueue (Capability *cap, StgTSO *tso)
{
    if (tso->_link == tso) {
      debugTrace(DEBUG_sched, "Bad stuff is about to happen...");
    }
    ASSERT(tso->_link == END_TSO_QUEUE);
    tso->ticks_remaining = tso->ticks;
    if (cap->run_queue_hd == END_TSO_QUEUE) {
        debugTrace(DEBUG_sched, "Bad stuff is about to happen here...");
        cap->run_queue_hd = tso;
        tso->block_info.prev = END_TSO_QUEUE;
    } else {
        debugTrace(DEBUG_sched, "Bad stuff is about to happen over here...");
        setTSOLink(cap, cap->run_queue_tl, tso);
        setTSOPrev(cap, tso, cap->run_queue_tl);
    }
    cap->run_queue_tl = tso;
    cap->n_run_queue++;
    ASSERT(tso->_link != tso);
}

/* Push a thread on the beginning of the run queue.
 * ASSUMES: cap->running_task is the current task.
 */
EXTERN_INLINE void
pushOnRunQueue (Capability *cap, StgTSO *tso);

EXTERN_INLINE void
pushOnRunQueue (Capability *cap, StgTSO *tso)
{
  ASSERT(tso != cap->run_queue_hd);
  if (tso->_link == tso) {
    debugTrace(DEBUG_sched, "Bad stuff is about to happen...");
  }
  if (tso->id == 0x8) {
    debugTrace(DEBUG_sched, "Bad stuff is about to happen...!!");
  }
  setTSOLink(cap, tso, cap->run_queue_hd);
  tso->block_info.prev = END_TSO_QUEUE;
  if (cap->run_queue_hd != END_TSO_QUEUE) {
      setTSOPrev(cap, cap->run_queue_hd, tso);
  }
  cap->run_queue_hd = tso;
  if (cap->run_queue_tl == END_TSO_QUEUE) {
      cap->run_queue_tl = tso;
  }
  cap->n_run_queue++;
  ASSERT(tso->_link != tso);
}

/* Pop the first thread off the runnable queue.
 */
INLINE_HEADER StgTSO *
popRunQueue (Capability *cap)
{
  StgTSO *t;
  t = cap->run_queue_hd;
  ASSERT(t != END_TSO_QUEUE);
  cap->run_queue_hd = t->_link;
  if (t->_link != END_TSO_QUEUE) {
      t->_link->block_info.prev = END_TSO_QUEUE;
  }
  t->_link = END_TSO_QUEUE; // no write barrier req'd
  if (cap->run_queue_hd == END_TSO_QUEUE) {
      cap->run_queue_tl = END_TSO_QUEUE;
  }
  cap->n_run_queue--;
  return t;
}

/* H threads will reach a point where they must find a higher (in the hierarchy)
 * thread to schedule. The algorithm for this is as follows.
 * 
 *  Loop until a thread is found or the main H thread is reached
 *  1. If there is a thread directly to the right, schedule it.
 *  2. Jump to the parent
 *  3. If the parent is the main H thread, append it to the run queue
 * 
 */
void
contextSwitchHThreadUP (Capability *cap, StgTSO *tso);

/* Context switching hierarchical threads is a little tricky. The only H thread
 * allowed to remain on the run queue is the H thread that owns the capability.
 * This way the queue does not get flooded with secret TSOs. A context switch
 * needs to find the next H thread to run and push it on the run queue.
 * Additionally, it needs to remove the previous H thread from the run queue,
 * so long as it is not the parent. If there is not another H thread to run,
 * this will append to the main H thread to the run queue.
 *
 * H Thread hierarchy
 *            |P|
 *           /   \
 *        |c1|   |c2|
 *       /    \
 *    |c3|    |c4|
 * 
 * 
 * Given the thread hierarchy, each time 'contextSwitchHThread' gets called,
 * the queue should be changed as follows. Threads t1...tn are normal threads
 * scheduled alongside the H threads.
 * 
 * Start: |P| -> |t1| -> ... -> |tn|
 *        ==>
 * CS 1:  |c1| -> |P| -> |t1| -> ... -> |tn|
 *        ==>
 * CS 2:  |c3| -> |P| -> |t1| -> ... -> |tn|
 *        ==>
 * CS 3:  |c4| -> |P| -> |t1| -> ... -> |tn|
 *        ==>
 * CS 4:  |c2| -> |P| -> |t1| -> ... -> |tn|
 *        ==>
 * CS 5:  |t1| -> ... -> |tn| -> |P|
 * 
 */
void
contextSwitchHThread (Capability *cap, StgTSO *tso);


INLINE_HEADER StgTSO *
peekRunQueue (Capability *cap)
{
  return cap->run_queue_hd;
}

void promoteInRunQueue (Capability *cap, StgTSO *tso);

/* Add a thread to the end of the blocked queue.
 */
#if !defined(THREADED_RTS)
INLINE_HEADER void
appendToBlockedQueue(StgTSO *tso)
{
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (blocked_queue_hd == END_TSO_QUEUE) {
        blocked_queue_hd = tso;
    } else {
        setTSOLink(&MainCapability, blocked_queue_tl, tso);
    }
    blocked_queue_tl = tso;
}
#endif

/* Check whether various thread queues are empty
 */
INLINE_HEADER bool
emptyQueue (StgTSO *q)
{
    return (q == END_TSO_QUEUE);
}

INLINE_HEADER bool
emptyRunQueue(Capability *cap)
{
    return cap->n_run_queue == 0;// && cap->n_hrun_queue == 0;
}

INLINE_HEADER void
truncateRunQueue(Capability *cap)
{
    cap->run_queue_hd = END_TSO_QUEUE;
    cap->run_queue_tl = END_TSO_QUEUE;
    cap->n_run_queue = 0;
}

#if !defined(THREADED_RTS)
#define EMPTY_BLOCKED_QUEUE()  (emptyQueue(blocked_queue_hd))
#define EMPTY_SLEEPING_QUEUE() (emptyQueue(sleeping_queue))
#endif

INLINE_HEADER bool
emptyThreadQueues(Capability *cap)
{
    return emptyRunQueue(cap)
#if !defined(THREADED_RTS)
        && EMPTY_BLOCKED_QUEUE() && EMPTY_SLEEPING_QUEUE()
#endif
    ;
}

#endif /* !IN_STG_CODE */

#include "EndPrivate.h"

#endif /* SCHEDULE_H */
