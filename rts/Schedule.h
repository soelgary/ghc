/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Schedule.c
 * (RTS internal scheduler interface)
 *
 * -------------------------------------------------------------------------*/

#pragma once

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
 *   - if idle GC is on, set ACTIVITY_INACTIVE and wakeUpRts()
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

int countChildren(StgTSO *tso);
int countSiblings(StgTSO *tso);
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
    if (tso->isHThread) return;
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (cap->run_queue_hd == END_TSO_QUEUE) {
        cap->run_queue_hd = tso;
        tso->block_info.prev = END_TSO_QUEUE;
    } else {
        setTSOLink(cap, cap->run_queue_tl, tso);
        setTSOPrev(cap, tso, cap->run_queue_tl);
    }
    cap->run_queue_tl = tso;
    cap->n_run_queue++;
    ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
}

/* Push a thread on the beginning of the run queue.
 * ASSUMES: cap->running_task is the current task.
 */
EXTERN_INLINE void
pushOnRunQueue (Capability *cap, StgTSO *tso);

EXTERN_INLINE void
pushOnRunQueue (Capability *cap, StgTSO *tso)
{
    if (tso->isHThread) {
      ASSERT(tso != END_TSO_QUEUE);
      cap->hrun_queue_current = tso;
      cap->cached_tso = tso;
      return;
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
    ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
}


EXTERN_INLINE StgTSO *
nextHThread (Capability *cap);

EXTERN_INLINE StgTSO *
nextHThread (Capability *cap)
{
  StgTSO *next = END_TSO_QUEUE;
  StgTSO *current = cap->hrun_queue_current;

  // An Hthread has not been scheduled yet
  if (cap->hrun_queue_current == END_TSO_QUEUE) {
    //ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
    return cap->hrun_queue_top;
  }

  /*
    Algorithm:
      1. If the tso has a child, it is next
      2. If the tso does not have a child and has a sibling, the sibling is next
      3. If the tso does not have a child or a sibling then...
        a. let current = tso->parent
        b. if current has a sibling, schedule it
        c. else if current does not have a parent, schedule it
        c. else jump to (a)
  */

  if (current->children != END_TSO_QUEUE) {
    next = current->children;
  } else if (current->hlink != END_TSO_QUEUE) {
    next = current->hlink;
  } else {
    while (1) {
      if (current->parent == END_TSO_QUEUE) {
        next = current;
        break;
      } else if (current->hlink != END_TSO_QUEUE) {
        next = current->hlink;
        break; 
      } else {
        current = current->parent;
      }
    }
  }
  ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
  return next;
}

EXTERN_INLINE void
popRunQueueH (Capability *cap);

EXTERN_INLINE void
popRunQueueH (Capability *cap)
{
  if (cap->hlast_run != END_TSO_QUEUE) {
    cap->hrun_queue_current = cap->hlast_run;
    cap->hlast_run = END_TSO_QUEUE;
    ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
    return;
  }
  if (cap->cached_tso != END_TSO_QUEUE) {
    cap->hrun_queue_current = cap->cached_tso;
    cap->cached_tso = END_TSO_QUEUE;
    ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
    return;
  }
  StgTSO *next = nextHThread(cap);

  cap->hrun_queue_current = next;
  //while (next->isDone || next->why_blocked != NotBlocked ) {
  //  next = nextHThread(cap);
  //  cap->hrun_queue_current = next;
  //}
  debugTrace(DEBUG_sched, "HSched: Queueing up TSO %d", next->id);
  ASSERT(next != END_TSO_QUEUE);
  ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
}



/* Pop the first thread off the runnable queue.
 */
INLINE_HEADER StgTSO *
popRunQueue (Capability *cap)
{
    popRunQueueH(cap);
    ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
    return cap->hrun_queue_current;
    /*StgTSO *t = cap->run_queue_hd;
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
    
    return t;*/
}

INLINE_HEADER StgTSO *
peekRunQueueH (Capability *cap)
{
  debugTrace(DEBUG_sched, "Starting peek");
  if (countChildren(cap->hrun_queue_top) == 0) {
    return END_TSO_QUEUE;
  }
  if (cap->hlast_run != END_TSO_QUEUE) {
    ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
    return cap->hlast_run;
  }
  if (cap->hrun_queue_current == END_TSO_QUEUE) {
    ASSERT(cap->hrun_queue_top != END_TSO_QUEUE);
    return cap->hrun_queue_top;
  }
  if (cap->no == 1) {
    debugTrace(DEBUG_sched, "Found our cap");
  }
  StgTSO *current = cap->hrun_queue_current;
  StgTSO *next = nextHThread(cap);
  cap->hrun_queue_current = next;
  while (next->what_next != ThreadRunGHC || next->why_blocked != NotBlocked) {
    //__attribute__((__unused__)) int count = countChildren(cap->hrun_queue_top);
    //debugTrace(DEBUG_sched, "There are %d in the queue!!", count);
    next = nextHThread(cap);
    cap->hrun_queue_current = next;
  }
  cap->hrun_queue_current = current;
  //ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
  return next;
}


INLINE_HEADER StgTSO *
peekRunQueue (Capability *cap)
{
  //if (cap->run_queue_hd != END_TSO_QUEUE) {
  //return cap->run_queue_hd;
  //}
  //ASSERT(cap->hrun_queue_current != END_TSO_QUEUE);
  return peekRunQueueH(cap);
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
emptyRunQueueH(Capability *cap)
{
  StgTSO *current = cap->hrun_queue_top;
  int count = countChildren(current);
  //debugTrace(DEBUG_sched, "There are %d in the queue", count);
  return count == 0;
  //return cap->n_hrun_queue == 0 || cap->hrun_queue_top == END_TSO_QUEUE;
}

INLINE_HEADER bool
emptyRunQueue(Capability *cap)
{
    return emptyRunQueueH(cap);
    //return cap->n_run_queue == 0;// && emptyRunQueueH(cap);
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
