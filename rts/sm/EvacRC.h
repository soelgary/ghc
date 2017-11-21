#ifndef SM_EVACRC_H
#define SM_EVACRC_H

#include "BeginPrivate.h"

// Use a register argument for evacuate, if available.
// Earlier, the regparm attribute was used whenever __GNUC__ >= 2, but this
// generated warnings on PPC. So the use is restricted further.
// See http://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html that says
//     regparm (number)
//         On the Intel 386, the regparm attribute causes the compiler to pass
//         arguments number one to number if they are of integral type in
//         registers EAX, EDX, and ECX instead of on the stack. Functions that
//         take a variable number of arguments will continue to be passed all of
//         their arguments on the stack.

void evac_rc (StgClosure **p, ResourceContainer *rc);

#include "EndPrivate.h"

#endif /* SM_EVAC_H */

