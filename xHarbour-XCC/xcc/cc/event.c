/****************************************************************************
 *                                                                          *
 * File    : event.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic function call queue.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#ifdef PROF

typedef struct _ENTRY {
    APPLYFN func;   /* fn(void *, void *, void *) */
    void *cl;
} ENTRY;

EVENTS events;

/****************************************************************************
 *                                                                          *
 * Function: attach                                                         *
 *                                                                          *
 * Purpose : Add a generic apply-function to the given list.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void attach(APPLYFN func, void *cl, LIST **list)
{
    ENTRY *p;

    p = memalloc(sizeof(*p), PERM);
    p->func = func;
    p->cl = cl;

    *list = listappend(p, *list);
}

/****************************************************************************
 *                                                                          *
 * Function: apply                                                          *
 *                                                                          *
 * Purpose : Call each apply-function in the given list.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void apply(LIST *event, void *arg1, void *arg2)
{
    if (event)
    {
        LIST *lp = event;

        do
        {
            ENTRY *p = lp->data;
            (*p->func)(p->cl, arg1, arg2);
            lp = lp->link;
        } while (lp != event);
    }
}

#endif  /* PROF */

