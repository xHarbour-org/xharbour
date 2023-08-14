/****************************************************************************
 *                                                                          *
 * File    : list.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Circular list management.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

static LIST *freenodes;  /* free list nodes */

/****************************************************************************
 *                                                                          *
 * Function: listappend                                                     *
 *                                                                          *
 * Purpose : Append data to list, return new list.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

LIST *listappend(void *data, LIST *list)
{
    LIST *newlist;

    if ((newlist = freenodes) != NULL)
        freenodes = freenodes->link;
    else
        newlist = memalloc(sizeof(*newlist), PERM);

    if (list != NULL)
    {
        newlist->link = list->link;
        list->link = newlist;
    }
    else
    {
        newlist->link = newlist;
    }

    newlist->data = data;
    return newlist;
}

/****************************************************************************
 *                                                                          *
 * Function: listinsert                                                     *
 *                                                                          *
 * Purpose : Insert data in list, return new list.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

LIST *listinsert(void *data, LIST *prev, LIST *list)
{
    LIST *newlist;

    if (prev == NULL)
        return listappend(data, list);

    if ((newlist = freenodes) != NULL)
        freenodes = freenodes->link;
    else
        newlist = memalloc(sizeof(*newlist), PERM);

    newlist->link = prev->link;
    prev->link = newlist;

    if (list == prev)
        list = newlist;

    newlist->data = data;
    return list;
}

/****************************************************************************
 *                                                                          *
 * Function: listdelete                                                     *
 *                                                                          *
 * Purpose : Delete data in list, return new list.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

LIST *listdelete(LIST *prev, LIST *list)
{
    LIST *oldlist = prev->link;

    if (prev != list)
    {
        prev->link = oldlist->link;

        oldlist->link = freenodes;
        freenodes = oldlist;

        return list;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: listelems                                                      *
 *                                                                          *
 * Purpose : Count number of elements in list.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int listelems(LIST *list)
{
    int n = 0;

    if (list != NULL)
    {
        LIST *lp = list;
        do
            n++;
        while ((lp = lp->link) != list);
    }

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: listvector                                                     *
 *                                                                          *
 * Purpose : Convert list to a NULL-terminated vector allocated in arena.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *listvector(LIST **list, uint_t arena)
{
    void **array = memarray(listelems(*list)+1, sizeof(array[0]), arena);
    int i = 0;

    if (*list)
    {
        LIST *lp = *list;
        do
        {
            lp = lp->link;
            array[i++] = lp->data;
        } while (lp != *list);

#ifndef PURIFY
        lp = (*list)->link;
        (*list)->link = freenodes;
        freenodes = lp;
#endif
    }

    *list = NULL;
    array[i] = NULL;

    return array;
}

