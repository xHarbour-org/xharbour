/****************************************************************************
 *                                                                          *
 * File    : hideset.c                                                      *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; preprocessor; hideset management.     *
 *                                                                          *
 * Comment : A hideset is a null-terminated array of NLIST pointers.        *
 *           They are referred to by indices in the hidesets array.         *
 *           Hideset 0 is empty.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

#define HSSIZ  32

typedef NLIST **HIDESET;

/* Locals */
static HIDESET *hidesets;
static int nhidesets = 0;
static int maxhidesets = 3;

/* Static function prototypes */
static int insert_hideset(HIDESET, HIDESET, NLIST *);

/****************************************************************************
 *                                                                          *
 * Function: setup_hideset                                                  *
 *                                                                          *
 * Purpose : Initialize the hideset manager.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-12-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

void setup_hideset(void)
{
    hidesets = (HIDESET *)my_alloc(maxhidesets * sizeof(HIDESET *));
    hidesets[0] = (HIDESET)my_alloc(sizeof(HIDESET));
    *hidesets[0] = NULL;
    nhidesets++;
}

/****************************************************************************
 *                                                                          *
 * Function: check_hideset                                                  *
 *                                                                          *
 * Purpose : Test for membership in a hideset.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-12-12  Created                                              *
 *           01-11-09  Added call to pp_error().                            *
 *                                                                          *
 ****************************************************************************/

bool_t check_hideset(int hs, NLIST *np)
{
    HIDESET hsp;

    if (hs >= nhidesets)
        pp_error(RCFATAL(ERROR_INTERNAL), "check_hideset");

    for (hsp = hidesets[hs]; *hsp; hsp++)
        if (*hsp == np) return TRUE;

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: new_hideset                                                    *
 *                                                                          *
 * Purpose : Return (possibly new) hideset obtained by adding np to hs.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-12-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

int new_hideset(int hs, NLIST *np)
{
    NLIST *nhs[HSSIZ+3];
    HIDESET hs1;
    HIDESET hs2;
    int len;
    int i;

    len = insert_hideset(nhs, hidesets[hs], np);
    for (i = 0; i < nhidesets; i++)
    {
        for (hs1 = nhs, hs2 = hidesets[i]; *hs1 == *hs2; hs1++, hs2++)
            if (*hs1 == NULL) return i;
    }

    if (len >= HSSIZ)
        return hs;

    if (nhidesets >= maxhidesets)
    {
        maxhidesets = 3*maxhidesets/2 + 1;
        hidesets = (HIDESET *)my_realloc(hidesets, maxhidesets * sizeof(HIDESET *));
    }

    hs1 = (HIDESET)my_alloc(len * sizeof(HIDESET));
    memmove(hs1, nhs, len * sizeof(HIDESET));
    hidesets[nhidesets] = hs1;

    return nhidesets++;
}

/****************************************************************************
 *                                                                          *
 * Function: insert_hideset                                                 *
 *                                                                          *
 * Purpose : Hideset insert.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-12-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int insert_hideset(HIDESET dhs, HIDESET shs, NLIST *np)
{
    HIDESET odhs = dhs;

    while (*shs && *shs < np)
        *dhs++ = *shs++;

    if (*shs != np)
        *dhs++ = np;

    do
        *dhs++ = *shs;
    while (*shs++);

    return dhs - odhs;
}

/****************************************************************************
 *                                                                          *
 * Function: union_hideset                                                  *
 *                                                                          *
 * Purpose : Hideset union.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-12-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

int union_hideset(int hs1, int hs2)
{
    HIDESET hp;

    for (hp = hidesets[hs2]; *hp; hp++)
        hs1 = new_hideset(hs1, *hp);

    return hs1;
}

/****************************************************************************
 *                                                                          *
 * Function: print_hideset                                                  *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-12-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef PODEBUG
void print_hideset(int hs)
{
    HIDESET np;

    for (np = hidesets[hs]; *np; np++)
    {
        fprintf(stdout, (char *)(*np)->name, (*np)->len);
        fprintf(stdout, " ");
    }
}
#endif

