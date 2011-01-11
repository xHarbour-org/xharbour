#ifndef _XSTRXFRM_H
#define _XSTRXFRM_H

#include <string.h>
#include <xstate.h>

/* xstrxfrm.h - internal header */

/* type definitions */
typedef struct {  /* storage for transformations */
    const unsigned char *sbegin, *sin, *send;
    long weight;
    unsigned short phase, state, wc;
} __xfrm;

/* declarations */
size_t __cstrxfrm(char *, size_t, __xfrm *);

#endif /* _XSTRXFRM_H */

