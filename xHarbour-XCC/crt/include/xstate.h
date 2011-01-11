#ifndef _XSTATE_H
#define _XSTATE_H

/* xstate.h - internal header */

/* macros for finite state machines */
#define _ST_CH      0x00ff
#define _ST_STATE   0x0f00
#define _ST_STOFF   8
#define _ST_FOLD    0x8000
#define _ST_INPUT   0x4000
#define _ST_OUTPUT  0x2000
#define _ST_ROTATE  0x1000
#define _NSTATE     16

/* type definitions */
typedef struct {
    const unsigned short *tab[_NSTATE];
} __fsmtab;

extern __fsmtab __collatetab;
extern __fsmtab __wcollatetab;
extern __fsmtab __mbtowctab;
extern __fsmtab __wctombtab;

#endif /* _XSTATE_H */

