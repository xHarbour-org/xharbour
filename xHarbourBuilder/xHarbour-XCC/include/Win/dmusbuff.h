#ifndef _DMUSBUFF_H
#define _DMUSBUFF_H

/* DirectMusic shared file buffer format definitions */

#define DMUS_EVENT_STRUCTURED   0x00000001

#define QWORD_ALIGN(x)  (((x)+7)&~7)
#define DMUS_EVENT_SIZE(cb)  QWORD_ALIGN(sizeof(DMUS_EVENTHEADER)+cb)

#include <pshpack4.h>
typedef struct _DMUS_EVENTHEADER *LPDMUS_EVENTHEADER;
typedef struct _DMUS_EVENTHEADER {
    DWORD cbEvent;
    DWORD dwChannelGroup;
    REFERENCE_TIME rtDelta;
    DWORD dwFlags;
} DMUS_EVENTHEADER;
#include <poppack.h>

#endif /* _DMUSBUFF_H */

