#ifndef _XWCSXFRM_H
#define _XWCSXFRM_H

#include <xstate.h>
#include <xwchar.h>

/* xwcsxfrm.h - internal header */

/* declarations */
size_t __wcsxfrm(wchar_t *, const wchar_t **, size_t, mbstate_t *);

#endif /* _XWCSXFRM_H */

