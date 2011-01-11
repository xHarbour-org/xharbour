#ifndef _XWCTYPE_H
#define _XWCTYPE_H

#include <stddef.h>
#include <wctype.h>

/* xwctype.h - internal header */

/* type definitions */
typedef struct {  /* wide-character type table */
    const char *name;
    size_t off;
} __wcinfo;

/* data declarations */
extern const __wcinfo *__wctranstab, *__wctypetab;

#endif /* _XWCTYPE_H */

