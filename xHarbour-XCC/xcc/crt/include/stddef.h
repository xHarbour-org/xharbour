#ifndef _STDDEF_H
#define _STDDEF_H

/* stddef.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#ifndef offsetof
#define offsetof(ty,m)  ((size_t)&(((ty*)0)->m))
#endif

/* type definitions */
#ifndef _PTRDIFF_T_DEFINED
#define _PTRDIFF_T_DEFINED
typedef int ptrdiff_t;
#endif

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

#ifndef _WCHAR_T_DEFINED
#define _WCHAR_T_DEFINED
typedef unsigned short wchar_t;
#endif

#endif /* _STDDEF_H */


