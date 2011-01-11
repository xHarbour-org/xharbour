#ifndef _STDARG_H
#define _STDARG_H

/* stdarg.h - C99 standard header */

/* type definitions */
#ifndef _VA_LIST_DEFINED
#define _VA_LIST_DEFINED
typedef char *va_list;
#endif

/* macros */
#define va_arg(ap,ty)      (*(ty *)(((ap) += __align(ty)) - __align(ty)))
#define va_end(ap)         (void)0
#define va_start(ap,arg0)  (void)((ap) = (char *)&(arg0) + __align(arg0))
#define va_copy(ap,sp)     (void)((ap) = (sp))

#define __align(ty)        ((sizeof(ty) + 0x3) & ~0x3)

#endif /* _STDARG_H */

