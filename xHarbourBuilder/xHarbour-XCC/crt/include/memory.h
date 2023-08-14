#ifndef _MEMORY_H
#define _MEMORY_H

/* memory.h - private header for memory definitions */

#pragma message("Use <string.h> instead of <memory.h>")

/* type definitions */
#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

/* declarations */
void * __cdecl memchr(const void *, int, size_t);
int __cdecl memcmp(const void *, const void *, size_t);
void * __cdecl memcpy(void * restrict, const void * restrict, size_t);
void * __cdecl memmove(void *, const void *, size_t);
void * __cdecl memset(void *, int, size_t);

#endif /* _MEMORY_H */

