#ifndef _MALLOC_H
#define _MALLOC_H

/* malloc.h - private header for memory allocation definitions */

#pragma message("Use <stdlib.h> instead of <malloc.h>")

/* type definitions */
#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

/* declarations */
void * __cdecl calloc(size_t, size_t);
void __cdecl free(void *);
void * __cdecl malloc(size_t);
void * __cdecl realloc(void *, size_t);
void * __cdecl _alloca(size_t);
size_t __cdecl _msize(void *);

#endif /* _MALLOC_H */


