#ifndef _STRING_H
#define _STRING_H

/* string.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

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
char * __cdecl strcat(char * restrict, const char * restrict);
char * __cdecl strchr(const char *, int);
int __cdecl strcmp(const char *, const char *);
char * __cdecl strcpy(char * restrict, const char * restrict);
size_t __cdecl strcspn(const char *, const char *);
size_t __cdecl strlen(const char *);
char * __cdecl strncat(char * restrict, const char * restrict, size_t);
int __cdecl strncmp(const char *, const char *, size_t);
char * __cdecl strncpy(char * restrict, const char * restrict, size_t);
char * __cdecl strpbrk(const char *, const char *);
char * __cdecl strrchr(const char *, int);
size_t __cdecl strspn(const char *, const char *);
char * __cdecl strstr(const char *, const char *);
char * __cdecl strtok(char * restrict, const char * restrict);
#ifndef _WINCE
int __cdecl strcoll(const char *, const char *);
char * __cdecl strerror(int);
size_t __cdecl strxfrm(char * restrict, const char * restrict, size_t);
#endif /* _WINCE */

/* private extensions to standard C */
int __cdecl _memicmp(const void *, const void *, size_t);
char * __cdecl _strdup(const char *);
int __cdecl _stricmp(const char *, const char *);
char * __cdecl _strlwr(char *);
int __cdecl _strnicmp(const char *, const char *, size_t);
char * __cdecl _strnset(char *, int, size_t);
char * __cdecl _strrev(char *);
char * __cdecl _strupr(char *);

/* compatibility names */
#ifdef __POCC__OLDNAMES
int __cdecl memicmp(const void *, const void *, size_t);
char * __cdecl strdup(const char *);
int __cdecl stricmp(const char *, const char *);
char * __cdecl strlwr(char *);
int __cdecl strnicmp(const char *, const char *, size_t);
char * __cdecl strnset(char *, int, size_t);
char * __cdecl strrev(char *);
char * __cdecl strupr(char *);
#endif /* __POCC__OLDNAMES */

#endif /* _STRING_H */
