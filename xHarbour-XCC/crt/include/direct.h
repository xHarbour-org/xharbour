#ifndef _DIRECT_H
#define _DIRECT_H

/* direct.h - private header for directory handling */

/* type definitions */
#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

/* declarations */
int __cdecl _chdir(const char *);
int __cdecl _chdrive(int);
char * __cdecl _getcwd(char *, size_t);
int __cdecl _getdrive(void);
int __cdecl _mkdir(const char *);
int __cdecl _rmdir(const char *);

#endif /* _DIRECT_H */
