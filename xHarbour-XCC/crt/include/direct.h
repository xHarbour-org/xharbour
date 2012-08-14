#ifndef _DIRECT_H
#define _DIRECT_H

/* direct.h - private header for directory handling */

/* type definitions */
#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

/* declarations */
int __cdecl _chdir(const char *);  /* WINCE: in crtce.lib */
#ifndef _WINCE
int __cdecl _chdrive(int);
char * __cdecl _getcwd(char *, size_t);
int __cdecl _getdrive(void);
#endif /* _WINCE */
int __cdecl _mkdir(const char *);  /* WINCE: in crtce.lib */
int __cdecl _rmdir(const char *);  /* WINCE: in crtce.lib */

/* compatibility names */
#ifdef __POCC__OLDNAMES
int __cdecl chdir(const char *);
char * __cdecl getcwd(char *, size_t);
int __cdecl mkdir(const char *);
int __cdecl rmdir(const char *);
#endif /* __POCC__OLDNAMES */

#endif /* _DIRECT_H */
