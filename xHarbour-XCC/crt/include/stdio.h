#ifndef _STDIO_H
#define _STDIO_H

/* stdio.h - C99 standard header */

#ifndef _CRTIMP
#ifdef _DLL
#define _CRTIMP  __declspec(dllimport)
#else
#define _CRTIMP
#endif
#endif /* _CRTIMP */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#define EOF  (-1)

#define FILENAME_MAX  260

#ifndef _WINCE

#define _IOFBF  0
#define _IOLBF  1
#define _IONBF  2

#define BUFSIZ  512

#define FOPEN_MAX  256 /*16*/

#define L_tmpnam  16
#define TMP_MAX  32

#endif /* _WINCE */

#define SEEK_SET  0
#define SEEK_CUR  1
#define SEEK_END  2

#ifndef _WINCE
#define stdin   (&__stdin)
#define stdout  (&__stdout)
#define stderr  (&__stderr)
#else /* _WINCE */
#define stdin  _getstdfilex(0)
#define stdout  _getstdfilex(1)
#define stderr  _getstdfilex(2)
#endif /* _WINCE */

/* type definitions */
#ifndef _MBSTATE_T_DEFINED
#define _MBSTATE_T_DEFINED
typedef struct mbstate_t {
    unsigned long wchar;
    unsigned short rsrv, state;
} mbstate_t;
#endif

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

#ifndef _VA_LIST_DEFINED
#define _VA_LIST_DEFINED
typedef char *va_list;
#endif

#ifndef _WINCE
typedef struct FILE {
    unsigned short mode;
    /*lockno*/
    int fh;
    unsigned char *buf, *bufend, *ptr;
    unsigned char *getend, *putend, *backptr;
    unsigned short /*wchar_t*/ *wbackptr, wbackbuf[2];
    unsigned char *getback, *wgetend, *wputend;
    mbstate_t wstate;
    char *tmpnam;
    unsigned char backbuf[8], cbuf;
} FILE;

typedef struct fpos_t {
    long off;  /* system dependent */
    mbstate_t wstate;
} fpos_t;
#else /* _WINCE */
typedef void FILE;
typedef long fpos_t;
#endif /* _WINCE */

/* operations on files */
int __cdecl remove(const char *);  /* WINCE: in crtce.lib */
int __cdecl rename(const char *, const char *);  /* WINCE: in crtce.lib */
#ifndef _WINCE
FILE * __cdecl tmpfile(void);
char * __cdecl tmpnam(char *);
#endif /* _WINCE */

/* file access functions */
int __cdecl fclose(FILE *);
int __cdecl fflush(FILE *);
FILE * __cdecl fopen(const char * restrict, const char * restrict);
int __cdecl setvbuf(FILE * restrict, char * restrict, int, size_t);
#ifndef _WINCE
FILE * __cdecl freopen(const char * restrict, const char * restrict, FILE * restrict);
void __cdecl setbuf(FILE * restrict, char * restrict);
#endif /* _WINCE */

/* formatted input/output functions */
int __cdecl fprintf(FILE * restrict, const char * restrict, ...);
int __cdecl fscanf(FILE * restrict, const char * restrict, ...);
int __cdecl sprintf(char * restrict, const char * restrict, ...);
int __cdecl sscanf(const char * restrict, const char * restrict, ...);
int __cdecl vfprintf(FILE * restrict, const char * restrict, va_list);
int __cdecl vsprintf(char * restrict, const char * restrict, va_list);
int __cdecl vprintf(const char * restrict, va_list);
int __cdecl printf(const char * restrict, ...);
int __cdecl scanf(const char * restrict, ...);
#ifndef _WINCE
int __cdecl vfscanf(FILE * restrict, const char * restrict, va_list);
int __cdecl vsscanf(const char * restrict, const char * restrict, va_list);
int __cdecl vscanf(const char * restrict, va_list);
int __cdecl snprintf(char * restrict, size_t, const char * restrict, ...);
int __cdecl vsnprintf(char * restrict, size_t, const char * restrict, va_list);
#else /* _WINCE */
#define snprintf  _snprintf
#define vsnprintf  _vsnprintf
#endif /* _WINCE */

/* character input/output functions */
int __cdecl fgetc(FILE *);
char * __cdecl fgets(char * restrict, int, FILE * restrict);
int __cdecl fputc(int, FILE *);
int __cdecl fputs(const char * restrict, FILE * restrict);
int __cdecl getchar(void);
char * __cdecl gets(char *);
int __cdecl putchar(int);
int __cdecl puts(const char *);
int __cdecl ungetc(int, FILE *);
#ifndef _WINCE
int __cdecl getc(FILE *);
int __cdecl putc(int, FILE *);
#else /* _WINCE */
#define getc(str)  fgetc(str)
#define putc(c,str)  fputc(c,str)
#endif /* _WINCE */

/* direct input/output functions */
size_t __cdecl fread(void * restrict, size_t, size_t, FILE * restrict);
size_t __cdecl fwrite(const void * restrict, size_t, size_t, FILE * restrict);

/* file positioning functions */
int __cdecl fgetpos(FILE * restrict, fpos_t * restrict);
int __cdecl fseek(FILE *, long, int);
int __cdecl fsetpos(FILE *, const fpos_t *);
long __cdecl ftell(FILE *);
void __cdecl rewind(FILE *);  /* WINCE: in crtce.lib */

/* error-handling functions */
void __cdecl clearerr(FILE *);
int __cdecl feof(FILE *);
int __cdecl ferror(FILE *);
#ifndef _WINCE
void __cdecl perror(const char *);
#endif /* _WINCE */

/* private extensions to standard C */
int __cdecl _fileno(FILE *);
int __cdecl _fcloseall(void);
#ifndef _WINCE
FILE * __cdecl _fdopen(int, const char *);
#else /* _WINCE */
FILE * __cdecl _wfopen(const unsigned short /*wchar_t*/ *, const unsigned short /*wchar_t*/ *);
FILE * __cdecl _wfreopen(const unsigned short /*wchar_t*/ *, const unsigned short /*wchar_t*/ *, FILE *);
FILE * __cdecl _getstdfilex(int);
#endif /* _WINCE */
int __cdecl _snprintf(char * restrict, size_t, const char * restrict, ...);
int __cdecl _vsnprintf(char * restrict, size_t, const char * restrict, va_list);

#ifndef _WINCE

/* internal stuff */
long __fgetpos(FILE *, fpos_t *);
int __fsetpos(FILE *, const fpos_t *, long, int);

/* data declarations */
extern _CRTIMP FILE __stdin, __stdout, __stderr;
#if !defined(_DLL)
extern FILE *__filetab[FOPEN_MAX];
#endif

/* macro overrides */
#if !defined(__MT__) && !defined(_DLL)
#define getc(str)     ((str)->ptr < (str)->getend ? *(str)->ptr++ : (fgetc)(str))
#define getchar()     (__filetab[0]->ptr < __filetab[0]->getend ? *__filetab[0]->ptr++ : (fgetc)(__filetab[0]))
#define putc(c,str)   ((str)->ptr < (str)->putend ? (*(str)->ptr++ = c) : (fputc)(c, str))
#define putchar(c)    (__filetab[1]->ptr < __filetab[1]->putend ? (*__filetab[1]->ptr++ = c) : (fputc)(c, __filetab[1]))
#define _fileno(str)  ((str)->fh)
#endif

#endif /* _WINCE */

/* compatibility names */
#ifdef __POCC__OLDNAMES
FILE * __cdecl fdopen(int, const char *);
int __cdecl fileno(FILE *);
#endif /* __POCC__OLDNAMES */

#endif /* _STDIO_H */
