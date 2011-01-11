#ifndef _STDIO_H
#define _STDIO_H

/* stdio.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#define _IOFBF  0
#define _IOLBF  1
#define _IONBF  2

#define BUFSIZ  512
#define EOF  (-1)

#define FOPEN_MAX  16
#define FILENAME_MAX  260

#define L_tmpnam  16
#define TMP_MAX  32

#define SEEK_SET  0
#define SEEK_CUR  1
#define SEEK_END  2

#define stdin   (&__stdin)
#define stdout  (&__stdout)
#define stderr  (&__stderr)

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

/* operations on files */
int __cdecl remove(const char *);
int __cdecl rename(const char *, const char *);
FILE * __cdecl tmpfile(void);
char * __cdecl tmpnam(char *);

/* file access functions */
int __cdecl fclose(FILE *);
int __cdecl fflush(FILE *);
FILE * __cdecl fopen(const char * restrict, const char * restrict);
FILE * __cdecl freopen(const char * restrict, const char * restrict, FILE * restrict);
void __cdecl setbuf(FILE * restrict, char * restrict);
int __cdecl setvbuf(FILE * restrict, char * restrict, int, size_t);

/* formatted input/output functions */
int __cdecl fprintf(FILE * restrict, const char * restrict, ...);
int __cdecl fscanf(FILE * restrict, const char * restrict, ...);
int __cdecl sprintf(char * restrict, const char * restrict, ...);
int __cdecl sscanf(const char * restrict, const char * restrict, ...);
int __cdecl vfprintf(FILE * restrict, const char * restrict, va_list);
int __cdecl vfscanf(FILE * restrict, const char * restrict, va_list);
int __cdecl vsprintf(char * restrict, const char * restrict, va_list);
int __cdecl vsscanf(const char * restrict, const char * restrict, va_list);
int __cdecl vprintf(const char * restrict, va_list);
int __cdecl vscanf(const char * restrict, va_list);
int __cdecl printf(const char * restrict, ...);
int __cdecl scanf(const char * restrict, ...);
int __cdecl snprintf(char * restrict, size_t, const char * restrict, ...);
int __cdecl vsnprintf(char * restrict, size_t, const char * restrict, va_list);

/* character input/output functions */
int __cdecl fgetc(FILE *);
char * __cdecl fgets(char * restrict, int, FILE * restrict);
int __cdecl fputc(int, FILE *);
int __cdecl fputs(const char * restrict, FILE * restrict);
int __cdecl getc(FILE *);
int __cdecl getchar(void);
char * __cdecl gets(char *);
int __cdecl putc(int, FILE *);
int __cdecl putchar(int);
int __cdecl puts(const char *);
int __cdecl ungetc(int, FILE *);

/* direct input/output functions */
size_t __cdecl fread(void * restrict, size_t, size_t, FILE * restrict);
size_t __cdecl fwrite(const void * restrict, size_t, size_t, FILE * restrict);

/* file positioning functions */
int __cdecl fgetpos(FILE * restrict, fpos_t * restrict);
int __cdecl fseek(FILE *, long, int);
int __cdecl fsetpos(FILE *, const fpos_t *);
long __cdecl ftell(FILE *);
void __cdecl rewind(FILE *);

/* error-handling functions */
void __cdecl clearerr(FILE *);
int __cdecl feof(FILE *);
int __cdecl ferror(FILE *);
void __cdecl perror(const char *);

/* private extensions to standard C */
int __cdecl _fileno(FILE *);

/* internal stuff */
long __fgetpos(FILE *, fpos_t *);
int __fsetpos(FILE *, const fpos_t *, long, int);

/* data declarations */
extern FILE __stdin, __stdout, __stderr;
extern FILE *__filetab[FOPEN_MAX];

/* macro overrides */
#if !defined(__MT__) || !_FILE_OP_LOCKS
#define getc(str)     ((str)->ptr < (str)->getend ? *(str)->ptr++ : (fgetc)(str))
#define getchar()     (__filetab[0]->ptr < __filetab[0]->getend ? *__filetab[0]->ptr++ : (fgetc)(__filetab[0]))
#define putc(c,str)   ((str)->ptr < (str)->putend ? (*(str)->ptr++ = c) : (fputc)(c, str))
#define putchar(c)    (__filetab[1]->ptr < __filetab[1]->putend ? (*__filetab[1]->ptr++ = c) : (fputc)(c, __filetab[1]))
#define _fileno(str)  ((str)->fh)
#endif

#ifndef _NO_OLDNAMES
#define fileno(str)  ((str)->fh)
#endif

#endif /* _STDIO_H */

