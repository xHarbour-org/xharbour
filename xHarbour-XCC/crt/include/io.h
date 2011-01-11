#ifndef _IO_H
#define _IO_H

/* io.h - private header for low-level I/O definitions */

/* type definitions */
#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef unsigned long time_t;
#endif

#ifdef __FSIZE_T_64
typedef unsigned long long _fsize_t;
#else
typedef unsigned long _fsize_t;
#endif

struct _finddata_t {
    unsigned int attrib;
    time_t time_create;
    time_t time_access;
    time_t time_write;
    _fsize_t size;
    char name[260];
};

/* file attributes for _findfirst() */
#define _A_NORMAL   0x00
#define _A_RDONLY   0x01
#define _A_HIDDEN   0x02
#define _A_SYSTEM   0x04
#define _A_SUBDIR   0x10
#define _A_ARCH     0x20

/* file sharing modes for _sopen() */
#define _SH_DENYRW  0x10
#define _SH_DENYWR  0x20
#define _SH_DENYRD  0x30
#define _SH_DENYNO  0x40

/* flags for _open() and _chmod() */
#ifndef _S_IREAD
   #define _S_IREAD    0x100
   #define _S_IWRITE   0x80
#endif

/* locking modes for _locking() */
#define _LK_UNLCK   0
#define _LK_LOCK    1
#define _LK_NBLCK   2
#define _LK_RLCK    3
#define _LK_NBRLCK  4

/* declarations */
int __cdecl _access(const char *, int);
int __cdecl _chmod(const char *, int);
int __cdecl _chsize(int, long);
int __cdecl _close(int);
int __cdecl _commit(int);
int __cdecl _creat(const char *, int);
int __cdecl _dup(int);
int __cdecl _dup2(int, int);
int __cdecl _eof(int);
long __cdecl _filelength(int);
long __cdecl _findfirst(const char *, struct _finddata_t *);
int __cdecl _findnext(long, struct _finddata_t *);
int __cdecl _findclose(long);
int __cdecl _isatty(int);
int __cdecl _locking(int, int, long);
long __cdecl _lseek(int, long, int);
int __cdecl _open(const char *, int, ...);
int __cdecl _pipe(int *, unsigned int, int);
int __cdecl _read(int, void *, unsigned int);
int __cdecl _setmode(int, int);
int __cdecl _sopen(const char *, int, int, ...);
long __cdecl _tell(int);
int __cdecl _unlink(const char *);
int __cdecl _write(int, const void *, unsigned int);

/* macros */
#define _tell(fh)  _lseek((fh),0L,/*SEEK_CUR*/1)

#ifndef _NO_OLDNAMES

#define access(_pc, _i)          _access(_pc, _i)
#define chmod(_pc, _i)           _chmod(_pc, _i)
#define chsize(_i, _l)           _chsize(_i, _l)
#define close(_i)                _close(_i)
#define creat(_cp, _i)           _creat(_cp, _i)
#define dup(_i)                  _dup(_i)
#define dup2(_i, _i2)            _dup2(_i, _i2)
#define eof(_i)                  _eof(_i)
#define filelength(_i)           _filelength(_i)
#define isatty(_i)               _isatty(_i)
#define locking(_1, _i2, _l)     _locking(_1, _i2, _l)
#define lseek(_i, _l, _i2)       _lseek(_i, _l, _i2)
#define mktemp(_cp)              _mktemp(_cp)

/*
#define open(_cp, _i)            _open(_cp, _i)
#define open(_cp, _i, _i2)       _open(_cp, _i, _i2)
*/
#define open                     _open

#define read(_i, _p, _ui)        _read(_i, _p, _ui)
#define setmode(_i, _i2)         _setmode(_i, _i2)

/*
#define sopen(_cp, _i, _i2)      _sopen(_cp, _i, _i2)
#define sopen(_cp, _i, _i2, _i3) _sopen(_cp, _i, _i2, _i3)
*/
#define sopen                    _sopen

#define tell(_i)                 _tell(_i)
#define umask(_i)                _umask(_i)
#define unlink(_cp)              _unlink(_cp)
#define write(_i, _p, _ui)       _write(_i, _p, _ui)

#endif

#endif /* _IO_H */

