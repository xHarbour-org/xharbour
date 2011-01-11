#ifndef _STAT_H
#define _STAT_H

/* sys/stat.h - private header for file status definitions */

#include <sys/types.h>

/* type definitions */
#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef unsigned long time_t;
#endif

struct _stat {
    _dev_t st_dev;
    _ino_t st_ino;
    unsigned short st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    _off_t st_size;
    time_t st_atime;
    time_t st_mtime;
    time_t st_ctime;
};

#define _S_IFMT    0xF000
#define _S_IFDIR   0x4000
#define _S_IFCHR   0x2000
#define _S_IFIFO   0x1000
#define _S_IFREG   0x8000
#define _S_IREAD   0x100
#define _S_IWRITE  0x80
#define _S_IEXEC   0x40

/* declarations */
/* int __cdecl _fstat(int, struct _stat *); */
int __cdecl _stat(const char *, struct _stat *);

#ifndef _NO_OLDNAMES

/*
#define stat(_cp, _statp) _stat(_cp, _statp)
*/
#define stat _stat

#endif

#endif /* _STAT_H */

