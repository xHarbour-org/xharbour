#ifndef _MEMDBG_H
#define _MEMDBG_H

extern void __cdecl crt_leaktrace(int);
extern void __cdecl crt_dumpleaktrace(FILE *);

extern void __cdecl __m_install_record(void *, const char *);
extern void __cdecl __m_delete_record(void *);

extern char _malloc_statsbuf[];

#ifdef _DEBUG
#define RECORD_FILE_AND_LINE(addr, fname, linenum) \
    { \
        (void)sprintf(_malloc_statsbuf, "%s(%d)", fname, linenum); \
        __m_install_record(addr, _malloc_statsbuf); \
    }

#define DELETE_RECORD(addr) \
    { \
        __m_delete_record(addr); \
    }
#else /* _DEBUG */
#define RECORD_FILE_AND_LINE(addr, fname, linenum)
#define DELETE_RECORD(addr)
#endif /* _DEBUG */

#endif /* _MEMDBG_H */
