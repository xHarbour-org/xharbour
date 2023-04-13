/*---------------------------------------------------------------------------*
 |              PDFlib - A library for generating PDF on the fly             |
 +---------------------------------------------------------------------------+
 | Copyright (c) 1997-2006 Thomas Merz and PDFlib GmbH. All rights reserved. |
 +---------------------------------------------------------------------------+
 |                                                                           |
 |    This software is subject to the PDFlib license. It is NOT in the       |
 |    public domain. Extended versions and commercial licenses are           |
 |    available, please check http://www.pdflib.com.                         |
 |                                                                           |
 *---------------------------------------------------------------------------*/

/* $Id$
 *
 * Declaration of various utility routines
 *
 */

#ifndef PC_UTIL_H
#define PC_UTIL_H

#if defined(__WATCOMC__)
   #pragma disable_message ( 202 )
   #pragma disable_message ( 106 )
   #pragma disable_message ( 124 )
#elif defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2130)
   #pragma warn(disable:2135)
   #pragma warn(disable:2117)
   #pragma warn(disable:2073)
   #pragma warn(disable:2115)
#elif defined(_MSC_VER) && (_MSC_VER>=1400) &&(!defined(_CRT_SECURE_NO_WARNINGS))
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdarg.h>

#if defined(__BORLANDC__)
   #pragma warn -prc
   #pragma warn -pia
   #pragma warn -rch
   #pragma warn -csu
   #pragma warn -aus
   #pragma warn -ccc
   #pragma warn -def
   #pragma warn -sig
   #pragma warn -sus
   #pragma warn -use
   #pragma warn -eff
#endif

#include "pc_config.h"
#include "pc_core.h"
#include "pc_ebcdic.h"
#include "pc_optparse.h"
#include "pc_encoding.h"
#include "pc_output.h"
#include "pc_unicode.h"
#include "pc_resource.h"

/* ---------------------------- forward typedefs --------------------------- */

#ifndef	PDC_STRINGS_DEFINED
#define	PDC_STRINGS_DEFINED
typedef struct pdc_bstr_s pdc_bstr;	/* byte strings			*/
typedef struct pdc_ustr_s pdc_ustr;	/* unicode strings		*/
#endif

#ifndef	PDC_MEMPOOL_DEFINED
#define	PDC_MEMPOOL_DEFINED
typedef struct pdc_mempool_s pdc_mempool;
#endif

/* ------------------------ the core public structure ---------------------- */

struct pdc_core_s
{
    pdc_core_priv *pr;                  /* pdcore private structure */

    pdc_reslist *reslist;               /* resource list */
    pdc_virtfile *filesystem;           /* virtual file system */
    pdc_loggdef *logg;                  /* logging definition */
    pdc_bool loggenv;                   /* logging environ. variable checked */
    pdc_encoding_stack *encstack;       /* encoding stack */
    pdc_priv_glyphtab *pglyphtab;       /* private glyph table */
    pdc_mempool *bstr_pool;		/* pdc_bstr pool */
    pdc_mempool *ustr_pool;		/* pdc_ustr pool */
    pdc_ulong last_rand;		/* for pdc_rand()/pdc_srand() */

    const char *prodname;               /* product name */
    const char *version;                /* version string */
    char *binding;                      /* name of the language binding */
    pdc_bool unicaplang;                /* Unicode capable language */
    pdc_bool objorient;                 /* binding object orientated */
    pdc_bool hastobepos;                /* handles have to be positiv */
    pdc_bool ptfrun;                    /* while PTF is running */
    pdc_bool smokerun;                  /* while smoketest is running */
    pdc_bool charref;                   /* HTML character references will
                                         * be resolved */
    pdc_bool escapesequ;                /* escape sequences will be resolved */
    pdc_bool honorlang;                 /* honor LANG codeset for file names */
    int compatibility;                  /* PDF version number * 10 */
    int floatdigits;                    /* floating point output precision */
    int uniqueno;                       /* unique number for numbering */

};

#define PDC_BOOLSTR(a) (a != 0 ? "true" : "false")

#define PDC_ABS(x)	(((x) < 0) ? -(x) : (x))

/* TODO: replace with PDC_MIN, PDC_MAX
*/
#ifndef MIN
#define MIN(a, b)	(((a) < (b) ? (a) : (b)))
#endif
#ifndef MAX
#define MAX(a, b)	(((a) > (b) ? (a) : (b)))
#endif

/* reasonable values for number limits */
#define PDC_FLOAT_MAX   ((double) 1e+18)
#define PDC_FLOAT_MIN   ((double) -1e+18)
#define PDC_FLOAT_PREC  ((double) 1e-6)

#define PDC_ROUND(x)        (((x) < 0) ? ceil((x) - 0.5) : floor((x) + 0.5))

#define PDC_FLOAT_ISNULL(x) \
    (((((x) < 0) ? -1 * (x) : (x)) < PDC_FLOAT_PREC) ? pdc_true : pdc_false)

#define PDC_SIGN(x) \
    (((x) < 0) ? -1 : 1)

/*
 * general buffer size and
 * obligatory size of buffers for formatting function pdc_vsprintf().
 */
#define PDC_GEN_BUFSIZE  4096

#define PDC_TIME_SBUF_SIZE      50

/* flags for pdc_split_stringlist */
#define PDC_SPLIT_ISOPTLIST (1L<<0)
#define PDC_SPLIT_ISARGLIST (1L<<1)

/* flags for convert functions */
#define PDC_INT_UNSIGNED  (1L<<0)
#define PDC_INT_CHAR      (1L<<1)
#define PDC_INT_SHORT     (1L<<2)
#define PDC_INT_HEXADEC   (1L<<4)
#define PDC_INT_DEC       (1L<<5)
#define PDC_INT_OCTAL     (1L<<6)
#define PDC_INT_CASESENS  (1L<<7)

#define PDC_INT_CODE  (PDC_INT_UNSIGNED | PDC_INT_CHAR | PDC_INT_HEXADEC)
#define PDC_INT_UNICODE  (PDC_INT_UNSIGNED | PDC_INT_SHORT | PDC_INT_HEXADEC)

#define PDC_GET_SHORT  pdc_get_le_short
#define PDC_GET_USHORT pdc_get_le_ushort
#define PDC_GET_WORD   pdc_get_le_ushort
#define PDC_GET_DWORD  pdc_get_le_ulong
#define PDC_GET_DWORD3 pdc_get_le_ulong3
#define PDC_GET_LONG   pdc_get_le_long
#define PDC_GET_ULONG  pdc_get_le_ulong

#define PDC_TREE_INHERIT (1L<<0)
#define PDC_TREE_ISLEAF  (1L<<1)

#define PDC_NAME_SEPARSIGN '.'
#define PDC_NAME_SEPARSTRG "."

#define PDC_KIDS_CHUNKSIZE 5

/* tree error codes */
typedef enum
{
    tree_ok = 0,
    tree_notfound,
    tree_nameexists,
    tree_isleaf
}
pdc_branch_error;

typedef struct pdc_branch_s pdc_branch;

void pdc_set_unsupp_error(pdc_core *pdc, int err_config, int err_lite,
        pdc_bool warning);
void pdc_ascii_error(pdc_core *pdc, int errnum, int flags, const char *parm1,
        const char *parm2, const char *parm3, const char *parm4);
void pdc_check_number(pdc_core *pdc, const char *paramname, double dz);
void pdc_check_number_limits(pdc_core *pdc, const char *paramname, double dz,
        double dmin, double dmax);
void pdc_check_number_zero(pdc_core *pdc, const char *paramname, double dz);
int pdc_check_text_length(pdc_core *pdc, const char **text, int len,
        int maxlen);

typedef struct
{
    int	second;
    int	minute;
    int	hour;
    int	mday;
    int	wday;
    int	month;
    int	year;
} pdc_time;

void	pdc_localtime(pdc_time *t);
void    pdc_get_timestr(char *str, pdc_bool ktoascii);

pdc_bool pdc_check_lang_code(pdc_core *pdc, const char* lang_code);

void     pdc_setbit(char *bitarr, int bit);
void     pdc_setbit_l2r(char *bitarr, int bit);
pdc_bool pdc_getbit(const char *bitarr, int bit);
void     pdc_setbit_text(char *bitarr, const unsigned char *text,
                         int len, int nbits, int size);

pdc_short  pdc_get_le_short(const pdc_byte *data);
pdc_ushort pdc_get_le_ushort(const pdc_byte *data);
pdc_sint32 pdc_get_le_long(const pdc_byte *data);
pdc_uint32 pdc_get_le_ulong3(const pdc_byte *data);
pdc_uint32 pdc_get_le_ulong(const pdc_byte *data);
pdc_short  pdc_get_be_short(const pdc_byte *data);
pdc_ushort pdc_get_be_ushort(const pdc_byte *data);
pdc_sint32 pdc_get_be_long(const pdc_byte *data);
pdc_uint32 pdc_get_be_ulong3(const pdc_byte *data);
pdc_uint32 pdc_get_be_ulong(const pdc_byte *data);

size_t  pdc_wstrlen(const char *str);
size_t  pdc_strlen(const char *str);
char *pdc_getenv(pdc_core *pdc, const char *envname);
char *pdc_getenv_filename(pdc_core *pdc, const char *envname);
char   *pdc_strdup_ext(pdc_core *pdc, const char *text, int flags,
                const char *fn);
char   *pdc_strdup(pdc_core *pdc, const char *text);
char   *pdc_strdup2(pdc_core *pdc, const char *text, size_t len);
char   *pdc_strdup_tmp(pdc_core *pdc, const char *text);
int     pdc_convert_pascal_str(const char *pstr, char *cstr);
pdc_bool pdc_logg_isprint(int c);
char   *pdc_strprint(pdc_core *pdc, const char *str, int leni,
                int maxchar, pdc_strform_kind strform);
char   *pdc_strdup_convert(pdc_core *pdc, pdc_encoding encto,
                pdc_encoding encfrom, const char *text, int flags,
                const char *fn);
const char *pdc_utf8strprint(pdc_core *pdc, const char *str);
int	pdc_split_stringlist(pdc_core *pdc, const char *text,
                    const char *i_separstr, int flags, char ***stringlist);
char   *pdc_substitute_variables(pdc_core *pdc, const char *string, char vchar,
           const char *delimiters, const char **varslist,
           const char **valslist, int nvars, int *errind);
void    pdc_cleanup_stringlist(pdc_core *pdc, char **stringlist);
int     pdc_strcmp(const char *s1, const char *s2);
int     pdc_stricmp(const char *s1, const char *s2);
int     pdc_stricmp_a(const char *s1, const char *s2);
int     pdc_strincmp(const char *s1, const char *s2, int n);
int     pdc_wstrcmp(const char *s1, const char *s2);
char    *pdc_strtrim(char *m_str);
char    *pdc_str2trim(char *m_str);
char    *pdc_strtoupper(char *str);
char    *pdc_strtolower(char *str);
void    pdc_swap_bytes2(const char *instring, int inlen, char *outstring);
void    pdc_swap_bytes4(const char *instring, int inlen, char *outstring);
char   *pdc_strdup_withbom(pdc_core *pdc, const char *text);
void    pdc_inflate_ascii(const char *instring, int inlen, char *outstring,
                          pdc_text_format textformat);

pdc_ushort pdc_get_string_value(pdc_byte *str, int i, int charlen);

int pdc_subst_backslash(pdc_core *pdc, pdc_byte *str, int len,
        pdc_encodingvector *ev, pdc_text_format textformat, pdc_bool verbose);

pdc_bool pdc_str2double(const char *string, double *o_dz);
pdc_bool pdc_str2integer(const char *string, int flags, void *o_iz);
pdc_bool pdc_str2integer_ext(pdc_core *pdc, const char *string, int len,
                             int dupflags, int flags, void *o_iz);

int pdc_vfprintf(pdc_core *pdc, pdc_bool pdfconf, FILE *fp,
                const char *format, va_list args);
int pdc_fprintf(pdc_core *pdc, pdc_bool pdfconf, FILE *fp,
                const char *format, ...);
int pdc_vsprintf(pdc_core *pdc, pdc_bool pdfconf, char *buf,
                const char *format, va_list args);
int pdc_sprintf(pdc_core *pdc, pdc_bool pdfconf, char *buf,
                const char *format, ...);
int pdc_vsnprintf(pdc_core *pdc, char *buf, size_t size,
                const char *format, va_list args);

pdc_branch *pdc_init_tree(pdc_core *pdc);
pdc_branch *pdc_create_treebranch(pdc_core *pdc, pdc_branch *root,
               const char *pathname, void *data, int flags, int size,
               pdc_branch_error *errcode, const char **name_p);
char *pdc_get_name_treebranch(pdc_branch *branch);
pdc_branch *pdc_get_parent_treebranch(pdc_branch *branch);
pdc_branch **pdc_get_kids_treebranch(pdc_branch *branch, int *nkids);
void *pdc_get_data_treebranch(pdc_branch *branch);
void pdc_cleanup_treebranch(pdc_core *pdc, pdc_branch *branch);
void pdc_deactivate_name_treebranch(pdc_core *pdc, pdc_branch *branch);

pdc_mempool *	pdc_mp_new(pdc_core *pdc, size_t item_size);
void		pdc_mp_delete(pdc_mempool *mp);
void *		pdc_mp_alloc(pdc_mempool *mp);
void		pdc_mp_free(pdc_mempool *mp, void *item);

int	pdc_name2idx(const char **names, int size, const char *name);
void *	pdc_lfind(const void *key, const void *base, size_t *nmemb,
		    size_t size, int (*compar)(const void *, const void *));

int	pdc_rand(pdc_core *pdc);
void	pdc_srand(pdc_core *pdc, pdc_uint seed);

#endif	/* PC_UTIL_H */

