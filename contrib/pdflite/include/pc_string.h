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
 * The core string classes.
 *
 */

#ifndef PC_STRING_H
#define PC_STRING_H

#include "pc_core.h"

/* there are two pdcore string classes. the structures "behind"
** these classes are opaque. conceptually, pdc_bstr objects are
** sequences of 'pdc_byte' (unsigned 8-bit entities), whereas
** pdc_ustr objects are sequences of 'pdc_ucval' ("unicode value";
** unsigned 32-bit entities).
*/
#ifndef	PDC_STRINGS_DEFINED
#define	PDC_STRINGS_DEFINED
typedef struct pdc_bstr_s pdc_bstr;	/* byte strings			*/
typedef struct pdc_ustr_s pdc_ustr;	/* unicode strings		*/
#endif


/* TODO: naming conventions for "per module" init/cleanup */
void pdc_init_strings(pdc_core *pdc);
void pdc_cleanup_strings(pdc_core *pdc);


/************************************************************************/
/*									*/
/*  string object construction and deletion.				*/
/*									*/
/************************************************************************/

/* convert raw memory into an (empty) string object.
*/
void pdc_bs_boot(pdc_core *pdc, pdc_bstr *s);
void pdc_us_boot(pdc_core *pdc, pdc_ustr *s);

/* release all resources allocated by a string object (if any).
*/
void pdc_bs_shutdown(pdc_bstr *s);
void pdc_us_shutdown(pdc_ustr *s);

/* allocate a new (empty) pdc_bstr object.
*/
pdc_bstr *pdc_bs_new(pdc_core *pdc);

/* allocate a new pdc_ustr object and initialize its contents with the
** 'n' values from 'src'. if 'src' is null or 'n' is zero,
** an empty string object is constructed.
*/
pdc_ustr *pdc_us_new(pdc_core *pdc, const pdc_ucval *src, size_t n);

/* TODO: more constructors for various "source" types, eg.
**
pdc_ustr *pdc_us_new_utf16(pdc_core *pdc, const pdc_ushort *src, size_t n);
pdc_ustr *pdc_us_new_utf8(pdc_core *pdc, const pdc_byte *src, size_t n);
*/

/* return a copy of string 'src' ("copy constructor").
*/
pdc_bstr *pdc_bs_dup(const pdc_bstr *src);
pdc_ustr *pdc_us_dup(const pdc_ustr *src);

/* delete a string object explicitly ("destructor").
*/
void pdc_bs_delete(pdc_bstr *s);
void pdc_us_delete(pdc_ustr *s);

/************************************************************************/
/*									*/
/*  "getters".								*/
/*									*/
/************************************************************************/

/* get the length of a string object in bytes or unicode values, resp.
*/
size_t		pdc_bs_length(const pdc_bstr *s);
size_t		pdc_us_length(const pdc_ustr *s);

/* string component access (range checked).
*/
pdc_byte	pdc_bs_get(const pdc_bstr *s, int idx);
pdc_ucval	pdc_us_get(const pdc_ustr *s, int idx);

/* TODO: try to get rid of that. */
const pdc_byte *pdc_bs_get_cptr(const pdc_bstr *s);
const pdc_byte *pdc_us_get_cptr(const pdc_ustr *s);

/************************************************************************/
/*									*/
/*  "modifiers".							*/
/*									*/
/************************************************************************/

/* copy 'src' to 'dst' ("assignment operator").
*/
void	pdc_bs_copy(pdc_bstr *dst, const pdc_bstr *src);
void	pdc_us_copy(pdc_ustr *dst, const pdc_ustr *src);

/* copy part of 'src' to 'dst'.
*/
void	pdc_bs_substr(pdc_bstr *dst, const pdc_bstr *src,
			size_t pos, size_t len);
void	pdc_us_substr(pdc_ustr *dst, const pdc_ustr *src,
			size_t pos, size_t len);

/* insert 'src' into 'dst' at 'pos'.
*/
void	pdc_bs_insert(pdc_bstr *dst, const pdc_bstr *src, size_t pos);
void	pdc_us_insert(pdc_ustr *dst, const pdc_ustr *src, size_t pos);

/* append 'src' to 'dst'.
*/
void	pdc_bs_concat(pdc_bstr *dst, const pdc_bstr *src);
void	pdc_us_concat(pdc_ustr *dst, const pdc_ustr *src);

/* string component access (range checked).
*/
void	pdc_bs_set(pdc_bstr *s, int idx, pdc_byte val);
void	pdc_us_set(pdc_ustr *s, int idx, pdc_ucval val);

/* case conversion.
*/
void	pdc_bs_tolower(pdc_bstr *s);
void	pdc_bs_toupper(pdc_bstr *s);

/************************************************************************/
/*									*/
/*  stream-like functions.						*/
/*									*/
/************************************************************************/

/* append 'n' values from 'src' to 'dst'. if 'n' is zero,
** or 'src' is null, 'dst' remains unchanged.
*/
void	pdc_bs_write(pdc_bstr *dst, const pdc_byte *src, size_t n);

/* append the null terminated string 'src' to 'dst'.
*/
void	pdc_bs_puts(pdc_bstr *dst, const pdc_byte *src);

/* append 'n' values from 'src' to 'dst'. if 'src' is null or 'n'
** is zero, 'dst' remains unchanged.
*/
void	pdc_us_write(pdc_ustr *dst, const pdc_ucval *src, size_t n);

void	pdc_us_write_utf16(pdc_ustr *dst, const pdc_ushort *src, size_t n);

/* TODO: more writer functions for various "source" types, eg.
**
void	pdc_us_write_utf8(pdc_ustr *dst, const pdc_byte *src, size_t n);
*/

/* reset 's' to an empty stream object.
*/
void	pdc_bs_rewrite(pdc_bstr *s);
void	pdc_us_rewrite(pdc_ustr *s);

/* append a single byte (or unicode value, resp.) to a string object.
*/
void	pdc_bs_putc(pdc_bstr *s, pdc_byte val);
void	pdc_us_putc(pdc_ustr *s, pdc_ucval val);

/* TODO: stream-like read access. again, the read functions for pdc_ustr
** objects will be available in several flavors in order to support
** conversion to various "external" formats.
**
void	pdc_bs_reset(pdc_bstr *s);
void	pdc_us_reset(pdc_ustr *s);
void	pdc_bs_seek(pdc_bstr *s, size_t pos);
void	pdc_us_seek(pdc_ustr *s, size_t pos);
size_t	pdc_bs_tell(const pdc_bstr *s);
size_t	pdc_us_tell(const pdc_ustr *s);
size_t	pdc_bs_read(pdc_bstr *src, pdc_byte *dst, size_t n);
size_t	pdc_us_read(pdc_ustr *src, pdc_ucval *dst, size_t n);
size_t	pdc_us_read_utf16(pdc_ustr *src, pdc_ushort *dst, size_t n);
size_t	pdc_us_read_utf8(pdc_ustr *src, pdc_byte *dst, size_t n);
*/

/************************************************************************/
/*									*/
/*  other utilities.							*/
/*									*/
/************************************************************************/

int	pdc_bs_compare(const pdc_bstr *s1, const pdc_bstr *s2);

/************************************************************************/
/*									*/
/*  PRIVATE SECTION							*/
/*									*/
/*  the declarations below are strictly private to the implementation	*/
/*  module, and must not be used by any client modules!			*/
/*									*/
/************************************************************************/

#define PDC_STR_INLINE_CAP	16

struct pdc_bstr_s
{
    pdc_core *	pdc;

    pdc_byte	buf0[PDC_STR_INLINE_CAP];
    pdc_byte *	buf;
    size_t	len;
    size_t	cap;
};

struct pdc_ustr_s
{
    pdc_core *	pdc;

    pdc_ucval	buf0[PDC_STR_INLINE_CAP];
    pdc_ucval *	buf;
    size_t	len;
    size_t	cap;
};

#if 0
/* string representation.
*/
typedef struct
{
    pdc_byte *	buf;		/* contents			*/
    size_t	cap;		/* capacity (unit: pdc_byte)	*/
    size_t	len;		/* length (unit: pdc_byte)	*/
    int		ref;		/* reference count		*/
} pdc_bs_rep;

typedef struct
{
    pdc_ucval *	buf;		/* contents			*/
    size_t	cap;		/* capacity (unit: pdc_ucval)	*/
    size_t	len;		/* length (unit: pdc_ucval)	*/
    int		ref;		/* reference count		*/
} pdc_us_rep;


struct pdc_bstr_s
{
    pdc_core *	pdc;
    pdc_bs_rep *rep;
};

struct pdc_ustr_s
{
    pdc_core *	pdc;
    pdc_us_rep *rep;
};
#endif

#endif	/* PC_STRING_H */
