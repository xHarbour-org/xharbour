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

#include <string.h>

#include "config.h"
#include "pc_util.h"
#include "pc_string.h"
#include "pc_ctype.h"

#undef	SBUF_RESERVE
#define	SBUF_RESERVE	PDC_STR_INLINE_CAP


/* TODO:

    - think over the pdcore temporary memory model.

    - s->buf should be reference-counted (delayed copy).
*/

void pdc_init_strings(pdc_core *pdc)
{
    pdc->bstr_pool = pdc_mp_new(pdc, sizeof (pdc_bstr));
    pdc->ustr_pool = pdc_mp_new(pdc, sizeof (pdc_ustr));
} /* pdc_init_strings */

void pdc_cleanup_strings(pdc_core *pdc)
{
    pdc_mp_delete(pdc->bstr_pool);
    pdc_mp_delete(pdc->ustr_pool);
} /* pdc_cleanup_strings */


/************************************************************************/
/*									*/
/*  string object construction and deletion.				*/
/*									*/
/************************************************************************/

void
pdc_bs_boot(pdc_core *pdc, pdc_bstr *s)
{
    s->pdc = pdc;
    s->buf = (pdc_byte *) 0;
    s->len = 0;
    s->cap = PDC_STR_INLINE_CAP;
} /* pdc_bs_boot */

void
pdc_us_boot(pdc_core *pdc, pdc_ustr *s)
{
    s->pdc = pdc;
    s->buf = (pdc_ucval *) 0;
    s->len = 0;
    s->cap = PDC_STR_INLINE_CAP;
} /* pdc_us_boot */


void
pdc_bs_shutdown(pdc_bstr *s)
{
    if (s->buf != (pdc_byte *) 0)
	pdc_free(s->pdc, s->buf);

    pdc_bs_boot(s->pdc, s);
} /* pdc_bs_shutdown */

void
pdc_us_shutdown(pdc_ustr *s)
{
    if (s->buf != (pdc_ucval *) 0)
	pdc_free(s->pdc, s->buf);

    pdc_us_boot(s->pdc, s);
} /* pdc_us_shutdown */


#undef	USE_POOL
#define	USE_POOL

pdc_bstr *
pdc_bs_new(pdc_core *pdc)
{
#ifndef	USE_POOL
    static const char fn[] = "pdc_bs_new";

    pdc_bstr *result = (pdc_bstr *) pdc_malloc(pdc, sizeof (pdc_bstr), fn);
#else
    pdc_bstr *result = (pdc_bstr *) pdc_mp_alloc(pdc->bstr_pool);
#endif

    pdc_bs_boot(pdc, result);
    return result;
} /* pdc_bs_new */

pdc_ustr *
pdc_us_new(pdc_core *pdc, const pdc_ucval *src, size_t len)
{
#ifndef	USE_POOL
    static const char fn[] = "pdc_us_new";

    pdc_ustr *result = (pdc_ustr *) pdc_malloc(pdc, sizeof (pdc_ustr), fn);
#else
    pdc_ustr *result = (pdc_ustr *) pdc_mp_alloc(pdc->ustr_pool);
#endif

    pdc_us_boot(pdc, result);
    pdc_us_write(result, src, len);
    return result;
} /* pdc_us_new */


pdc_bstr *
pdc_bs_dup(const pdc_bstr *src)
{
    const pdc_byte *	buf = src->buf ? src->buf : src->buf0;
    pdc_bstr *		result = pdc_bs_new(src->pdc);

    pdc_bs_write(result, buf, src->len);
    return result;
} /* pdc_bs_dup */

pdc_ustr *
pdc_us_dup(const pdc_ustr *src)
{
    const pdc_ucval *buf = src->buf ? src->buf : src->buf0;

    return pdc_us_new(src->pdc, buf, src->len);
} /* pdc_us_dup */


void
pdc_bs_delete(pdc_bstr *s)
{
    pdc_bs_shutdown(s);
#ifndef	USE_POOL
    pdc_free(s->pdc, s);
#else
    pdc_mp_free(s->pdc->bstr_pool, s);
#endif
} /* pdc_bs_delete */

void
pdc_us_delete(pdc_ustr *s)
{
    pdc_us_shutdown(s);
#ifndef	USE_POOL
    pdc_free(s->pdc, s);
#else
    pdc_mp_free(s->pdc->ustr_pool, s);
#endif
} /* pdc_bs_delete */


/************************************************************************/
/*									*/
/*  "getters".								*/
/*									*/
/************************************************************************/

size_t
pdc_bs_length(const pdc_bstr *s)
{
    return s->len;
} /* pdc_bs_length */

size_t
pdc_us_length(const pdc_ustr *s)
{
    return s->len;
} /* pdc_us_length */


pdc_byte
pdc_bs_get(const pdc_bstr *s, int idx)
{
    static const char fn[] = "pdc_bs_get";

    const pdc_byte *buf = s->buf ? s->buf : s->buf0;

    if (idx < 0 || s->len <= (size_t) idx)
	pdc_error(s->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(s->pdc, "%d", idx), fn, 0, 0);

    return buf[idx];
} /* pdc_bs_get */

pdc_ucval
pdc_us_get(const pdc_ustr *s, int idx)
{
    static const char fn[] = "pdc_us_get";

    const pdc_ucval *buf = s->buf ? s->buf : s->buf0;

    if (idx < 0 || s->len <= (size_t) idx)
	pdc_error(s->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(s->pdc, "%d", idx), fn, 0, 0);

    return buf[idx];
} /* pdc_us_get */


const pdc_byte *
pdc_bs_get_cptr(const pdc_bstr *s)
{
    static const pdc_byte empty = 0;

    /* TODO: avoid the ugly "un-const" cast. */
    pdc_byte *buf = (pdc_byte *) (s->buf ? s->buf : s->buf0);

    if (!s->len)
	return &empty;

    buf[s->len] = 0;
    return buf;
}

const pdc_byte *
pdc_us_get_cptr(const pdc_ustr *s)
{
    static const pdc_byte empty = 0;

    const pdc_ucval *buf = s->buf ? s->buf : s->buf0;

    if (!s->len)
	return &empty;

    return (const pdc_byte *) buf;
}


/************************************************************************/
/*									*/
/*  "modifiers".							*/
/*									*/
/************************************************************************/

void
pdc_bs_copy(pdc_bstr *dst, const pdc_bstr *src)
{
    const pdc_byte *buf = src->buf ? src->buf : src->buf0;

    dst->len = 0;

    if (src->len)
	pdc_bs_write(dst, buf, src->len);
} /* pdc_bs_copy */

void
pdc_us_copy(pdc_ustr *dst, const pdc_ustr *src)
{
    const pdc_ucval *buf = src->buf ? src->buf : src->buf0;

    dst->len = 0;

    if (src->len)
	pdc_us_write(dst, buf, src->len);
} /* pdc_us_copy */


void
pdc_bs_substr(pdc_bstr *dst, const pdc_bstr *src, size_t pos, size_t len)
{
    static const char fn[] = "pdc_bs_substr";

    const pdc_byte *buf = src->buf ? src->buf : src->buf0;

    if ((pos < 0) || (len < 0) || (pos > src->len) || ((pos + len) > src->len))
	pdc_error(src->pdc, PDC_E_INT_ILLARG, fn, 0, 0, 0);

    dst->len = 0;
    pdc_bs_write(dst, buf + pos, len);
} /* pdc_bs_substr */

void
pdc_us_substr(pdc_ustr *dst, const pdc_ustr *src, size_t pos, size_t len)
{
    static const char fn[] = "pdc_us_substr";

    const pdc_ucval *buf = src->buf ? src->buf : src->buf0;

    if ((pos < 0) || (len < 0) || (pos > src->len) || ((pos + len) > src->len))
	pdc_error(src->pdc, PDC_E_INT_ILLARG, fn, 0, 0, 0);

    dst->len = 0;
    pdc_us_write(dst, buf + pos, len);
} /* pdc_us_substr */


void
pdc_bs_concat(pdc_bstr *dst, const pdc_bstr *src)
{
    const pdc_byte *buf = src->buf ? src->buf : src->buf0;

    if (src->len)
	pdc_bs_write(dst, buf, src->len);
} /* pdc_bs_concat */

void
pdc_us_concat(pdc_ustr *dst, const pdc_ustr *src)
{
    const pdc_ucval *buf = src->buf ? src->buf : src->buf0;

    if (src->len)
	pdc_us_write(dst, buf, src->len);
} /* pdc_us_concat */


void
pdc_bs_set(pdc_bstr *s, int idx, pdc_byte val)
{
    static const char fn[] = "pdc_bs_set";

    pdc_byte *buf = s->buf ? s->buf : s->buf0;

    if (idx < 0 || s->len <= (size_t) idx)
	pdc_error(s->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(s->pdc, "%d", idx), fn, 0, 0);

    buf[idx] = val;
} /* pdc_bs_set */

void
pdc_us_set(pdc_ustr *s, int idx, pdc_ucval val)
{
    static const char fn[] = "pdc_us_set";

    pdc_ucval *buf = s->buf ? s->buf : s->buf0;

    if (idx < 0 || s->len <= (size_t) idx)
	pdc_error(s->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(s->pdc, "%d", idx), fn, 0, 0);

    buf[idx] = val;
} /* pdc_us_set */


void
pdc_bs_tolower(pdc_bstr *s)
{
    pdc_byte *	buf = s->buf ? s->buf : s->buf0;
    int		i;

    for (i = 0; i < (int) s->len; ++i)
	buf[i] = pdc_tolower(buf[i]);
} /* pdc_bs_tolower */

void
pdc_bs_toupper(pdc_bstr *s)
{
    pdc_byte *	buf = s->buf ? s->buf : s->buf0;
    int		i;

    for (i = 0; i < (int) s->len; ++i)
        buf[i] = pdc_toupper(buf[i]);
} /* pdc_bs_toupper */


/************************************************************************/
/*									*/
/*  stream-like functions.						*/
/*									*/
/************************************************************************/

void
pdc_bs_write(pdc_bstr *dst, const pdc_byte *src, size_t len)
{
    static const char fn[] = "pdc_bs_write";

    pdc_byte *buf = dst->buf ? dst->buf : dst->buf0;

    if (!src || !len)
	return;

    if (dst->cap < dst->len + len + 1)
    {
	dst->cap = dst->len + len + 1 + SBUF_RESERVE;

	if (!dst->buf)
	{
	    dst->buf = (pdc_byte *) pdc_malloc(dst->pdc, dst->cap, fn);
	    memcpy(dst->buf, dst->buf0, dst->len);
	}
	else
	{
	    dst->buf = (pdc_byte *) pdc_realloc(dst->pdc,
		dst->buf, dst->cap, fn);
	}

	buf = dst->buf;
    }

    memcpy(buf + dst->len, src, len);
    dst->len += len;
} /* pdc_bs_write */


void
pdc_bs_puts(pdc_bstr *dst, const pdc_byte *src)
{
    if (!src)
	return;

    pdc_bs_write(dst, src, strlen((char *) src));
} /* pdc_bs_puts */


void
pdc_us_write(pdc_ustr *dst, const pdc_ucval *src, size_t len)
{
    static const char fn[] = "pdc_us_write";

    pdc_ucval *buf = dst->buf ? dst->buf : dst->buf0;

    if (!src || len == 0)
	return;

    if (dst->cap < dst->len + len)
    {
	dst->cap = dst->len + len + SBUF_RESERVE;

	if (!dst->buf)
	{
	    dst->buf = (pdc_ucval *)
		pdc_malloc(dst->pdc, dst->cap * sizeof (pdc_ucval), fn);

	    memcpy(dst->buf, dst->buf0, dst->len * sizeof (pdc_ucval));
	}
	else
	{
	    dst->buf = (pdc_ucval *) pdc_realloc(dst->pdc,
		dst->buf, dst->cap * sizeof (pdc_ucval), fn);
	}

	buf = dst->buf;
    }

    memcpy(buf + dst->len, src, len * sizeof (pdc_ucval));
    dst->len += len;
} /* pdc_us_write */


void
pdc_bs_rewrite(pdc_bstr *s)
{
    s->len = 0;
} /* pdc_bs_rewrite */

void
pdc_us_rewrite(pdc_ustr *s)
{
    s->len = 0;
} /* pdc_us_rewrite */


void
pdc_bs_putc(pdc_bstr *s, pdc_byte val)
{
    pdc_bs_write(s, &val, 1);
} /* pdc_bs_putc */

void
pdc_us_putc(pdc_ustr *s, pdc_ucval val)
{
    pdc_us_write(s, &val, 1);
} /* pdc_us_putc */


/************************************************************************/
/*									*/
/*  other utilities.							*/
/*									*/
/************************************************************************/

int
pdc_bs_compare(const pdc_bstr *s1, const pdc_bstr *s2)
{
    const char *buf1 = (const char *) (s1->buf ? s1->buf : s1->buf0);
    const char *buf2 = (const char *) (s2->buf ? s2->buf : s2->buf0);
    int		result;

    if (s1->len < s2->len)
    {
	if ((result = strncmp(buf1, buf2, s1->len)) != 0)
	    return result;

	return -1;
    }

    if (s2->len < s1->len)
    {
	if ((result = strncmp(buf1, buf2, s2->len)) != 0)
	    return result;

	return +1;
    }

    return strncmp(buf1, buf2, s1->len);
} /* pdc_bs_compare */
