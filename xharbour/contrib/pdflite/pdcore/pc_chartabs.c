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
 * PDFlib routines for converting glyph or character names to Unicode
 * and vice versa
 *
 */

#define PC_CHARTABS_C

#include "pc_util.h"
#include "pc_chartabs.h"
#include "pc_ctype.h"


/* ---------------- general character search functions ------------------- */

/*
 * Binary search for list of codes in a pdc_glyph_tab array sorted by glyphname
 */
int
pdc_glyphname2codelist(const char *glyphname, const pdc_glyph_tab *glyphtab,
                       int tabsize, pdc_ushort *codelist)
{
    const char *s1, *s2;
    int lo = 0;
    int hi = glyphname ? tabsize : lo;
    int i, j, cmp, nv = 0;

    while (lo < hi)
    {
        i = (lo + hi) / 2;

        s1 = glyphname;
        s2 = glyphtab[i].name;
        for (; *s1; ++s1, ++s2)
        {
            if (*s1 != *s2)
                break;
        }
        cmp = (*s1 - *s2);

        if (cmp == 0)
        {
            j = i;
            for (; i >= 1; i--)
            {
                s1 = glyphname;
                s2 = glyphtab[i-1].name;
                for (; *s1; ++s1, ++s2)
                {
                    if (*s1 != *s2)
                        break;
                }
                if (*s1 != *s2)
                    break;
            }
            for (; i < tabsize; i++)
            {
                if (i > j)
                {
                    s1 = glyphname;
                    s2 = glyphtab[i].name;
                    for (; *s1; ++s1, ++s2)
                    {
                        if (*s1 != *s2)
                            break;
                    }
                    if (*s1 != *s2)
                        break;
                }
                codelist[nv] = glyphtab[i].code;
                nv++;
            }
            return nv;
        }

        if (cmp < 0)
            hi = i;
        else
            lo = i + 1;
    }

    return nv;
}

/*
 * Binary search for code in a pdc_glyph_tab array sorted by glyphname
 */
int
pdc_glyphname2code(const char *glyphname, const pdc_glyph_tab *glyphtab,
                   int tabsize)
{
    const char *s1, *s2;
    int lo = 0;
    int hi = glyphname ? tabsize : lo;
    int i, cmp;

    while (lo < hi)
    {
        i = (lo + hi) / 2;

        s1 = glyphname;
        s2 = glyphtab[i].name;
        for (; *s1; ++s1, ++s2)
        {
            if (*s1 != *s2)
                break;
        }
        cmp = (*s1 - *s2);

        if (cmp == 0)
            return (int) glyphtab[i].code;

        if (cmp < 0)
            hi = i;
        else
            lo = i + 1;
    }

    return -1;
}

/*
 * Binary search for glyphname in a pdc_glyph_tab array sorted by code
 */
const char *
pdc_code2glyphname(pdc_ushort code, const pdc_glyph_tab *glyphtab, int tabsize)
{
    int lo = 0;
    int hi = tabsize;

    while (lo < hi)
    {
        int i = (lo + hi) / 2;

        if (code == glyphtab[i].code)
            return glyphtab[i].name;

        if (code < glyphtab[i].code)
            hi = i;
        else
            lo = i + 1;
    }

    return NULL;
}

/*
 * Binary search for list of codes in a pdc_code_map array sorted by source code
 */
int
pdc_code2codelist(pdc_core *pdc, pdc_ushort code,
                  const pdc_code_map *codemap, int tabsize,
                  pdc_ushort *codelist, int listsize)
{
    int lo = 0;
    int hi = tabsize;
    int nv = 0;

    while (lo < hi)
    {
        int i = (lo + hi) / 2;

        if (codemap[i].src == code)
        {
            for (; i >= 1; i--)
            {
                if (codemap[i-1].src != code)
                    break;
            }

            for (; i < tabsize; i++)
            {
                if (codemap[i].src != code)
                    break;

                if (nv >= listsize)
                    pdc_error(pdc, PDC_E_CONV_LIST_MEMOVERFLOW, 0, 0, 0, 0);

                codelist[nv] = codemap[i].dst;
                nv++;
            }

            return nv;
        }
        if (codemap[i].src > code)
            hi = i;
        else
            lo = i + 1;
    }

    return nv;
}

/*
 * Binary search for glyphname in a pdc_glyph_tab array sorted by glyphname
 * to get the static pointer for the glyphname.
 */
const char *
pdc_glyphname2glyphname(const char *glyphname,
                        const pdc_glyph_tab *glyphtab, int tabsize)
{
    const char *s1, *s2;
    int lo = 0;
    int hi = tabsize;
    int cmp, i;

    while (lo < hi)
    {
        i = (lo + hi) / 2;

        s1 = glyphname;
        s2 = glyphtab[i].name;
        for (; *s1; ++s1, ++s2)
        {
            if (*s1 != *s2)
                break;
        }
        cmp = (*s1 - *s2);

        if (cmp == 0)
            return glyphtab[i].name;

        if (cmp < 0)
            hi = i;
        else
            lo = i + 1;
    }

    return NULL;
}


/* ---------------- special character search functions ------------------- */

/*
 * Returns the Unicode value of a glyph name in Adobe Glyph List 1.2'.
 * If the name is not contained in AGL, -1 will be returned.
 */
int
pdc_adobe2unicode(const char *glyphname)
{
    return pdc_glyphname2code(glyphname, tab_agl2uni,
                             (sizeof (tab_agl2uni)) / (sizeof (pdc_glyph_tab)));
}

/*
 * Returns the name in AGL 1.2' or ZapfDingbats font,
 * which corresponds to the supplied Unicode value.
 * If the value doesn't have a corresponding glyph name,
 * NULL will be returned.
 * For control codes ".notdef" will be returned.
 * But this is not compatibel with AGL 2.0!
 */
const char *
pdc_unicode2adobe(pdc_ushort uv)
{
    const char *glyphname;

    /* AGL 1.2' glyphname */
    glyphname = pdc_code2glyphname(uv, tab_uni2agl,
                            (sizeof tab_uni2agl) / (sizeof (pdc_glyph_tab)));
    if (glyphname != NULL)
        return glyphname;

    /* C0 and C1 control characters.
     * They have never a graphical representation but are defined.
     */
    if (uv < PDC_UNICODE_SPACE ||
        (uv >= PDC_UNICODE_DELETE && uv < PDC_UNICODE_NBSP))
        return glyph__notdef;

    return NULL;
}

const char *
pdc_get_notdef_glyphname(void)
{
    return (char *) glyph__notdef;
}

/*
 * Returns the Unicode value of a ZapfDingbats glyph name.
 * If the name is not contained in the ZapfDingbats list
 * -1 will be returned.
 */
int
pdc_zadb2unicode(const char *glyphname)
{
    return pdc_glyphname2code(glyphname, tab_zadb2uni,
                          (sizeof (tab_zadb2uni)) / (sizeof (pdc_glyph_tab)));
}

/*
 * Returns the glyph name in the ZapfDingbats font which corresponds
 * to the supplied Unicode value. If the value doesn't have a
 * corresponding glyph name NULL will be returned.
 */
const char *
pdc_unicode2zadb(pdc_ushort uv)
{
    return pdc_code2glyphname(uv, tab_uni2zadb,
                             (sizeof tab_uni2zadb) / (sizeof (pdc_glyph_tab)));
}

/*
 * Returns the Unicode values of a glyph name in Adobe Glyph List 2.0
 * which is not contained in AGL-1.2'.
 *
 * The function presupposes that uvlist is an array of PDC_MAX_UVLIST.
 *
 * Return value is the number of Unicodes.
 */
int
pdc_newadobe2unicodelist(const char *glyphname, pdc_ushort *uvlist)
{
    return pdc_glyphname2codelist(glyphname, tab_diffagl2uni,
                           (sizeof tab_diffagl2uni) / (sizeof (pdc_glyph_tab)),
                           uvlist);
}

/*
 * Returns the glyph name in Adobe Glyph List 2.0
 * which is not contained in AGL-1.2' corresponding
 * to the supplied Unicode value. Ambiguous Unicode
 * values or glyph names are not supported!
 * If the value doesn't have a corresponding glyph name
 * NULL will be returned.
 */
const char *
pdc_unicode2newadobe(pdc_ushort uv)
{
    return pdc_code2glyphname(uv, tab_uni2diffagl,
                         (sizeof tab_uni2diffagl) / (sizeof (pdc_glyph_tab)));
}

/*
 * Returns the glyph name in Adobe Glyph List 2.0
 * which is not contained in AGL-1.2' and which matches
 * the supplied glyph name.
 * If no match is found NULL will be returned.
 */
const char *
pdc_get_newadobe_glyphname(const char *glyphname)
{
    return pdc_glyphname2glyphname(glyphname, tab_diffagl2uni,
                         (sizeof tab_diffagl2uni) / (sizeof (pdc_glyph_tab)));
}


/*
 * Returns the alternative Unicode value of a double-mapped
 * AGL-1.2 glyph name. If the name is not double-mapped,
 * -1 will be returned.
 */
int
pdc_glyphname2altunicode(const char *glyphname)
{
    return pdc_glyphname2code(glyphname, tab_double_mappping,
                   (sizeof (tab_double_mappping)) / (sizeof (pdc_glyph_tab)));
}

/*
 * Returns true if a character name is contained in pc_standard_latin_charset.
 * Otherwise false will be returned.
 */
pdc_bool
pdc_is_std_charname(const char *glyphname)
{
    const char *s1, *s2;
    int lo = 0;
    int hi = ((sizeof pc_standard_latin_charset) / (sizeof (char *)));
    int cmp, i;

    if (glyphname)
    {
        while (lo < hi)
        {
            i = (lo + hi) / 2;

            s1 = glyphname;
            s2 = pc_standard_latin_charset[i];
            for (; *s1; ++s1, ++s2)
            {
                if (*s1 != *s2)
                    break;
            }
            cmp = (*s1 - *s2);

            if (cmp == 0)
                return pdc_true;

            if (cmp < 0)
                hi = i;
            else
                lo = i + 1;
        }
    }

    return pdc_false;
}



/* -------------- special character mapping for Type1 fonts --------------- */

/*
 * Deletes a bit in a bit mask. The bit indicates that
 * the respective glyph name of AGL 2.0 is not available
 * in a PostScript font. The glyph name is used to avoid
 * ambiguities (see comment in pc_chartabs.h)
 *
 */

#define PDC_BIT_NBSP         (1L<<0)
#define PDC_BIT_SHY          (1L<<1)
#define PDC_BIT_MODMACRON    (1L<<2)
#define PDC_BIT_CAPDELTA     (1L<<3)
#define PDC_BIT_CAPOMEGA     (1L<<4)
#define PDC_BIT_DIVSLASH     (1L<<5)
#define PDC_BIT_BULLETOP     (1L<<6)
#define PDC_BIT_SMALLMU      (1L<<7)

void
pdc_delete_missingglyph_bit(pdc_ushort uv, pdc_ulong *bmask)
{
    switch(uv)
    {
        case PDC_UNICODE_NBSP:
        *bmask &= ~PDC_BIT_NBSP;
        return;

        case PDC_UNICODE_SHY:
        *bmask &= ~PDC_BIT_SHY;
        return;

        case PDC_UNICODE_MODMACRON:
        *bmask &= ~PDC_BIT_MODMACRON;
        return;

        case PDC_UNICODE_CAPDELTA:
        *bmask &= ~PDC_BIT_CAPDELTA;
        return;

        case PDC_UNICODE_CAPOMEGA:
        *bmask &= ~PDC_BIT_CAPOMEGA;
        return;

        case PDC_UNICODE_DIVSLASH:
        *bmask &= ~PDC_BIT_DIVSLASH;
        return;

        case PDC_UNICODE_BULLETOP:
        *bmask &= ~PDC_BIT_BULLETOP;
        return;

        case PDC_UNICODE_SMALLMU:
        *bmask &= ~PDC_BIT_SMALLMU;
        return;

        default:
        return;
    }
}

/*
 * Returnes an alternative Unicode value and/or glyph name for an
 * AGL 2.0 glyph name which is not available in a PostScript font.
 *
 */

pdc_ushort
pdc_get_alter_glyphname(pdc_ushort uv, pdc_ulong bmask, char **glyphname)
{
    switch(uv)
    {
        case PDC_UNICODE_NBSP:
        if (bmask & PDC_BIT_NBSP)
        {
            if (glyphname)
                *glyphname = (char *) glyph_space;
            return PDC_UNICODE_SPACE;
        }
        break;

        case PDC_UNICODE_SHY:
        if (bmask & PDC_BIT_SHY)
        {
            if (glyphname)
                *glyphname = (char *) glyph_hyphen;
            return PDC_UNICODE_HYPHEN;
        }
        break;

        case PDC_UNICODE_MODMACRON:
        if (bmask & PDC_BIT_MODMACRON)
        {
            if (glyphname)
                *glyphname = (char *) glyph_macron;
            return PDC_UNICODE_MACRON;
        }
        break;

        case PDC_UNICODE_CAPDELTA:
        if (bmask & PDC_BIT_CAPDELTA)
        {
            if (glyphname)
                *glyphname = (char *) glyph_Delta;
            return PDC_UNICODE_INCREMENT;
        }
        break;

        case PDC_UNICODE_CAPOMEGA:
        if (bmask & PDC_BIT_CAPOMEGA)
        {
            if (glyphname)
                *glyphname = (char *) glyph_Omega;
            return PDC_UNICODE_OHMSIGN;
        }
        break;

        case PDC_UNICODE_DIVSLASH:
        if (bmask & PDC_BIT_DIVSLASH)
        {
            if (glyphname)
                *glyphname = (char *) glyph_fraction;
            return PDC_UNICODE_FRACSLASH;
        }

        case PDC_UNICODE_BULLETOP:
        if (bmask & PDC_BIT_BULLETOP)
        {
            if (glyphname)
                *glyphname = (char *) glyph_periodcentered;
            return PDC_UNICODE_MIDDLEDOT;
        }

        case PDC_UNICODE_SMALLMU:
        if (bmask & PDC_BIT_SMALLMU)
        {
            if (glyphname)
                *glyphname = (char *) glyph_mu;
            return PDC_UNICODE_MICRO;
        }

        default:
        if (glyphname)
        {
            if (*glyphname == NULL)
                *glyphname = (char *) pdc_get_notdef_glyphname();
            return 0;
        }
    }

    return uv;
}

/*
 * Returns the Unicode value for a given string Unicode expression:
 *
 *    - Byte 1...255 -> U0001...U00FF
 *    - U+XXXXX
 *    - 0xXXXXX
 *    - HTML character reference without frame syntax &...;
 *
 * If no conversion is possible -1 will be returned.
 */
int
pdc_string2unicode(pdc_core *pdc, const char *text, int i_flags,
                   const pdc_keyconn *keyconn, pdc_bool verbose)
{
    int iz = PDC_KEY_NOTFOUND, usv = -1;
    pdc_bool seterr = pdc_false;
    int flags = PDC_INT_UNSIGNED;
    int i = 0;

    (void) verbose;

    /* single byte as Unicode value */
    if (strlen(text) == 1)
    {
        char c = text[0];
        usv = (pdc_byte) c;
    }
    else
    {
        /* keyword */
        if (keyconn)
        {
            if (i_flags & PDC_INT_CASESENS)
                iz = pdc_get_keycode(text, keyconn);
            else
                iz = pdc_get_keycode_ci(text, keyconn);
        }
        if (iz != PDC_KEY_NOTFOUND)
        {
            usv = iz;
        }
        else
        {
            /* Unicode value */
            if (!pdc_strincmp(text, "U+", 2))
            {
                flags |= PDC_INT_HEXADEC;
                i = 2;
            }
            if (!pdc_str2integer(&text[i], flags, &iz))
            {
                    seterr = pdc_true;
            }
            else if (iz >= PDC_NUM_UNIVAL ||
                     (iz >= PDC_UNICODE_MINHIGHSUR &&
                      iz <= PDC_UNICODE_MAXLOWSUR))
            {
                seterr = pdc_true;
            }
            else
            {
                usv = iz;
            }
        }
    }

    if (seterr)
    {
        pdc_set_errmsg(pdc, PDC_E_CONV_ILLUTF32CHAR, &text[i], 0, 0, 0);
        if (verbose)
            pdc_error(pdc, -1, 0, 0, 0, 0);
    }

    return usv;
}

/*
 * Returns true if Unicode character is a character relevant for line breaking
 *
 */
pdc_bool
pdc_is_linebreaking_relchar(pdc_ushort uv)
{
    switch (uv)
    {
        case PDC_UNICODE_HT:
        case PDC_UNICODE_LF:
        case PDC_UNICODE_VT:
        case PDC_UNICODE_FF:
        case PDC_UNICODE_CR:
        case PDC_UNICODE_NEL:
        case PDC_UNICODE_SHY:
        case PDC_UNICODE_LS:
        case PDC_UNICODE_PS:
        return pdc_true;
    }

    return pdc_false;
}


