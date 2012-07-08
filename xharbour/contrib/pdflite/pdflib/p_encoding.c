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
 * PDFlib encoding handling routines
 *
 */

#include "p_intern.h"
#include "p_font.h"

void
pdf__encoding_set_char(PDF *p, const char *encoding, int slot,
                       const char *glyphname, int uv)
{
    int enc;
    pdc_encodingvector *ev;
    char given;

    if (!encoding || !*encoding)
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "encoding", 0, 0, 0);

    if (slot < 0 || slot > 255)
        pdc_error(p->pdc, PDC_E_ILLARG_INT,
            "slot", pdc_errprintf(p->pdc, "%d", slot), 0, 0);

    if (uv < 0 || uv >= PDC_NUM_BMPVAL)
        pdc_error(p->pdc, PDC_E_ILLARG_INT,
            "uv", pdc_errprintf(p->pdc, "%d", uv), 0, 0);

    if (!glyphname || !*glyphname)
    {
        if (uv == 0)
            pdc_error(p->pdc, PDF_E_ENC_GLYPHORCODE, 0, 0, 0, 0);
    }

    for (enc = (int) pdc_invalidenc + 1; enc < (int) pdc_firstvarenc; enc++)
    {
        if (!strcmp(encoding, pdc_get_fixed_encoding_name((pdc_encoding) enc)))
            pdc_error(p->pdc, PDF_E_ENC_CANTCHANGE, encoding, 0, 0, 0);
    }

    if (uv == 0)
    {
        given = 1;
        uv = (int) pdc_insert_glyphname(p->pdc, glyphname);
    }
    else if (!glyphname || !*glyphname)
    {
        given = 0;
        glyphname = pdc_insert_unicode(p->pdc, (pdc_ushort) uv);
    }
    else
    {
        const char *reg_glyphname;
        pdc_ushort reg_uv;
        int retval;

        given = 1;
        reg_glyphname = pdc_unicode2glyphname(p->pdc, (pdc_ushort) uv);
        if (reg_glyphname)
        {
            if (strcmp(reg_glyphname, glyphname))
            {
                pdc_warning(p->pdc, PDF_E_ENC_BADGLYPH,
                    glyphname,
                    pdc_errprintf(p->pdc, "%04X", uv),
                    reg_glyphname, 0);
            }

            /* We take the registered name */
        }
        else
        {
            retval = pdc_glyphname2unicode(p->pdc, glyphname);
            if (retval > -1)
            {
                reg_uv = (pdc_ushort) retval;
                if (reg_uv && reg_uv != (pdc_ushort) uv)
                {
                    pdc_error(p->pdc, PDF_E_ENC_BADUNICODE,
                        pdc_errprintf(p->pdc, "%04X", uv), glyphname,
                        pdc_errprintf(p->pdc, "%04X", reg_uv), 0);
                }
            }

            /* We register the new glyph name and unicode value */
            pdc_register_glyphname(p->pdc, glyphname, (pdc_ushort) uv,
                                   pdc_false);
        }
    }

    /* search for a registered encoding */
    enc = pdc_find_encoding(p->pdc, encoding);

    /* not found */
    if (enc == pdc_invalidenc)
    {
        ev = pdc_new_encoding(p->pdc, encoding);
        ev->flags |= PDC_ENC_USER;
        ev->flags |= PDC_ENC_SETNAMES;
        ev->flags |= PDC_ENC_ALLOCCHARS;

        enc = pdc_insert_encoding_vector(p->pdc, ev);
    }

    /* encoding vector */
    ev = pdc_get_encoding_vector(p->pdc, (pdc_encoding)enc);
    if (!(ev->flags & PDC_ENC_USER))
    {
	pdc_error(p->pdc, PDF_E_ENC_CANTCHANGE, encoding, 0, 0, 0);
    }
    else if (ev->flags & PDC_ENC_USED)
    {
	pdc_error(p->pdc, PDF_E_ENC_INUSE, encoding, 0, 0, 0);
    }

    /* Free character name */
    if (ev->chars[slot] != NULL)
        pdc_free(p->pdc, ev->chars[slot]);

    /* Saving */
    ev->codes[slot] = (pdc_ushort) uv;
    if (glyphname != NULL)
        ev->chars[slot] = pdc_strdup(p->pdc, glyphname);
    ev->given[slot] = given;

    pdc_encoding_logg_protocol(p->pdc, ev);
}

pdc_encoding
pdf_get_hypertextencoding_param(PDF *p, int *codepage)
{
    if (p->hypertextencoding == pdc_invalidenc)
    {
        p->hypertextencoding = pdf_get_hypertextencoding(p, "auto",
            &p->hypertextcodepage, pdc_true);

        if (p->hypertextencoding == pdc_invalidenc)
            pdc_error(p->pdc, -1, 0, 0, 0, 0);
    }

    if (codepage)
        *codepage = p->hypertextcodepage;

    pdc_logg_cond(p->pdc, 3, trc_encoding,
                  "\t\thypertextformat=%d\n"
                  "\t\thypertextencoding=%s\n"
                  "\t\thypertextcodepage=%d\n",
                  p->hypertextformat,
                  pdc_get_user_encoding(p->pdc, p->hypertextencoding),
                  p->hypertextcodepage);

    return p->hypertextencoding;
}

pdc_encoding
pdf_get_hypertextencoding(PDF *p, const char *encoding, int *codepage,
                          pdc_bool verbose)
{
    pdc_encoding enc = pdc_invalidenc;

    *codepage = 0;

    if (!*encoding)
    {
        enc = pdc_unicode;
    }
    else
    {
        {
            enc = pdc_get_encoding(p->pdc, encoding, codepage, verbose);
            if (enc < 0 && enc != pdc_invalidenc && enc != pdc_unicode)
            {
                pdc_set_errmsg(p->pdc, PDF_E_ENC_BADHYPTEXTENC, encoding,
                               0, 0, 0);
                enc = pdc_invalidenc;
            }
        }
    }

    return enc;
}
