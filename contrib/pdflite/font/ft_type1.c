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
 * FONT Type1 font handling routines
 *
 */

#include "ft_font.h"

pdc_bool
fnt_test_type1_font(pdc_core *pdc, const pdc_byte *img)
{
    char startsequ[5];

    strcpy(startsequ, FNT_PFA_STARTSEQU);

    /* ASCII block sign and begin of text at byte 7 */
    if (img[0] == 0x80 && img[1] == 0x01 &&
        strncmp((const char *)&img[6], startsequ, 4) == 0)
    {
        pdc_logg_cond(pdc, 1, trc_font,
                          "\tPostScript Type1 font detected\n");
        return pdc_true;
    }
    return pdc_false;
}


