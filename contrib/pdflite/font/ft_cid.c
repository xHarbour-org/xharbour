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
 * FONT CID functions
 *
 */

#define FT_CID_C

#include <errno.h>

#include "ft_font.h"
#include "pc_file.h"

/* ------------------------ Predefined CMaps ------------------------ */

/* Predefined CMaps and the corresponding character collection */
static const fnt_cmap_info fnt_predefined_cmaps[] =
{
    { "83pv-RKSJ-H",      cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "90ms-RKSJ-H",      cc_japanese, 0, PDC_1_3, 2, 2, 2, 2, 0},
    { "90ms-RKSJ-V",      cc_japanese, 0, PDC_1_3, 2, 2, 2, 2, 1},
    { "90msp-RKSJ-H",     cc_japanese, 0, PDC_1_3, 2, 2, 2, 2, 0},
    { "90msp-RKSJ-V",     cc_japanese, 0, PDC_1_3, 2, 2, 2, 2, 1},
    { "90pv-RKSJ-H",      cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "Add-RKSJ-H",       cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "Add-RKSJ-V",       cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 1},
    { "EUC-H",            cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "EUC-V",            cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 1},
    { "Ext-RKSJ-H",       cc_japanese, 0, PDC_1_3, 2, 2, 2, 2, 0},
    { "Ext-RKSJ-V",       cc_japanese, 0, PDC_1_3, 2, 2, 2, 2, 1},
    { "H",                cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "V",                cc_japanese, 0, PDC_1_3, 1, 1, 1, 1, 1},
    { "UniJIS-UCS2-H",    cc_japanese, 2, PDC_1_3, 2, 4, 4, 6, 0},
    { "UniJIS-UCS2-V",    cc_japanese, 2, PDC_1_3, 2, 4, 4, 6, 1},
    { "UniJIS-UCS2-HW-H", cc_japanese,-2, PDC_1_3, 2, 4, 4, 6, 0},
    { "UniJIS-UCS2-HW-V", cc_japanese,-2, PDC_1_3, 2, 4, 4, 6, 1},
    { "UniJIS-UTF16-H",   cc_japanese, 2, PDC_1_5, 0, 0, 5, 6, 0},
    { "UniJIS-UTF16-V",   cc_japanese, 2, PDC_1_5, 0, 0, 5, 6, 1},

    { "GB-EUC-H",         cc_simplified_chinese, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "GB-EUC-V",         cc_simplified_chinese, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "GBpc-EUC-H",       cc_simplified_chinese, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "GBpc-EUC-V",       cc_simplified_chinese, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "GBK-EUC-H",        cc_simplified_chinese, 0, PDC_1_3, 2, 2, 2, 2, 0},
    { "GBK-EUC-V",        cc_simplified_chinese, 0, PDC_1_3, 2, 2, 2, 2, 1},
    { "GBKp-EUC-H",       cc_simplified_chinese, 0, PDC_1_4, 0, 2, 2, 2, 0},
    { "GBKp-EUC-V",       cc_simplified_chinese, 0, PDC_1_4, 0, 2, 2, 2, 1},
    { "GBK2K-H",          cc_simplified_chinese, 0, PDC_1_4, 0, 4, 4, 5, 0},
    { "GBK2K-V",          cc_simplified_chinese, 0, PDC_1_4, 0, 4, 4, 5, 1},
    { "UniGB-UCS2-H",     cc_simplified_chinese, 2, PDC_1_3, 2, 4, 4, 4, 0},
    { "UniGB-UCS2-V",     cc_simplified_chinese, 2, PDC_1_3, 2, 4, 4, 4, 1},
    { "UniGB-UTF16-H",    cc_simplified_chinese, 2, PDC_1_5, 0, 0, 4, 5, 0},
    { "UniGB-UTF16-V",    cc_simplified_chinese, 2, PDC_1_5, 0, 0, 4, 5, 1},

    { "B5pc-H",           cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "B5pc-V",           cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "HKscs-B5-H",       cc_traditional_chinese, 0, PDC_1_4, 0, 3, 3, 5, 0},
    { "HKscs-B5-V",       cc_traditional_chinese, 0, PDC_1_4, 0, 3, 3, 5, 1},
    { "ETen-B5-H",        cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "ETen-B5-V",        cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "ETenms-B5-H",      cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "ETenms-B5-V",      cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "CNS-EUC-H",        cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "CNS-EUC-V",        cc_traditional_chinese, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "UniCNS-UCS2-H",    cc_traditional_chinese, 2, PDC_1_3, 0, 3, 3, 3, 0},
    { "UniCNS-UCS2-V",    cc_traditional_chinese, 2, PDC_1_3, 0, 3, 3, 3, 1},
    { "UniCNS-UTF16-H",   cc_traditional_chinese, 2, PDC_1_5, 0, 0, 4, 4, 0},
    { "UniCNS-UTF16-V",   cc_traditional_chinese, 2, PDC_1_5, 0, 0, 4, 4, 1},

    { "KSC-EUC-H",        cc_korean, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "KSC-EUC-V",        cc_korean, 0, PDC_1_3, 0, 0, 0, 0, 1},
    { "KSCms-UHC-H",      cc_korean, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "KSCms-UHC-V",      cc_korean, 0, PDC_1_3, 1, 1, 1, 1, 1},
    { "KSCms-UHC-HW-H",   cc_korean, 0, PDC_1_3, 1, 1, 1, 1, 0},
    { "KSCms-UHC-HW-V",   cc_korean, 0, PDC_1_3, 1, 1, 1, 1, 1},
    { "KSCpc-EUC-H",      cc_korean, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "UniKS-UCS2-H",     cc_korean, 2, PDC_1_3, 1, 1, 1, 1, 0},
    { "UniKS-UCS2-V",     cc_korean, 2, PDC_1_3, 1, 1, 1, 1, 1},
    { "UniKS-UTF16-H",    cc_korean, 2, PDC_1_5, 0, 0, 2, 2, 0},
    { "UniKS-UTF16-V",    cc_korean, 2, PDC_1_5, 0, 0, 2, 2, 1},

    { "Identity-H",       cc_identity, 0, PDC_1_3, 0, 0, 0, 0, 0},
    { "Identity-V",       cc_identity, 0, PDC_1_3, 0, 0, 0, 0, 1},

    { NULL,               cc_none, 0, 0, 0, 0, 0, 0, 0},
};

static int
fnt_get_predefined_cmap_slot(const char *cmapname)
{
    int slot;

    for (slot = 0; ; slot++)
    {
        if (fnt_predefined_cmaps[slot].name == NULL)
        {
            slot = -1;
            break;
        }
        if (!strcmp(fnt_predefined_cmaps[slot].name, cmapname))
            break;
    }

    return slot;
}

int
fnt_get_predefined_cmap_info(const char *cmapname, fnt_cmap_info *cmapinfo)
{
    int slot;

    slot = fnt_get_predefined_cmap_slot(cmapname);

    if (slot == -1)
        return cc_none;

    if (cmapinfo)
        *cmapinfo = fnt_predefined_cmaps[slot];

    return fnt_predefined_cmaps[slot].charcoll;
}

static const pdc_keyconn fnt_charcoll_keylist[] =
{
    { "Japan1",    cc_japanese},
    { "GB1",       cc_simplified_chinese},
    { "CNS1",      cc_traditional_chinese},
    { "Korea1",    cc_korean},
    { "Identity",  cc_identity},
    { "Unknown",   cc_unknown},
    { NULL, 0}
};

const char *
fnt_get_ordering_cid(int charcoll)
{
    return pdc_get_keyword(charcoll, fnt_charcoll_keylist);
}

int
fnt_get_charcoll(const char *ordering)
{
    int charcoll;

    charcoll = (int) pdc_get_keycode(ordering, fnt_charcoll_keylist);

    if (charcoll == PDC_KEY_NOTFOUND)
        return cc_unknown;
    else
        return charcoll;
}

int
fnt_get_supplement(fnt_cmap_info *cmapinfo, int compatibility)
{
    int retval = 0;

    switch(compatibility)
    {
        case PDC_1_3:
        retval = cmapinfo->supplement13;
        break;

        case PDC_1_4:
        retval = cmapinfo->supplement14;
        break;

        case PDC_1_5:
        retval = cmapinfo->supplement15;
        break;

        default:
        case PDC_1_6:
        retval = cmapinfo->supplement16;
        break;
    }

    return retval;
}

/*
 * See:
 * Adobe Technical Note #5078 (Japanese1)
 * Adobe Technical Note #5079 (GB1)
 * Adobe Technical Note #5080 (CNS1)
 * Adobe Technical Note #5093 (Korea1)
 */

int
fnt_get_maxcid(int charcoll, int supplement)
{
    switch(charcoll)
    {
        case cc_japanese:
        switch(supplement)
        {
            case 0:
            return 8283;

            case 1:
            return 8358;

            case 2:
            return 8719;

            case 3:
            return 9353;

            case 4:
            return 15443;

            case 5:
            return 20316;

            case 6:
            default:
            return 23057;
        }

        case cc_simplified_chinese:
        switch(supplement)
        {
            case 0:
            return 7716;

            case 1:
            return 9896;

            case 2:
            return 22126;

            case 3:
            return 22352;

            case 4:
            return 29063;

            case 5:
            default:
            return 30283;
        }

        case cc_traditional_chinese:
        switch(supplement)
        {
            case 0:
            return 14098;

            case 1:
            return 17407;

            case 2:
            return 17600;

            case 3:
            return 18845;

            case 4:
            return 18964;

            case 5:
            default:
            return 19087;
        }

        case cc_korean:
        switch(supplement)
        {
            case 0:
            return 9332;

            case 1:
            return 18154;

            case 2:
            default:
            return 18351;
        }

        case cc_identity:
        case cc_unknown:
        return FNT_MAXCID;

        default:
        return 0;
    }
}
