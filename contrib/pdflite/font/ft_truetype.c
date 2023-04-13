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
 * FONT TrueType handling routines
 *
 */

#include "ft_font.h"
#include "ft_truetype.h"

#ifdef PDF_TRUETYPE_SUPPORTED

void
tt_assert(tt_file *ttf)
{
    pdc_core *pdc = ttf->pdc;

    if (ttf->filename)
        pdc_error(pdc, FNT_E_TT_ASSERT2, ttf->filename, 0, 0, 0);
    else
        pdc_error(pdc, FNT_E_TT_ASSERT1, 0, 0, 0, 0);
} /* tt_assert */

void
tt_error(tt_file *ttf)
{
    pdc_core *pdc = ttf->pdc;

    if (ttf->filename)
        pdc_error(pdc, FNT_E_TT_CORRUPT2, ttf->filename, 0, 0, 0);
    else
        pdc_error(pdc, FNT_E_TT_CORRUPT1, 0, 0, 0, 0);
} /* tt_error */

void
tt_seek(tt_file *ttf, long offset)
{
    if (ttf->incore)
    {
        TT_IOCHECK(ttf, ttf->img + (tt_ulong) offset <= ttf->end);
        ttf->pos = ttf->img + (tt_ulong) offset;
    }
    else
        TT_IOCHECK(ttf, pdc_fseek(ttf->fp, offset, SEEK_SET) == 0);
}

void
tt_read(tt_file *ttf, void *buf, unsigned int nbytes)
{
    if (ttf->incore)
    {
        TT_IOCHECK(ttf, ttf->pos + (tt_ulong) nbytes <= ttf->end);
        memcpy(buf, ttf->pos, (size_t) nbytes);
        ttf->pos += (tt_ulong) nbytes;
    }
    else
        TT_IOCHECK(ttf, PDC_OK_FREAD(ttf->fp, buf, nbytes));
}

long
tt_tell(tt_file *ttf)
{
    if (ttf->incore)
        return (long) (ttf->pos - ttf->img);
    else
        return (long) pdc_ftell(ttf->fp);
}

tt_ushort
tt_get_ushort(tt_file *ttf)
{
    tt_byte *pos, buf[2];

    if (ttf->incore)
    {
        pos = ttf->pos;
        TT_IOCHECK(ttf, (ttf->pos += 2) <= ttf->end);
    }
    else
    {
        pos = buf;
        TT_IOCHECK(ttf, PDC_OK_FREAD(ttf->fp, buf, 2));
    }

    return pdc_get_be_ushort(pos);
}

tt_short
tt_get_short(tt_file *ttf)
{
    tt_byte *pos, buf[2];

    if (ttf->incore)
    {
        pos = ttf->pos;
        TT_IOCHECK(ttf, (ttf->pos += 2) <= ttf->end);
    }
    else
    {
        pos = buf;
        TT_IOCHECK(ttf, PDC_OK_FREAD(ttf->fp, buf, 2));
    }

    return pdc_get_be_short(pos);
}

tt_ulong
tt_get_ulong3(tt_file *ttf)
{
    tt_byte *pos, buf[3];

    if (ttf->incore)
    {
        pos = ttf->pos;
        TT_IOCHECK(ttf, (ttf->pos += 3) <= ttf->end);
    }
    else
    {
        pos = buf;
        TT_IOCHECK(ttf, PDC_OK_FREAD(ttf->fp, buf, 3));
    }

    return pdc_get_be_ulong3(pos);
}

tt_ulong
tt_get_ulong(tt_file *ttf)
{
    tt_byte *pos, buf[4];

    if (ttf->incore)
    {
        pos = ttf->pos;
        TT_IOCHECK(ttf, (ttf->pos += 4) <= ttf->end);
    }
    else
    {
        pos = buf;
        TT_IOCHECK(ttf, PDC_OK_FREAD(ttf->fp, buf, 4));
    }

    return pdc_get_be_ulong(pos);
}

tt_long
tt_get_long(tt_file *ttf)
{
    tt_byte *pos, buf[4];

    if (ttf->incore)
    {
        pos = ttf->pos;
        TT_IOCHECK(ttf, (ttf->pos += 4) <= ttf->end);
    }
    else
    {
        pos = buf;
        TT_IOCHECK(ttf, PDC_OK_FREAD(ttf->fp, buf, 4));
    }

    return pdc_get_be_long(pos);
}

tt_ulong
tt_get_offset(tt_file *ttf, tt_byte offsize)
{
    tt_byte buf;

    switch (offsize)
    {
        case 1:
        tt_read(ttf, &buf, 1);
        return (tt_ulong) buf;

        case 2:
        return (tt_ulong) tt_get_ushort(ttf);

        case 3:
        return (tt_ulong) tt_get_ulong3(ttf);

        case 4:
        return (tt_ulong) tt_get_ulong(ttf);
    }
    return 0;
}

static void
tt_get_dirent(tt_dirent *dirent, tt_file *ttf)
{
    tt_read(ttf, dirent->tag, TT_TABTAG_SIZE);
    dirent->tag[TT_TABTAG_SIZE] = 0;
    dirent->checksum = tt_get_ulong(ttf);
    dirent->offset = tt_get_ulong(ttf);
    dirent->length = tt_get_ulong(ttf);
} /* tt_get_dirent */

int
tt_tag2idx(tt_file *ttf, char *tag)
{
    int i;

    for (i = 0; i < ttf->n_tables; ++i)
	if (strcmp(ttf->dir[i].tag, tag) == 0)
	    return i;

    return -1;
} /* tt_tag2idx */

void *
tt_get_tab(tt_file *ttf, char *tag, size_t nbytes, pdc_bool tterror,
           tt_ulong *offset)
{
    static const char *fn = "tt_get_tab";
    pdc_core *pdc = ttf->pdc;
    int idx = tt_tag2idx(ttf, tag);

    if (idx == -1)
    {
        if (tterror)
            tt_error(ttf);
        return NULL;
    }

    pdc_logg_cond(pdc, 3, trc_font,
        "\t\treading table \"%s\" (offset=0x%05X, length=%d)\n",
        tag, ttf->dir[idx].offset, ttf->dir[idx].length);

    tt_seek(ttf, (long) ttf->dir[idx].offset);

    if (offset)
        *offset = ttf->dir[idx].offset;

    return pdc_malloc(pdc, nbytes, fn);
}

static void
tt_get_cmap0(tt_file *ttf, tt_cmap0_6 *cm0_6)
{
    static const char *fn = "tt_get_cmap0";
    pdc_core *pdc = ttf->pdc;
    tt_ushort c;
    tt_byte buf[256];

    cm0_6->glyphIdArray = (tt_ushort *) 0;

    cm0_6->length       = tt_get_ushort(ttf);
    cm0_6->language     = tt_get_ushort(ttf);

    /* These are not used in format 0 */
    cm0_6->firstCode	= 0;
    cm0_6->entryCount	= 256;

    cm0_6->glyphIdArray = (tt_ushort *)
        pdc_malloc(pdc, (size_t) (sizeof (tt_ushort) * 256), fn);

    tt_read(ttf, buf, 256);

    for (c = 0; c < 256; c++)
	cm0_6->glyphIdArray[c] = (tt_ushort) buf[c];

} /* tt_get_cmap0 */

static void
tt_get_cmap6(tt_file *ttf, tt_cmap0_6 *cm0_6)
{
    static const char *fn = "tt_get_cmap6";
    pdc_core *pdc = ttf->pdc;
    tt_ushort c, last, cmax;

    cm0_6->glyphIdArray = (tt_ushort *) 0;

    cm0_6->length       = tt_get_ushort(ttf);
    cm0_6->language     = tt_get_ushort(ttf);
    cm0_6->firstCode    = tt_get_ushort(ttf);
    cm0_6->entryCount   = tt_get_ushort(ttf);

    last = (tt_ushort) (cm0_6->firstCode + cm0_6->entryCount);
    cmax = MAX(last, 256);

    cm0_6->glyphIdArray = (tt_ushort *)
        pdc_malloc(pdc, (size_t) (sizeof (tt_ushort) * cmax), fn);

    /* default for codes outside the range specified in this table */
    for (c = 0; c < cmax; c++)
        cm0_6->glyphIdArray[c] = (tt_ushort) 0;

    for (c = cm0_6->firstCode; c < last; c++)
        cm0_6->glyphIdArray[c] = tt_get_ushort(ttf);

} /* tt_get_cmap6 */

static void
tt_cleanup_cmap4(tt_file *ttf, tt_cmap4 *cm4)
{
    pdc_core *pdc = ttf->pdc;

    if (cm4 != (tt_cmap4 *) 0)
    {
        if (cm4->endCount != (tt_ushort *) 0)
            pdc_free(pdc, cm4->endCount);
        if (cm4->startCount != (tt_ushort *) 0)
            pdc_free(pdc, cm4->startCount);
        if (cm4->idDelta != (tt_short *) 0)
            pdc_free(pdc, cm4->idDelta);
        if (cm4->idRangeOffs != (tt_ushort *) 0)
            pdc_free(pdc, cm4->idRangeOffs);
        if (cm4->glyphIdArray != (tt_ushort *) 0)
            pdc_free(pdc, cm4->glyphIdArray);

        pdc_free(pdc, cm4);
    }
}

static tt_cmap4 *
tt_get_cmap4(tt_file *ttf, tt_cmap4 *cm4)
{
    static const char *fn = "tt_get_cmap4";
    pdc_core *pdc = ttf->pdc;
    int		i, segs;

    /* the instruction order is critical for cleanup after exceptions!
    */
    cm4->endCount	= (tt_ushort *) 0;
    cm4->startCount	= (tt_ushort *) 0;
    cm4->idDelta	= (tt_short *)  0;
    cm4->idRangeOffs	= (tt_ushort *) 0;
    cm4->glyphIdArray	= (tt_ushort *) 0;

    cm4->length         = tt_get_ushort(ttf);
    cm4->version        = tt_get_ushort(ttf);
    cm4->segCountX2     = tt_get_ushort(ttf);
    cm4->searchRange    = tt_get_ushort(ttf);
    cm4->entrySelector  = tt_get_ushort(ttf);
    cm4->rangeShift     = tt_get_ushort(ttf);

    segs = cm4->segCountX2 / 2;

    if (segs >= 1)
    {
        cm4->numGlyphIds  = (tt_ushort)(
            ((cm4->length - ( 16L + 8L * segs )) & 0xFFFFU) / 2);

        TT_IOCHECK(ttf, 0 <= cm4->numGlyphIds);

        cm4->endCount =
            (tt_ushort *)
                pdc_malloc(pdc, (size_t) (sizeof (tt_ushort) * segs), fn);
        cm4->startCount =
            (tt_ushort *)
                pdc_malloc(pdc, (size_t) (sizeof (tt_ushort) * segs), fn);
        cm4->idDelta =
            (tt_short *)
                pdc_malloc(pdc, (size_t) (sizeof (tt_ushort) * segs), fn);
        cm4->idRangeOffs =
            (tt_ushort *)
                pdc_malloc(pdc, (size_t) (sizeof (tt_ushort) * segs), fn);

        if (cm4->numGlyphIds)
        {
            cm4->glyphIdArray = (tt_ushort *)
                pdc_malloc(pdc,
                    (size_t) (sizeof (tt_ushort) * cm4->numGlyphIds), fn);
        }

        for (i = 0; i < segs; ++i)
            cm4->endCount[i] = tt_get_ushort(ttf);

        TT_IOCHECK(ttf, cm4->endCount[segs - 1] == 0xFFFF);

        (void) tt_get_ushort(ttf);     /* padding */
        for (i = 0; i < segs; ++i)  cm4->startCount[i] = tt_get_ushort(ttf);
        for (i = 0; i < segs; ++i)  cm4->idDelta[i] = tt_get_short(ttf);
        for (i = 0; i < segs; ++i)  cm4->idRangeOffs[i] = tt_get_ushort(ttf);

        for (i = 0; i < cm4->numGlyphIds; ++i)
            cm4->glyphIdArray[i] = tt_get_ushort(ttf);
    }

    /* empty cmap */
    if (segs == 0 || (segs == 1 && cm4->endCount[0] == cm4->startCount[0]))
    {
        tt_cleanup_cmap4(ttf, cm4);

        cm4 = (tt_cmap4 *) 0;
    }

    return cm4;

} /* tt_get_cmap4 */


void
tt_get_tab_cmap(tt_file *ttf)
{
    static const char *fn = "tt_get_tab_cmap";
    pdc_core *pdc = ttf->pdc;
    tt_tab_cmap *tp = NULL;
    tt_ulong	offset;
    tt_ushort	numEncTabs;
    tt_ushort   platformID = 0;
    tt_ushort   encodingID = 0;
    tt_ushort   tableFormat = 0;
    tt_ulong    offsetEncTab = 0;
    tt_ulong    offset_mac = 0;
    tt_ulong    offset_win = 0;
    tt_ulong    offset_ucs4 = 0;
    tt_long     pos = 0;
    int         i;

    tp = (tt_tab_cmap *) tt_get_tab(ttf, fnt_str_cmap, sizeof (tt_tab_cmap),
                                    !ttf->fortet, &offset);
    if (tp == NULL)
        return;
    ttf->tab_cmap = tp;

    tp->win = (tt_cmap4 *) 0;
    tp->mac = (tt_cmap0_6 *) 0;
    tp->ucs4 = (tt_cmap12 *) 0;

    tp->platform = 0;
    tp->encoding = 0;
    tp->format = 0;
    tp->offset = 0;
    tp->length = 0;

    (void) tt_get_ushort(ttf);     /* version */
    numEncTabs = tt_get_ushort(ttf);

    pdc_logg_cond(pdc, 2, trc_font,
                      "\tSearching for cmap table entries:\n");
    for (i = 0; i < numEncTabs; ++i)
    {
        platformID = tt_get_ushort(ttf);
        encodingID = tt_get_ushort(ttf);
        offsetEncTab = tt_get_ulong(ttf);
        pos = tt_tell(ttf);

        tt_seek(ttf, (long) (offset + offsetEncTab));
        tableFormat = tt_get_ushort(ttf);

        pdc_logg_cond(pdc, 2, trc_font,
            "\t\tplatformID: %d,  encodingID: %2d,  "
            "tableFormat: %2d,  offsetEncTab: 0x%04X\n",
             platformID, encodingID, tableFormat, offsetEncTab);

        /*
         * platformID: 0  encodingID: 0  tableFormat: 0
         * platformID: 1  encodingID: 0  tableFormat: 0/6
         */
        if (((platformID == tt_pfid_uni && tableFormat == 0) ||
              platformID == tt_pfid_mac) && encodingID == tt_wenc_symbol)
	{
	    /* we currently do not support cmaps
	    ** other than format 0 and 6 for Macintosh cmaps.
	    */

	    if (tableFormat == 0 && tp->mac == (tt_cmap0_6 *) 0)
	    {
		tp->mac = (tt_cmap0_6 *)
                            pdc_malloc(pdc, sizeof (tt_cmap0_6), fn);
		tp->mac->format = 0;
                tt_get_cmap0(ttf, tp->mac);

                offset_mac = offsetEncTab;
	    }
	    else if (tableFormat == 6 && tp->mac == (tt_cmap0_6 *) 0)
	    {
		tp->mac = (tt_cmap0_6 *)
                            pdc_malloc(pdc, sizeof (tt_cmap0_6), fn);
		tp->mac->format = 6;
                tt_get_cmap6(ttf, tp->mac);

                offset_mac = offsetEncTab;
	    }
            else if (numEncTabs == 1 && tableFormat == 4)
            {
                /* simulating Windows */
                tp->win = (tt_cmap4 *) pdc_malloc(pdc, sizeof (tt_cmap4), fn);
                tp->win->format = tableFormat;
                tp->win->encodingID = encodingID;
                tp->win = tt_get_cmap4(ttf, tp->win);

                /* cmap picking not necessary */
            }
	}

        /*
         * platformID: 0  encodingID: 3     tableFormat: 4  (old mac)
         * platformID: 3  encodingID: 0/1/4 tableFormat: 4  preferred!
         */
        else if (tableFormat == 4 &&
                 ((platformID == tt_pfid_win &&
                   (encodingID == tt_wenc_symbol ||
                    encodingID == tt_wenc_text ||
                    encodingID == tt_wenc_big5)) ||
                  (platformID == tt_pfid_uni &&
                   encodingID == tt_wenc_mtext)))
        {
            if (tp->win == (tt_cmap4 *) 0 ||
                (tp->win != (tt_cmap4 *) 0 &&
                 ((tp->win->encodingID == tt_wenc_mtext ||
                   tp->win->encodingID == tt_wenc_big5) &&
                  encodingID < tt_wenc_mtext)))
            {
                if (tp->win != (tt_cmap4 *) 0)
                    tt_cleanup_cmap4(ttf, tp->win);
                tp->win = (tt_cmap4 *) pdc_malloc(pdc, sizeof (tt_cmap4), fn);

                tp->win->format = tableFormat;
                tp->win->encodingID = encodingID;
                tp->win = tt_get_cmap4(ttf, tp->win);

                if (tp->win != (tt_cmap4 *) 0)
                    offset_win = offsetEncTab;
            }
        }


        tt_seek(ttf, pos);
    } /* for */

    /* we suppose a windows platform (see old mac hostfont Times) */
    if (tp->win && tp->win->encodingID == tt_wenc_mtext)
    {
        encodingID = tt_wenc_text;
    }

    /* is symbol font */
    ttf->issymbol = (tp->win && tp->win->encodingID == tt_wenc_symbol) ?
                    pdc_true : pdc_false;

    /* has Unicode/CMap cmap */
    ttf->haswinuni = (!ttf->issymbol && (tp->win || tp->ucs4)) ?
                     pdc_true : pdc_false;

    /* has only Mac cmap */
    ttf->hasonlymac = (tp->mac && !tp->win && !tp->ucs4) ?
                      pdc_true : pdc_false;

    if (ttf->hasonlymac)
    {
        tp->platform = tt_pfid_mac;
        tp->encoding = tt_wenc_symbol;
        tp->format = tp->mac->format;
        tp->offset = offset_mac;
        tp->length = tp->mac->length;
    }
    else if (tp->win || tp->ucs4)
    {
        tp->platform = tt_pfid_win;
        if (ttf->issymbol)
        {
            tp->encoding = tt_wenc_symbol;
            tp->format = tp->win->format;
            tp->offset = offset_win;
            tp->length = tp->win->length;
        }
        else if (tp->ucs4)
        {
            tp->encoding = tt_wenc_utext;
            tp->format = tp->ucs4->format;
            tp->offset = offset_ucs4;
            tp->length = tp->ucs4->length;
        }
        else
        {
            ttf->hasbig5cmap = tp->win->encodingID == tt_wenc_big5;
            tp->encoding = tt_wenc_text;
            tp->format = tp->win->format;
            tp->offset = offset_win;
            tp->length = tp->win->length;
        }
    }

    pdc_logg_cond(ttf->pdc, 1, trc_font,
        "\tUsed cmap table entry:\n"
        "\t\tplatformID: %d,  encodingID: %2d,  tableFormat: %2d  (%s font)\n",
        tp->platform, tp->encoding, tp->format,
        ttf->issymbol ? "symbol" : "text");

    if (ttf->hasbig5cmap)
        pdc_logg_cond(ttf->pdc, 1, trc_font, "\t\tCID font with Big5 cmap\n");

    /* for subsetting and symbolic font:
     * tp->platform = tt_pfid_mac according PDF specification
     * otherwise GS will emit an error message
     */
    if (ttf->issymbol && offset_mac > 0)
    {
        ttf->forcesubset = !ttf->hasonlymac;
        tp->platform = tt_pfid_mac;
        tp->encoding = tt_wenc_symbol;
        tp->format = tp->mac->format;
        tp->offset = offset_mac;
        tp->length = tp->mac->length;
    }

} /* tt_get_tab_cmap */

void
tt_get_tab_head(tt_file *ttf)
{
    tt_tab_head *tp = NULL;

    tp = (tt_tab_head *) tt_get_tab(ttf, fnt_str_head, sizeof (tt_tab_head),
                                    !ttf->fortet, NULL);
    if (tp == NULL)
        return;
    ttf->tab_head = tp;

    tp->version                 = tt_get_fixed(ttf);
    tp->fontRevision            = tt_get_fixed(ttf);
    tp->checkSumAdjustment      = tt_get_ulong(ttf);
    tp->magicNumber             = tt_get_ulong(ttf);
    tp->flags                   = tt_get_ushort(ttf);
    tp->unitsPerEm              = tt_get_ushort(ttf);
    tp->created[1]              = tt_get_ulong(ttf);
    tp->created[0]              = tt_get_ulong(ttf);
    tp->modified[1]             = tt_get_ulong(ttf);
    tp->modified[0]             = tt_get_ulong(ttf);
    tp->xMin                    = tt_get_fword(ttf);
    tp->yMin                    = tt_get_fword(ttf);
    tp->xMax                    = tt_get_fword(ttf);
    tp->yMax                    = tt_get_fword(ttf);
    tp->macStyle                = tt_get_ushort(ttf);
    tp->lowestRecPPEM           = tt_get_ushort(ttf);
    tp->fontDirectionHint       = tt_get_short(ttf);
    tp->indexToLocFormat        = tt_get_short(ttf);
    tp->glyphDataFormat         = tt_get_short(ttf);
} /* tt_get_tab_head */

void
tt_get_tab_hhea(tt_file *ttf)
{
    tt_tab_hhea *tp = NULL;

    tp = (tt_tab_hhea *) tt_get_tab(ttf, fnt_str_hhea, sizeof (tt_tab_hhea),
                                    !ttf->fortet, NULL);
    if (tp == NULL)
        return;
    ttf->tab_hhea = tp;

    tp->version                 = tt_get_fixed(ttf);
    tp->ascender                = tt_get_fword(ttf);
    tp->descender               = tt_get_fword(ttf);
    tp->lineGap                 = tt_get_fword(ttf);
    tp->advanceWidthMax         = tt_get_fword(ttf);
    tp->minLeftSideBearing      = tt_get_fword(ttf);
    tp->minRightSideBearing     = tt_get_fword(ttf);
    tp->xMaxExtent              = tt_get_fword(ttf);
    tp->caretSlopeRise          = tt_get_short(ttf);
    tp->caretSlopeRun           = tt_get_short(ttf);
    tp->res1                    = tt_get_short(ttf);
    tp->res2                    = tt_get_short(ttf);
    tp->res3                    = tt_get_short(ttf);
    tp->res4                    = tt_get_short(ttf);
    tp->res5                    = tt_get_short(ttf);
    tp->metricDataFormat        = tt_get_short(ttf);
    tp->numberOfHMetrics        = tt_get_ushort(ttf);
} /* tt_get_tab_hhea */

static void
tt_get_tab_hmtx(tt_file *ttf)
{
    static const char *fn = "tt_get_tab_hmtx";
    pdc_core *pdc = ttf->pdc;
    tt_tab_hmtx *tp = NULL;
    int         n_metrics;
    int         n_lsbs;
    int         i;

    tp = (tt_tab_hmtx *) tt_get_tab(ttf, fnt_str_hmtx, sizeof (tt_tab_hmtx),
                                    !ttf->fortet, NULL);
    if (tp == NULL)
        return;
    ttf->tab_hmtx = tp;

    TT_ASSERT(ttf, ttf->tab_hhea != 0);
    TT_ASSERT(ttf, ttf->tab_maxp != 0);

    tp->metrics = 0;
    tp->lsbs = 0;

    n_metrics = ttf->tab_hhea->numberOfHMetrics;
    n_lsbs = ttf->numGlyphs - n_metrics;

    TT_IOCHECK(ttf, n_metrics != 0);
    TT_IOCHECK(ttf, n_lsbs >= 0);
    tp->metrics = (tt_metric *)
        pdc_malloc(pdc, n_metrics * sizeof (tt_metric), fn);

    for (i = 0; i < n_metrics; ++i)
    {
        tp->metrics[i].advanceWidth = tt_get_fword(ttf);
        tp->metrics[i].lsb = tt_get_fword(ttf);
    }

    if (n_lsbs == 0)
	tp->lsbs = (tt_fword *) 0;
    else
    {
	tp->lsbs = (tt_fword *)
                        pdc_malloc(pdc, n_lsbs * sizeof (tt_fword), fn);
	for (i = 0; i < n_lsbs; ++i)
            tp->lsbs[i] = tt_get_fword(ttf);
    }
} /* tt_get_tab_hmtx */



pdc_bool
tt_get_tab_CFF_(tt_file *ttf)
{
    static const char *fn = "tt_get_tab_CFF_";
    pdc_core *pdc = ttf->pdc;
    int idx = tt_tag2idx(ttf, fnt_str_CFF_);

    if (idx != -1)
    {
        /* CFF table found */
        ttf->tab_CFF_ = (tt_tab_CFF_ *)
                            pdc_malloc(pdc, sizeof (tt_tab_CFF_), fn);
        ttf->tab_CFF_->offset = ttf->dir[idx].offset;
        ttf->tab_CFF_->length = ttf->dir[idx].length;
    }
    else if (!ttf->fortet)
    {
        idx = tt_tag2idx(ttf, fnt_str_glyf);
        if (idx == -1 || !ttf->dir[idx].length)
        {
            pdc_set_errmsg(pdc, FNT_E_TT_NOGLYFDESC, 0, 0, 0, 0);
	    return pdc_false;
        }
        idx = -1;
    }


    return pdc_true;

} /* tt_get_tab_CFF_ */

void
tt_get_tab_maxp(tt_file *ttf)
{
    tt_tab_maxp *tp = NULL;

    tp = (tt_tab_maxp *) tt_get_tab(ttf, fnt_str_maxp, sizeof (tt_tab_maxp),
                                    !ttf->fortet, NULL);
    if (tp == NULL)
        return;
    ttf->tab_maxp = tp;

    tp->version                 = tt_get_fixed(ttf);
    tp->numGlyphs               = tt_get_ushort(ttf);
    tp->maxPoints               = tt_get_ushort(ttf);
    tp->maxContours             = tt_get_ushort(ttf);
    tp->maxCompositePoints      = tt_get_ushort(ttf);
    tp->maxCompositeContours    = tt_get_ushort(ttf);
    tp->maxZones                = tt_get_ushort(ttf);
    tp->maxTwilightPoints       = tt_get_ushort(ttf);
    tp->maxStorage              = tt_get_ushort(ttf);
    tp->maxFunctionDefs         = tt_get_ushort(ttf);
    tp->maxInstructionDefs      = tt_get_ushort(ttf);
    tp->maxStackElements        = tt_get_ushort(ttf);
    tp->maxSizeOfInstructions   = tt_get_ushort(ttf);
    tp->maxComponentElements    = tt_get_ushort(ttf);
    tp->maxComponentDepth       = tt_get_ushort(ttf);

    ttf->numGlyphs = tp->numGlyphs;

} /* tt_get_tab_maxp */

pdc_bool
tt_get_tab_name(tt_file *ttf)
{
    static const char *fn = "tt_get_tab_name";
    pdc_core *pdc = ttf->pdc;
    pdc_bool logg5 = pdc_logg_is_enabled(pdc, 5, trc_font);
    tt_tab_name *tp = NULL;
    int		i, j, k, namid, irec, irec4 = -1, irec6 = -1;
    size_t      len;
    tt_nameref  *namerec = NULL, lastnamerec;
    char        *localname = NULL;
    tt_ulong	offset, offs;

    tp = (tt_tab_name  *) tt_get_tab(ttf, fnt_str_name, sizeof (tt_tab_name ),
                                     pdc_false, &offset);
    if (tp == NULL)
        return pdc_false;
    ttf->tab_name = tp;

    tp->namerecords = NULL;
    tp->englishname4 = NULL;
    tp->englishname6 = NULL;
    tp->producer = NULL;
    tp->format = tt_get_ushort(ttf);

    /* Format 0 is the only document one, but some Apple fonts use 65535.
     * This is very consequent since it follows Microsoft's lead in
     * disregarding one's own published specifications.
     */
    TT_IOCHECK(ttf, (tp->format == 0 || tp->format == 65535));

    tp->numNameRecords = (tt_ushort)
        tt_get_offset(ttf, sizeof(tt_ushort));
    tp->offsetStrings = tt_get_ushort(ttf);
    offs = offset + tp->offsetStrings;

    pdc_logg_cond(pdc, 1, trc_font,
        "\tRecords in name table of format %d: %d:\n",
        tp->format, tp->numNameRecords);

    if (ttf->utf16fontname != NULL)
    {
        pdc_logg_cond(pdc, 1, trc_font,
            "\tSearching for a host font with Unicode name \"%T\"\n",
            ttf->utf16fontname, ttf->fnamelen);
    }

    /* this was observed. we ignore it in TET */
    if (ttf->fortet && tp->numNameRecords == 0)
       return pdc_true;

    TT_IOCHECK(ttf, (tp->numNameRecords > 0));

    len = tp->numNameRecords * sizeof (tt_nameref);
    tp->namerecords = (tt_nameref *) pdc_malloc(pdc, len, fn);

    for (i = 0; i < tp->numNameRecords; ++i)
    {
        tt_ushort platformID    = tt_get_ushort(ttf);
        tt_ushort encodingID    = tt_get_ushort(ttf);
        tt_ushort languageID    = tt_get_ushort(ttf);
        tt_ushort nameID        = tt_get_ushort(ttf);
        tt_ushort stringLength  = tt_get_ushort(ttf);
        tt_ushort stringOffset  = tt_get_ushort(ttf);

        namerec = &tp->namerecords[i];
        namerec->platform = platformID;
        namerec->encoding = encodingID;
        namerec->language = languageID;
        namerec->namid = nameID;
        namerec->length = stringLength;
        namerec->offset = stringOffset;
    }

    namid = 4;
    for (k = 0; k < 2; k++)
    {
        lastnamerec.platform = 0;
        lastnamerec.language = 0;
        lastnamerec.namid = 0;
        lastnamerec.length = 0;
        lastnamerec.offset = 0;

        for (i = 0; i < tp->numNameRecords; ++i)
        {
            localname = NULL;
            namerec = &tp->namerecords[i];

            if (logg5 && !k)
            {
                pdc_logg(pdc, "\t\t\t%2d. platformID: %d\n"
                               "\t\t\t    encodingID: %d\n"
                               "\t\t\t    languageID: %d\n"
                               "\t\t\t    nameID:     %d\n"
                               "\t\t\t    length:     %d\n"
                               "\t\t\t    offset:     %d\n",
                        i,
                        namerec->platform, namerec->encoding,
                        namerec->language, namerec->namid,
                        namerec->length, namerec->offset);

                /* read font name */
                if (namerec->length)
                {
                    localname =
                        (char *) pdc_calloc(pdc, (size_t) namerec->length, fn);
                    tt_seek(ttf, (long) (offs + namerec->offset));
                    tt_read(ttf, localname, (unsigned int) namerec->length);

                    pdc_logg_hexdump(pdc, "data", "\t\t\t    ",
                                     localname, namerec->length);
                }
                pdc_logg(pdc, "\n");
            }

            if (tp->producer == NULL &&
                namerec->platform == tt_pfid_mac &&
                namerec->encoding == tt_wenc_symbol &&
                namerec->language == 0 &&
                namerec->namid == 0)
            {
                tp->producer = (char *) pdc_calloc(pdc,
                                              (size_t) namerec->length + 1, fn);
                tt_seek(ttf, (long) (offs + namerec->offset));
                tt_read(ttf, tp->producer, (unsigned int) namerec->length);
            }

            if (namerec->length && namerec->namid == namid)
            {
                /* TTC font search */
                if (ttf->utf16fontname != NULL)
                {
                    /* read font name */
                    if (localname == NULL)
                    {
                        localname = (char *) pdc_calloc(pdc,
                                                  (size_t) namerec->length, fn);
                        tt_seek(ttf, (long) (offs + namerec->offset));
                        tt_read(ttf, localname, (unsigned int) namerec->length);
                    }

                    if (namerec->platform == tt_pfid_win)
                    {
                        pdc_logg_cond(pdc, 1, trc_font,
                            "\tUnicode fontname: \"%T\"\n",
                            localname, namerec->length);

                        if (namerec->length == ttf->fnamelen &&
                            !memcmp(localname, ttf->utf16fontname,
                                    (size_t) ttf->fnamelen))
                        {
                             /* font found */
                             pdc_free(pdc, localname);
                             return pdc_true;
                        }
                    }
                }

                /* search for the records with english names */
                else
                {
                    /* we take the names from platformID 3 (Microsoft) or
                     * 1 (Macintosh). We favor Macintosh and then Microsoft
                     * with American English (LanguageID = 0x0409 = 1033)
                     */
                    if ((lastnamerec.language != 0x0409 ||
                         lastnamerec.platform != tt_pfid_win) &&
                        (namerec->platform == tt_pfid_win ||
                         (namerec->platform == tt_pfid_mac &&
                          namerec->language == 0)))
                    {
                        lastnamerec = *namerec;

                        /* We simulate always English */
                        if (namerec->platform == tt_pfid_mac)
                            lastnamerec.language = 0x0409;

                        if (namid == 4) irec4 = i;
                        if (namid == 6) irec6 = i;
                    }
                }
            }

            if (localname != NULL)
                pdc_free(pdc, localname);
        }
        namid = 6;
    }

    /* TTC font not found */
    if (ttf->utf16fontname != NULL)
        return pdc_false;

    /* English font names */
    namid = 4;
    irec = irec4;
    for (k = 0; k < 2; k++)
    {
        if (irec == -1)
            continue;
        namerec = &tp->namerecords[irec];

        /* read font name */
        len = (size_t) (namerec->length + 1);
        localname = (char *) pdc_calloc(pdc, (size_t) len, fn);
        tt_seek(ttf, (long) (offs + namerec->offset));
        tt_read(ttf, localname, (unsigned int) namerec->length);

        /* low byte picking */
        if (namerec->platform == tt_pfid_win)
	{
            for (j = 0; j < namerec->length / 2; j++)
            {
                /* We don't support wide character names */
                if (localname[2*j] != 0)
                {
                    pdc_free(pdc, localname);
                    localname = NULL;
                    break;
                }
                localname[j] = localname[2*j + 1];
            }
            if (localname)
                localname[j] = 0;
	}

        /* We observed this in EUDC fonts */
        if (localname && !strcmp(localname, "\060\060"))
        {
            pdc_free(pdc, localname);
            localname = NULL;
        }

        if (namid == 4 && localname)
            tp->englishname4 = localname;
        else if (namid == 6 && localname)
            tp->englishname6 = localname;

        namid = 6;
        irec = irec6;
    }

    /* font name 4 (full font name) is required */
    if (tp->englishname6 == NULL && tp->englishname4 == NULL)
    {
        if (!ttf->fortet)
        {
            pdc_set_errmsg(pdc, FNT_E_TT_NONAME, 0, 0, 0, 0);
            return pdc_false;
        }
    }
    else
    {
        if (tp->englishname4 == NULL)
            tp->englishname4 = pdc_strdup(pdc, tp->englishname6);
        if (tp->englishname6 == NULL)
            tp->englishname6 = pdc_strdup(pdc, tp->englishname4);
    }

    return pdc_true;
} /* tt_get_tab_name */

static int tt_cpflag2cp[64] =
{
    1252, 1250, 1251, 1253, 1254, 1255, 1256, 1257,
    1258,    0,    0,    0,    0,    0,    0,    0,
    874,   932,  936,  949,  950, 1361,    0,    0,
    0,       0,    0,    0,    0,    0,    0,    0,
    0,       0,    0,    0,    0,    0,    0,    0,
    0,       0,    0,    0,    0,    0,    0,    0,
    869,   866,  865,  864,  863,  862,  861,  860,
    857,   855,  852,  775,  737,  708,  850,  437
};

static int tt_cpflag2charcoll[4] =
{
    cc_japanese, cc_simplified_chinese, cc_korean, cc_traditional_chinese
};

void
tt_get_tab_OS_2(tt_file *ttf)
{
    pdc_bool logg3 = pdc_logg_is_enabled(ttf->pdc, 3, trc_font);
    pdc_bool logg5 = pdc_logg_is_enabled(ttf->pdc, 5, trc_font);
    int i, j;

    tt_tab_OS_2 *tp = NULL;

    tp = (tt_tab_OS_2 *) tt_get_tab(ttf, fnt_str_OS_2, sizeof (tt_tab_OS_2),
                                    pdc_false, NULL);
    if (tp == NULL)
        return;
    ttf->tab_OS_2 = tp;

    tp->version = tt_get_ushort(ttf);
    tp->xAvgCharWidth = tt_get_short(ttf);
    tp->usWeightClass = tt_get_ushort(ttf);
    tp->usWidthClass = tt_get_ushort(ttf);
    tp->fsType = tt_get_ushort(ttf);
    tp->ySubscriptXSize = tt_get_short(ttf);
    tp->ySubscriptYSize = tt_get_short(ttf);
    tp->ySubscriptXOffset = tt_get_short(ttf);
    tp->ySubscriptYOffset = tt_get_short(ttf);
    tp->ySuperscriptXSize = tt_get_short(ttf);
    tp->ySuperscriptYSize = tt_get_short(ttf);
    tp->ySuperscriptXOffset = tt_get_short(ttf);
    tp->ySuperscriptYOffset = tt_get_short(ttf);
    tp->yStrikeoutSize = tt_get_short(ttf);
    tp->yStrikeoutPosition = tt_get_short(ttf);
    tp->sFamilyClass = tt_get_ushort(ttf);

    tt_read(ttf, tp->panose, 10);

    tp->ulUnicodeRange1 = tt_get_ulong(ttf);
    tp->ulUnicodeRange2 = tt_get_ulong(ttf);
    tp->ulUnicodeRange3 = tt_get_ulong(ttf);
    tp->ulUnicodeRange4 = tt_get_ulong(ttf);

    tt_read(ttf, tp->achVendID, 4);

    tp->fsSelection = tt_get_ushort(ttf);
    tp->usFirstCharIndex = tt_get_ushort(ttf);
    tp->usLastCharIndex = tt_get_ushort(ttf);
    tp->sTypoAscender = tt_get_short(ttf);
    tp->sTypoDescender = tt_get_short(ttf);
    tp->sTypoLineGap = tt_get_short(ttf);
    tp->usWinAscent = tt_get_ushort(ttf);
    tp->usWinDescent = tt_get_ushort(ttf);

    if (tp->version >= 1)
    {
        tp->ulCodePageRange1 = tt_get_ulong(ttf);
        tp->ulCodePageRange2 = tt_get_ulong(ttf);
    }
    else
    {
	tp->ulCodePageRange1 = 0;
	tp->ulCodePageRange2 = 0;
    }

    for (i = 0; i < PDC_NUMCHARCOLL; i++)
    {
        j = i + 17;
        if (tp->ulCodePageRange1 & (1<<j) || (ttf->hasbig5cmap &&
            tt_cpflag2charcoll[i] == cc_traditional_chinese))
            tp->charcolls[i] = tt_cpflag2charcoll[i];
        else
            tp->charcolls[i] = cc_none;
    }

    if (tp->version >= 2)
    {
        tp->sxHeight = tt_get_short(ttf);
        tp->sCapHeight = tt_get_short(ttf);
        tp->usDefaultChar = tt_get_ushort(ttf);
        tp->usBreakChar = tt_get_ushort(ttf);
        tp->usMaxContext = tt_get_ushort(ttf);
    }
    else
    {
        tp->sxHeight = FNT_MISSING_FONTVAL;
        tp->sCapHeight = FNT_MISSING_FONTVAL;
	tp->usDefaultChar = 0;
	tp->usBreakChar = 0;
	tp->usMaxContext = 0;
    }

    if (logg5)
    {
        pdc_logg(ttf->pdc, "\t\t\tusFirstCharIndex=0x%04X\n",
                 ttf->tab_OS_2->usFirstCharIndex);
        if (ttf->tab_cmap && ttf->tab_cmap->win)
            pdc_logg(ttf->pdc, "\t\t\tstartCount[0]=0x%04X\n",
                     ttf->tab_cmap->win->startCount[0]);
    }

    /* there are fonts with inconsistent usFirstCharIndex */
    if (ttf->tab_cmap && ttf->tab_cmap->win &&
        tp->usFirstCharIndex != ttf->tab_cmap->win->startCount[0])
        ttf->tab_OS_2->usFirstCharIndex = ttf->tab_cmap->win->startCount[0];

    if (logg3)
    {
        int nbit = 8 * sizeof(tt_ulong);

        pdc_logg_bitarr(ttf->pdc, "\t\tulUnicodeRange1 ",
                        (char *) &tp->ulUnicodeRange1, nbit);
        pdc_logg_bitarr(ttf->pdc, "\t\tulUnicodeRange2 ",
                        (char *) &tp->ulUnicodeRange2, nbit);
        pdc_logg_bitarr(ttf->pdc, "\t\tulUnicodeRange3 ",
                        (char *) &tp->ulUnicodeRange3, nbit);
        pdc_logg_bitarr(ttf->pdc, "\t\tulUnicodeRange4 ",
                        (char *) &tp->ulUnicodeRange4, nbit);

        if (tp->version >= 1)
        {
            int n = 0;

            pdc_logg_bitarr(ttf->pdc, "\t\tulCodePageRange1",
                            (char *) &tp->ulCodePageRange1, nbit);
            pdc_logg_bitarr(ttf->pdc, "\t\tulCodePageRange2",
                            (char *) &tp->ulCodePageRange2, nbit);

            for (i = 0; i < 32; i++)
            {
                if ((tp->ulCodePageRange1 & (1<<i)) && tt_cpflag2cp[i])
                {
                    pdc_logg(ttf->pdc, "%s%d",
                             (n ? ", " : "\t\tsupported code pages: "),
                             tt_cpflag2cp[i]);
                    n++;
                }
            }
            for (i = 0; i < 32; i++)
            {
                j = i + 32;
                if ((tp->ulCodePageRange1 & (1<<i)) && tt_cpflag2cp[j])
                {
                    pdc_logg(ttf->pdc, "%s%d",
                             (n ? ", " : "\t\tsupported code pages: "),
                             tt_cpflag2cp[j]);
                    n++;
                }
            }

            if (n)
                pdc_logg(ttf->pdc, "\n");

            n = 0;
            for (i = 0; i < PDC_NUMCHARCOLL; i++)
            {
                if (tp->charcolls[i])
                {
                    pdc_logg(ttf->pdc, "%s%s",
                         (n ? ", " : "\t\tsupported character collections: "),
                         fnt_get_ordering_cid(tp->charcolls[i]));
                    n++;
                }
            }
            if (n)
                pdc_logg(ttf->pdc, "\n");
        }
    }
} /* tt_get_tab_OS_2 */


static void
tt_get_tab_post(tt_file *ttf)
{
    tt_tab_post *tp = NULL;


    tp = (tt_tab_post *) tt_get_tab(ttf, fnt_str_post, sizeof (tt_tab_post),
                                    !ttf->fortet, NULL);
    if (tp == NULL)
        return;
    ttf->tab_post = tp;

    tp->formatType = tt_get_fixed(ttf);
    tp->italicAngle = (tt_get_fixed(ttf) / 65536.0);
    tp->underlinePosition = tt_get_fword(ttf);
    tp->underlineThickness = tt_get_fword(ttf);
    tp->isFixedPitch = tt_get_ulong(ttf);
    tp->minMemType42 = tt_get_ulong(ttf);
    tp->maxMemType42 = tt_get_ulong(ttf);
    tp->minMemType1 = tt_get_ulong(ttf);
    tp->maxMemType1 = tt_get_ulong(ttf);
    tp->numberOfGlyphs = (tt_ushort) ttf->numGlyphs;

    /* there are subset fonts with different number of glyphs
     * see bug #1418
     */
    ttf->numGlyphs = MAX(tp->numberOfGlyphs, ttf->numGlyphs);

} /* tt_get_tab_post */



/*--------------------------- general functions ------------------------------*/

#define FNT_O        ((char) 0x4f)           /* ASCII 'O'  */
#define FNT_T        ((char) 0x54)           /* ASCII 'T'  */

#define FNT_t        ((char) 0x74)           /* ASCII 't'  */
#define FNT_r        ((char) 0x72)           /* ASCII 'r'  */
#define FNT_u        ((char) 0x75)           /* ASCII 'u'  */
#define FNT_e        ((char) 0x65)           /* ASCII 'e'  */

#define FNT_c        ((char) 0x63)           /* ASCII 'c'  */
#define FNT_f        ((char) 0x66)           /* ASCII 'f'  */

static const char *fnt_filetypes[4] =
{
    "TrueType", "OpenType", "Apple TrueType", "TrueType Collection"
};

pdc_bool
fnt_test_tt_font(pdc_core *pdc, tt_byte *img, tt_ulong *n_fonts,
                 pdc_bool requested)
{
    tt_ushort n_tables;
    int ift = 0;
    pdc_bool retval = requested ? pdc_false : pdc_undef;

    /* The TrueType (including OpenType/TT) "version" is always 0x00010000 */
    if (!(img[0] == 0x00 && img[1] == 0x01 &&
          img[2] == 0x00 && img[3] == 0x00))
    {
        ift++;

        /* The OpenType/CFF version is always 'OTTO' */
        if (!(img[0] == FNT_O && img[1] == FNT_T &&
              img[2] == FNT_T && img[3] == FNT_O))
        {
            ift++;

            /* Old Apple fonts have 'true' */
            if (!(img[0] == FNT_t && img[1] == FNT_r &&
                  img[2] == FNT_u && img[3] == FNT_e))
            {
                ift++;

                /* TrueType Collection */
                if (n_fonts == NULL ||
                    !(img[0] == FNT_t && img[1] == FNT_t &&
                      img[2] == FNT_c && img[3] == FNT_f))
                    return retval;

                /* Version is always 0x00010000 or 0x00020000  */
                if (!(img[4] == 0x00 && (img[5] == 0x01 || img[5] == 0x02) &&
                      img[6] == 0x00 && img[7] == 0x00))
                    return retval;

                /* Number of fonts */
                *n_fonts = pdc_get_be_ulong(&img[8]);

                pdc_logg_cond(pdc, 1, trc_font,
                    "\t%s font with %d single fonts detected\n",
                    fnt_filetypes[ift], *n_fonts);

                return pdc_true;
            }
        }
    }

    /* Number of tables */
    n_tables = pdc_get_be_ushort(&img[4]);
    if (n_tables < 8 && n_tables > 64)  /* max. 32 tables ? */
        return retval;

    /* Further check of the next 6 bytes not implemented */

    if (n_fonts == NULL)
        pdc_logg_cond(pdc, 1, trc_font,
            "\t%s font with %d tables detected\n",
            fnt_filetypes[ift], n_tables);

    return pdc_true;
}


pdc_bool
fnt_is_opentype_font(tt_file *ttf)
{
    return (ttf->img[0] == FNT_O &&
            ttf->img[1] == FNT_T &&
            ttf->img[2] == FNT_T &&
            ttf->img[3] == FNT_O) ? pdc_true : pdc_false;
}

pdc_bool
fnt_read_offset_tab(tt_file *ttf)
{
    static const char *fn = "fnt_get_tab_offset";
    pdc_core *pdc = ttf->pdc;
    tt_byte img[TT_OFFSETTAB_SIZE];
    int i;

    /* Check */
    tt_read(ttf, img, TT_OFFSETTAB_SIZE);
    if (fnt_test_tt_font(pdc, img, NULL, pdc_true) == pdc_false)
    {
        pdc_set_errmsg(pdc, FNT_E_TT_NOFONT, ttf->filename, 0, 0, 0);
	return pdc_false;
    }

    /* number of table directories */
    ttf->n_tables = pdc_get_be_ushort(&img[4]);

    /* set up table directory */
    ttf->dir = (tt_dirent *) pdc_malloc(pdc,
            (size_t) (ttf->n_tables * sizeof (tt_dirent)), fn);

    tt_seek(ttf, (long) (ttf->offset + TT_OFFSETTAB_SIZE));

    for (i = 0; i < ttf->n_tables; ++i)
    {
        tt_get_dirent(ttf->dir + i, ttf);
    }

    /* make sure this isn't a bitmap-only Mac font */
    if (tt_tag2idx(ttf, fnt_str_bhed) != -1)
    {
        pdc_set_errmsg(pdc, FNT_E_TT_BITMAP, 0, 0, 0, 0);
	return pdc_false;
    }

    return pdc_true;

} /* fnt_read_offset_tab */

pdc_bool
fnt_read_tt(tt_file *ttf)
{
    pdc_core *pdc = ttf->pdc;

    PDC_TRY(pdc)
    {
        if (fnt_read_offset_tab(ttf) == pdc_false)
        {
            PDC_EXIT_TRY(pdc);
            return pdc_false;
        }

        /* These are all required TrueType tables;
         * optional tables are not read.
         */
        tt_get_tab_cmap(ttf);
        tt_get_tab_head(ttf);
        tt_get_tab_hhea(ttf);
        tt_get_tab_maxp(ttf);
        if (!ttf->fortet)
            tt_get_tab_hmtx(ttf);  /* MUST be read AFTER hhea & maxp! */
        if (tt_get_tab_name(ttf) == pdc_false && !ttf->fortet)
        {
            PDC_EXIT_TRY(pdc);
            return pdc_false;
        }
        tt_get_tab_post(ttf);
        tt_get_tab_OS_2(ttf);      /* may be missing from some Apple fonts */

        /* this is an optional table, present only in OpenType fonts */
        if (tt_get_tab_CFF_(ttf) == pdc_false && !ttf->fortet)
        {
            PDC_EXIT_TRY(pdc);
            return pdc_false;
        }


        PDC_EXIT_TRY(pdc);
        return pdc_true;
    }
    PDC_CATCH(pdc)
    {
    }

    return pdc_false;
} /* fnt_read_tt */



/* convert Unicode scalar to glyph ID using cmap12 or cmap4.
 */
int
tt_unicode2gidx(tt_file *ttf, int usv, pdc_bool logg)
{
    pdc_core *pdc = ttf->pdc;
    tt_cmap4 *cm4 = ttf->tab_cmap->win;
    pdc_ushort uv;
    int lo, hi, segs;
    int gidx = 0, i = 0;

    uv = (pdc_ushort) usv;
    if (logg) pdc_logg(pdc, "\t\t\tUCS2: %04X: ", uv);
    segs = cm4->segCountX2 / 2;

    lo = 0;
    hi = segs;
    while (lo < hi)
    {
        i = (lo + hi) / 2;

        if (uv <= cm4->endCount[i])
        {
            if (uv >= cm4->startCount[i])
            {
                break;
            }
            else if (!i || uv > cm4->endCount[i - 1])
            {
                i = -1;
                break;
            }
        }

        if (uv < cm4->startCount[i])
            hi = i;
        else
            lo = i + 1;
    }


    if (logg) pdc_logg(pdc, "i=%d start=UCS2: %04X  ", i, cm4->startCount[i]);

    TT_IOCHECK(ttf, i != segs);
    if (i == -1 || uv == 0xFFFF)
    {
        if (logg) pdc_logg(pdc, "==> gidx=0\n");
        return 0;
    }

    if (logg) pdc_logg(pdc, "offs=%d  ", cm4->idRangeOffs[i]);

    if (cm4->idRangeOffs[i] == 0)
    {
        if (logg) pdc_logg(pdc, "delta=%d  ", cm4->idDelta[i]);
        gidx = (int)((uv + cm4->idDelta[i]) & 0xFFFF);
    }
    else
    {
        int idx = (int) cm4->idRangeOffs[i] / 2
                       + (int) (uv - cm4->startCount[i]) - (segs - i);

        if (idx < 0 || idx >= cm4->numGlyphIds)
        {
            pdc_warning(pdc, FNT_E_TT_GLYPHIDNOTFOUND,
                        pdc_errprintf(pdc, "%04X", uv),
                        0, 0, 0);
            return 0;
        }

        if (logg) pdc_logg(pdc, "array[%d]=%d  ", idx, gidx);
        if (cm4->glyphIdArray[idx] == 0)
        {
            if (logg) pdc_logg(pdc, "==> gidx=0\n");
            return 0;
        }
        else
        {
            if (logg) pdc_logg(pdc, "delta=%d  ", cm4->idDelta[i]);
            gidx = (int)((cm4->glyphIdArray[idx] + cm4->idDelta[i]) & 0xFFFF);
        }
    }

    if (logg) pdc_logg(pdc, "gidx=%d  ", gidx);

    /* this is necessary because of some Mac fonts (e.g. Hiragino) */
    if (gidx >= ttf->numGlyphs)
    {
        gidx = 0;
        if (logg) pdc_logg(pdc, "==> gidx=0\n");
    }
    else if (logg)
        pdc_logg(pdc, "\n");

    return gidx;
}

int
tt_gidx2width(tt_file *ttf, int gidx)
{
    TT_ASSERT(ttf, ttf->tab_hmtx != (tt_tab_hmtx *) 0);
    TT_ASSERT(ttf, ttf->tab_hhea != (tt_tab_hhea *) 0);

    {
        int n_metrics = ttf->tab_hhea->numberOfHMetrics;

        if (gidx >= n_metrics)
            gidx = n_metrics - 1;
        if (ttf->monospace)
            return ttf->monospace;
        else
        {
            return FNT_TT2PDF(ttf->tab_hmtx->metrics[gidx].advanceWidth);
        }
    }
} /* tt_gidx2width */

void
fnt_set_tt_fontvalues(tt_file *ttf)
{
    pdc_bool logg3 = pdc_logg_is_enabled(ttf->pdc, 3, trc_font);
    fnt_font *font = ttf->font;
    fnt_font_metric *ftm = &font->m;

    if (ttf->onlyCFF)
        return;

    if (logg3)
        pdc_logg(ttf->pdc, "\tUnits per EM: %d\n", ttf->tab_head->unitsPerEm);

    if (ttf->tab_head)
    {
        ftm->llx = FNT_TT2PDF(ttf->tab_head->xMin);
        ftm->lly = FNT_TT2PDF(ttf->tab_head->yMin);
        ftm->urx = FNT_TT2PDF(ttf->tab_head->xMax);
        ftm->ury = FNT_TT2PDF(ttf->tab_head->yMax);
    }

    if (ttf->tab_post)
    {
        ftm->italicAngle = ttf->tab_post->italicAngle;
        ftm->isFixedPitch = (pdc_bool) ttf->tab_post->isFixedPitch;
        ftm->underlinePosition = FNT_TT2PDF(ttf->tab_post->underlinePosition);
        ftm->underlineThickness =FNT_TT2PDF(ttf->tab_post->underlineThickness);
    }

    if (ttf->tab_OS_2)
    {

        font->weight = fnt_check_weight(ttf->tab_OS_2->usWeightClass);
        ftm->ascender = FNT_TT2PDF(ttf->tab_OS_2->sTypoAscender);
        ftm->descender = FNT_TT2PDF(ttf->tab_OS_2->sTypoDescender);

        if (ttf->tab_OS_2->sCapHeight != FNT_MISSING_FONTVAL)
            ftm->capHeight = FNT_TT2PDF(ttf->tab_OS_2->sCapHeight);
        if (ttf->tab_OS_2->sxHeight != FNT_MISSING_FONTVAL)
            ftm->xHeight = FNT_TT2PDF(ttf->tab_OS_2->sxHeight);
        font->linegap = FNT_TT2PDF(ttf->tab_OS_2->sTypoLineGap);
    }

    /* some fonts have no OS/2 table and
     * some Apple fonts have zero values in the OS/2 table .
     */
    if (ttf->tab_OS_2 == NULL ||
        (ttf->tab_OS_2->usWeightClass == 0 &&
         ttf->tab_OS_2->sTypoAscender == 0 &&
         ttf->tab_OS_2->sTypoDescender == 0 &&
         ttf->tab_OS_2->sTypoLineGap == 0))
    {
        font->weight = fnt_macfontstyle2weight(ttf->tab_head->macStyle);
        ftm->ascender = FNT_TT2PDF(ttf->tab_hhea->ascender);
        ftm->descender = FNT_TT2PDF(ttf->tab_hhea->descender);
        font->linegap = FNT_TT2PDF(ttf->tab_hhea->lineGap);
    }

    /* default width */
    if (!ttf->fortet)
    {
        ftm->defwidth = tt_gidx2width(ttf, 0);
    }
}


/*
 *  Create and fill some arrays in font structure.
 *
 *  Before calling function the members 'encoding' and 'ev'
 *  of font struct have to been set:
 *
 *  The desired arrays must be requested by following flags
 *  (enc == font->enc):
 *
 *  TT_FONT_encvec:     Encoding vector (enc == pdc_builtin)
 *
 *  TT_FONT_gid2code:   font->gid2code[font->numglyphs]:
 *                      glyph ID -> 8-bit code  (enc >= 0, == pdc_builtin)
 *                      glyph ID -> Unicode  otherwise.
 *
 *  TT_FONT_code2gid:   font->code2gid[font->numcodes]:
 *                      8-bit code -> glyph ID (enc >= 0, == pdc_builtin)
 *                      Unicode -> glyph ID (enc == pdc_unicode)
 *                      glyph ID -> glyph ID (enc == pdc_glyphid)
 *
 *  TT_FONT_gid2name    font->gdi2name[font->numglyphs]:
 *                      glyph ID -> glyph name (enc == pdc_glyphid only)
 *
 *  TT_FONT_name2unitab font->name2unitab[font->tabsize]:
 *                      glyph name -> Unicode for unregistered names
 *
 *  TT_FONT_m_ciw       font->m.ciw[font->m.numinters] (Interval table)
 *                      Unicode -> glyph width (enc == pdc_unicode only)
 *
 *  TT_FONT_m_widths    font->m.widths[font->numcodes]
 *                      8-bit code -> glyph width (enc >= 0, == pdc_builtin)
 *                      glyph ID -> glyph width (enc == pdc_glyphid)
 *
 *  TT_FONT_names       font->m.name = englishname4
 *                      font->name = englishname6
 */
int
fnt_set_tt_fontarrays(tt_file *ttf, int flags)
{
    static const char *fn = "pdc_set_tt_fontarrays";
    pdc_core *pdc = ttf->pdc;
    fnt_font *font = ttf->font;
    pdc_bool logg2 = pdc_logg_is_enabled(pdc, 2, trc_font);
    pdc_bool logg5 = pdc_logg_is_enabled(pdc, 5, trc_font);
    pdc_bool logg7 = pdc_logg_is_enabled(pdc, 7, trc_font);
    pdc_encoding enc = font->enc, encb = pdc_invalidenc;
    pdc_encodingvector *ev = NULL, *evb = NULL;
    const char *glyphname;
    pdc_bool regorder, isencbyte = pdc_false;
    pdc_ushort uc, uv;
    int ncodes = 0, gidx = 0, width = 0, foundglyphs = 0, uvoffset = 0;
    int ucoffset = 0;
    int code;

    /* Unicode offset for symbol fonts */
    if (ttf->issymbol == pdc_true)
    {
        if (ttf->tab_OS_2)
        {
            /* e.g. WingDings has usFirstChar = 0xF020, but we must start
            ** at 0xF000. I haven't seen non-symbol fonts with a usFirstChar
            ** like that; perhaps we have to apply similar tricks then...
            */
            uvoffset = (ttf->tab_OS_2->usFirstCharIndex & 0xFF00);

            if (logg5)
                pdc_logg(pdc, "\t\tuvoffset=0x%04X\n", uvoffset);
        }
        else
        {
            /* This would be an Apple font with an encoding different
             * from macroman or a subset font withot OS_2 table.
             */
            if (!ttf->fortet)
            {
                pdc_set_errmsg(pdc, FNT_E_TT_SYMBOLOS2, 0, 0, 0, 0);
                return -1;
            }
            uvoffset = 0xF000;
        }

        if (logg7)
            pdc_logg(pdc, "\t\t\tuvoffset: U+%04X\n", uvoffset);
    }

    /* font names */
    if (flags & TT_FONT_names && ttf->tab_name)
    {
        font->m.name = pdc_strdup(pdc, ttf->tab_name->englishname4);
        font->name = pdc_strdup(pdc, ttf->tab_name->englishname6);
    }

    /* is Standard Latin font */
    font->issymbfont = ttf->issymbol;

    /* number of glyphs */
    font->numglyphs = ttf->numGlyphs;

    /* number of codes */
    switch(enc)
    {
        case pdc_cid:
        case pdc_unicode:
        font->numcodes = ttf->numunicode;
        break;

        case pdc_glyphid:
        font->numcodes = font->numglyphs;
        break;

        default:
        font->numcodes = 256;
        ev = pdc_get_encoding_vector(pdc, enc);
        isencbyte = pdc_true;
        break;
    }


    if (enc < 0 && ttf->hasonlymac)
    {
        encb = pdc_macroman;
        evb = pdc_get_encoding_vector(pdc, encb);
    }
    else
    {
        encb = enc;
        evb = ev;

        if (flags & TT_FONT_encvec && enc == pdc_builtin)
        {
            evb = fnt_create_font_ev(pdc, font);
            ev = evb;
        }
    }

    if (flags & TT_FONT_code2gid)
    {
        if (ttf->numunicode <= PDC_NUM_BMPVAL || isencbyte ||
            enc == pdc_glyphid)
        {
            font->code2gid = (pdc_ushort *) pdc_calloc(pdc,
                                   font->numcodes * sizeof (pdc_ushort), fn);
        }
    }

    if ((flags & TT_FONT_gid2code) || logg2)
    {
        if (ttf->numunicode <= PDC_NUM_BMPVAL || isencbyte)
        {
            font->gid2code = (pdc_ushort *) pdc_calloc(pdc,
                                   font->numglyphs * sizeof (pdc_ushort), fn);
        }
    }


    if (flags & TT_FONT_m_widths)
    {
        font->m.numwidths = font->numcodes;
        font->m.widths = (int *) pdc_calloc(pdc,
                               font->m.numwidths * sizeof(int), fn);
    }


    /*
     * Build the char width tables for the font, set mappings GID <-> Code,
     * and count the characters.
     */
    foundglyphs = 0;
    regorder = pdc_true;
    ncodes = (enc == pdc_glyphid) ? ttf->numunicode : font->numcodes;
    for (code = 0; code < ncodes; code++)
    {
        uc = (pdc_ushort) code;
        uv = 0;
        gidx = 0;


        if (encb == pdc_macroman && ttf->tab_cmap->mac)
        {
            tt_cmap0_6 *cm = ttf->tab_cmap->mac;

            if (code > -1 && code < (int) (cm->firstCode + cm->entryCount))
                gidx = cm->glyphIdArray[code];
        }
        else if (ttf->issymbol == pdc_true)
        {
            switch (encb)
            {
                case pdc_unicode:
                if (!ttf->fortet)
                {
                    if (code < 0x00FF)
                    {
                        /* we map the lower Unicode values too */
                        if (uvoffset > 0x00FF)
                            regorder = pdc_false;
                        uv = (pdc_ushort) (code + uvoffset);
                        break;
                    }
                    regorder = pdc_true;
                }

                case pdc_glyphid:
                uv = (pdc_ushort) code;
                break;

                default:
                {
                    uv = (pdc_ushort) (code + uvoffset);
                }
                if (evb != NULL)
                    evb->codes[code] = uv;
                break;
            }

            if (!gidx)
                gidx = tt_unicode2gidx(ttf, (int) uv, logg7);
        }
        else
        {
            uv = evb->codes[code];
            if (uv)
            {
                    gidx = tt_unicode2gidx(ttf, (int) uv, logg7);
            }
        }

        /*
         * Mapping GID -> [Uni]code
         * This is a 1:n relation (e.g. 1 -> SPACE, 1 -> NBSP)
         * We prefer the first occurence (SPACE) (regorder),
         * in the case of TT symbol fonts the second occurence.
         */
        if (gidx && regorder && uc >= ucoffset)
        {
            /* mapping gid -> code */
            if (font->gid2code)
            {
                if (!font->gid2code[gidx])
                {
                    font->gid2code[gidx] = uc;
                    if (logg5)
                        pdc_logg(pdc, "\t\tGID: %d -> U+%04X\n",
                                 gidx, font->gid2code[gidx]);
                }
                else if (logg2)
                {
                    pdc_logg(pdc, "\t\tGID: %d: U+%04X vs. U+%04X\n",
                             gidx, font->gid2code[gidx], uc);
                }
            }
            foundglyphs++;

        }

        switch (enc)
        {

            default:
            if (font->m.numwidths)
                font->m.widths[code] = tt_gidx2width(ttf, gidx);
            break;
        }

        /* mapping code -> gid */
        if (font->code2gid)
        {
            font->code2gid[code] = (pdc_ushort) gidx;
            if (logg5 && gidx)
                pdc_logg(pdc, "\t\tU+%04X -> GID: %d\n",
                         code, font->code2gid[code]);
        }
    }


    /* logging protocol and/or to check the completeness
     * of the glyph names
     */
    if (logg2
        )
    {
        if (logg2)
            pdc_logg(pdc,
                     "\n\t\tGlyph mapping for %d glyphs:\n", ttf->numGlyphs);

        width = -1;
        for (gidx = 0; gidx < ttf->numGlyphs; gidx++)
        {
            pdc_bool fontspecific = pdc_false;
            glyphname = NULL;


            code = fnt_get_code(gidx, font);
            if (!ttf->fortet)
                width = tt_gidx2width(ttf, gidx);

            if (code >= 0 && glyphname == NULL)
            {
                if (enc >= 0 || (ttf->issymbol && ev != NULL))
                    glyphname = ev->chars[code];
                else if (enc != pdc_builtin && code <= 0xFFFF)
                    glyphname = (char *) pdc_unicode2glyphname(pdc,
                                                        (pdc_ushort) code);
            }

            pdc_logg(pdc, "\t\tGID%5d: ", gidx);
            if (!ttf->fortet)
                pdc_logg(pdc, "width=%4d  ", width);

            switch (enc)
            {

                default:
                if (!gidx || code > 0)
                {
                    if (enc >= 0 || (ttf->issymbol && ev != NULL))
                    {
                        uv = ev->codes[code];
                        pdc_logg(pdc, "code=%3d  U+%04X ", code, uv);
                    }
                    else
                    {
                        if (ttf->fortet && enc == pdc_builtin)
                            pdc_logg(pdc, "U+%04X  ", code);
                        else
                            pdc_logg(pdc, "code=%3d  ", code);
                    }
                }
                break;
            }
            if (glyphname != NULL)
                pdc_logg(pdc, "\"%s\"", glyphname);
            if (fontspecific)
                pdc_logg(pdc, " (specific)");
            pdc_logg(pdc, "\n");
        }
    }

    if (!(flags & TT_FONT_gid2code))
    {
        if (ttf->numunicode <= PDC_NUM_BMPVAL && font->gid2code != NULL)
        {
            pdc_free(pdc, font->gid2code);
            font->gid2code = NULL;
        }
    }

    return foundglyphs;
}

pdc_encoding
fnt_get_tt_encoding_key(tt_file *ttf, pdc_encoding inenc)
{
    pdc_encoding outenc = inenc;

    /* Symbol font */
    if (ttf->issymbol && inenc >= pdc_winansi)
        outenc = pdc_builtin;

    /* MacRoman font */
    if (ttf->hasonlymac && inenc >= pdc_builtin)
        outenc = pdc_macroman;

    if (!ttf->issymbol && !ttf->haswinuni && !ttf->hasonlymac)
    {
        outenc = pdc_invalidenc;
        pdc_logg_cond(ttf->pdc, 1, trc_font,
            "\tTrueType font contains %s cmap table\n",
             ttf->tab_cmap ? "unsupported" : "no" );
    }
    else
    {
        pdc_logg_cond(ttf->pdc, 1, trc_font,
                "\tEncoding \"%s\" will be determined\n",
                pdc_get_user_encoding(ttf->pdc, outenc));
    }

    return outenc;
}

static pdc_bool
fnt_check_and_read_ttc(pdc_core *pdc, pdc_file *fp,
                       const char *filename, const char *fontname,
                       fnt_font *font, tt_ulong n_fonts)
{
    static const char *fn = "fnt_check_and_read_ttc";
    const char *sf;
    tt_file *ttf;
    pdc_byte *utf16fontname = NULL;
    pdc_bool retval = pdc_false;
    pdc_text_format textformat = PDC_UTF8;
    pdc_text_format targettextformat = pdc_utf16be;
    int i, inlen, outlen = 0;
    int ift = -1;

    /* initialize */
    ttf = fnt_new_tt(pdc, font);
    ttf->filename = filename;
    ttf->fontname = fontname;
    ttf->check = pdc_true;
    ttf->fp = fp;
    ttf->verbose = pdc_false;

    /* searching for font index in font name */
    sf = strrchr(fontname, ':');
    if (sf != NULL)
    {
        sf++;
        if (!*sf)
            ift = 0;
        else if (pdc_str2integer(sf, PDC_INT_UNSIGNED, &i))
            ift = i;
    }

    /* create UTF-16-BE font name string for searching in font file */
    if (ift == -1)
    {
        inlen = (int) strlen(font->utf8name);
        if (pdc_convert_string(pdc, textformat, 0, NULL,
                               (pdc_byte *) font->utf8name, inlen,
                               &targettextformat, NULL,
                               &utf16fontname, &outlen,
                               PDC_CONV_NOBOM | PDC_CONV_INFLATE, ttf->verbose))
        {
            goto FNT_TRUETYPE_EXIT;
        }
    }

    /* search font */
    for (i = 0; i < (int)n_fonts; ++i)
    {
        if (i) fnt_delete_tt(ttf);

        tt_seek(ttf, (long) (TT_OFFSETTAB_SIZE + i * sizeof(tt_ulong)));
        ttf->offset = tt_get_ulong(ttf);
        tt_seek(ttf, (long) ttf->offset);

        pdc_logg_cond(pdc, 1, trc_font, "\tChecking font #%d \n", i+1);

        /* Offset Table */
        if (!fnt_read_offset_tab(ttf))
            goto FNT_TRUETYPE_EXIT;

        /* font name match in Naming Table */
        if (ift > -1)
        {
            if (ift == i)
                break;
        }
        else
        {
            /* font name match in Naming Table */
            if (utf16fontname == NULL)
                break;
            ttf->utf16fontname = (char *) utf16fontname;
            ttf->fnamelen = outlen;
            if (tt_get_tab_name(ttf))
                break;
        }
    }
    if (utf16fontname != NULL)
        pdc_free(pdc, utf16fontname);

    /* font found */
    if (i < (int)n_fonts)
    {
        tt_byte *pos;
        tt_ulong headlen, dirlen, tablen, length, offset;

        /* create file in memory */
        tablen = 0;
        dirlen = 4 * sizeof(tt_ulong);
        headlen = (tt_ulong) (TT_OFFSETTAB_SIZE + ttf->n_tables * dirlen);
        font->filelen = headlen;
        for (i = 0; i < ttf->n_tables; i++)
        {
            length = ttf->dir[i].length;
            if (length > tablen) tablen = length;
            font->filelen += length;
        }
        font->img = (pdc_byte *) pdc_malloc(pdc, font->filelen, fn);

        /* read font file */
        tt_seek( ttf, (long) ttf->offset);
        tt_read( ttf, font->img, headlen);
        pos = font->img + headlen;
        for (i = 0; i < ttf->n_tables; i++)
        {
            length = ttf->dir[i].length;
            tt_seek( ttf, (long) ttf->dir[i].offset);
            tt_read( ttf, pos, length);
            ttf->dir[i].offset = (unsigned int) (pos - font->img);
            pos += length;
        }

        /* correct offsets in Table Directory */
        pos = font->img + TT_OFFSETTAB_SIZE + 2 * sizeof(tt_ulong);
        for (i = 0; i < ttf->n_tables; i++)
        {
            offset = ttf->dir[i].offset;
            pos[0] = (tt_byte) (offset >> 24);
            pos[1] = (tt_byte) (offset >> 16);
            pos[2] = (tt_byte) (offset >> 8);
            pos[3] = (tt_byte) (offset);
            pos += dirlen;
        }
        retval = pdc_true;
    }
    else
    {
        pdc_set_errmsg(pdc, FNT_E_TTC_NOTFOUND, filename, 0, 0, 0);
        goto FNT_TRUETYPE_EXIT;
    }

    FNT_TRUETYPE_EXIT:

    ttf->check = pdc_false;
    fnt_delete_tt(ttf);

    return retval;
}

pdc_bool
fnt_check_tt_font(pdc_core *pdc, const char *filename, const char *fontname,
                  fnt_font *font, pdc_bool requested)
{
    pdc_file   *fp;
    char        fullname[PDC_FILENAMELEN];
    tt_byte     img[TT_OFFSETTAB_SIZE];
    pdc_bool    ismem = pdc_false;
    tt_ulong    n_fonts = 0;
    int         retval = requested ? pdc_false : pdc_undef;

    fp = pdc_fsearch_fopen(pdc, filename, fullname, "font ",PDC_FILE_BINARY);
    if (fp != NULL)
    {
        if (PDC_OK_FREAD(fp, img, TT_OFFSETTAB_SIZE))
        {
            pdc_logg_cond(pdc, 1, trc_font,
                "\tLoading TrueType fontfile \"%s\":\n", fullname);

            retval = fnt_test_tt_font(pdc, img, &n_fonts, requested);
            if (retval == pdc_true)
            {
                ismem = pdc_file_isvirtual(fp);

                if (fontname != NULL)
                {
                    if (n_fonts > 1)
                    {
                        retval = fnt_check_and_read_ttc(pdc, fp, filename,
                                                   fontname, font, n_fonts);
                        fp = NULL;
                    }
                    else
                    {
                        font->img = (pdc_byte *)
                                       pdc_freadall(fp, &font->filelen, NULL);
                    }

                    if (retval == pdc_true)
                    {
                        if (font->filelen == 0)
                        {
                            pdc_set_errmsg(pdc,
                                PDC_E_IO_READ, fullname, 0, 0, 0);
                            retval = pdc_false;
                        }
                    }
                }

                if (retval == pdc_true)
                {
                    if (fp != NULL && ismem)
                    {
                        font->imgname = pdc_strdup(pdc, filename);
                        pdc_lock_pvf(pdc, font->imgname);
                    }

                    font->filename = pdc_strdup(pdc, fullname);
                }
            }
        }

        if (fp != NULL)
            pdc_fclose(fp);
    }
    else
    {
        retval = pdc_check_fopen_errmsg(pdc, requested);
    }

    return retval;
}


/*
 * After fnt_new_tt initialize following members
 * (here with the opposite of default):
 *
 * ttf->filename = filename;
 * ttf->fontname = fontname;
 *
 * ttf->verbose = pdc_false;
 * ttf->kerning = pdc_true;
 * ttf->vertical = pdc_true;
 * ttf->ignorename = pdc_true;
 * ttf->cmst = ~NULL;
 * ttf->fortet = pdc_true;
 *
 * ttf->check = pdc_true;
 * ttf->incore = pdc_true;
 * ttf->savecff = pdc_true;
 * ttf->monospace = 1000;
 *
 * ttf->fp = fp;
 *
 */
tt_file *
fnt_new_tt(pdc_core *pdc, fnt_font *font)
{
    static const char *fn = "fnt_new_tt";

    tt_file *ttf = (tt_file *)
                pdc_malloc(pdc, (size_t) sizeof (tt_file), fn);

    ttf->pdc = pdc;
    ttf->font = font;

    ttf->img = (tt_byte *) font->img;
    ttf->pos = ttf->img;
    ttf->end = ttf->img + font->filelen;

    ttf->filename = NULL;
    ttf->fontname = NULL;
    ttf->verbose = pdc_true;
    ttf->fortet = pdc_false;
    ttf->check = pdc_false;
    ttf->incore = pdc_false;
    ttf->savecff = pdc_false;
    ttf->monospace = 0;
    ttf->fp = NULL;

    ttf->n_tables = 0;
    ttf->offset = 0;
    ttf->dir = (tt_dirent *) 0;

    ttf->tab_cmap = (tt_tab_cmap *) 0;
    ttf->tab_head = (tt_tab_head *) 0;
    ttf->tab_hhea = (tt_tab_hhea *) 0;
    ttf->tab_hmtx = (tt_tab_hmtx *) 0;
    ttf->tab_maxp = (tt_tab_maxp *) 0;
    ttf->tab_name = (tt_tab_name *) 0;
    ttf->tab_post = (tt_tab_post *) 0;
    ttf->tab_OS_2 = (tt_tab_OS_2 *) 0;
    ttf->tab_CFF_ = (tt_tab_CFF_ *) 0;

    ttf->numGlyphs = 0;
    ttf->onlyCFF = 0;
    ttf->hasGlyphNames = 0;
    ttf->numunicode = PDC_NUM_BMPVAL;
    ttf->builtinenc = pdc_stdenc;
    ttf->regisadobe = pdc_false;
    ttf->charcoll = cc_none;
    ttf->supplement = 0;

    ttf->issymbol = pdc_false;
    ttf->haswinuni = pdc_false;
    ttf->hasonlymac = pdc_false;
    ttf->hasbig5cmap = pdc_false;
    ttf->forcesubset = pdc_false;
    ttf->gidunequcid = pdc_false;


    ttf->utf16fontname = (char *) 0;
    ttf->fnamelen = 0;

    return ttf;

} /* fnt_new_tt */

void
fnt_delete_tt(tt_file *ttf)
{
    pdc_core *pdc = ttf->pdc;

    if (ttf->check == pdc_false && ttf->fp != (pdc_file *) 0)
        pdc_fclose(ttf->fp);

    if (ttf->dir != (tt_dirent *) 0)
        pdc_free(pdc, ttf->dir);
    ttf->dir = (tt_dirent *) 0;


    if (ttf->tab_head != (tt_tab_head *) 0)
        pdc_free(pdc, ttf->tab_head);
    if (ttf->tab_hhea != (tt_tab_hhea *) 0)
        pdc_free(pdc, ttf->tab_hhea);
    if (ttf->tab_maxp != (tt_tab_maxp *) 0)
        pdc_free(pdc, ttf->tab_maxp);
    if (ttf->tab_OS_2 != (tt_tab_OS_2 *) 0)
        pdc_free(pdc, ttf->tab_OS_2);
    if (ttf->tab_CFF_ != (tt_tab_CFF_ *) 0)
        pdc_free(pdc, ttf->tab_CFF_);
    if (ttf->tab_post != (tt_tab_post *) 0)
    {
        pdc_free(pdc, ttf->tab_post);
    }

    if (ttf->tab_cmap != (tt_tab_cmap *) 0)
    {
        if (ttf->tab_cmap->mac != (tt_cmap0_6 *) 0)
        {
            if (ttf->tab_cmap->mac->glyphIdArray)
                pdc_free(pdc, ttf->tab_cmap->mac->glyphIdArray);
            pdc_free(pdc, ttf->tab_cmap->mac);
        }

        tt_cleanup_cmap4(ttf, ttf->tab_cmap->win);

        if (ttf->tab_cmap->ucs4 != (tt_cmap12 *) 0)
        {
            tt_cmap12 *cm12 = (tt_cmap12 *) ttf->tab_cmap->ucs4;

            if (cm12->grouptab != 0) pdc_free(pdc, cm12->grouptab);

            pdc_free(pdc, cm12);
        }

        pdc_free(pdc, ttf->tab_cmap);
    }

    if (ttf->tab_hmtx != (tt_tab_hmtx *) 0)
    {
        if (ttf->tab_hmtx->metrics != (tt_metric *) 0)
            pdc_free(pdc, ttf->tab_hmtx->metrics);
        if (ttf->tab_hmtx->lsbs != (tt_fword *) 0)
            pdc_free(pdc, ttf->tab_hmtx->lsbs);
        pdc_free(pdc, ttf->tab_hmtx);
    }

    if (ttf->tab_name != (tt_tab_name *) 0)
    {
        if (ttf->tab_name->namerecords != (tt_nameref *) 0)
            pdc_free(pdc, ttf->tab_name->namerecords);
        if (ttf->tab_name->englishname4 != (char *) 0)
            pdc_free(pdc, ttf->tab_name->englishname4);
        if (ttf->tab_name->englishname6 != (char *) 0)
            pdc_free(pdc, ttf->tab_name->englishname6);
        if (ttf->tab_name->producer != (char *) 0)
            pdc_free(pdc, ttf->tab_name->producer);
        pdc_free(pdc, ttf->tab_name);
    }
    ttf->tab_name = (tt_tab_name *) 0;


    /* Note: do not clean up ttf->img since the data belongs to font->img
    */

    if (ttf->check == pdc_false)
        pdc_free(pdc, ttf);

} /* fnt_delete_tt */


#endif /* PDF_TRUETYPE_SUPPORTED */
