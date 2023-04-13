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
 * PDFlib matchbox related routines
 *
 */

#define P_MBOX_C

#include "p_intern.h"
#include "p_color.h"
#include "p_font.h"
#include "p_defopt.h"

static const pdc_defopt pdf_create_mbox_options[] =
{
    {"name", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_INT_MAX, NULL},

    {"boxheight", pdc_scalarlist, PDC_OPT_NONE, 2, 2,
      0.0, PDC_FLOAT_MAX, pdf_boxheight_keylist},

    {"clipping", pdc_scalarlist, PDC_OPT_PERCENT, 4, 4,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},


    {"innerbox", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"openrect", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"fillcolor", pdc_stringlist, PDC_OPT_NONE, 1, 5,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"strokecolor", pdc_stringlist, PDC_OPT_NONE, 1, 5,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"borderwidth", pdc_scalarlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_FLOAT_MAX, NULL},

    {"dasharray", pdc_scalarlist, PDC_OPT_NONE, 0, PDF_MAX_DASHLENGTH,
      PDC_FLOAT_PREC, PDC_FLOAT_MAX, NULL},

    {"dashphase", pdc_scalarlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_FLOAT_MAX, NULL},

    {"linecap", pdc_integerlist, PDC_OPT_NONE, 1, 1,
      0.0, 2.0, pdf_linecap_keylist},

    {"linejoin", pdc_integerlist, PDC_OPT_NONE, 1, 1,
      0.0, 2.0, pdf_linejoin_keylist},

    {"drawleft", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"drawbottom", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"drawright", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"drawtop", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"margin", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"offsetleft", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"offsetbottom", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"offsetright", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"offsettop", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    PDC_OPT_TERMINATE
};

struct pdf_mbox_s
{
    char *name;
    pdc_rectangle rect;
    pdc_matrix ctm;
    pdc_scalar boxheight[2];
    pdc_scalar clipping[4];
    pdc_bool percentclipping[4];
    pdc_bool innerbox;
    pdc_bool openrect;
    pdf_coloropt fillcolor;
    pdf_coloropt strokecolor;
    pdc_scalar borderwidth;
    int linecap;
    int linejoin;
    pdc_scalar dasharray[PDF_MAX_DASHLENGTH];
    int dashlength;
    pdc_scalar dashphase;
    pdc_bool drawleft;
    pdc_bool drawbottom;
    pdc_bool drawright;
    pdc_bool drawtop;
    pdc_scalar offsetleft;
    pdc_bool percentleft;
    pdc_scalar offsetbottom;
    pdc_bool percentbottom;
    pdc_scalar offsetright;
    pdc_bool percentright;
    pdc_scalar offsettop;
    pdc_bool percenttop;
};

static void
pdf_reclaim_mbox(void *item)
{
    pdf_mbox *mbox = (pdf_mbox *) item;

    mbox->name = NULL;
    pdc_rect_init(&mbox->rect, 0, 0, 0, 0);
    pdc_identity_matrix(&mbox->ctm);
    mbox->boxheight[0] = (pdc_scalar) text_capheight;
    mbox->boxheight[1] = (pdc_scalar) text_none;
    mbox->clipping[0] = 0;
    mbox->clipping[1] = 0;
    mbox->clipping[2] = 1;
    mbox->clipping[3] = 1;
    mbox->percentclipping[0] = pdc_true;
    mbox->percentclipping[1] = pdc_true;
    mbox->percentclipping[2] = pdc_true;
    mbox->percentclipping[3] = pdc_true;
    mbox->innerbox = pdc_false;
    mbox->openrect = pdc_false;
    mbox->fillcolor.type = (int) color_none;
    mbox->strokecolor.type = (int) color_none;
    mbox->borderwidth = 0.0;
    mbox->linecap = 0;
    mbox->linejoin = 0;
    mbox->dasharray[0] = 0.0;
    mbox->dasharray[1] = 0.0;
    mbox->dashlength = 0;
    mbox->dashphase = 0;
    mbox->drawleft = pdc_true;
    mbox->drawbottom = pdc_true;
    mbox->drawright = pdc_true;
    mbox->drawtop = pdc_true;
    mbox->offsetleft = 0.0;
    mbox->percentleft = pdc_false;
    mbox->offsetbottom = 0.0;
    mbox->percentbottom = pdc_false;
    mbox->offsetright = 0.0;
    mbox->percentright = pdc_false;
    mbox->offsettop = 0.0;
    mbox->percenttop = pdc_false;
}

static void
pdf_release_mbox(void *context, void *item)
{
    PDF *p = (PDF *) context;
    pdf_mbox *mbox = (pdf_mbox *) item;

    if (mbox->name != NULL)
    {
        pdc_free(p->pdc, mbox->name);
        mbox->name = NULL;
    }
}

static pdc_ced pdf_mbox_ced =
{
    sizeof(pdf_mbox), pdf_reclaim_mbox, pdf_release_mbox, NULL
};

static pdc_vtr_parms pdf_mbox_parms =
{
    0, 10, 10
};

pdc_vtr *
pdf_new_mboxes(PDF *p, pdf_mbox *mbox, pdc_vtr *mboxes)
{
    static const char fn[] = "pdf_new_mboxes";
    char *name = mbox->name;

    if (mboxes == NULL)
        mboxes = pdc_vtr_new(p->pdc, &pdf_mbox_ced, p, &pdf_mbox_parms);

    if (mbox->name != NULL)
        mbox->name = pdc_strdup_ext(p->pdc, mbox->name, 0, fn);
    pdc_vtr_push(mboxes, *mbox, pdf_mbox);

    mbox->name = name;

    return mboxes;
}

void
pdf_add_page_mbox(PDF *p, pdf_mbox *mbox)
{
    /* save current trafo matrix */
    mbox->ctm = p->curr_ppt->gstate[p->curr_ppt->sl].ctm;

    if (mbox->name && strlen(mbox->name))
    {
        pdc_vtr *mboxes_new;
        pdc_vtr *mboxes = p->curr_ppt->mboxes;

        mboxes_new = pdf_new_mboxes(p, mbox, mboxes);
        if (mboxes_new != mboxes)
            p->curr_ppt->mboxes = mboxes_new;

    }
}

void
pdf_delete_mbox(PDF *p, pdf_mbox *mbox)
{
    if (mbox != NULL)
    {
        pdf_release_mbox(p, mbox);
        pdc_free(p->pdc, mbox);
    }
}

pdf_mbox *
pdf_get_mbox(PDF *p, pdc_vtr *mboxes, const char *name, int number,
             int *o_count)
{
    pdf_mbox *o_mbox = NULL;
    int count = 0;

    if (mboxes == NULL)
        mboxes = p->curr_ppt->mboxes;

    if (mboxes != NULL)
    {
        if (name == NULL && number <= 0)
        {
            count = pdc_vtr_size(mboxes);
        }
        else
        {
            int i, n = pdc_vtr_size(mboxes);

            for (i = 0; i < n; i++)
            {
                pdf_mbox *mbox = (pdf_mbox *) &pdc_vtr_at(mboxes, i, pdf_mbox);

                if (name == NULL || !pdc_strcmp(name, mbox->name))
                {
                    count++;
                    if (o_count == NULL && count == number)
                    {
                        o_mbox = mbox;
                        break;
                    }
                }
            }
        }
    }

    if (o_count != NULL)
        *o_count = count;

    return o_mbox;
}

pdf_mbox *
pdf_parse_mbox_optlist(PDF *p, const char *optlist)
{
    static const char fn[] = "pdf_parse_mbox_optlist";
    pdc_resopt *resopts = NULL;
    pdf_mbox *mbox;
    char **strlist = NULL;
    pdc_scalar margin;
    int i, ns;

    resopts = pdc_parse_optionlist(p->pdc, optlist, pdf_create_mbox_options,
                                   NULL, pdc_true);

    mbox = (pdf_mbox *) pdc_malloc(p->pdc, sizeof(pdf_mbox), fn);
    pdf_reclaim_mbox(mbox);

    if (pdc_get_optvalues("name", resopts, NULL, NULL))
        mbox->name = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    pdc_get_optvalues("boxheight", resopts, mbox->boxheight, NULL);
    if (pdc_get_optvalues("clipping", resopts, mbox->clipping, NULL))
    {
        for (i = 0; i < 4; i++)
            mbox->percentclipping[i] = pdc_is_lastopt_percent(resopts, i) ?
                                       pdc_true : pdc_false;
    }


    pdc_get_optvalues("innerbox", resopts, &mbox->innerbox, NULL);
    pdc_get_optvalues("openrect", resopts, &mbox->openrect, NULL);

    ns = pdc_get_optvalues("fillcolor", resopts, NULL, &strlist);
    if (ns)
        pdf_parse_coloropt(p, "fillcolor", strlist, ns, (int) color_max,
                           &mbox->fillcolor);

    pdf_init_coloropt(p, &mbox->strokecolor);
    ns = pdc_get_optvalues("strokecolor", resopts, NULL, &strlist);
    if (ns)
        pdf_parse_coloropt(p, "strokecolor", strlist, ns, (int) color_max,
                           &mbox->strokecolor);

    pdc_get_optvalues("borderwidth", resopts, &mbox->borderwidth, NULL);
    mbox->dashlength =
        pdc_get_optvalues("dasharray", resopts, mbox->dasharray, NULL);
    pdc_get_optvalues("dashphase", resopts, &mbox->dashphase, NULL);
    pdc_get_optvalues("linecap", resopts, &mbox->linecap, NULL);
    pdc_get_optvalues("linejoin", resopts, &mbox->linejoin, NULL);

    pdc_get_optvalues("drawleft", resopts, &mbox->drawleft, NULL);
    pdc_get_optvalues("drawbottom", resopts, &mbox->drawbottom, NULL);
    pdc_get_optvalues("drawright", resopts, &mbox->drawright, NULL);
    pdc_get_optvalues("drawtop", resopts, &mbox->drawtop, NULL);

    if (pdc_get_optvalues("margin", resopts, &margin, NULL))
    {
        mbox->offsetleft = margin;
        mbox->percentleft = pdc_is_lastopt_percent(resopts, 0);

        mbox->offsetbottom = margin;
        mbox->percentbottom = pdc_is_lastopt_percent(resopts, 0);

        mbox->offsetright = -margin;
        mbox->percentright = pdc_is_lastopt_percent(resopts, 0);

        mbox->offsettop = -margin;
        mbox->percenttop = pdc_is_lastopt_percent(resopts, 0);
    }

    if (pdc_get_optvalues("offsetleft", resopts, &mbox->offsetleft, NULL))
    {
        mbox->percentleft = pdc_is_lastopt_percent(resopts, 0);
    }
    if (pdc_get_optvalues("offsetbottom", resopts, &mbox->offsetbottom, NULL))
    {
        mbox->percentbottom = pdc_is_lastopt_percent(resopts, 0);
    }
    if (pdc_get_optvalues("offsetright", resopts, &mbox->offsetright, NULL))
    {
        mbox->percentright = pdc_is_lastopt_percent(resopts, 0);
    }
    if (pdc_get_optvalues("offsettop", resopts, &mbox->offsettop, NULL))
    {
        mbox->percenttop = pdc_is_lastopt_percent(resopts, 0);
    }

    pdc_cleanup_optionlist(p->pdc, resopts);

    return mbox;
}

void
pdf_get_mbox_boxheight(PDF *p, pdf_mbox *mbox, pdc_scalar *boxheight)
{
    (void) p;

    if (mbox == NULL)
    {
        boxheight[0] = (pdc_scalar) text_capheight;
        boxheight[1] = (pdc_scalar) text_none;
    }
    else
    {
        boxheight[0] = mbox->boxheight[0];
        boxheight[1] = mbox->boxheight[1];
    }
}

pdc_bool
pdf_get_mbox_clipping(PDF *p, pdf_mbox *mbox,
                      pdc_scalar width, pdc_scalar height,
                      pdc_box *clipbox)
{
    (void) p;

    if (mbox == NULL)
    {
        clipbox->ll.x = 0;
        clipbox->ll.y = 0;
        clipbox->ur.x = width;
        clipbox->ur.y = height;
    }
    else
    {
        if (mbox->percentclipping[0])
            clipbox->ll.x = mbox->clipping[0] * width;
        else
            clipbox->ll.x = mbox->clipping[0];

        if (mbox->percentclipping[1])
            clipbox->ll.y = mbox->clipping[1] * height;
        else
            clipbox->ll.y = mbox->clipping[1];

        if (mbox->percentclipping[2])
            clipbox->ur.x = mbox->clipping[2] * width;
        else
            clipbox->ur.x = mbox->clipping[2];

        if (mbox->percentclipping[3])
            clipbox->ur.y = mbox->clipping[3] * height;
        else
            clipbox->ur.y = mbox->clipping[3];
    }

    return (clipbox->ll.x != 0 || clipbox->ll.y != 0 ||
            clipbox->ur.x != width || clipbox->ur.y != height) ?
            pdc_true : pdc_false;
}

void
pdf_set_mbox_rectangle(PDF *p, pdf_mbox *mbox, pdc_rectangle *rect, int flags)
{
    pdc_scalar width, height;

    (void) p;

    mbox->rect = *rect;

    width = mbox->rect.urx - mbox->rect.llx;
    height = mbox->rect.ury - mbox->rect.lly;

    if (!(flags & mbox_statleft))
    {
        if (mbox->percentleft)
            mbox->rect.llx += mbox->offsetleft * width;
        else
            mbox->rect.llx += mbox->offsetleft;
    }

    if (!(flags & mbox_statbottom))
    {
        if (mbox->percentbottom)
            mbox->rect.lly += p->ydirection * mbox->offsetbottom * height;
        else
            mbox->rect.lly += p->ydirection * mbox->offsetbottom;
    }

    if (!(flags & mbox_statright))
    {
        if (mbox->percentright)
            mbox->rect.urx += mbox->offsetright * width;
        else
            mbox->rect.urx += mbox->offsetright;
    }

    if (!(flags & mbox_stattop))
    {
        if (mbox->percenttop)
            mbox->rect.ury += p->ydirection * mbox->offsettop * height;
        else
            mbox->rect.ury += p->ydirection * mbox->offsettop;
    }
}

double
pdf_get_mbox_info(PDF *p, pdf_mbox *mbox, const char *keyword)
{
    (void) p;


    if (!strcmp(keyword, "openrect"))
        return (double) mbox->openrect;

    if (!strcmp(keyword, "innerbox"))
        return (double) mbox->innerbox;

    return 0;
}

pdc_bool
pdf_get_mbox_drawborder(PDF *p, pdf_mbox *mbox, int keycode)
{
    pdc_bool drawborder = mbox->borderwidth > 0 &&
                          mbox->strokecolor.type != (int) color_none;

    (void) p;

    switch (keycode)
    {
        case mbox_openleft:
        return drawborder && mbox->drawleft;

        case mbox_openright:
        return drawborder && mbox->drawright;

        case mbox_openbottom:
        return drawborder && mbox->drawbottom;

        case mbox_opentop:
        return drawborder && mbox->drawtop;
    }

    return pdc_false;
}

void
pdf_get_mbox_rectangle(PDF *p, pdf_mbox *mbox, pdc_vector *polyline)
{
    pdc_matrix ctminv;

    pdc_invert_matrix(p->pdc, &ctminv,
                      &p->curr_ppt->gstate[p->curr_ppt->sl].ctm);
    pdc_multiply_matrix(&mbox->ctm, &ctminv);
    pdc_rect2polyline(&ctminv, &mbox->rect, polyline);
}

void
pdf_draw_mbox_rectangle(PDF *p, pdf_mbox *mbox, int flags)
{
    pdc_bool drawleft, drawright, drawbottom, drawtop;
    pdc_bool saverestore = (flags & mbox_saverestore) &&
        ((flags & mbox_area &&
          mbox->fillcolor.type != (int) color_none) ||
         (flags & mbox_border &&
          mbox->strokecolor.type != (int) color_none && mbox->borderwidth > 0));

    if (saverestore)
        pdf__save(p);

    if (flags & mbox_area && mbox->fillcolor.type != (int) color_none &&
        mbox->rect.llx != mbox->rect.urx &&
        mbox->rect.lly != mbox->rect.ury)
    {
        pdf_set_coloropt(p, pdf_fill, &mbox->fillcolor);
        pdf__moveto(p, mbox->rect.llx, mbox->rect.lly);
        pdf__lineto(p, mbox->rect.urx, mbox->rect.lly);
        pdf__lineto(p, mbox->rect.urx, mbox->rect.ury);
        pdf__lineto(p, mbox->rect.llx, mbox->rect.ury);
        pdf__lineto(p, mbox->rect.llx, mbox->rect.lly);
        pdf__fill(p);
    }

    if (flags & mbox_border &&
        mbox->strokecolor.type != (int) color_none && mbox->borderwidth > 0)
    {
        pdf_set_coloropt(p, pdf_stroke, &mbox->strokecolor);
        pdf__setlinewidth(p, mbox->borderwidth);
        pdf_setdashpattern_internal(p, mbox->dasharray, mbox->dashlength,
                                    mbox->dashphase);
        pdf__setlinecap(p, mbox->linecap);
        pdf__setlinejoin(p, mbox->linejoin);

        drawbottom = mbox->drawbottom &&
                    (!(flags & mbox_openbottom) || !mbox->openrect);
        if (drawbottom)
        {
            pdf__moveto(p, mbox->rect.llx, mbox->rect.lly);
            pdf__lineto(p, mbox->rect.urx, mbox->rect.lly);
        }

        drawright = mbox->drawright &&
                    (!(flags & mbox_openright) || !mbox->openrect);
        if (drawright)
        {
            if (!drawbottom)
                pdf__moveto(p, mbox->rect.urx, mbox->rect.lly);
            pdf__lineto(p, mbox->rect.urx, mbox->rect.ury);
        }

        drawtop = mbox->drawtop &&
                    (!(flags & mbox_opentop) || !mbox->openrect);
        if (drawtop)
        {
            if (!drawright)
                pdf__moveto(p, mbox->rect.urx, mbox->rect.ury);
            pdf__lineto(p, mbox->rect.llx, mbox->rect.ury);
        }

        drawleft = mbox->drawleft &&
                    (!(flags & mbox_openleft) || !mbox->openrect);
        if (drawleft)
        {
            if (!drawtop)
                pdf__moveto(p, mbox->rect.llx, mbox->rect.ury);
            if (drawbottom && drawright && drawtop)
                pdf__closepath(p);
            else
                pdf__lineto(p, mbox->rect.llx, mbox->rect.lly);
        }

        pdf__stroke(p);
    }

    if (saverestore)
        pdf__restore(p);
}

const char *
pdf_get_usematchbox(PDF *p, const char *option, const char *optval,
                    int *istart, int *istop)
{
    const char *boxname = NULL, *stemp = NULL;
    char **strlist = NULL;
    int errcode = 0;
    int k, ir, ns, irect = 1, nrect = 0;

    ns = pdc_split_stringlist(p->pdc, optval, NULL, PDC_SPLIT_ISOPTLIST,
                              &strlist);
    if (ns)
    {
        boxname = pdc_strdup_tmp(p->pdc, strlist[0]);

        /* number of rectangles */
        pdf_get_mbox(p, NULL, boxname, 0, &nrect);

        if (ns == 2)
        {
            stemp = pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN,
                                  strlist[1]);

            /* rectangle number or all rectangles */
            if (!pdc_str2integer(stemp, 0, &ir))
            {
                k = pdc_get_keycode_ci(stemp, pdf_mbox_keylist);
                if (k == PDC_KEY_NOTFOUND)
                {
                    errcode = PDC_E_OPT_ILLKEYWORD;
                    goto PDF_USEMATCHBOX_ERROR;
                }
            }
            else if (ir <= 0)
            {
                errcode = PDC_E_OPT_ILLINTEGER;
                goto PDF_USEMATCHBOX_ERROR;
            }
            else
            {
                irect = ir;
                nrect = MIN(irect, nrect);
            }
        }
        else
        {
            irect = 1;
        }
    }

    PDF_USEMATCHBOX_ERROR:

    pdc_cleanup_stringlist(p->pdc, strlist);

    if (errcode)
        pdc_error(p->pdc, errcode, option, stemp, 0, 0);

    *istart = irect;
    *istop = nrect;

    return boxname;
}

static const pdc_keyconn pdf_info_keylist[] =
{
    {"count",   0},
    {"exists",  1},
    {"name",    2},

    {"width",   3},
    {"height",  4},
    {"x1",      5},
    {"y1",      6},
    {"x2",      7},
    {"y2",      8},
    {"x3",      9},
    {"y3",     10},
    {"x4",     11},
    {"y4",     12},
    {NULL, 0}
};

double
pdf__info_matchbox(PDF *p, const char *boxname, int len, int num,
                   const char *keyword)
{
    pdf_mbox *mbox;
    double mbinfo = 0;
    int infokey, count;

    if (boxname == NULL)
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "boxname", 0, 0, 0);

    if (keyword == NULL || *keyword == '0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "keyword", 0, 0, 0);

    /* Converting boxname */
    boxname = pdf_convert_name(p, boxname, len, PDC_CONV_TMPALLOC);
    if (boxname == NULL || *boxname == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "boxname", 0, 0, 0);

    infokey = pdc_get_keycode_ci(keyword, pdf_info_keylist);
    if (infokey == PDC_KEY_NOTFOUND)
        pdc_error(p->pdc, PDC_E_ILLARG_STRING, "keyword", keyword, 0, 0);

    if (!strcmp(boxname, "*"))
        boxname = NULL;

    /* count */
    if (!infokey)
    {
        pdf_get_mbox(p, NULL, boxname, -1, &count);
        mbinfo = (double) count;
    }
    else
    {
        if (num < 1)
            pdc_error(p->pdc, PDC_E_ILLARG_INT, "num",
                      pdc_errprintf(p->pdc, "%d", num), 0, 0);

        mbox = pdf_get_mbox(p, NULL, boxname, num, NULL);
        if (mbox != NULL)
        {
            pdc_vector polyline[5];

            if (infokey > 1)
                pdf_get_mbox_rectangle(p, mbox, polyline);

            switch (infokey)
            {
                /* exists */
                case 1:
                mbinfo = 1;
                break;

                /* name */
                case 2:
                mbinfo = (double)
                    pdf_insert_utilstring(p, mbox->name, pdc_true);
                break;

                /* geometrical keys */
                case 3:
                mbinfo = pdc_get_vector_length(&polyline[0], &polyline[1]);
                break;

                case 4:
                mbinfo = pdc_get_vector_length(&polyline[0], &polyline[3]);
                break;

                case 5:
                mbinfo = polyline[0].x;
                break;

                case 6:
                mbinfo = polyline[0].y;
                break;

                case 7:
                mbinfo = polyline[1].x;
                break;

                case 8:
                mbinfo = polyline[1].y;
                break;

                case 9:
                mbinfo = polyline[2].x;
                break;

                case 10:
                mbinfo = polyline[2].y;
                break;

                case 11:
                mbinfo = polyline[3].x;
                break;

                case 12:
                mbinfo = polyline[3].y;
                break;
            }
        }
        else if (infokey == 2)
        {
            mbinfo = -1;
        }
    }

    return mbinfo;
}


/* -------------------------- fit functions --------------------------- */

void
pdf_init_fit_options(PDF *p, pdc_bool fortflow, pdf_fit_options *fit)
{
    (void) p;
    (void) fortflow;

    fit->boxsize[0] = 0;
    fit->boxsize[1] = 0;
    fit->flags = 0;
    fit->fitmethod = pdc_nofit;
    fit->margin[0] = 0;
    fit->margin[1] = 0;
    fit->mask = 0;
    fit->pcmask = 0;
    fit->shrinklimit = 0.75;
    fit->position[0] = 0;
    fit->position[1] = 0;
    fit->orientate = 0;
    fit->rotate = 0;
    fit->refpoint[0] = 0;
    fit->refpoint[1] = 0;
    fit->showborder = pdc_false;
    fit->matchbox = NULL;
    fit->alignchar = 0;
}

void
pdf_cleanup_fit_options(PDF *p, pdf_fit_options *fit)
{
    pdf_delete_mbox(p, fit->matchbox);
    fit->matchbox = NULL;


}

void
pdf_set_position_values(PDF *p, pdc_scalar *i_position, int nv)
{
    pdc_scalar position[2];
    int i, ipos;

    (void) p;

    position[0] = 0;
    position[1] = 0;

    for (i = 0; i < nv; i++)
    {
        ipos = (int) i_position[i];
        switch(ipos)
        {
            case pos_left:
            case pos_right:
            position[0] = i_position[i] - pos_left;
            break;

            case pos_bottom:
            case pos_top:
            position[1] = i_position[i] - pos_bottom;
            break;

            default:
            position[i] = i_position[i];
            break;
        }
    }

    if (nv == 1)
        position[1] = position[0];

    i_position[0] = position[0];
    i_position[1] = position[1];
}


void
pdf_get_fit_options(PDF *p, pdc_bool fortflow, pdf_fit_options *fit,
                    pdc_resopt *resopts)
{
    char **strlist = NULL;
    int inum;

    (void) fortflow;

    if (pdc_get_optvalues("fitmethod", resopts, &inum, NULL))
    {
        fit->fitmethod = (pdc_fitmethod) inum;
        fit->mask |= (1L << fit_fitmethod);
    }

    if (pdc_get_optvalues("rotate", resopts, &fit->rotate, NULL))
        fit->mask |= (1L << fit_rotate);

    if (pdc_get_optvalues("orientate", resopts, &fit->orientate, NULL))
        fit->mask |= (1L << fit_orientate);

    pdc_get_optvalues("showborder", resopts, &fit->showborder, NULL);

    if (fit->flags & is_textline)
    {
        inum = pdc_get_optvalues("margin", resopts, fit->margin, NULL);
        if (inum)
        {
            if (inum == 1)
                fit->margin[1] = fit->margin[0];
            fit->mask |= (1L << fit_margin);
        }

        if (pdc_get_optvalues("alignchar", resopts, &inum, NULL))
        {
            fit->alignchar = (pdc_ushort) inum;
            fit->mask |= (1L << fit_alignchar);
        }

    }

    if (fit->flags & is_block)
    {
        if (pdc_get_optvalues("refpoint", resopts, fit->refpoint, NULL))
            fit->mask |= (1L << fit_refpoint);
    }


    if (fit->flags & is_block || !(fit->flags & is_textflow))
    {
        if (pdc_get_optvalues("boxsize", resopts, fit->boxsize, NULL))
            fit->mask |= (1L << fit_boxsize);

        if (pdc_get_optvalues("shrinklimit", resopts, &fit->shrinklimit, NULL))
            fit->mask |= (1L << fit_shrinklimit);

        inum = pdc_get_optvalues("position", resopts, fit->position, NULL);
        if (inum)
        {
            pdf_set_position_values(p, fit->position, inum);
            fit->mask |= (1L << fit_position);
        }

        if (pdc_get_optvalues("matchbox", resopts, NULL, &strlist))
        {
            fit->matchbox = pdf_parse_mbox_optlist(p, strlist[0]);
            fit->mask |= (1L << fit_matchbox);
        }
    }
}

pdc_bool
pdf_is_horiz_orientated(pdf_fit_options *fit)
{
    return (fit->orientate == 0 || fit->orientate == 180) ?
           pdc_true : pdc_false;
}

