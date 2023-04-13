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
 * PDFlib routines for annnotations
 *
 */

#define P_ANNOTS_C

#include "p_intern.h"
#include "p_color.h"
#include "p_font.h"
#include "p_image.h"




/* annotation types */
typedef enum
{
    ann_text            = (1<<0),
    ann_link            = (1<<1),
    ann_freetext        = (1<<2),
    ann_line            = (1<<3),
    ann_square          = (1<<4),
    ann_circle          = (1<<5),
    ann_highlight       = (1<<6),
    ann_underline       = (1<<7),
    ann_squiggly        = (1<<8),
    ann_strikeout       = (1<<9),
    ann_stamp           = (1<<10),
    ann_ink             = (1<<11),
    ann_polygon         = (1<<12),
    ann_polyline        = (1<<13),
    ann_popup           = (1<<14),
    ann_fileattachment  = (1<<15),
    ann_3d              = (1<<16),
    ann_movie           = (1<<17)
}
pdf_annottype;

static const pdc_keyconn pdf_annottype_pdfkeylist[] =
{
    {"Text",            ann_text},
    {"Link",            ann_link},
    {"FreeText",        ann_freetext},
    {"Line",            ann_line},
    {"Square",          ann_square},
    {"Circle",          ann_circle},
    {"Highlight",       ann_highlight},
    {"Underline",       ann_underline},
    {"Squiggly",        ann_squiggly},
    {"StrikeOut",       ann_strikeout},
    {"Stamp",           ann_stamp},
    {"Polygon",         ann_polygon},
    {"PolyLine",        ann_polyline},
    {"Ink",             ann_ink},
    {"Popup",           ann_popup},
    {"FileAttachment",  ann_fileattachment},
    {"3D",              ann_3d},
    {"Movie",           ann_movie},
    {NULL, 0}
};

static const pdc_keyconn pdf_polyline_pdfkeylist[] =
{
    {"QuadPoints",  ann_link},
    {"QuadPoints",  ann_highlight},
    {"QuadPoints",  ann_underline},
    {"QuadPoints",  ann_squiggly},
    {"QuadPoints",  ann_strikeout},
    {"InkList",     ann_ink},
    {"Vertices",    ann_polygon},
    {"Vertices",    ann_polyline},
    {NULL, 0}
};

/* flags for annotation dictionary entries */
typedef enum
{
    anndict_a          = (1<<0),
    anndict_bs         = (1<<1),
    anndict_c          = (1<<2),
    anndict_contents   = (1<<3),
    anndict_f          = (1<<4),
    anndict_fs         = (1<<5),
    anndict_h          = (1<<6),
    anndict_ic         = (1<<7),
    anndict_inklist    = (1<<8),
    anndict_l          = (1<<9),
    anndict_le         = (1<<10),
    anndict_movie      = (1<<11),
    anndict_name       = (1<<12),
    anndict_nm         = (1<<13),
    anndict_open       = (1<<14),
    anndict_parent     = (1<<15),
    anndict_popup      = (1<<16),
    anndict_q          = (1<<17),
    anndict_quadpoints = (1<<18),
    anndict_rect       = (1<<19),
    anndict_subtype    = (1<<20),
    anndict_t          = (1<<21),
    anndict_vertices   = (1<<22),
    anndict_3dd        = (1<<23),
    anndict_3da        = (1<<24),
    anndict_3dv        = (1<<25)
}
pdf_anndictentries;

static const pdc_keyconn pdf_perm_entries_pdfkeylist[] =
{
    {"Contents",   anndict_contents},
    {"Name",       anndict_name},
    {"NM",         anndict_nm},
    {"Open",       anndict_open},
    {NULL, 0}
};

static const pdc_keyconn pdf_forb_entries_pdfkeylist[] =
{
    {"A",          anndict_a},
    {"BS",         anndict_bs},
    {"C",          anndict_c},
    {"F",          anndict_f},
    {"FS",         anndict_fs},
    {"H",          anndict_h},
    {"IC",         anndict_ic},
    {"InkList",    anndict_inklist},
    {"L",          anndict_l},
    {"LE",         anndict_le},
    {"Movie",      anndict_movie},
    {"Parent",     anndict_parent},
    {"Popup",      anndict_popup},
    {"Q",          anndict_q},
    {"QuadPoints", anndict_quadpoints},
    {"Rect",       anndict_rect},
    {"Subtype",    anndict_subtype},
    {"T",          anndict_t},
    {"Vertices",   anndict_vertices},
    {"3DD",        anndict_3dd},
    {"3DV",        anndict_3dv},
    {"3DA",        anndict_3da},
    {NULL, 0}
};

/* line ending styles */
typedef enum
{
    line_none,
    line_square,
    line_circle,
    line_diamond,
    line_openarrow,
    line_closedarrow
}
pdf_endingstyles;

static const pdc_keyconn pdf_endingstyles_pdfkeylist[] =
{
    {"None",            line_none},
    {"Square",          line_square},
    {"Circle",          line_circle},
    {"Diamond",         line_diamond},
    {"OpenArrow",       line_openarrow},
    {"ClosedArrow",     line_closedarrow},
    {NULL, 0}
};

/* text icon names */
typedef enum
{
    icon_text_comment,
    icon_text_help,
    icon_text_key,
    icon_text_insert,
    icon_text_newparagraph,
    icon_text_note,
    icon_text_paragraph
}
pdf_text_iconnames;

static const pdc_keyconn pdf_text_iconnames_pdfkeylist[] =
{
    {"Comment",         icon_text_comment},
    {"Help",            icon_text_help},
    {"Key",             icon_text_key},
    {"Insert",          icon_text_insert},
    {"NewParagraph",    icon_text_newparagraph},
    {"Note",            icon_text_note},
    {"Paragraph",       icon_text_paragraph},
    {NULL, 0}
};

/* stamp icon names */
typedef enum
{
    icon_stamp_approved,
    icon_stamp_asls,
    icon_stamp_confidential,
    icon_stamp_departmental,
    icon_stamp_draft,
    icon_stamp_experimental,
    icon_stamp_expired,
    icon_stamp_final,
    icon_stamp_forcomment,
    icon_stamp_forpublicrelease,
    icon_stamp_notapproved,
    icon_stamp_notforpublicrelease,
    icon_stamp_sold,
    icon_stamp_topsecret
}
pdf_stamp_iconnames;

static const pdc_keyconn pdf_stamp_iconnames_pdfkeylist[] =
{
    {"Approved",               icon_stamp_approved},
    {"AsIs",                   icon_stamp_asls},
    {"Confidential",           icon_stamp_confidential},
    {"Departmental",           icon_stamp_departmental},
    {"Draft",                  icon_stamp_draft},
    {"Experimental",           icon_stamp_experimental},
    {"Expired",                icon_stamp_expired},
    {"Final",                  icon_stamp_final},
    {"ForComment",             icon_stamp_forcomment},
    {"ForPublicRelease",       icon_stamp_forpublicrelease},
    {"NotApproved",            icon_stamp_notapproved},
    {"NotForPublicRelease",    icon_stamp_notforpublicrelease},
    {"Sold",                   icon_stamp_sold},
    {"TopSecret",              icon_stamp_topsecret},
    {NULL, 0}
};

/* file attachment icon names */
typedef enum
{
    icon_attach_graph,
    icon_attach_paperclip,
    icon_attach_pushpin,
    icon_attach_tag
}
pdf_attach_iconnames;

static const pdc_keyconn pdf_attach_iconnames_pdfkeylist[] =
{
    {"Graph",           icon_attach_graph},
    {"Paperclip",       icon_attach_paperclip},
    {"PushPin",         icon_attach_pushpin},
    {"Tag",             icon_attach_tag},
    {NULL, 0}
};


static const pdc_keyconn pdf_3dview_keylist[] =
{
    {NULL, 0}
};


typedef enum
{
    movieposter_none = -1,
    movieposter_auto = -2
}
pdf_movieposter_states;

static const pdc_keyconn pdf_movieposter_keylist[] =
{
    {"none",     movieposter_none},
    {"auto",     movieposter_auto},
    {NULL, 0}
};

typedef enum
{
    playmode_once,
    playmode_open,
    playmode_repeat,
    playmode_palindrome
}
pdf_playmode_states;

static const pdc_keyconn pdf_playmode_pdfkeylist[] =
{
    {"Once",       playmode_once},
    {"Open",       playmode_open},
    {"Repeat",     playmode_repeat},
    {"Palindrome", playmode_palindrome},
    {NULL, 0}
};

#define PDF_ANN_FULLSCREEN -9999

static const pdc_keyconn pdf_windowscale_keylist[] =
{
    {"fullscreen",  PDF_ANN_FULLSCREEN},
    {NULL, 0}
};


#define PDF_LAYER_FLAG PDC_OPT_UNSUPP

#define PDF_3DANNOT_FLAG PDC_OPT_UNSUPP
static const pdc_defopt pdf_create_annot_options[] =
{
    /* deprecated */
    {"annotwarning", pdc_booleanlist, PDC_OPT_PDFLIB_7, 1, 1,
      0.0, 0.0, NULL},

    {"usercoordinates", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"hypertextencoding", pdc_stringlist,  PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"custom", pdc_stringlist, PDC_OPT_NONE, 1, 64,
      0.0, PDC_INT_MAX, NULL},

    {"name", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"parentname", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"popup", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"title", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"subject", pdc_stringlist, PDC_OPT_PDC_1_5, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"annotcolor", pdc_stringlist, PDC_OPT_NONE, 1, 5,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"borderstyle", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_borderstyle_keylist},

    {"dasharray", pdc_scalarlist, PDC_OPT_NONE, 1, 2,
      PDC_FLOAT_PREC, PDC_FLOAT_MAX, NULL},

    {"linewidth", pdc_integerlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"opacity", pdc_scalarlist, PDC_OPT_PDC_1_4 | PDC_OPT_PERCENT, 1, 1,
     0.0, 1.0, NULL},

    {"highlight", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_highlight_keylist},

    {"display", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_display_keylist},

    {"zoom", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"rotate", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"readonly", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"locked", pdc_booleanlist, PDC_OPT_PDC_1_4, 1, 1,
      0.0, 0.0, NULL},

    {"open", pdc_booleanlist, PDC_OPT_PDC_1_4, 1, 1,
      0.0, 0.0, NULL},

    {"createdate", pdc_booleanlist, PDC_OPT_PDC_1_5, 1, 1,
      0.0, 0.0, NULL},

    {"fillcolor", pdc_stringlist, PDC_OPT_NONE, 2, 5,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"alignment", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_quadding_keylist},

    {"font", pdc_fonthandle, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"fontsize", pdc_scalarlist, PDC_OPT_SUBOPTLIST | PDC_OPT_KEYLIST1, 1, 2,
      0.0, PDC_FLOAT_MAX, pdf_fontsize_keylist},

    {"orientate", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_orientate_keylist},

    {"contents", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_INT_MAX, NULL},

    {"destination", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"destname", pdc_stringlist, PDC_OPT_IGNOREIF1, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"filename", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      1.0, PDC_FILENAMELEN, NULL},

    {"mimetype", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"iconname", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"endingstyles", pdc_keywordlist, PDC_OPT_NONE, 2, 2,
      0.0, 0.0, pdf_endingstyles_pdfkeylist},

    {"interiorcolor", pdc_stringlist, PDC_OPT_NONE, 1, 5,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"cloudy", pdc_scalarlist, PDC_OPT_PDC_1_5, 1, 1,
      0.0, 2.0, NULL},

    {"line", pdc_scalarlist, PDC_OPT_NONE, 4, 4,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"polylinelist", pdc_polylinelist, PDC_OPT_NONE, 1, PDF_MAXARRAYSIZE,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"action", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"usematchbox", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"layer", pdc_layerhandle, PDF_LAYER_FLAG, 1, 1,
      0.0, 0.0, NULL},

    {"3dactivate", pdc_stringlist, PDF_3DANNOT_FLAG, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"3dbox", pdc_scalarlist, PDF_3DANNOT_FLAG, 4, 4,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    {"3ddata", pdc_3ddatahandle, PDF_3DANNOT_FLAG, 1, 1,
      0.0, 0.0, NULL},

    {"3dinteractive", pdc_booleanlist, PDF_3DANNOT_FLAG, 1, 1,
      0.0, 0.0, NULL},

    {"3dshared", pdc_booleanlist, PDF_3DANNOT_FLAG, 1, 1,
      0.0, 0.0, NULL},

    {"3dinitialview", pdc_3dviewhandle, PDF_3DANNOT_FLAG, 1, 1,
      0.0, 0.0, pdf_3dview_keylist},

    {"movieposter", pdc_templatehandle, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_movieposter_keylist},

    {"showcontrols", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"playmode", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_playmode_pdfkeylist},

    {"windowscale", pdc_scalarlist, PDC_OPT_NONE, 1, 1,
      PDC_FLOAT_PREC, PDC_FLOAT_MAX, pdf_windowscale_keylist},

    {"windowposition", pdc_scalarlist, PDC_OPT_NONE, 1, 2,
      0.0, 100.0, pdf_position_keylist},

    {"soundvolume", pdc_scalarlist, PDC_OPT_NONE, 1, 1,
      -1.0, 1.0, NULL},

    PDC_OPT_TERMINATE
};

/* Annotation member */
typedef struct pdf_annot_s
{
    pdc_bool iscopy;
    pdc_id obj_id;
    pdf_annottype atype;
    int mask;
    pdc_rectangle rect;
    pdc_bool usercoordinates;
    pdc_encoding hypertextencoding;
    int hypertextcodepage;
    pdf_coloropt annotcolor;
    pdf_coloropt interiorcolor;
    pdf_coloropt fillcolor;
    int linewidth;
    pdc_scalar opacity;
    pdf_borderstyle borderstyle;
    pdc_scalar dasharray[2];
    pdf_highlight highlight;
    pdf_display display;
    pdc_bool zoom;
    pdc_bool rotate;
    pdc_bool kreadonly;
    pdc_bool locked;
    pdc_bool open;
    pdc_bool createdate;
    int font;
    pdc_scalar fontsize;
    int orientate;
    pdf_quadding alignment;
    pdf_endingstyles endingstyles[2];
    pdc_scalar cloudy;
    pdf_dest *dest;
    char *name;
    char *parentname;
    char *popup;
    char *title;
    char *subject;
    char *contents;
    char *filename;
    char *nativefilename;
    char *mimetype;
    const char *iconname;
    pdc_off_t filesize;
    pdc_scalar *line;
    pdc_polyline *polylinelist;
    int nplines;
    char **custom;
    int ncustoms;
    char *action;



    pdc_id movieposter;
    pdc_bool showcontrols;
    pdf_playmode_states playmode;
    pdc_scalar windowscale;
    pdc_scalar windowposition[2];
    pdc_scalar soundvolume;
}
pdf_annot;

static void
pdf_reclaim_annot(void *item)
{
    pdf_annot *ann = (pdf_annot *) item;

    ann->iscopy = pdc_false;
    ann->obj_id = PDC_BAD_ID;
    ann->atype = (pdf_annottype)0;
    ann->mask = 0;
    ann->usercoordinates = pdc_false;
    ann->hypertextencoding = pdc_invalidenc;
    ann->hypertextcodepage = 0;
    ann->annotcolor.type = (int) color_none;
    ann->interiorcolor.type = (int) color_none;
    ann->fillcolor.type = (int) color_none;
    ann->linewidth = 1;
    ann->opacity = 1;
    ann->borderstyle = border_solid;
    ann->dasharray[0] = 3;
    ann->dasharray[1] = 3;
    ann->highlight = high_invert;
    ann->display = disp_visible;
    ann->zoom = pdc_true;
    ann->rotate = pdc_true;
    ann->kreadonly = pdc_false;
    ann->locked = pdc_false;
    ann->open = pdc_false;
    ann->createdate = pdc_false;
    ann->font = -1;
    ann->fontsize = 0;
    ann->orientate = 0;
    ann->alignment = quadd_left;
    ann->cloudy = -1;
    ann->endingstyles[0] = line_none;
    ann->endingstyles[1] = line_none;
    ann->dest = NULL;
    ann->name = NULL;
    ann->parentname = NULL;
    ann->popup = NULL;
    ann->title = NULL;
    ann->subject = NULL;
    ann->contents = NULL;
    ann->filename = NULL;
    ann->nativefilename = NULL;
    ann->mimetype = NULL;
    ann->iconname = NULL;
    ann->filesize = 0;
    ann->line = NULL;
    ann->polylinelist = NULL;
    ann->nplines = 0;
    ann->custom = NULL;
    ann->ncustoms = 0;
    ann->action = NULL;





    ann->movieposter = PDC_BAD_ID /* = movieposter_none */;
    ann->showcontrols = pdc_false;
    ann->playmode = playmode_once;
    ann->windowscale = 0;
    ann->windowposition[0] = 50.0;
    ann->windowposition[1] = 50.0;
    ann->soundvolume = 1.0;
}

static void
pdf_release_annot(void *context, void *item)
{
    PDF *p = (PDF *) context;
    pdf_annot *ann = (pdf_annot *) item;

    /* is not a copy */
    if (!ann->iscopy)
    {
        pdf_cleanup_destination(p, ann->dest);
        ann->dest = NULL;

        if (ann->name)
        {
            pdc_free(p->pdc, ann->name);
            ann->name = NULL;
        }
        if (ann->parentname)
        {
            pdc_free(p->pdc, ann->parentname);
            ann->parentname = NULL;
        }
        if (ann->popup)
        {
            pdc_free(p->pdc, ann->popup);
            ann->popup = NULL;
        }
        if (ann->title)
        {
            pdc_free(p->pdc, ann->title);
            ann->title = NULL;
        }
        if (ann->subject)
        {
            pdc_free(p->pdc, ann->subject);
            ann->subject = NULL;
        }
        if (ann->contents)
        {
            pdc_free(p->pdc, ann->contents);
            ann->contents = NULL;
        }
        if (ann->filename)
        {
            pdc_free(p->pdc, ann->filename);
            ann->filename = NULL;
        }
        if (ann->nativefilename)
        {
            pdc_free(p->pdc, ann->nativefilename);
            ann->nativefilename = NULL;
        }
        if (ann->mimetype)
        {
            pdc_free(p->pdc, ann->mimetype);
            ann->mimetype = NULL;
        }
        if (ann->line)
        {
            pdc_free(p->pdc, ann->line);
            ann->line = NULL;
        }
        if (ann->custom)
        {
            pdc_cleanup_optstringlist(p->pdc, ann->custom, ann->ncustoms);
            ann->custom = NULL;
            ann->ncustoms = 0;
        }
        if (ann->action)
        {
            pdc_free(p->pdc, ann->action);
            ann->action = NULL;
        }
    }

    ann->polylinelist = (pdc_polyline *)pdc_delete_polylinelist(
			    p->pdc, ann->polylinelist, ann->nplines);
}

static pdc_ced pdf_annot_ced =
{
    sizeof(pdf_annot), pdf_reclaim_annot, pdf_release_annot, NULL
};

static pdc_vtr_parms pdf_annot_parms =
{
    0, 10, 10
};

static pdf_annot *
pdf_new_annot(PDF *p, pdf_annottype atype)
{
    pdc_vtr *annots = pdf_get_annots_list(p);
    pdf_annot *ann;

    if (annots == NULL)
    {
        annots = pdc_vtr_new(p->pdc, &pdf_annot_ced, p, &pdf_annot_parms);
        pdf_set_annots_list(p, annots);
    }

    ann = pdc_vtr_incr(annots, pdf_annot);
    ann->atype = atype;


    ann->usercoordinates = p->usercoordinates;
    ann->hypertextencoding = p->hypertextencoding;
    ann->hypertextcodepage = p->hypertextcodepage;
    pdf_init_coloropt(p, &ann->fillcolor);

    return ann;
}

static void
pdf_delete_last_annot(PDF *p)
{
    pdc_vtr *annots = pdf_get_annots_list(p);

    if (annots != NULL)
    {
        if (pdc_vtr_size(annots) > 1)
        {
            pdc_vtr_pop(annots);
        }
        else
        {
            pdc_vtr_delete(annots);
            pdf_set_annots_list(p, NULL);
        }
    }
}

static void
pdf_init_rectangle(PDF *p, pdf_annot *ann,
         pdc_scalar llx, pdc_scalar lly, pdc_scalar urx, pdc_scalar ury,
         pdc_vector *polyline)
{
    static const char fn[] = "pdf_init_rectangle";
    pdc_matrix *ctm = &p->curr_ppt->gstate[p->curr_ppt->sl].ctm;
    int i;

    pdc_check_number(p->pdc, "llx", llx);
    pdc_check_number(p->pdc, "lly", lly);
    pdc_check_number(p->pdc, "urx", urx);
    pdc_check_number(p->pdc, "ury", ury);

    pdc_delete_polylinelist(p->pdc, ann->polylinelist, ann->nplines);
    ann->nplines = 1;
    ann->polylinelist = (pdc_polyline *) pdc_malloc(p->pdc,
                               ann->nplines * sizeof(pdc_polyline), fn);
    ann->polylinelist[0].np = 5;
    ann->polylinelist[0].p = (pdc_vector *) pdc_malloc(p->pdc,
                      ann->polylinelist[0].np * sizeof(pdc_vector), fn);

    if (polyline == NULL)
    {
        if (!ann->usercoordinates)
            ctm = NULL;
        pdc_rect_init(&ann->rect, llx, lly, urx, ury);
        pdc_rect2polyline(ctm, &ann->rect, ann->polylinelist[0].p);
    }
    else
    {
        for (i = 0; i < 5; i++)
            pdc_transform_vector(ctm, &polyline[i],
                                 &ann->polylinelist[0].p[i]);
    }

    if (ctm != NULL)
        pdc_polyline2rect(ann->polylinelist[0].p, 4, &ann->rect);
}

/* because of Acrobat muddle */
static void
pdf_permute_coordinates(pdf_annot *ann, pdf_annottype atype)
{
    if (ann->nplines == 1 &&
         (atype == ann_highlight ||
          atype == ann_underline ||
          atype == ann_squiggly ||
          atype == ann_strikeout))
    {
        pdc_vector pl[5];
        int i;

        for (i = 0; i < ann->polylinelist[0].np; i++)
            pl[i] = ann->polylinelist[0].p[i];

        ann->polylinelist[0].p[0] = pl[3];
        ann->polylinelist[0].p[1] = pl[2];
        ann->polylinelist[0].p[2] = pl[0];
        ann->polylinelist[0].p[3] = pl[1];
        ann->polylinelist[0].p[4] = pl[3];
    }
}

static const pdc_keyconn pdf_keytype_keylist[] =
{
    {"boolean", pdc_booleanlist},
    {"name",    pdc_keywordlist},
    {"string",  pdc_stringlist},
    {NULL, 0}
};

static const pdc_defopt pdf_custom_list_options[] =
{
    {"key", pdc_stringlist, PDC_OPT_REQUIRED, 1, 1,
      1.0, PDF_MAX_NAMESTRING, NULL},

    {"type", pdc_keywordlist, PDC_OPT_REQUIRED, 1, 1,
      0.0, 0.0, pdf_keytype_keylist},

    {"value", pdc_stringlist, PDC_OPT_REQUIRED, 1, 1,
      1.0, PDC_USHRT_MAX, NULL},

    PDC_OPT_TERMINATE
};

static void
pdf_parse_and_write_annot_customlist(PDF *p, pdf_annot *ann, pdc_bool output)
{
    int i;

    /* custom entries */
    for (i = 0; i < ann->ncustoms; i++)
    {
        pdc_resopt *resopts = NULL;
        const char *stemp;
        const char *keyword;
        char **strlist = NULL;
        char *string;
        int inum;

        resopts = pdc_parse_optionlist(p->pdc, ann->custom[i],
                               pdf_custom_list_options, NULL, pdc_true);

        keyword = "key";
        pdc_get_optvalues(keyword, resopts, NULL, &strlist);
        string = strlist[0];

        inum = pdc_get_keycode(string, pdf_forb_entries_pdfkeylist);
        if (inum != PDC_KEY_NOTFOUND)
        {
            stemp = pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN, string);
            pdc_error(p->pdc, PDF_E_ANN_ILLCUSTOMKEY, stemp, 0, 0, 0);
        }
        inum = pdc_get_keycode(string, pdf_perm_entries_pdfkeylist);
        if (inum != PDC_KEY_NOTFOUND)
            ann->mask |= inum;

        if (output)
            pdc_printf(p->out, "/%s", string);

        keyword = "type";
        pdc_get_optvalues(keyword, resopts, &inum, NULL);

        keyword = "value";
        pdc_get_optvalues(keyword, resopts, NULL, &strlist);
        string = strlist[0];

        switch (inum)
        {
            case pdc_booleanlist:
            if (pdc_stricmp(string, "true") && pdc_stricmp(string, "false"))
            {
                stemp =
		    pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN, string);

                pdc_error(p->pdc, PDC_E_OPT_ILLBOOLEAN, keyword, stemp, 0, 0);
            }
            if (output)
                pdc_printf(p->out, " %s",
                           PDC_BOOLSTR(pdc_stricmp(string, "false")));
            break;

            case pdc_keywordlist:
            if (output)
                pdc_printf(p->out, "/%s", string);
            break;

            case pdc_stringlist:
            pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                        ann->hypertextcodepage, pdc_true, NULL, &string, NULL);
            if (output)
                pdf_put_hypertext(p, string);
            break;
        }
        if (output)
            pdc_puts(p->out, "\n");
    }
}



static void
pdf_opt_alrdef(PDF *p, const char *keyword, pdf_annot *ann, int flag)
{
    if (ann->mask & flag)
        pdc_error(p->pdc, PDF_E_ANN_OPTALRDEF, keyword, 0, 0, 0);
}

static int
pdf_opt_effectless(PDF *p, const char *keyword, pdf_annottype curratype,
                   pdf_annottype intendatypes)
{
    if ((pdf_annottype) !(intendatypes & curratype))
    {
        const char *type = pdc_get_keyword(curratype, pdf_annottype_pdfkeylist);
        pdc_warning(p->pdc, PDF_E_ANN_OPTEFFLESS_FORTYPE, keyword, type,
                    0, 0);
        return 1;
    }
    return 0;
}

void
pdf__create_annotation(PDF *p,
     pdc_scalar llx, pdc_scalar lly, pdc_scalar urx, pdc_scalar ury,
     const char *type, const char *optlist)
{
    pdc_resopt *resopts = NULL;
    pdc_clientdata cdata;
    pdf_annottype atype;
    pdf_annot *ann;
    pdf_dest *dest = NULL;
    const char *keyword, *keyword_s = NULL;
    char **strlist = NULL;
    pdc_scalar *line;
    int i, j, k, ns, nss[2];
    pdf_colortype maxcs = color_rgb;
    pdf_ppt *ppt = p->curr_ppt;
    pdc_matrix *ctm = &ppt->gstate[ppt->sl].ctm;

    pdc_check_number(p->pdc, "llx", llx);
    pdc_check_number(p->pdc, "lly", lly);
    pdc_check_number(p->pdc, "urx", urx);
    pdc_check_number(p->pdc, "ury", ury);

    if (type == NULL || *type == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "type", 0, 0, 0);

    k = pdc_get_keycode_ci(type, pdf_annottype_pdfkeylist);
    if (k == PDC_KEY_NOTFOUND)
        pdc_error(p->pdc, PDC_E_ILLARG_STRING, "type", type, 0, 0);
    atype = (pdf_annottype) k;


    /* compatibility */
    if (p->compatibility < PDC_1_5 &&
        (atype == ann_polygon || atype == ann_polyline))
    {
        pdc_error(p->pdc, PDC_E_PAR_VERSION, type,
                  pdc_get_pdfversion(p->pdc, PDC_1_5), 0, 0);
    }

    if (p->compatibility >= PDC_1_6)
        maxcs = color_cmyk;

    /* Parsing option list */
    pdf_set_clientdata(p, &cdata);
    resopts = pdc_parse_optionlist(p->pdc, optlist, pdf_create_annot_options,
                                   &cdata, pdc_true);

    /* Initializing */
    ann = pdf_new_annot(p, atype);

    keyword = "usercoordinates";
    pdc_get_optvalues(keyword, resopts, &ann->usercoordinates, NULL);
    pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);

    ann->hypertextencoding =
        pdf_get_hypertextencoding_opt(p, resopts, &ann->hypertextcodepage,
                                      pdc_true);

    keyword = "custom";
    ns = pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                 ann->hypertextcodepage, pdc_true, NULL, NULL, &ann->custom);
    if (ns)
    {
        pdc_save_lastopt(resopts, PDC_OPT_SAVEALL);
        ann->ncustoms = ns;
        pdf_parse_and_write_annot_customlist(p, ann, pdc_false);
    }

    keyword = "name";
    ns = pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                 ann->hypertextcodepage, pdc_true, NULL, &ann->name, NULL);
    if (ns)
    {
        pdf_opt_alrdef(p, keyword, ann, anndict_nm);
        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    }

    keyword = "parentname";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL))
        ann->parentname = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "popup";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL))
        ann->popup = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "title";
    if (pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                    ann->hypertextcodepage, pdc_true, NULL, &ann->title, NULL))
        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "subject";
    if (pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                  ann->hypertextcodepage, pdc_true, NULL, &ann->subject, NULL))
        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "annotcolor";
    ns = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
    if (ns)
    {
        pdf_parse_coloropt(p, keyword, strlist, ns, (int) maxcs,
                           &ann->annotcolor);
    }

    keyword = "borderstyle";
    if (pdc_get_optvalues(keyword, resopts, &ns, NULL))
        ann->borderstyle = (pdf_borderstyle) ns;

    keyword = "dasharray";
    ns = pdc_get_optvalues(keyword, resopts, ann->dasharray, NULL);
    if (ns)
    {
        if (ns == 1)
            ann->dasharray[1] = ann->dasharray[0];
        if (ann->borderstyle != border_dashed)
            pdc_warning(p->pdc, PDC_E_OPT_IGNORED, keyword, 0, 0, 0);
    }

    keyword = "linewidth";
    pdc_get_optvalues(keyword, resopts, &ann->linewidth, NULL);

    keyword = "opacity";
    pdc_get_optvalues(keyword, resopts, &ann->opacity, NULL);


    keyword = "highlight";
    if (pdc_get_optvalues(keyword, resopts, &ns, NULL))
    {
        pdf_opt_effectless(p, keyword, atype, ann_link);
        ann->highlight = (pdf_highlight) ns;
    }

    keyword = "display";
    if (pdc_get_optvalues(keyword, resopts, &ann->display, NULL))
        ann->display = (pdf_display) ns;

    keyword = "zoom";
    pdc_get_optvalues(keyword, resopts, &ann->zoom, NULL);

    keyword = "rotate";
    pdc_get_optvalues(keyword, resopts, &ann->rotate, NULL);

    keyword = "readonly";
    pdc_get_optvalues(keyword, resopts, &ann->kreadonly, NULL);

    keyword = "locked";
    pdc_get_optvalues(keyword, resopts, &ann->locked, NULL);

    keyword = "open";
    if (pdc_get_optvalues(keyword, resopts, &ann->open, NULL))
    {
        pdf_opt_alrdef(p, keyword, ann, anndict_open);
        pdf_opt_effectless(p, keyword, atype,
                           (pdf_annottype) (ann_text | ann_popup));
    }

    keyword = "createdate";
    pdc_get_optvalues(keyword, resopts, &ann->createdate, NULL);

    keyword = "fillcolor";
    ns = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
    if (ns && !pdf_opt_effectless(p, keyword, atype, ann_freetext))
    {
        pdf_parse_coloropt(p, keyword, strlist, ns, (int) color_cmyk,
                           &ann->fillcolor);
    }

    keyword = "alignment";
    if (pdc_get_optvalues(keyword, resopts, &ns, NULL))
        ann->alignment = (pdf_quadding) ns;

    keyword = "font";
    if (pdc_get_optvalues(keyword, resopts, &ann->font, NULL))
        pdf_opt_effectless(p, keyword, atype, ann_freetext);

    keyword = "fontsize";
    if (pdf_get_fontsize_option(p, ann->font, resopts, &ann->fontsize))
    {
        pdf_opt_effectless(p, keyword, atype, ann_freetext);
        if (ann->usercoordinates == pdc_true)
            ann->fontsize = pdc_transform_scalar(ctm, ann->fontsize);
    }

    keyword = "orientate";
    if (pdc_get_optvalues(keyword, resopts, &ann->orientate, NULL))
        pdf_opt_effectless(p, keyword, atype,
                (pdf_annottype) (ann_freetext | ann_stamp));

    keyword = "contents";
    if (atype == ann_freetext)
    {
        pdc_encoding enc = pdc_invalidenc;
        int codepage = 0;

        if (ann->font > -1)
        {
            enc = p->fonts[ann->font].ft.enc;
            codepage = p->fonts[ann->font].codepage;
        }
        pdf_get_opt_textlist(p, keyword, resopts, enc, codepage,
                             pdc_false, NULL, &ann->contents, NULL);
    }
    else
    {
        pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                 ann->hypertextcodepage, pdc_true, NULL, &ann->contents, NULL);
    }
    if (ann->contents)
    {
        pdf_opt_alrdef(p, keyword, ann, anndict_contents);
        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    }

    keyword = "destination";
    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist) &&
        !pdf_opt_effectless(p, keyword, atype, ann_link))
    {
        ann->dest = pdf_parse_destination_optlist(p, strlist[0], 0,
                        pdf_locallink);
        keyword_s = keyword;
    }
    else
    {
        keyword = "destname";
        if (atype == ann_link)
            dest = pdf_get_option_destname(p, resopts, ann->hypertextencoding,
                                           ann->hypertextcodepage);
        else if (pdc_get_optvalues(keyword, resopts, NULL, NULL))
            pdf_opt_effectless(p, keyword, atype, ann_link);
        if (dest)
        {
            ann->dest = dest;
            keyword_s = keyword;
        }
    }

    keyword = "filename";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL) &&
        !pdf_opt_effectless(p, keyword, atype,
                           (pdf_annottype) (ann_fileattachment | ann_movie)))
    {
        /* DON'T change order */

        /* native filename */
        ann->nativefilename = pdf_get_opt_filename(p, keyword, resopts,
                          ann->hypertextencoding, ann->hypertextcodepage);

        pdf_get_opt_textlist(p, keyword, resopts, ann->hypertextencoding,
                             ann->hypertextcodepage,
             /* bug #2344 */ atype == ann_fileattachment ? pdc_undef : pdc_true,
                             NULL, &ann->filename, NULL);
        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

        if (atype == ann_fileattachment)
            ann->filesize = pdf_check_file(p, ann->filename, pdc_true);
    }

    keyword = "mimetype";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL) &&
        !pdf_opt_effectless(p, keyword, atype, ann_fileattachment))
        ann->mimetype = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "iconname";
    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist) &&
        !pdf_opt_effectless(p, keyword, atype,
                (pdf_annottype) (ann_text | ann_stamp | ann_fileattachment)))
    {
        const pdc_keyconn *kc = pdf_text_iconnames_pdfkeylist;

        pdf_opt_alrdef(p, keyword, ann, anndict_name);

        if (atype == ann_stamp)
            kc = pdf_stamp_iconnames_pdfkeylist;
        else if (atype == ann_fileattachment)
            kc = pdf_attach_iconnames_pdfkeylist;

        ann->iconname = pdc_get_int_keyword(strlist[0], kc);
        if (ann->iconname == NULL)
            pdc_error(p->pdc, PDC_E_OPT_ILLKEYWORD, keyword, strlist[0],
                      0, 0);
    }

    keyword = "endingstyles";
    if (pdc_get_optvalues(keyword, resopts, nss, NULL))
    {
        ann->endingstyles[0] = (pdf_endingstyles) nss[0];
        ann->endingstyles[1] = (pdf_endingstyles) nss[1];
        pdf_opt_effectless(p, keyword, atype,
                           (pdf_annottype) (ann_line | ann_polyline));
    }

    keyword = "interiorcolor";
    ns = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
    if (ns && !pdf_opt_effectless(p, keyword, atype,
                (pdf_annottype) (ann_line | ann_polyline |
                                 ann_square | ann_circle)))
    {
        pdf_parse_coloropt(p, keyword, strlist, ns, (int) maxcs,
                           &ann->interiorcolor);
    }

    keyword = "cloudy";
    if (pdc_get_optvalues(keyword, resopts, &ann->cloudy, NULL))
        pdf_opt_effectless(p, keyword, atype, ann_polygon);

    keyword = "line";
    ns = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
    if (ns && !pdf_opt_effectless(p, keyword, atype, ann_line))
    {
        line = (pdc_scalar *) strlist;
        if (ann->usercoordinates == pdc_true)
        {
            pdc_transform_point(ctm, line[0], line[1], &line[0], &line[1]);
            pdc_transform_point(ctm, line[2], line[3], &line[2], &line[3]);
        }
        ann->line = (pdc_scalar *) pdc_save_lastopt(resopts, PDC_OPT_SAVEALL);
    }

    keyword = "polylinelist";
    ns = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
    if (ns)
    {
        if (!pdf_opt_effectless(p, keyword, atype,
               (pdf_annottype) (ann_ink | ann_polygon | ann_polyline |
                                ann_highlight | ann_underline |
                                ann_squiggly | ann_strikeout)))
        {
            pdc_polyline *pl = (pdc_polyline *) strlist;

            for (j = 0; j < ns; j++)
            {
                if (pl[j].np < 2 ||
                    (atype != ann_ink && atype != ann_polygon &&
                     atype != ann_polyline && pl[j].np != 4))
                {
                    pdc_error(p->pdc, PDF_E_ANN_BADNUMCOORD, keyword, 0, 0, 0);
                }
                for (i = 0; i < pl[j].np; i++)
                {
                    if (ann->usercoordinates == pdc_true)
                        pdc_transform_vector(ctm, &pl[j].p[i], NULL);
                }
            }
            pdc_delete_polylinelist(p->pdc, ann->polylinelist, ann->nplines);
            ann->polylinelist = pl;
            ann->nplines = ns;
            pdc_save_lastopt(resopts, PDC_OPT_SAVEALL);
        }
    }
    else
        pdf_permute_coordinates(ann, atype);

    keyword = "action";
    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist))
    {
        /*
         * PDF 1.7 allows the A action dictionary only for annotation types
         * link, widget and screen. Widget is handled elsewhere, and screen
         * is not yet implemented, so only link is allowed with compatiblity
         * 1.7.
         * Must be extended that also screen annotation is accepted here
         * once it is implemented.
         */
        if (p->compatibility >= PDC_1_7)
        {
            if (ann->atype != ann_link)
            {
                pdc_error(p->pdc, PDF_E_ANN_ACTIONNOTALLOWED,
                        pdc_get_keyword(ann->atype, pdf_annottype_pdfkeylist),
                        0, 0, 0);
            }
        }

        if (ann->dest)
        {
            pdf_cleanup_destination(p, ann->dest);
            ann->dest = NULL;
            pdc_warning(p->pdc, PDC_E_OPT_IGNORE, keyword_s, keyword, 0, 0);
        }

        /* parsing of action list */
        pdf_parse_and_write_actionlist(p, event_annotation, NULL,
                                       (const char *) strlist[0]);
        ann->action = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    }



    keyword = "movieposter";
    if (pdc_get_optvalues(keyword, resopts, &ns, NULL))
    {
        pdf_opt_effectless(p, keyword, atype, ann_movie);
        if (ns < 0)
        {
            ann->movieposter = (pdc_id) ns;
        }
        else
        {
#if 0
            int cstype = pdf_get_image_colorspace(p, ns);

            if (cstype != (int) DeviceGray && cstype != (int) DeviceRGB)
                pdc_error(p->pdc, PDF_E_ANN_ILLTEMPLATE, 0, 0, 0, 0);
#endif
            ann->movieposter = pdf_get_xobject(p, ns);
        }
    }

    keyword = "showcontrols";
    if (pdc_get_optvalues(keyword, resopts, &ann->showcontrols, NULL))
        pdf_opt_effectless(p, keyword, atype, ann_movie);

    keyword = "playmode";
    if (pdc_get_optvalues(keyword, resopts, &ns, NULL))
    {
        pdf_opt_effectless(p, keyword, atype, ann_movie);
        ann->playmode = (pdf_playmode_states) ns;
    }

    keyword = "windowscale";
    if (pdc_get_optvalues(keyword, resopts, &ann->windowscale, NULL))
        pdf_opt_effectless(p, keyword, atype, ann_movie);

    keyword = "windowposition";
    ns = pdc_get_optvalues(keyword, resopts, ann->windowposition, NULL);
    if (ns)
    {
        pdf_opt_effectless(p, keyword, atype, ann_movie);
        pdf_set_position_values(p, ann->windowposition, ns);
    }

    keyword = "soundvolume";
    if (pdc_get_optvalues(keyword, resopts, &ann->soundvolume, NULL))
        pdf_opt_effectless(p, keyword, atype, ann_movie);

    /* named annotation, to be accessible by a GoTo3DView or Movie action */
    if ((atype == ann_3d || atype == ann_movie) && ann->name != NULL)
    {
        char *name = (char *) pdc_strdup(p->pdc, ann->name);

        ann->obj_id = pdc_alloc_id(p->out);
        pdf_insert_name(p, name, names_annots, ann->obj_id);
    }

    /* work-around for Acrobat 8 (see bug #1225) */
    if (ann->annotcolor.type == (int) color_none &&
        (atype == ann_square || atype == ann_circle))
    {
        ann->annotcolor.type = (int) color_rgb;
        ann->annotcolor.value[0] = 1.0;
        ann->annotcolor.value[1] = 1.0;
        ann->annotcolor.value[2] = 1.0;
    }

    /* required options */
    keyword = NULL;
    if (ann->contents == NULL &&
        atype != ann_link && atype != ann_popup && atype != ann_movie)
        keyword = "contents";
    if (ann->fontsize == 0 && atype == ann_freetext)
        keyword = "fontsize";
    if (ann->font == -1 && atype == ann_freetext)
        keyword = "font";
    if (ann->filename == NULL &&
        (atype == ann_fileattachment || atype == ann_movie))
        keyword = "filename";
    if (ann->line == NULL && atype == ann_line)
        keyword = "line";
    if (ann->polylinelist == NULL &&
        (atype == ann_ink || atype == ann_polygon || atype == ann_polyline))
        keyword = "polylinelist";

    if (keyword)
        pdc_error(p->pdc, PDC_E_OPT_NOTFOUND, keyword, 0, 0, 0);


    if (atype == ann_freetext)
    {
        pdf_font *font_s = &p->fonts[ann->font];
        const char *fontname =
            pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN, font_s->ft.name);

        if (!strcmp(pdf_get_pdf_fontname(font_s), fontname))
            pdc_error(p->pdc, PDF_E_ANN_NOSTDFONT, fontname, 0, 0, 0);
    }

    /*
     * matchbox available
     */
    keyword = "usematchbox";
    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist))
    {
        pdf_annot *ann_first = ann;
        const char *mboxname;
        int ir, nrect, irect;

        mboxname = pdf_get_usematchbox(p, keyword, strlist[0], &irect, &nrect);
        if (mboxname != NULL)
        {
            if (irect > nrect)
            {
                pdf_delete_last_annot(p);
            }
            else
            {
                pdf_mbox *mbox;
                pdc_vector pl[5];

                /* rectangle loop */
                for (ir = irect; ir <= nrect; ir++)
                {
                    if (ir > irect)
                    {
                        /* create copy */
                        ann = pdf_new_annot(p, atype);
                        memcpy(ann, ann_first, sizeof(pdf_annot));
                        ann->obj_id = PDC_BAD_ID;
                        ann->iscopy = pdc_true;
                        ann->nplines = 0;
                        ann->polylinelist = NULL;
                    }

                    /* rectangle #ir */
                    mbox = pdf_get_mbox(p, NULL, mboxname, ir, NULL);
                    pdf_get_mbox_rectangle(p, mbox, pl);
                    pdf_init_rectangle(p, ann, 0, 0, 0, 0, pl);
                    pdf_permute_coordinates(ann, atype);
                    ann->usercoordinates = pdc_true;
                }
            }
        }
    }

}

pdc_id
pdf_write_annots_root(PDF *p, pdc_vtr *annots, pdf_widget *widgetlist)
{
    pdc_id result = PDC_BAD_ID;

    /* Annotations array */
    if (annots != NULL || widgetlist)
    {
        result = pdc_begin_obj(p->out, PDC_NEW_ID);
	pdc_begin_array(p->out);

        if (annots != NULL)
        {
            pdf_annot *ann;
            int i, na = pdc_vtr_size(annots);

            for (i = 0; i < na; i++)
            {
                ann = (pdf_annot *) &pdc_vtr_at(annots, i, pdf_annot);
                if (ann->obj_id == PDC_BAD_ID)
                    ann->obj_id = pdc_alloc_id(p->out);
                pdc_objref_c(p->out, ann->obj_id);
            }
        }

        (void) widgetlist;

	pdc_end_array(p->out);
	pdc_end_obj(p->out);
    }

    return result;
}

static void
pdf_write_defappstring(PDF *p, pdf_annot *ann)
{
    char buf[PDC_GEN_BUFSIZE], *bufc;
    pdf_coloropt *fs;
    int ct;

    if (ann->font == -1)
        return;

    bufc = buf;

    /* font and fontsize */
    bufc +=  pdc_sprintf(p->pdc, pdc_true, bufc, "/%s %f Tf",
                         pdf_get_pdf_fontname(&p->fonts[ann->font]),
                         ann->fontsize);

    /* fill and stroke color */
    fs = &ann->fillcolor;
    ct = fs->type;
    switch (ct)
    {
        case color_gray:
        bufc +=  pdc_sprintf(p->pdc, pdc_true, bufc, " %f g",
                             fs->value[0]);
        break;

        case color_rgb:
        bufc +=  pdc_sprintf(p->pdc, pdc_true, bufc, " %f %f %f rg",
                             fs->value[0], fs->value[1], fs->value[2]);
        break;

        case color_cmyk:
        bufc +=  pdc_sprintf(p->pdc, pdc_true, bufc, " %f %f %f %f k",
                             fs->value[0], fs->value[1],
                             fs->value[2], fs->value[3]);
        break;
    }

    pdc_puts(p->out, "/DA");
    pdf_put_hypertext(p, buf);
    pdc_puts(p->out, "\n");
}

static void
pdf_write_colorentry(PDF *p, const char *keyname, pdf_coloropt *coloropt)
{
    if (p->compatibility < PDC_1_6)
    {
        if (coloropt->type != (int) color_none)
        {
            pdc_printf(p->out, "%s[%f %f %f]\n",
                       keyname,
                       coloropt->value[0],
                       coloropt->value[1],
                       coloropt->value[2]);
        }
    }
    else
    {
        switch (coloropt->type)
        {
            case color_none:
            /* this doesn't work in Acrobat 8
            pdc_printf(p->out, "%s[0]\n",
                       keyname);
            */
            break;

            case color_gray:
            pdc_printf(p->out, "%s[%f]\n",
                       keyname,
                       coloropt->value[0]);
            break;

            case color_rgb:
            pdc_printf(p->out, "%s[%f %f %f]\n",
                       keyname,
                       coloropt->value[0],
                       coloropt->value[1],
                       coloropt->value[2]);
            break;

            case color_cmyk:
            pdc_printf(p->out, "%s[%f %f %f %f]\n",
                       keyname,
                       coloropt->value[0],
                       coloropt->value[1],
                       coloropt->value[2],
                       coloropt->value[3]);
            break;
        }
    }
}

void
pdf_write_page_annots(PDF *p, pdc_vtr *annots)
{
    pdf_annot *ann, *annpar;
    pdc_id act_idlist[PDF_MAX_EVENTS];
    int i, j, k, na, flags;

    na = pdc_vtr_size(annots);

    for (k = 0; k < na; k++)
    {
        ann = (pdf_annot *) &pdc_vtr_at(annots, k, pdf_annot);


        /* write action objects */
        if (ann->action)
            pdf_parse_and_write_actionlist(p, event_annotation, act_idlist,
                                           (const char *) ann->action);


        pdc_begin_obj(p->out, ann->obj_id);     /* Annotation object */
        pdc_begin_dict(p->out);                 /* Annotation dict */

        pdc_puts(p->out, "/Type/Annot\n");
        pdc_printf(p->out, "/Subtype/%s\n",
                   pdc_get_keyword(ann->atype, pdf_annottype_pdfkeylist));




        /* Contents */
        if (ann->contents)
        {
            pdc_puts(p->out, "/Contents");
            if (ann->atype == ann_freetext)
                pdf_put_fieldtext(p, ann->contents, ann->font);
            else
                pdf_put_hypertext(p, ann->contents);
            pdc_puts(p->out, "\n");
        }

        /* Current Page */
	pdc_objref(p->out, "/P", pdf_get_page_id(p, 0));

        /* Rectangle */
        pdc_printf(p->out, "/Rect[%f %f %f %f]\n",
            ann->rect.llx, ann->rect.lly, ann->rect.urx, ann->rect.ury);

        /* Name */
        if (ann->name)
        {
            pdc_puts(p->out, "/NM");
            pdf_put_hypertext(p, ann->name);
            pdc_puts(p->out, "\n");
        }

        /* Flags */
        flags = 0;
        if (ann->display != disp_noprint)
        {
            flags = (1<<2);
            flags |= ann->display;
        }
        if (!ann->zoom)
            flags |= (1<<3);
        if (!ann->rotate)
            flags |= (1<<4);
        if (ann->kreadonly)
            flags |= (1<<6);
        if (ann->locked)
            flags |= (1<<7);
        if (flags)
            pdc_printf(p->out, "/F %d\n", flags);

        /* Border style dictionary */
        if (ann->linewidth != 1 || ann->borderstyle != border_solid)
        {
            pdc_puts(p->out, "/BS");
            pdc_begin_dict(p->out);             /* BS dict */

            pdc_printf(p->out, "/W %d", ann->linewidth);
            pdc_printf(p->out, "/S/%s",
                pdc_get_keyword(ann->borderstyle, pdf_borderstyle_pdfkeylist));
            if (ann->borderstyle == border_dashed)
                pdc_printf(p->out, "/D[%f %f]",
                           ann->dasharray[0], ann->dasharray[1]);

            pdc_end_dict(p->out);               /* BS dict */

            /* Write the Border key in old-style PDF 1.1 format
             * because of a bug in PDF 1.4 and earlier
             */
            pdc_printf(p->out, "/Border[0 0 %f", (double) ann->linewidth);

            if (ann->borderstyle == border_dashed)
                pdc_printf(p->out, "[%f %f]",
                           ann->dasharray[0], ann->dasharray[1]);
            pdc_puts(p->out, "]\n");
        }

        /* Annotation color */
        pdf_write_colorentry(p, "/C", &ann->annotcolor);

        /* Title */
        if (ann->title && *ann->title)
        {
            pdc_puts(p->out, "/T");
            pdf_put_hypertext(p, ann->title);
            pdc_puts(p->out, "\n");
        }

        /* Subject */
        if (ann->subject && *ann->subject)
        {
            pdc_puts(p->out, "/Subj");
            pdf_put_hypertext(p, ann->subject);
            pdc_puts(p->out, "\n");
        }

        /* Popup */
        if (ann->popup && *ann->popup)
        {
            for (i = 0; i < na; i++)
            {
                annpar = (pdf_annot *) &pdc_vtr_at(annots, i, pdf_annot);
                if (annpar->name != NULL &&
                    !strcmp(ann->popup, annpar->name))
                {
		    pdc_objref(p->out, "/Popup", annpar->obj_id);
                    break;
                }
            }
        }

        /* Icon Name */
        if (ann->iconname && *ann->iconname)
            pdc_printf(p->out, "/Name/%s\n", ann->iconname);

        /* CreationDate */
        if (ann->createdate)
        {
            char time_str[PDC_TIME_SBUF_SIZE];

            pdc_get_timestr(time_str, pdc_false);
            pdc_puts(p->out, "/CreationDate ");
            pdf_put_hypertext(p, time_str);
            pdc_puts(p->out, "\n");
        }

        /* Opacity */
        if (ann->opacity != 1)
            pdc_printf(p->out, "/CA %f\n", ann->opacity);

        /* write Action entries */
        if (ann->action)
            pdf_write_action_entries(p, event_annotation, act_idlist);

        /* custom entries */
        pdf_parse_and_write_annot_customlist(p, ann, pdc_true);

        switch (ann->atype)
        {
            /* Open */
            case ann_text:
            case ann_popup:
            if (ann->open)
                pdc_puts(p->out, "/Open true\n");
            break;

            /* Alignment, Default appearance string */
            case ann_freetext:
            if (ann->alignment != quadd_left)
                pdc_printf(p->out, "/Q %d\n", ann->alignment);
            pdf_write_defappstring(p, ann);
            break;

            /* Line */
            case ann_line:
            pdc_printf(p->out, "/L[%f %f %f %f]\n",
                       ann->line[0], ann->line[1], ann->line[2], ann->line[3]);
            break;

            /* InkList, QuadPoints and Vertices */
            case ann_link:
            if (!ann->usercoordinates || p->compatibility < PDC_1_6)
                break;
            case ann_highlight:
            case ann_underline:
            case ann_squiggly:
            case ann_strikeout:
            ann->polylinelist[0].np = 4;  /* because of Acrobat error */
            case ann_ink:
            case ann_polygon:
            case ann_polyline:
            pdc_printf(p->out, "/%s",
                       pdc_get_keyword(ann->atype, pdf_polyline_pdfkeylist));
	    pdc_begin_array(p->out);
            for (i = 0; i < ann->nplines; i++)
            {
                if (ann->atype == ann_ink)
		    pdc_begin_array(p->out);
                for (j = 0; j < ann->polylinelist[i].np; j++)
                     pdc_printf(p->out, "%f %f ", ann->polylinelist[i].p[j].x,
                                                  ann->polylinelist[i].p[j].y);
                if (ann->atype == ann_ink)
		    pdc_end_array_c(p->out);
            }
	    pdc_end_array(p->out);
            break;

            default:
            break;
        }

        switch (ann->atype)
        {
            /* Destination, Highlight */
            case ann_link:
            if (ann->dest)
            {
                pdc_puts(p->out, "/Dest");
                pdf_write_destination(p, ann->dest);
            }
            if (ann->highlight != high_invert)
                pdc_printf(p->out, "/H/%s\n",
                    pdc_get_keyword(ann->highlight, pdf_highlight_pdfkeylist));
            break;

            /* Line ending styles */
            case ann_line:
            case ann_polyline:
            if (ann->endingstyles[0] != line_none ||
                ann->endingstyles[1] != line_none)
                pdc_printf(p->out, "/LE[/%s /%s]\n",
                    pdc_get_keyword(ann->endingstyles[0],
                                    pdf_endingstyles_pdfkeylist),
                    pdc_get_keyword(ann->endingstyles[1],
                                    pdf_endingstyles_pdfkeylist));
            break;

            /* border effect dictionary */
            case ann_polygon:
            if (ann->cloudy > -1)
            {
                pdc_puts(p->out, "/BE");
                pdc_begin_dict(p->out);                 /* BE dict */
                pdc_puts(p->out, "/S/C");
                if (ann->cloudy > 0)
                    pdc_printf(p->out, "/I %f", ann->cloudy);
                pdc_end_dict(p->out);                   /* BE dict */
            }

            /* rotate */
            case ann_stamp:
            case ann_freetext:
            if (ann->orientate)
                pdc_printf(p->out, "/Rotate %d\n", ann->orientate);
            break;

            default:
            break;
        }

        switch (ann->atype)
        {
            /* Interior color */
            case ann_line:
            case ann_polyline:
            case ann_square:
            case ann_circle:
            pdf_write_colorentry(p, "/IC", &ann->interiorcolor);
            break;

            /* Parent Annotation */
            case ann_popup:
            if (ann->parentname && *ann->parentname)
            {
                for (i = 0; i < na; i++)
                {
                    annpar = (pdf_annot *) &pdc_vtr_at(annots, i, pdf_annot);
                    if (annpar->name != NULL &&
                        !strcmp(ann->parentname, annpar->name))
                    {
			pdc_objref(p->out, "/Parent", annpar->obj_id);
                        break;
                    }
                }
            }
            break;

            /* File specification */
            case ann_fileattachment:
            {
                /* see bug #1439 */
                const char *basename = pdc_file_strip_dirs(ann->nativefilename);

                pdc_puts(p->out, "/FS");
                pdc_begin_dict(p->out);                 /* FS dict */
                pdc_puts(p->out, "/Type/Filespec\n");
                pdc_puts(p->out, "/F");
                pdf_put_pdffilename(p, basename);
                pdc_puts(p->out, "\n");
                if (p->compatibility >= PDC_1_7)
                {
                    pdc_puts(p->out, "/UF");
                    pdf_put_pdfunifilename(p, basename);
                    pdc_puts(p->out, "\n");
                }

                /* alloc id for the actual embedded file stream */
                ann->obj_id = pdc_alloc_id(p->out);
                pdc_puts(p->out, "/EF");
                pdc_begin_dict(p->out);
                pdc_objref(p->out, "/F", ann->obj_id);
                pdc_end_dict(p->out);
                pdc_end_dict(p->out);                   /* FS dict */
            }
            break;


            case ann_movie:
            {
                pdc_puts(p->out, "/Movie");
                pdc_begin_dict(p->out);                 /* Movie dict */

                /* File specification */
                pdc_puts(p->out, "/F");
                pdc_begin_dict(p->out);                 /* F dict */
                pdc_puts(p->out, "/Type/Filespec\n");
                pdc_puts(p->out, "/F");
                pdf_put_pdffilename(p, ann->nativefilename);
                pdc_puts(p->out, "\n");
                if (p->compatibility >= PDC_1_7)
                {
                    pdc_puts(p->out, "/UF");
                    pdf_put_pdfunifilename(p, ann->filename);
                    pdc_puts(p->out, "\n");
                }
                pdc_end_dict(p->out);                   /* F dict */

                /* Aspect - not supported by Acrobat */
                /* Rotate - doesn't work */

                /* Poster */
                if (ann->movieposter != movieposter_none)
                {
                    if (ann->movieposter == movieposter_auto)
                        pdc_puts(p->out, "/Poster true");
                    else
                        pdc_objref(p->out, "/Poster", ann->movieposter);
                }

                pdc_end_dict(p->out);                   /* Movie dict */

                if (ann->soundvolume != 1.0 ||
                    ann->showcontrols ||
                    ann->playmode != playmode_once ||
                    ann->windowscale != 0)
                {
                    pdc_puts(p->out, "/A");
                    pdc_begin_dict(p->out);             /* Activation dict */

                    /* Start, Duration, Rate, Synchronus - not supported */

                    /* Volume */
                    if (ann->soundvolume != 1.0)
                        pdc_printf(p->out, "/Volume %f\n", ann->soundvolume);

                    /* ShowControls */
                    if (ann->showcontrols)
                        pdc_puts(p->out, "/ShowControls true\n");

                    /* Mode */
                    if (ann->playmode != playmode_once)
                        pdc_printf(p->out, "/Mode/%s\n",
                                   pdc_get_keyword(ann->playmode,
                                                   pdf_playmode_pdfkeylist));

                    /* window */
                    if (ann->windowscale != 0)
                    {
                        int sx, sy;

                        if (ann->windowscale == PDF_ANN_FULLSCREEN)
                        {
                            /* see PDF Reference 1.7, Appendix H, 153. */
                            sx = 999;
                            sy = 1;
                        }
                        else
                        {
                            pdc_scalar sf, sp;

                            sp = 1;
                            for (i = 0; ; i++)
                            {
                                sf = sp * ann->windowscale;
                                if (sf == (double) (int) (sf) || i == 5)
                                    break;
                                sp *= 10;
                            }

                            sx = (int) sf;
                            sy = (int) sp;
                        }

                        /* FWScale */
                        pdc_printf(p->out, "/FWScale[%d %d]\n", sx, sy);

                        /* FWPosition */
                        if (ann->windowposition[0] != 50.0 ||
                            ann->windowposition[1] != 50.0)
                        {
                            pdc_printf(p->out, "/FWPosition[%f %f]\n",
                                       ann->windowposition[0] / 100.0,
                                       1.0 - ann->windowposition[1] / 100.0);
                        }
                    }

                    pdc_end_dict(p->out);               /* Activation dict */
                }
            }
            break;

            default:
            break;
        }


        pdc_end_dict(p->out);                   /* Annotation dict */
        pdc_end_obj(p->out);                    /* Annotation object */
    }

    /* Write the actual embedded files with preallocated ids */
    for (k = 0; k < na; k++)
    {
        ann = (pdf_annot *) &pdc_vtr_at(annots, k, pdf_annot);
        if (ann->atype == ann_fileattachment)
            pdf_embed_file(p, ann->obj_id, ann->filename, ann->mimetype,
                           ann->filesize);
    }
}

/*****************************************************************************/
/**            deprecated historical annotation functions                   **/
/*****************************************************************************/

void
pdf_create_link(
    PDF *p,
    const char *type,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    const char *annopts,
    const char *utext,
    int len)
{
    static const char *fn = "pdf_create_link";
    char *optlist;
    char *name;
    int acthdl;

    name = pdf_convert_name(p, utext, len, PDC_CONV_WITHBOM);
    optlist = (char *) pdc_malloc(p->pdc, strlen(name) + 80, fn);

    if (!pdc_stricmp(type, "URI"))
        strcpy(optlist, "url {");
    else if (!pdc_stricmp(type, "GoTo"))
        strcpy(optlist, "destname {");
    else if (!pdc_stricmp(type, "GoToR"))
        strcpy(optlist, "destination {page 1} filename {");
    strcat(optlist, name);
    strcat(optlist, "}");

    acthdl = pdf__create_action(p, type, optlist);
    if (acthdl > -1)
    {
        if (p->pdc->hastobepos) acthdl++;
        pdc_sprintf(p->pdc, pdc_false, optlist,
                    "action {activate %d} usercoordinates ", acthdl);
        strcat(optlist, annopts);
        pdf__create_annotation(p, llx, lly, urx, ury, "Link", optlist);
    }

    pdc_free(p->pdc, optlist);
    pdc_free(p->pdc, name);
}

void
pdf_init_annot_params(PDF *p)
{
    /* annotation border style defaults */
    p->border_style = border_solid;
    p->border_width = 1;
    p->border_red = 0;
    p->border_green = 0;
    p->border_blue = 0;
    p->border_dash1 = 3;
    p->border_dash2 = 3;

    /* auxiliary function parameters */
    p->launchlink_parameters = NULL;
    p->launchlink_operation = NULL;
    p->launchlink_defaultdir = NULL;
}

void
pdf_cleanup_annot_params(PDF *p)
{
    if (p->launchlink_parameters)
    {
        pdc_free(p->pdc, p->launchlink_parameters);
        p->launchlink_parameters = NULL;
    }

    if (p->launchlink_operation)
    {
        pdc_free(p->pdc, p->launchlink_operation);
        p->launchlink_operation = NULL;
    }

    if (p->launchlink_defaultdir)
    {
        pdc_free(p->pdc, p->launchlink_defaultdir);
        p->launchlink_defaultdir = NULL;
    }
}

static void
pdf_insert_annot_params(PDF *p, pdf_annot *ann)
{
    ann->borderstyle = p->border_style;
    ann->linewidth = (int) p->border_width;
    ann->annotcolor.type = (int) color_rgb;
    ann->annotcolor.value[0] = p->border_red;
    ann->annotcolor.value[1] = p->border_green;
    ann->annotcolor.value[2] = p->border_blue;
    ann->annotcolor.value[3] = 0;
    ann->dasharray[0] = p->border_dash1;
    ann->dasharray[1] = p->border_dash2;
}

void
pdf__attach_file(
    PDF *p,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    const char *filename,
    int len_filename,
    const char *description,
    int len_descr,
    const char *author,
    int len_auth,
    const char *mimetype,
    const char *icon)
{
    pdc_file *attfile;
    pdf_annot *ann;
    pdf_attach_iconnames icontype = icon_attach_pushpin;

    filename = pdf_convert_filename(p, filename, len_filename, "filename",
                                    PDC_CONV_WITHBOM);

    if (icon != NULL && *icon)
    {
        int k = pdc_get_keycode_ci(icon, pdf_attach_iconnames_pdfkeylist);
        if (k == PDC_KEY_NOTFOUND)
            pdc_error(p->pdc, PDC_E_ILLARG_STRING, "icon", icon, 0, 0);
        icontype = (pdf_attach_iconnames) k;
    }

    attfile = pdc_fsearch_fopen(p->pdc, filename, NULL, "attachment ", 0);
    if (attfile == NULL)
        pdc_error(p->pdc, -1, 0, 0, 0, 0);

    pdc_lock_pvf(p->pdc, filename);
    pdc_fclose(attfile);

    /* fill up annotation struct */
    ann = pdf_new_annot(p, ann_fileattachment);
    ann->zoom = pdc_false;
    ann->rotate = pdc_false;
    pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);
    pdf_insert_annot_params(p, ann);
    ann->filename = pdc_strdup(p->pdc, filename);
    ann->nativefilename = pdc_strdup(p->pdc, filename);
    ann->filesize = pdf_check_file(p, ann->filename, pdc_true);
    ann->contents = pdf_convert_hypertext_depr(p, description, len_descr);
    ann->title = pdf_convert_hypertext_depr(p, author, len_auth);
    if (mimetype != NULL && mimetype)
        ann->mimetype = pdc_strdup(p->pdc, mimetype);
    if (icontype != icon_attach_pushpin)
        ann->iconname =
            pdc_get_keyword(icontype, pdf_attach_iconnames_pdfkeylist);
}

void
pdf__add_note(
    PDF *p,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    const char *contents,
    int len_cont,
    const char *title,
    int len_title,
    const char *icon,
    int kopen)
{
    pdf_annot *ann;
    pdf_text_iconnames icontype = icon_text_note;

    if (icon != NULL && *icon)
    {
        int k = pdc_get_keycode_ci(icon, pdf_text_iconnames_pdfkeylist);
        if (k == PDC_KEY_NOTFOUND)
            pdc_error(p->pdc, PDC_E_ILLARG_STRING, "icon", icon, 0, 0);
        icontype = (pdf_text_iconnames) k;
    }

    /* fill up annotation struct */
    ann = pdf_new_annot(p, ann_text);
    pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);
    pdf_insert_annot_params(p, ann);
    ann->contents = pdf_convert_hypertext_depr(p, contents, len_cont);
    ann->title = pdf_convert_hypertext_depr(p, title, len_title);
    if (icontype != icon_text_note)
        ann->iconname = pdc_get_keyword(icontype,pdf_text_iconnames_pdfkeylist);
    ann->open = kopen;
    ann->display = disp_noprint;
}

void
pdf__add_pdflink(
    PDF *p,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    const char *filename,
    int page,
    const char *optlist)
{
    static const char *fn = "pdf__add_pdflink";
    char *actoptlist, *sopt;
    pdf_annot *ann;
    int acthdl;
    size_t size;

    if (filename == NULL || *filename == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "filename", 0, 0, 0);

    if (optlist == NULL)
        optlist = "";

    size = strlen(filename) + strlen(optlist) + 80;
    actoptlist = (char *) pdc_malloc(p->pdc, size, fn);

    /* creating a GoToR action */
    actoptlist[0] = 0;
    sopt = actoptlist;
    sopt += pdc_sprintf(p->pdc, pdc_false, sopt, "filename {%s} ", filename);
    sopt += pdc_sprintf(p->pdc, pdc_false, sopt, "destination {%s page %d} ",
                        optlist, page);
    acthdl = pdf__create_action(p, "GoToR", actoptlist);

    /* fill up annotation struct */
    if (acthdl > -1)
    {
        ann = pdf_new_annot(p, ann_link);
        pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);
        pdf_insert_annot_params(p, ann);
        if (p->pdc->hastobepos) acthdl++;
        pdc_sprintf(p->pdc, pdc_false, actoptlist, "activate %d", acthdl);
        ann->action = pdc_strdup(p->pdc, actoptlist);
        ann->display = disp_noprint;
    }

    pdc_free(p->pdc, actoptlist);
}

void
pdf__add_launchlink(
    PDF *p,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    const char *filename)
{
    static const char *fn = "pdf__add_launchlink";
    char *actoptlist, *sopt;
    pdf_annot *ann;
    int acthdl;
    size_t size;

    if (filename == NULL || *filename == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "filename", 0, 0, 0);
    size = strlen(filename) + 80;
    if (p->launchlink_parameters)
        size += strlen(p->launchlink_parameters);
    if (p->launchlink_operation)
        size += strlen(p->launchlink_operation);
    if (p->launchlink_defaultdir)
        size += strlen(p->launchlink_defaultdir);
    actoptlist = (char *) pdc_malloc(p->pdc, size, fn);

    /* creating a Launch action */
    actoptlist[0] = 0;
    sopt = actoptlist;
    sopt += pdc_sprintf(p->pdc, pdc_false, sopt, "filename {%s} ", filename);
    if (p->launchlink_parameters)
    {
        sopt += pdc_sprintf(p->pdc, pdc_false, sopt, "parameters {%s} ",
                                          p->launchlink_parameters);
        pdc_free(p->pdc, p->launchlink_parameters);
        p->launchlink_parameters = NULL;
    }
    if (p->launchlink_operation)
    {
        sopt += pdc_sprintf(p->pdc, pdc_false, sopt, "operation {%s} ",
                                          p->launchlink_operation);
        pdc_free(p->pdc, p->launchlink_operation);
        p->launchlink_operation = NULL;
    }
    if (p->launchlink_defaultdir)
    {
        sopt += pdc_sprintf(p->pdc, pdc_false, sopt, "defaultdir {%s} ",
                                          p->launchlink_defaultdir);
        pdc_free(p->pdc, p->launchlink_defaultdir);
        p->launchlink_defaultdir = NULL;
    }
    acthdl = pdf__create_action(p, "Launch", actoptlist);

    /* fill up annotation struct */
    if (acthdl > -1)
    {
        ann = pdf_new_annot(p, ann_link);
        pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);
        pdf_insert_annot_params(p, ann);
        if (p->pdc->hastobepos) acthdl++;
        pdc_sprintf(p->pdc, pdc_false, actoptlist, "activate %d", acthdl);
        ann->action = pdc_strdup(p->pdc, actoptlist);
        ann->display = disp_noprint;
    }

    pdc_free(p->pdc, actoptlist);
}

void
pdf__add_locallink(
    PDF *p,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    int page,
    const char *optlist)
{
    pdf_annot *ann;

    /* fill up annotation struct */
    ann = pdf_new_annot(p, ann_link);
    pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);
    pdf_insert_annot_params(p, ann);
    ann->dest = pdf_parse_destination_optlist(p, optlist, page, pdf_locallink);
    ann->display = disp_noprint;
}

void
pdf__add_weblink(
    PDF *p,
    pdc_scalar llx,
    pdc_scalar lly,
    pdc_scalar urx,
    pdc_scalar ury,
    const char *url)
{
    static const char *fn = "pdf__add_weblink";
    char *actoptlist;
    pdf_annot *ann;
    int acthdl;

    if (url == NULL || *url == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "url", 0, 0, 0);
    actoptlist = (char *) pdc_malloc(p->pdc, strlen(url) + 80, fn);

    /* creating a URI action */
    pdc_sprintf(p->pdc, pdc_false, actoptlist, "url {%s} ", url);
    acthdl = pdf__create_action(p, "URI", actoptlist);

    /* fill up annotation struct */
    if (acthdl > -1)
    {
        ann = pdf_new_annot(p, ann_link);
        pdf_init_rectangle(p, ann, llx, lly, urx, ury, NULL);
        pdf_insert_annot_params(p, ann);
        if (p->pdc->hastobepos) acthdl++;
        pdc_sprintf(p->pdc, pdc_false, actoptlist, "activate %d", acthdl);
        ann->action = pdc_strdup(p->pdc, actoptlist);
        ann->display = disp_noprint;
    }

    pdc_free(p->pdc, actoptlist);
}

void
pdf__set_border_style(PDF *p, const char *style, pdc_scalar width)
{
    int k;

    p->border_style = border_solid;
    if (style)
    {
        k = pdc_get_keycode_ci(style, pdf_borderstyle_keylist);
        if (k == PDC_KEY_NOTFOUND)
            pdc_error(p->pdc, PDC_E_ILLARG_STRING, "style", style, 0, 0);
        p->border_style = (pdf_borderstyle) k;
    }

    pdc_check_number_limits(p->pdc, "width", width, 0.0, PDC_FLOAT_MAX);

    p->border_width = width;
}

void
pdf__set_border_color(PDF *p, pdc_scalar red, pdc_scalar green, pdc_scalar blue)
{
    pdc_check_number_limits(p->pdc, "red", red, 0.0, 1.0);
    pdc_check_number_limits(p->pdc, "green", green, 0.0, 1.0);
    pdc_check_number_limits(p->pdc, "blue", blue, 0.0, 1.0);

    p->border_red = red;
    p->border_green = green;
    p->border_blue = blue;
}

void
pdf__set_border_dash(PDF *p, pdc_scalar b, pdc_scalar w)
{
    pdc_check_number_limits(p->pdc, "b", b, 0.0, PDC_FLOAT_MAX);
    pdc_check_number_limits(p->pdc, "w", w, 0.0, PDC_FLOAT_MAX);

    p->border_dash1 = b;
    p->border_dash2 = w;
}
