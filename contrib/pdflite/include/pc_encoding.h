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
 * Encoding data structures and routines
 *
 */

#ifndef PC_ENCODING_H
#define PC_ENCODING_H

/*
 * Symbolic names for predefined font encodings. 0 and above are used
 * as indices in the pdc_encodingvector array. The encodings starting at
 * pdc_firstvarenc have no enumeration name, because they are loaded
 * dynamically.
 * The predefined encodings must not be changed or rearranged.
 * The order of encodings here must match that of pdc_core_encodings
 * and pdc_fixed_encoding_names in pc_encoding.c.
 */
typedef enum
{
    pdc_invalidenc = -5,
    pdc_glyphid = -4,
    pdc_unicode = -3,
    pdc_builtin = -2,
    pdc_cid = -1,

    pdc_winansi = 0,
    pdc_macroman = 1,
    pdc_macroman_apple = 2,
    pdc_ebcdic = 3,
    pdc_ebcdic_37 = 4,
    pdc_ebcdic_winansi = 5,
    pdc_pdfdoc = 6,
    pdc_stdenc = 7,
    pdc_macexpert = 8,
    pdc_firstvarenc = 9,

    pdc_encmax = PDC_INT_MAX
}
pdc_encoding;

/* Predefined character collections */
typedef enum
{
    cc_none = 0,
    cc_japanese,
    cc_simplified_chinese,
    cc_traditional_chinese,
    cc_korean,
    cc_identity,
    cc_unknown
}
pdc_charcoll;

#define PDC_NUMCHARCOLL 4

/* treatment of non-resolvable character references */
typedef enum
{
    text_nocheck  = -2,
    text_error    = -1,
    text_replace  = 0
}
pdc_glyphcheck;



#define PDC_EBCDIC_NAME "ebcdic"
#define PDC_EBCDIC_ENC pdc_ebcdic

typedef struct pdc_charclass_tab_s pdc_charclass_tab;
typedef struct pdc_code_map_s pdc_code_map;
typedef struct pdc_encoding_info_s pdc_encoding_info;
typedef struct pdc_encoding_stack_s pdc_encoding_stack;
typedef struct pdc_encodingvector_s pdc_encodingvector;
typedef struct pdc_priv_glyphtab_s pdc_priv_glyphtab;
typedef struct pdc_glyph_tab_s pdc_glyph_tab;

struct pdc_encodingvector_s
{
    char *apiname;              /* PDFlib's name of the encoding at the API */
    pdc_ushort codes[256];      /* unicode values */
    char *chars[256];           /* character names */
    char given[256];            /* flags for kind of given character name */
    pdc_byte *sortedslots;      /* slots for sorted unicode values */
    int nslots;                 /* number of sorted slots */
    unsigned long flags;        /* flags, see PDC_ENC_... */
};

struct pdc_encoding_info_s
{
    pdc_encodingvector *ev;     /* encoding vector */
    pdc_id id;                  /* encoding object id */
    pdc_id tounicode_id;        /* tounicode object ids */
    pdc_bool used_in_formfield; /* encoding is in use in form field */
    pdc_bool stored;            /* encoding is stored in PDF */
};

struct pdc_code_map_s
{
    pdc_ushort src;             /* source code */
    pdc_ushort dst;             /* destination code */
};

struct pdc_glyph_tab_s
{
    pdc_ushort code;
    const char *name;
};


#define PDC_ENC_INCORE      (1L<<0) /* encoding from in-core */
#define PDC_ENC_FILE        (1L<<1) /* encoding from file */
#define PDC_ENC_HOST        (1L<<2) /* encoding from host system */
#define PDC_ENC_USER        (1L<<3) /* encoding from user */
#define PDC_ENC_FONT        (1L<<4) /* encoding from font resp. for a font*/
#define PDC_ENC_GENERATE    (1L<<5) /* encoding generated from Unicode page */
#define PDC_ENC_USED        (1L<<6) /* encoding already used */
#define PDC_ENC_SETNAMES    (1L<<7) /* character names are set */
#define PDC_ENC_ALLOCCHARS  (1L<<8) /* character names are allocated */
#define PDC_ENC_STDNAMES    (1L<<9) /* character names are all Adobe standard */
#define PDC_ENC_TEMP       (1L<<10) /* temporary generated encoding */

#define PDC_ENC_MODSEPAR     "_"          /* separator of modified encoding */
#define PDC_ENC_MODWINANSI   "winansi_"   /* prefix of modified winansi enc */
#define PDC_ENC_MODMACROMAN  "macroman_"  /* prefix of modified macroman enc */
#define PDC_ENC_MODEBCDIC    "ebcdic_"    /* prefix of modified ebcdic enc */
#define PDC_ENC_ISO8859      "iso8859-"   /* begin of iso8859 enc name */
#define PDC_ENC_CP125        "cp125"      /* begin of ANSI enc name */

#define PDC_ENC_TEMPNAME "__temp__enc__"  /* name of temporary encoding */

#define PDC_ENCNAME_LEN PDC_FILENAMELEN

/* Adobe glyph names can have maximal 7 components */
#define PDC_MAX_UVLIST 8

/* maximal length of glyph names */
#define PDC_CHARREF_MAXNAMLEN 64

/* types of glyph names */
#define PDC_GLF_ISUNDEF     (1<<0)  /* is undefined (unknown Unicode(s) */
#define PDC_GLF_ISAGL12NAME (1<<1)  /* is AGL 1.2' name (without ambiguity) */
#define PDC_GLF_ISAGL20NAME (1<<2)  /* is AGL 2.0 and not AGL 1.2' */
#define PDC_GLF_ISZADBNAME  (1<<3)  /* is ZapfDingbats name */
#define PDC_GLF_ISUNINAME   (1<<4)  /* is a "uni" name (with single Unicode) */
#define PDC_GLF_ISAMBIG     (1<<5)  /* is ambiguous name (double mapping) */
#define PDC_GLF_ISVARIANT   (1<<6)  /* is glyphic variant (contains period) */
#define PDC_GLF_ISDECOMP    (1<<7)  /* is decomposed glyph (contains underscores
                                     * or more than one Unicode values) */
#define PDC_GLF_ISCUS       (1<<8)  /* is a glyph from Unicode's Corporate
                                     * Use Subarea (CUS) used by Adobe
                                     * (U+F600 - U+F8FF) */
#define PDC_GLF_ISLIGATURE  (1<<9)  /* is a Latin or Armenian ligature glyph */
#define PDC_GLF_ISSURROGAT (1<<10)  /* is a surrogate glyph */
#define PDC_GLF_ISMISNAMED (1<<11)  /* is misnamed (see tab_misnamed2uni) */
#define PDC_GLF_ISINCORE   (1<<12)  /* is incore (AGL, ZapfDingabts, misnamed)*/
#define PDC_GLF_ISPRIVATE  (1<<13)  /* is private glyph (in supplied glyphtab)
                                     * or a heuristic determined character */

#define PDC_GLF_REDVARIANT  (1<<0)  /* reduce glyphic variant */
#define PDC_GLF_DECOMPNAME  (1<<1)  /* decompose glyph name */
#define PDC_GLF_CONVUNINAME (1<<2)  /* convert unixxxx name */
#define PDC_GLF_RESOLCUS    (1<<3)  /* resolve CUS value */
#define PDC_GLF_RESOLLIGAT  (1<<4)  /* resolve ligature value */
#define PDC_GLF_ALTGREEKMAP (1<<5)  /* take alternative greek mapping */
#define PDC_GLF_ALTMAPPING  (1<<6)  /* take alternative mapping */
#define PDC_GLF_STDTYPE3MAP (1<<7)  /* standard Type3 glyph name mapping */

/* standard flags */
#define PDC_GLF_STANDARD1  (PDC_GLF_REDVARIANT | PDC_GLF_DECOMPNAME | \
                            PDC_GLF_CONVUNINAME | PDC_GLF_RESOLCUS | \
                            PDC_GLF_RESOLLIGAT)

/* standard flags with keeping standard ligatures and CUS values */
#define PDC_GLF_STANDARD2  (PDC_GLF_REDVARIANT | PDC_GLF_DECOMPNAME | \
                            PDC_GLF_CONVUNINAME)

/* pc_chartabs.c */
int pdc_glyphname2codelist(const char *glyphname, const pdc_glyph_tab *glyphtab,
    int tabsize, pdc_ushort *codelist);
int pdc_glyphname2code(const char *glyphname, const pdc_glyph_tab *glyphtab,
    int tabsize);
const char *pdc_code2glyphname(pdc_ushort code, const pdc_glyph_tab *glyphtab,
    int tabsize);
int pdc_code2codelist(pdc_core *pdc, pdc_ushort code,
    const pdc_code_map *codemap, int tabsize, pdc_ushort *codelist,
    int listsize);
const char *pdc_glyphname2glyphname(const char *glyphname,
    const pdc_glyph_tab *glyphtab, int tabsize);

int pdc_adobe2unicode(const char *glyphname);
const char *pdc_unicode2adobe(pdc_ushort uv);
const char *pdc_get_notdef_glyphname(void);
int pdc_zadb2unicode(const char *glyphname);
const char *pdc_unicode2zadb(pdc_ushort uv);
int pdc_newadobe2unicodelist(const char *glyphname, pdc_ushort *uvlist);
const char *pdc_unicode2newadobe(pdc_ushort uv);
const char *pdc_get_newadobe_glyphname(const char *glyphname);
int pdc_glyphname2altunicode(const char *glyphname);

pdc_bool pdc_is_std_charname(const char *glyphname);
void pdc_delete_missingglyph_bit(pdc_ushort uv, pdc_ulong *bmask);
pdc_ushort pdc_get_alter_glyphname(pdc_ushort uv, pdc_ulong bmask,
        char **glyphname);
int pdc_string2unicode(pdc_core *pdc, const char *text, int i_flags,
                              const pdc_keyconn *keyconn, pdc_bool verbose);
pdc_bool pdc_is_linebreaking_relchar(pdc_ushort uv);



/* pc_core.c */
void pdc_set_encodingstack_ptr(pdc_core *pdc, pdc_encoding_stack *encstack);
pdc_encoding_stack *pdc_get_encodingstack_ptr(pdc_core *pdc);
void pdc_set_pglyphtab_ptr(pdc_core *pdc, pdc_priv_glyphtab *pglyphtab);
pdc_priv_glyphtab *pdc_get_pglyphtab_ptr(pdc_core *pdc);

/* pc_encoding.c */
void pdc_init_encoding(pdc_core *pdc, pdc_encodingvector *ev,
                       const char *name);
pdc_encodingvector *pdc_new_encoding(pdc_core *pdc, const char *name);
void pdc_refresh_encoding(pdc_core *pdc, pdc_encodingvector *ev,
        const char *name);
void pdc_cleanup_encoding(pdc_core *pdc, pdc_encodingvector *ev);
pdc_encodingvector *pdc_copy_encoding(pdc_core *pdc, pdc_encodingvector *evfrom,
        const char *name);
int pdc_get_encoding_bytecode(pdc_core *pdc, pdc_encodingvector *ev,
                              pdc_ushort uv);
pdc_byte pdc_transform_bytecode(pdc_core *pdc, pdc_encodingvector *evto,
                                pdc_encodingvector *evfrom, pdc_byte code);
pdc_encodingvector *pdc_copy_core_encoding(pdc_core *pdc, const char *encoding);
const char *pdc_get_fixed_encoding_name(pdc_encoding enc);


pdc_encodingvector *pdc_read_encoding(pdc_core *pdc, const char *encoding,
        const char *filename, pdc_bool verbose);
pdc_encodingvector *pdc_generate_encoding(pdc_core *pdc, const char *encoding);
void pdc_encoding_logg_protocol(pdc_core *pdc, pdc_encodingvector *ev);

pdc_encoding_stack *pdc_new_encodingstack(pdc_core *pdc);
void pdc_delete_encodingstack(pdc_core *pdc);
pdc_encoding pdc_insert_encoding_vector(pdc_core *pdc,
        pdc_encodingvector *ev);
pdc_encoding pdc_get_encoding(pdc_core *pdc, const char *encoding,
        int *codepage, pdc_bool verbose);
pdc_encoding pdc_insert_encoding(pdc_core *pdc, const char *encoding,
        int *codepage, pdc_bool verbose);
void pdc_remove_encoding_vector(pdc_core *pdc, int slot);
pdc_encoding pdc_find_encoding(pdc_core *pdc, const char *encoding);
void pdc_set_encoding_glyphnames(pdc_core *pdc, pdc_encoding enc);
pdc_bool pdc_get_encoding_isstdflag(pdc_core *pdc, pdc_encoding enc);
pdc_bool pdc_is_encoding_subset(pdc_core *pdc, pdc_encodingvector *testev,
        pdc_encodingvector *refev);

int pdc_get_encodingstack_number(pdc_core *pdc);
pdc_encoding_info *pdc_get_encoding_info(pdc_core *pdc,
        pdc_encoding enc);
pdc_encodingvector *pdc_get_encoding_vector(pdc_core *pdc,
        pdc_encoding enc);
pdc_encodingvector *pdc_get_encoding_vector_direct(pdc_core *pdc,
        pdc_encoding enc);
const char *pdc_get_user_encoding(pdc_core *pdc, pdc_encoding enc);
void pdc_init_encoding_info_ids(pdc_core *pdc);

const char *pdc_get_pdf_encoding_name(int enc);
int pdc_get_pdf_encoding_code(const char *encname);
pdc_encodingvector *pdc_generate_pdfencoding(pdc_core *pdc,
        const char *pdfname);

pdc_priv_glyphtab *pdc_new_pglyphtab(pdc_core *pdc);
void pdc_delete_pglyphtab(pdc_core *pdc);
pdc_ushort pdc_register_glyphname(pdc_core *pdc,
        const char *glyphname, pdc_ushort uv, pdc_bool forcepua);
int pdc_privglyphname2unicode(pdc_core *pdc, const char *glyphname);
int pdc_glyphname2unicodelist(pdc_core *pdc, const char *glyphname,
        pdc_ushort *uvlist);
int pdc_glyphname2unicode(pdc_core *pdc, const char *glyphname);
int pdc_glyphname2utf32(pdc_core *pdc, const char *glyphname);
const char *pdc_glyphname2privglyphname(pdc_core *pdc, const char *glyphname);
const char *pdc_unicode2glyphname(pdc_core *pdc, pdc_ushort uv);
pdc_ushort pdc_insert_glyphname(pdc_core *pdc, const char *glyphname);
const char *pdc_insert_unicode(pdc_core *pdc, pdc_ushort uv);


#endif  /* PC_ENCODING_H */
