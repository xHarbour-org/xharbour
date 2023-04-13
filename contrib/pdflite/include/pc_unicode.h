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
 * Unicode glyph name conversion routines
 *
 */

#ifndef PC_UNICODE_H
#define PC_UNICODE_H

#define PDC_NUM_BMPVAL           0x10000
#define PDC_NUM_UNIVAL           0x110000
#define PDC_MAX_UNIVAL           0x10FFFF

#define PDC_UNICODE_HT           0x0009
#define PDC_UNICODE_LF           0x000A
#define PDC_UNICODE_VT           0x000B
#define PDC_UNICODE_FF           0x000C
#define PDC_UNICODE_CR           0x000D
#define PDC_UNICODE_ETB          0x0017
#define PDC_UNICODE_ESC          0x001B
#define PDC_UNICODE_SPACE        0x0020
#define PDC_UNICODE_QUOTMARK     0x0022
#define PDC_UNICODE_AMPERSAND    0x0026
#define PDC_UNICODE_APOSTROPHE   0x0027
#define PDC_UNICODE_HYPHEN       0x002D
#define PDC_UNICODE_PERIOD       0x002E
#define PDC_UNICODE_SEMICOLON    0x003B
#define PDC_UNICODE_LESS_THAN    0x003C
#define PDC_UNICODE_GREATER_THAN 0x003E
#define PDC_UNICODE_BACKSLASH    0x005C
#define PDC_UNICODE_LEFT_CURLY   0x007B
#define PDC_UNICODE_RIGHT_CURLY  0x007D
#define PDC_UNICODE_DELETE       0x007F
#define PDC_UNICODE_NEL          0x0085
#define PDC_UNICODE_NBSP         0x00A0
#define PDC_UNICODE_SHY          0x00AD
#define PDC_UNICODE_MACRON       0x00AF
#define PDC_UNICODE_MICRO        0x00B5
#define PDC_UNICODE_MIDDLEDOT    0x00B7
#define PDC_UNICODE_MODMACRON    0x02C9
#define PDC_UNICODE_CAPDELTA     0x0394
#define PDC_UNICODE_CAPOMEGA     0x03A9
#define PDC_UNICODE_SMALLMU      0x03BC
#define PDC_UNICODE_LS           0x2028
#define PDC_UNICODE_PS           0x2029
#define PDC_UNICODE_NNBSP        0x202F
#define PDC_UNICODE_FRACSLASH    0x2044
#define PDC_UNICODE_MMSPACE      0x205F
#define PDC_UNICODE_EURO         0x20AC
#define PDC_UNICODE_OHMSIGN      0x2126
#define PDC_UNICODE_INCREMENT    0x2206
#define PDC_UNICODE_DIVSLASH     0x2215
#define PDC_UNICODE_BULLETOP     0x2219
#define PDC_UNICODE_IDEOSPACE    0x3000

/* maximal value of Latin-1 characters */
#define PDC_UNICODE_MAXASCII     0x007F
#define PDC_UNICODE_MAXLATIN1    0x00FF

/* maximal resp. single value of Japanese HW characters */
#define PDC_UNICODE_MAXHW        0x007E
#define PDC_UNICODE_SINGHW       0x00A5

/* Unicode borders of fullwidth forms of ASCII characters */
#define PDC_UNICODE_MINFWASCII   0xFF00
#define PDC_UNICODE_MAXFWASCII   0xFF5E
#define PDC_UNICODE_DIFFWASCII   0xFEE0
                              /* PDC_UNICODE_MINFASCII - PDC_UNICODE_SPACE */

/* Unicode borders of fullwidth forms of Symbol characters */
#define PDC_UNICODE_MINFWSYMBOL  0xFFE0
#define PDC_UNICODE_MAXFWSYMBOL  0xFFE6

/* Unicode borders of Private Use Area (PUA) */
#define PDC_UNICODE_MINPUA       0xE000
#define PDC_UNICODE_MAXPUA       0xF8FF

/* Begin of PDFlib PUA */
#define PDC_UNICODE_PDFPUA       0xF200

/* Unicode borders of Unicode Corporate Use Subarea as used by Adobe Systems */
#define PDC_UNICODE_MINCUS       0xF600
#define PDC_UNICODE_MAXCUS       0xF8FF

/* Unicode Surrogate ranges */
#define PDC_UNICODE_MINHIGHSUR   0xD800
#define PDC_UNICODE_MAXHIGHSUR   0xDBFF
#define PDC_UNICODE_MINLOWSUR    0xDC00
#define PDC_UNICODE_MAXLOWSUR    0xDFFF

/* Unicode borders of higher Unicode spaces */
#define PDC_UNICODE_MINSPACE     0x2000
#define PDC_UNICODE_MAXSPACE     0x200B

/* Unicode borders of CJK compatibility forms and small form variants */
#define PDC_UNICODE_MINCJKFORMS  0xFE30
#define PDC_UNICODE_MIDCJKFORMS  0xFE48
#define PDC_UNICODE_MAXCJKFORMS  0xFE6F

/* replacement character */
#define PDC_UNICODE_REPLCHAR     0xFFFD

/* special character for CRLF */
#define PDF_UNICODE_CRLF         0xFDD0

/* not a character */
#define PDC_UNICODE_NOTCHAR      0xFFFF

/* Latin and Armenian ligatures */
#define PDC_UNICODE_CAPLIGATIJ   0x0132
#define PDC_UNICODE_SMALLLIGATIJ 0x0133
#define PDC_UNICODE_MINLIGAT     0xFB00
#define PDC_UNICODE_MAXLIGAT     0xFB17


/* The Unicode byte order mark (BOM) byte parts */
#define PDC_UNICODE_BOM          0xFEFF
#define PDF_BOM0		 0xFE
#define PDF_BOM1                 0xFF
#define PDF_BOM2                 0xEF
#define PDF_BOM3                 0xBB
#define PDF_BOM4                 0xBF

/*
 * check whether the string is UTF-16 unicode by looking for the BOM
 * in big-endian or little-endian format resp.
 * s must not be NULL.
 */
#define pdc_is_utf16be_unicode(s) \
        (((pdc_byte *)(s))[0] == PDF_BOM0 && \
         ((pdc_byte *)(s))[1] == PDF_BOM1)

#define pdc_is_utf16le_unicode(s) \
        (((pdc_byte *)(s))[0] == PDF_BOM1 && \
         ((pdc_byte *)(s))[1] == PDF_BOM0)

/*
 * check whether the string is UTF-32 unicode by looking for the BOM
 * in big-endian or little-endian format resp.
 * s must not be NULL.
 */
#define pdc_is_utf32be_unicode(s) \
        (((pdc_byte *)(s))[0] == 0x00 && \
         ((pdc_byte *)(s))[1] == 0x00 && \
         ((pdc_byte *)(s))[2] == PDF_BOM0 && \
         ((pdc_byte *)(s))[3] == PDF_BOM1)

#define pdc_is_utf32le_unicode(s) \
        (((pdc_byte *)(s))[0] == PDF_BOM1 && \
         ((pdc_byte *)(s))[1] == PDF_BOM0 && \
         ((pdc_byte *)(s))[2] == 0x00 && \
         ((pdc_byte *)(s))[3] == 0x00)

/*
 * check whether the string is UTF-8 unicode by looking for the BOM
 * s must not be NULL.
 */
#define pdc_is_utf8_unicode(s) \
        (((pdc_byte *)(s))[0] == PDF_BOM2 && \
         ((pdc_byte *)(s))[1] == PDF_BOM3 && \
         ((pdc_byte *)(s))[2] == PDF_BOM4)


#define PDC_UTF8_STRING "\xEF\xBB\xBF"
#define pdc_is_utf8_bytecode(s) \
        (((pdc_byte *)(s))[0] == PDF_BOM2 && \
         ((pdc_byte *)(s))[1] == PDF_BOM3 && \
         ((pdc_byte *)(s))[2] == PDF_BOM4)
#define pdc_copy_utf8_bom(s) \
         ((pdc_byte *)(s))[0] = PDF_BOM2, \
         ((pdc_byte *)(s))[1] = PDF_BOM3, \
         ((pdc_byte *)(s))[2] = PDF_BOM4;
#define PDC_UTF8 pdc_utf8
#define PDC_UTF8_STRG "utf8"
#define PDC_UTF8_FLAG pdc_false


#define PDC_HTML_CTRLCHAR     '&'
#define PDC_HTML_DELIMITCHAR  ';'

typedef enum
{
    conversionOK,       /* conversion successful */
    sourceExhausted,    /* partial character in source, but hit end */
    targetExhausted,    /* insuff. room in target for conversion */
    sourceIllegal       /* source sequence is illegal/malformed */
}
pdc_convers_result;

typedef enum
{
    strictConversion = 0,
    lenientConversion
}
pdc_convers_flags;

/* flags for pdc_convert_string(), pdc_strdup_ext(),
 * pdc_utfxx6_to_utfxx(), pdc_convert_name_ext()
 */
#define PDC_CONV_FORCEUTF16 (1<<0)
#define PDC_CONV_TRY7BYTES  (1<<1)
#define PDC_CONV_TRYBYTES   (1<<2)
#define PDC_CONV_WITHBOM    (1<<3)
#define PDC_CONV_NOBOM      (1<<4)
#define PDC_CONV_AUTOBOM    (1<<5)
#define PDC_CONV_ANALYZE    (1<<6)
#define PDC_CONV_TMPALLOC   (1<<7)
#define PDC_CONV_HTMLCHAR   (1<<8)
#define PDC_CONV_NEWALLOC   (1<<9)
#define PDC_CONV_INFLATE    (1<<10)
#define PDC_CONV_ESCSEQU    (1<<11)
#define PDC_CONV_BSSEQU     (1<<12)
#define PDC_CONV_EBCDIC     (1<<13)
#define PDC_CONV_ENCERROR   (1<<14)
#define PDC_CONV_KEEPLBCHAR (1<<15)
#define PDC_CONV_LOGGING    (1<<16)
#define PDC_CONV_ISUTF8     (1<<17)
#define PDC_CONV_ASCII      (1<<18)
#define PDC_CONV_MAXSTRLEN  (1<<19)
#define PDC_CONV_FILENAME   (1<<20)


/* DON'T change the order */
typedef enum
{
    pdc_auto       = 1,
    pdc_auto2      = 2,
    pdc_bytes      = 3,
    pdc_bytes2     = 4,
    pdc_utf8       = 5,    /* UTF-8 */

    pdc_utf16      = 7,    /* UTF-16 */
    pdc_utf16be    = 8,    /* UTF-16 big endian */
    pdc_utf16le    = 9,    /* UTF-16 little endian */
    pdc_utf32      = 10    /* UTF-32 */
}
pdc_text_format;

/* copy for pdflib in p_keyconn.h */
#if defined(PC_UNICODE_C)
static const pdc_keyconn pdc_textformat_keylist[] =
{
    {"auto",       pdc_auto},
    {"auto2",      pdc_auto2},
    {"bytes",      pdc_bytes},
    {"bytes2",     pdc_bytes2},
    {"utf8",       pdc_utf8},
    {"utf16",      pdc_utf16},
    {"utf16be",    pdc_utf16be},
    {"utf16le",    pdc_utf16le},
    {NULL, 0}
};
#endif /* PC_UNICODE_C */

const char *pdc_get_textformat(int textformat);

int pdc_convert_string(pdc_core *pdc,
    pdc_text_format inutf, int codepage, pdc_encodingvector *inev,
    pdc_byte *instring, int inlen, pdc_text_format *oututf_p,
    pdc_encodingvector *outev, pdc_byte **outstring, int *outlen, int flags,
    pdc_bool verbose);

int pdc_convert_textstring(pdc_core *pdc,
    pdc_text_format inutf, int codepage, pdc_encodingvector *inev,
    const pdc_glyph_tab *glyphtab, int tabsize, int replchar,
    pdc_byte *instring, int inlen,
    pdc_text_format *oututf_p, pdc_encodingvector *outev,
    pdc_byte **outstring, int *outlen, int flags,
    pdc_bool verbose);

char *pdc_convert_name(pdc_core *pdc, const char *name, int len, int flags);
char *pdc_convert_name_ext(pdc_core *pdc, const char *name, int len,
                           pdc_encoding enc, int codepage, int flags);

char *pdc_utf8_to_hostbytes(pdc_core *pdc, pdc_bool honorlang, char *name);
char *pdc_hostbytes_to_utf8(pdc_core *pdc, pdc_bool honorlang, char *name);

char *pdc_utf16_to_utf8(pdc_core *pdc, const char *utf16string, int len,
                        int flags, int *size);
char *pdc_utf8_to_utf16(pdc_core *pdc, const char *utf8string,
                        const char *format, int flags, int *size);
char *pdc_utf16_to_utf32(pdc_core *pdc, const char *utf16string, int len,
                         int *size);
char *pdc_utf32_to_utf8(pdc_core *pdc, const char *utf32string, int len,
                        int flags, int *size);
char *pdc_utf32_to_utf16(pdc_core *pdc, const char *utf32string, int len,
                         const char *format, int flags, int *size);
int pdc_char16_to_char32(pdc_core *pdc, const pdc_ushort *ustext, int *ic,
                         int len, pdc_bool verbose);
int pdc_char32_to_char16(pdc_core *pdc, int usv, pdc_ushort *uvlist,
                         pdc_bool verbose);

#endif /* PC_UNICODE_H */
