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
 * PDFlib output defines and routines
 *
 */

#ifndef PC_OUTPUT_H
#define PC_OUTPUT_H

/* Define to test special MVS output features */
#undef MVS_TEST

/* -------------------- some ASCII characters and strings ------------- */

#define PDF_NEWLINE             ((char) 0x0A)           /* ASCII '\n' */
#define PDF_RETURN              ((char) 0x0D)           /* ASCII '\r' */
#define PDF_SPACE               ((char) 0x20)           /* ASCII ' '  */
#define PDF_HASH                ((char) 0x23)           /* ASCII '#'  */
#define PDF_PARENLEFT           ((char) 0x28)           /* ASCII '('  */
#define PDF_PARENRIGHT          ((char) 0x29)           /* ASCII ')'  */
#define PDF_PLUS                ((char) 0x2B)           /* ASCII '+'  */
#define PDF_SLASH               ((char) 0x2F)           /* ASCII '/'  */
#define PDF_COLON               ((char) 0x3A)           /* ASCII ':'  */
#define PDF_BACKSLASH           ((char) 0x5C)           /* ASCII '\\' */

#define PDF_A                   ((char) 0x41)           /* ASCII 'A'  */
#define PDF_n                   ((char) 0x6E)           /* ASCII 'n'  */
#define PDF_r                   ((char) 0x72)           /* ASCII 'r'  */

#define PDF_STRING_0123456789ABCDEF     \
        "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x41\x42\x43\x44\x45\x46"


/* ------------------------ some PDF constant -------------------------- */

/* Acrobat viewers change absolute values < 1/65536 to zero */
#define PDF_SMALLREAL   (0.000015)

/* Acrobat viewers have an upper limit on real and integer numbers */
#define PDF_BIGREAL     (32768.0)
#define PDF_BIGINT      (2147483647.0)

/* maximum capacity of a dictionary, in entries */
#define PDF_MAXDICTSIZE   (4095)

/* maximum capacity of an array, in elements */
#define PDF_MAXARRAYSIZE  (8191)

/* maximum capacity of a text string in content stream for Tj, in bytes
 * PDF Reference, TABLE C.1: 32767
 * But an error will occur: "Token type not recognized"
 */
#define PDF_MAXTEXTSIZE   (32763)

/* maximum capacity of a string, in bytes */
#define PDF_MAXSTRINGSIZE  (65535)

/* maximum capacity of indirect objects */
#define PDF_MAXINDOBJS  (8388607)

/* Acrobat limit for page dimensions */
#define PDF_ACRO_MINPAGE       (3.0)           /* 1/24 inch = 0.106 cm */
#define PDF_ACRO_MAXPAGE       (14400.0)       /* 200  inch = 508 cm   */

/* PDF versions */
#define PDC_1_1                 11              /* PDF 1.1 = Acrobat 2 */
#define PDC_1_2                 12              /* PDF 1.2 = Acrobat 3 */
#define PDC_1_3                 13              /* PDF 1.3 = Acrobat 4 */
#define PDC_1_4                 14              /* PDF 1.4 = Acrobat 5 */
#define PDC_1_5                 15              /* PDF 1.5 = Acrobat 6 */
#define PDC_1_6                 16              /* PDF 1.6 = Acrobat 7 */
#define PDC_1_7                 17              /* PDF 1.7 = Acrobat 8 */
#define PDC_X_X_LAST            17


/* ------------------- some defines for special PDFs ----------------------- */




/* ------------------- some special enumerations -------------------------- */


typedef enum
{
    pdc_pbox_none,
    pdc_pbox_art,
    pdc_pbox_bleed,
    pdc_pbox_crop,
    pdc_pbox_media,
    pdc_pbox_trim
}
pdc_pagebox;


/* ----------------------- PDF output ---------------------------- */

typedef struct pdc_output_s pdc_output;

typedef enum
{
    pdc_flush_none      = 0,            /* end of document */
    pdc_flush_page      = 1<<0,         /* after page */
    pdc_flush_content   = 1<<1,         /* font, xobj, annots */

    /* temporary workaround; see bugzilla #167.
    */
    /* pdc_flush_heavy  = 1<<4             before realloc attempt */
    pdc_flush_heavy     = pdc_flush_page | pdc_flush_content
}
pdc_flush_state;

/* output control.
*/
typedef struct
{
    /* exactly one of the members 'filename', 'fp', and 'writeproc'
    ** must be supplied, the others must be NULL:
    **
    ** filename		use supplied file name to create a named output file
    **			filename == "" means generate output in-core
    ** fp		use supplied FILE * to write to file
    ** writeproc	use supplied procedure to write output data
    */
    const char *filename;
    FILE *	fp;
    size_t    (*writeproc)(pdc_output *out, void *data, size_t size);

    pdc_flush_state flush;




#if defined(MVS) || defined(MVS_TEST)
    const char *fopenparams;            /* additional fopen() parameters */
    int		recordsize;		/* file record size */
#endif
} pdc_outctl;


/* ----------- service function to get PDF version string  -------------- */

const char *pdc_get_pdfversion(pdc_core *pdc, int compatibility);


/* --------------------------- Setup --------------------------- */

pdc_output *	pdc_boot_output(pdc_core *pdc);
void		pdc_init_outctl(pdc_outctl *oc);
pdc_bool	pdc_init_output(void *opaque, pdc_output *out,
		    int compatibility, pdc_outctl *oc);
void		pdc_cleanup_output(pdc_output *out, pdc_bool keep_buf);
void *		pdc_get_opaque(pdc_output *out);

/* --------------------------- Digest --------------------------- */

void		pdc_init_digest(pdc_output *out);
void		pdc_update_digest(pdc_output *out, unsigned char *input,
		    unsigned int len);
void		pdc_finish_digest(pdc_output *out, pdc_bool settime);
unsigned char * pdc_get_digest(pdc_output *out);


/* --------------------------- Objects and ids --------------------------- */

pdc_id	pdc_alloc_id(pdc_output *out);
pdc_id	pdc_map_id(pdc_output *out, pdc_id old_id);
void	pdc_mark_free(pdc_output *out, pdc_id obj_id);

pdc_id	pdc_begin_obj(pdc_output *out, pdc_id obj_id);
#define pdc_end_obj(out)		pdc_puts(out, "endobj\n")

#define PDC_NEW_ID	0L
#define PDC_BAD_ID	-1L
#define PDC_FREE_ID	-2L


/* --------------------------- Strings --------------------------- */
/* output a string (including parentheses) and quote all required characters */
void	pdc_put_pdfstring(pdc_output *out, const char *text, int len);
void    pdc_put_pdffilename(pdc_output *out, const char *text, int len);


/* --------------------------- Names --------------------------- */
/* output a PDF name (including leading slash) and quote all required chars */
void    pdc_put_pdfname(pdc_output *out, const char *text, size_t len);


/* --------------------------- Arrays  --------------------------- */
#define pdc_begin_array(out)	pdc_puts(out, "[")
#define pdc_end_array(out)      pdc_puts(out, "]\n")
#define pdc_end_array_c(out)    pdc_puts(out, "]")


/* --------------------------- Dictionaries --------------------------- */
#define pdc_begin_dict(out)	pdc_puts(out, "<<")
#define pdc_end_dict(out)       pdc_puts(out, ">>\n")
#define pdc_end_dict_c(out)     pdc_puts(out, ">>")


/* --------------------------- Object References --------------------------- */
#define pdc_objref(out, name, obj_id)       \
		pdc_printf(out, "%s %ld 0 R\n", name, obj_id)

#define pdc_objref_c(out, obj_id)       \
		pdc_printf(out, " %ld 0 R", obj_id)

#define pdc_objref_c2(out, obj_id, gen_no)       \
		pdc_printf(out, " %ld %d R", obj_id, gen_no)

/* --------------------------- Streams --------------------------- */
void		pdc_begin_pdfstream(pdc_output *out);
void		pdc_end_pdfstream(pdc_output *out);
pdc_off_t	pdc_get_pdfstreamlength(pdc_output *out);
void		pdc_put_pdfstreamlength(pdc_output *out, pdc_id length_id);

int		pdc_get_compresslevel(pdc_output *out);
void		pdc_set_compresslevel(pdc_output *out, int compresslevel);



/* --------------------------- Document sections  --------------------------- */
void		pdc_write_xref(pdc_output *out);

void		pdc_write_digest(pdc_output *out);
void		pdc_write_trailer(pdc_output *out, pdc_id info_id,
		    pdc_id root_id, int root_gen, long xref_size,
		    pdc_off_t xref_prev, pdc_off_t xref_pos);
void		pdc_write_eof(pdc_output *out);


/* --------------------------- Low-level output --------------------------- */
void		pdc_flush_stream(pdc_output *out);
pdc_off_t	pdc_tell_out(pdc_output *out);
void		pdc_close_output(pdc_output *out);
							/* TODO2GB: size_t? */
char *		pdc_get_stream_contents(pdc_output *out, pdc_off_t *size);
int		pdc_stream_not_empty(pdc_output *out);

void		pdc_write(pdc_output *out, const void *data, size_t size);
void		pdc_puts(pdc_output *out, const char *s);
void		pdc_putc(pdc_output *out, const char c);


/* ------------------------- High-level output ------------------------- */
void		pdc_printf(pdc_output *out, const char *fmt, ...);

#endif	/* PC_OUTPUT_H */

