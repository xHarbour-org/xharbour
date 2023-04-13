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
 * Definitions for file routines
 *
 */

#ifndef PC_FILE_H
#define PC_FILE_H


#define PDC_FILENAMELEN  1024    /* maximum file name length */

#define PDC_FILE_TEXT       (1L<<0)  /* text file - whether ASCII file or not
                                      * depends on pdc->asciifile */

#define PDC_FILE_ASCII      (1L<<1)  /* treat text or binary file as ASCII file
                                      * even on EBCDIC platforms */

#define PDC_FILE_BINARY     (1L<<2)  /* open as binary file,
                                      * otherwise as text file */

#define PDC_FILE_WRITEMODE  (1L<<10) /* open file in writing mode,
                                      * otherwise in reading mode */

#define PDC_FILE_APPENDMODE (1L<<11) /* open file in appending mode */


/* flags for pdc_read_textfile() */

#define PDC_FILE_BSSUBST    (1<<0)   /* backslash substitution */
#define PDC_FILE_KEEPLF     (1<<1)   /* keep linefeed at line continuation */

#define PDC_BUFSIZE 1024

#define PDC_OK_FREAD(file, buffer, len) \
    (pdc_fread(buffer, 1, len, file) == len)

typedef struct pdc_file_s pdc_file;

/* pc_file.c */

#if defined(WIN32)
int pdc_set_maxfilehandles(pdc_core *pdc, int maxfps);
int pdc_get_maxfilehandles(void);
#endif

int		pdc__fseek(FILE *fp, pdc_off_t offset, int whence);
pdc_off_t	pdc__ftell(FILE *fp);
size_t		pdc__fread(void *ptr, size_t size, size_t nmemb, FILE *fp);
size_t          pdc__fwrite(const void *ptr, size_t size, size_t nmemb,
                            FILE *fp);

#define         pdc__fgetc(fp)  fgetc(fp)
#define         pdc__feof(fp)   feof(fp)

FILE   *pdc_get_fileptr(pdc_file *sfp);
pdc_core   *pdc_get_pdcptr(pdc_file *sfp);
int     pdc_get_fopen_errnum(pdc_core *pdc, int errnum);
void    pdc_set_fopen_errmsg(pdc_core *pdc, int errnum, const char *qualifier,
                            const char *filename);
void    pdc_set_fwrite_errmsg(pdc_core *pdc, const char *filename);
pdc_bool pdc_check_fopen_errmsg(pdc_core *pdc, pdc_bool requested);

void    *pdc_read_file(pdc_core *pdc, FILE *fp, pdc_off_t *o_filelen,
			    int incore);
int     pdc_read_textfile(pdc_core *pdc, pdc_file *sfp, int flags,
                          char ***linelist);
char *	pdc_temppath(pdc_core *pdc, char *outbuf, const char *inbuf,
			    size_t inlen, const char *dirname);

char *pdc_check_filename(pdc_core *pdc, char *filename);
char *pdc_get_filename(pdc_core *pdc, char *filename);
const char *pdc_convert_filename_ext(pdc_core *pdc, const char *filename,
        int len, const char *paramname, pdc_encoding enc, int codepage,
        int flags);
const char *pdc_convert_filename(pdc_core *pdc, const char *filename, int len,
        const char *paramname, pdc_bool withbom);
FILE *pdc_fopen_logg(pdc_core *pdc, const char *filename, const char *mode);

pdc_file *	pdc_fopen(pdc_core *pdc, const char *filename,
		    const char *qualifier, const pdc_byte *data,
		    size_t size, int flags);
pdc_core *	pdc_file_getpdc(pdc_file *sfp);
char   *	pdc_file_name(pdc_file *sfp);
pdc_off_t	pdc_file_size(pdc_file *sfp);
pdc_bool	pdc_file_isvirtual(pdc_file *sfp);
char   *	pdc_fgetline(char *s, int size, pdc_file *sfp);
pdc_off_t	pdc_ftell(pdc_file *sfp);
int		pdc_fseek(pdc_file *sfp, pdc_off_t offset, int whence);
size_t		pdc_fread(void *ptr, size_t size, size_t nmemb, pdc_file *sfp);
const void *	pdc_freadall(pdc_file *sfp, size_t *filelen,
		    pdc_bool *ismem);
size_t          pdc_fwrite(const void *ptr, size_t size, size_t nmemb,
                     pdc_file *sfp);
void            pdc_freset(pdc_file *sfp, size_t size);

int     pdc_ungetc(int c, pdc_file *sfp);
int     pdc_fgetc(pdc_file *sfp);
int     pdc_feof(pdc_file *sfp);
void    pdc_fclose(pdc_file *sfp);
void    pdc_fclose_logg(pdc_core *pdc, FILE *fp);
void    pdc_file_fullname(pdc_core *pdc, const char *dirname,
                const char *basename, char *fullname);
char *pdc_file_fullname_mem(pdc_core *pdc, const char *dirname,
        const char *basename);
char *pdc_file_concat(pdc_core *pdc, const char *dirname, const char *basename,
        const char *extension);
const char *pdc_file_strip_dirs(const char *pathname);
char *pdc_file_strip_name(char *pathname);
char *pdc_file_strip_ext(char *pathname);

size_t pdc_fwrite_ascii(pdc_core *pdc, const char *str, size_t len, FILE *fp);
size_t pdc_write_file(pdc_core *pdc, const char *filename,
        const char *qualifier, const char *content, size_t len, int flags);


/* pc_resource.c */

pdc_file *pdc_fsearch_fopen(pdc_core *pdc, const char *filename, char *fullname,
        const char *qualifier, int flags);

#endif  /* PC_FILE_H */
