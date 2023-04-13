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
 * CID data structures
 *
 */

#ifndef FT_CID_H
#define FT_CID_H

typedef struct fnt_cmap_s fnt_cmap;
typedef struct fnt_cmap_info_s fnt_cmap_info;
typedef struct fnt_cmap_stack_s fnt_cmap_stack;

#define FNT_MAXCID 30000     /* actual maximal CID */

#define FNT_CIDMETRIC_INCR 5 /* increment of table fnt_cid_width_arrays */

#define FNT_MAX_ILLBYTES 8   /* maximal number of illegal bytes */


/* Predefined CMap info */
struct fnt_cmap_info_s
{
    const char *name;           /* CMap name */
    int charcoll;               /* character collection */
    short codesize;             /* =0: not UTF-16, =2: UTF-16, -2: HW UTF-16 */
    short compatibility;        /* PDF version */
    short supplement13;         /* supplement for PDF 1.3 */
    short supplement14;         /* supplement for PDF 1.4 */
    short supplement15;         /* supplement for PDF 1.5 */
    short supplement16;         /* supplement for PDF 1.6 */
    short vertical;             /* =1: vertical, =0: horizontal */
};

/* internal CMap types */
typedef enum
{
    cmap_code2cid,
    cmap_cid2unicode,
    cmap_code2unicode
}
fnt_cmaptype;

int fnt_get_predefined_cmap_info(const char *cmapname, fnt_cmap_info *cmapinfo);
const char *fnt_get_ordering_cid(int charcoll);
int fnt_get_maxcid(int charcoll, int supplement);
int fnt_get_charcoll(const char *ordering);
int fnt_get_supplement(fnt_cmap_info *cmapinfo, int compatibility);


#endif  /* FT_CID_H */
