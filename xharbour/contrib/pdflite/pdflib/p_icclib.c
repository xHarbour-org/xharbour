/* $Id$
 *
 * ICClib routines for PDFlib, slightly modified from the original ICClib.
 * (see below).
 *
 * $Log: p_icclib.c,v $
 * Revision 1.18.8.4  2007/11/22 17:54:38  kurt
 * bug #1538 (Relax date check for embedded ICC profile) fixed
 *
 * Revision 1.18.8.3  2007/05/23 21:15:00  york
 * fixed #1230: "Performance problems in mulithreaded env with tolower/toupper".
 *
 * Revision 1.18.8.2  2007/05/23 19:37:21  york
 * implemented pdc_isXXX() character classification macros.
 * started to replace isXXX() with pdc_isXXX() in all modules.
 *
 * Revision 1.18.8.1  2007/03/28 12:47:10  kurt
 * bug #1180 (Function prefixes missing for zlib assembler and ICC code) fixed
 * names of all external functions of ICClib have the prefix "pdf_" now
 *
 * Revision 1.18  2004/08/05 09:11:27  rjs
 * merged 6.0.x to pdflib-x
 *
 * Revision 1.17.2.1  2004/07/30 16:14:30  kurt
 * icc_read: all free statements in the error case removed
 *           (because of program crash in icc_delete)
 * icc_delete: more security checks
 * new public function: icc_get_errmsg
 *
 */

/*
 * International Color Consortium Format Library (icclib)
 * For ICC profile version 3.4
 *
 * Author:  Graeme W. Gill
 * Date:    2002/04/22
 * Version: 2.02
 *
 * Copyright 1997 - 2002 Graeme W. Gill
 * See Licence.txt file for conditions of use.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#ifdef __sun
#include <unistd.h>
#endif
#if defined(__IBMC__) && defined(_M_IX86)
#include <float.h>
#endif

/* PDFlib */
#include "pc_util.h"
#include "pc_core.h"
#include "pc_ctype.h"
