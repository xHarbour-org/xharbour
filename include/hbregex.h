/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regular Expressions Interface functions
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */


#include "hbapi.h"

#if ( defined(__XCC__) || defined(__LCC__) )
   #include "source\rtl\pcre\config.h"
   #include "source\rtl\pcre\pcre_internal.h"
   #include "source\rtl\pcre\pcreposix.h"
#else
   #include "../source/rtl/pcre/config.h"
   #include "../source/rtl/pcre/pcre_internal.h"
   #include "../source/rtl/pcre/pcreposix.h"
#endif

HB_EXTERN_BEGIN

typedef struct
{
   regex_t *   pReg;
   regmatch_t  aMatches[1];
   BOOL        fFree;
   int         iCFlags;
   int         iEFlags;
} HB_REGEX;
typedef HB_REGEX * PHB_REGEX;

/* The functions */
extern HB_EXPORT regex_t * hb_getregex( PHB_ITEM pRegEx, BOOL lIgnCase, BOOL lNL, BOOL *fFree );
extern HB_EXPORT void hb_freeregex( regex_t *pReg );

extern HB_EXPORT BOOL hb_regexCompile( PHB_REGEX pRegEx, const char *szRegEx, int iCFlags, int iEFlags );
extern HB_EXPORT BOOL hb_regexGet( PHB_REGEX pRegEx, PHB_ITEM pRegExItm, int iCFlags, int iEFlags );
extern HB_EXPORT void      hb_regexFree( PHB_REGEX pRegEx );
extern HB_EXPORT BOOL      hb_regexMatch( PHB_REGEX pRegEx, const char * szString, BOOL fFull );

HB_EXTERN_END
