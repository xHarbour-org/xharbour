/*
 * $Id: hbapicdp.h,v 1.1 2003/05/16 19:52:06 druzus Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_APICDP_H_
#define HB_APICDP_H_

#include <ctype.h>
#include "hbapi.h"
#include "hbinit.h"

/* This hack is needed to force preprocessing if id is also a macro */
#define HB_CODEPAGE_REQUEST( id )           HB_CODEPAGE_REQUEST_( id )
#define HB_CODEPAGE_REQUEST_( id )          extern HB_FUNC( HB_CODEPAGE_##id ); \
                                        void hb_codepage_ForceLink( void ) \
                                        { \
                                           HB_FUNCNAME( HB_CODEPAGE_##id )(); \
                                        }

#define HB_CODEPAGE_ANNOUNCE( id )          HB_FUNC( HB_CODEPAGE_##id ) {}

typedef struct _HB_MULTICHAR
{
   char  cLast[2];
   char  cFirst[2];
   int   nCode;
} HB_MULTICHAR, * PHB_MULTICHAR;

typedef struct _HB_CODEPAGE
{
   char *id;
   int   nChars;
   char *CharsUpper;
   char *CharsLower;
   BOOL  lLatin;
   BOOL  lAccEqual;
   BOOL  lAccInterleave;
   BOOL  lSort;
   BYTE *s_chars;
   BYTE *s_upper;
   BYTE *s_lower;
   BYTE *s_accent;
   int   nMulti;
   PHB_MULTICHAR multi;
} HB_CODEPAGE, * PHB_CODEPAGE;

extern BOOL HB_EXPORT hb_cdpRegister( PHB_CODEPAGE );
extern char HB_EXPORT * hb_cdpSelectID( char * );
extern PHB_CODEPAGE HB_EXPORT hb_cdpSelect( PHB_CODEPAGE );
extern PHB_CODEPAGE HB_EXPORT hb_cdpFind( char * );
extern void HB_EXPORT hb_cdpTranslate( char*, PHB_CODEPAGE, PHB_CODEPAGE );
extern void HB_EXPORT hb_cdpnTranslate( char*, PHB_CODEPAGE, PHB_CODEPAGE, unsigned int );
extern int HB_EXPORT hb_cdpcmp( char*, char*, ULONG, PHB_CODEPAGE, ULONG* );

#endif /* HB_APICDP_H_ */

