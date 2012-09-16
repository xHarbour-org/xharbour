/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CHARSORT() CT3 string functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"

/* statics */
static size_t  ssCompareLen;  /* TODO: make this thread safe */
static size_t  ssElementPos;  /* TODO: make this thread safe */
static int     siDescend;     /* TODO: make this thread safe */

/* qsort function */
#ifdef __IBMCPP__
int extern _LNK_CONV
#else
static int
#endif
do_charsort( const void * p1, const void * p2 )
{

   char *   pc1   = ( char * ) p1;
   char *   pc2   = ( char * ) p2;
   int      iCmp;

   pc1   += ssElementPos;
   pc2   += ssElementPos;

   iCmp  = strncmp( pc1, pc2, ssCompareLen );
   iCmp  *= ( siDescend ? -1 : 1 );

   return iCmp;
}

HB_FUNC( CHARSORT )
{
   int iNoRet;

   /* suppressing return value ? */
   iNoRet = ct_getref() && ISBYREF( 1 );

   /* param check I */
   if( ISCHAR( 1 ) )
   {
      /* get parameters */
      const char *   pcString = hb_parc( 1 );
      char *         pcRet;
      size_t         sStrLen  = ( size_t ) hb_parclen( 1 );
      size_t         sElementLen, sIgnore, sSortLen;

      if( ISNUM( 2 ) )
         sElementLen = hb_parnl( 2 );
      else
         sElementLen = 1;

      if( ISNUM( 3 ) )
         ssCompareLen = hb_parnl( 3 );
      else
         ssCompareLen = sElementLen;

      if( ISNUM( 4 ) )
         sIgnore = hb_parnl( 4 );
      else
         sIgnore = 0;

      if( ISNUM( 5 ) )
         ssElementPos = hb_parnl( 5 );
      else
         ssElementPos = 0;

      if( ISNUM( 6 ) )
         sSortLen = hb_parnl( 6 );
      else
         sSortLen = sStrLen - sIgnore;

      if( ISLOG( 7 ) )
         siDescend = hb_parl( 7 );
      else
         siDescend = 0;

      /* param check II */
      if( ( sElementLen == 0 ) ||
          ( ssCompareLen > sElementLen ) ||
          ( sIgnore + sElementLen > sStrLen ) ||
          ( ( ssElementPos + ssCompareLen ) > sElementLen ) ||
          ( sSortLen + sIgnore > sStrLen ) )
      {
         int iArgErrorMode = ct_getargerrormode();
         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARSORT,
                      NULL, "CHARSORT", 0, EF_CANDEFAULT, 7,
                      hb_paramError( 1 ), hb_paramError( 2 ),
                      hb_paramError( 3 ), hb_paramError( 4 ),
                      hb_paramError( 5 ), hb_paramError( 6 ),
                      hb_paramError( 7 ) );
         }
         if( iNoRet )
            hb_retl( 0 );
         else
            hb_retc( "" );
         return;
      }

      pcRet = ( char * ) hb_xgrab( sStrLen );
      hb_xmemcpy( pcRet, pcString, sStrLen );

      qsort( pcRet + sIgnore, ( sSortLen / sElementLen ), sElementLen, do_charsort );

      /* return string */
      if( ISBYREF( 1 ) )
         hb_storclen( pcRet, sStrLen, 1 );

      if( iNoRet )
         hb_retl( 0 );
      else
         hb_retclen( pcRet, sStrLen );

      hb_xfree( pcRet );

   }
   else /* if (ISCHAR (1)) */
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARSORT,
                                  NULL, "CHARSORT", 0, EF_CANSUBSTITUTE, 7,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ), hb_paramError( 4 ),
                                  hb_paramError( 5 ), hb_paramError( 6 ),
                                  hb_paramError( 7 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         if( iNoRet )
            hb_retl( 0 );
         else
            hb_retc( "" );
      }
   }
}

