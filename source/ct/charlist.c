/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - CHARLIST()
 *     - CHARSLIST()  (NEW)
 *     - CHARNOLIST()
 *     - CHARHIST()  (NEW)
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

/* defines */
#define DO_LIST_CHARLIST   0
#define DO_LIST_CHARNOLIST 1
#define DO_LIST_CHARHIST   2
#define DO_LIST_CHARSLIST  3

/* helper function for the list function */
static void do_list( int iSwitch )
{
   const char *   pcString;
   size_t         sStrLen;

   size_t         asCharCnt[ 256 ];
   size_t         sCnt;

   /* init asCharCnt */
   for( sCnt = 0; sCnt < 256; sCnt++ )
   {
      asCharCnt[ sCnt ] = 0;
   }

   /* init params */
   if( ISCHAR( 1 ) )
   {
      pcString = hb_parc( 1 );
      sStrLen  = ( size_t ) hb_parclen( 1 );
   }
   else
   {
      pcString = "";
      sStrLen  = 0;
   }

   /* count characters */
   if( iSwitch == DO_LIST_CHARLIST )
   {
      char     pcRet[ 256 ];
      size_t   sRetStrLen = 0;

      for( sCnt = 0; sCnt < sStrLen; sCnt++ )
      {
         if( asCharCnt[ ( size_t ) ( pcString[ sCnt ] ) ] == 0 )
         {
            pcRet[ sRetStrLen++ ]                        = pcString[ sCnt ];
            asCharCnt[ ( size_t ) ( pcString[ sCnt ] ) ] = 1;
         }
      }

      hb_retclen( pcRet, sRetStrLen );
   }
   else
   {
      for( sCnt = 0; sCnt < sStrLen; sCnt++ )
      {
         size_t sIndex = ( size_t ) ( unsigned char ) ( *( pcString + sCnt ) );
         asCharCnt[ sIndex ] = asCharCnt[ sIndex ] + 1;
      }

      switch( iSwitch )
      {
         case DO_LIST_CHARSLIST:
         {
            char *   pcRet;
            size_t   sRetStrLen = 0;

            pcRet = ( char * ) hb_xgrab( 256 );

            for( sCnt = 0; sCnt < 256; sCnt++ )
            {
               if( asCharCnt[ sCnt ] != 0 )
               {
                  *( pcRet + sRetStrLen ) = ( unsigned char ) sCnt;
                  sRetStrLen++;
               }
            }

            hb_retclen( pcRet, sRetStrLen );
            hb_xfree( pcRet );

         }; break;

         case DO_LIST_CHARNOLIST:
         {

            char *   pcRet;
            size_t   sRetStrLen = 0;

            pcRet = ( char * ) hb_xgrab( 256 );

            for( sCnt = 0; sCnt < 256; sCnt++ )
            {
               if( asCharCnt[ sCnt ] == 0 )
               {
                  *( pcRet + sRetStrLen ) = ( unsigned char ) sCnt;
                  sRetStrLen++;
               }
            }

            hb_retclen( pcRet, sRetStrLen );
            hb_xfree( pcRet );

         }; break;

         case DO_LIST_CHARHIST:
         {
            PHB_ITEM pArray = hb_itemArrayNew( 256 );

            for( sCnt = 0; sCnt < 256; sCnt++ )
            {
               hb_arraySetNS( pArray, sCnt + 1, asCharCnt[ sCnt ] );
            }
            hb_itemRelease( hb_itemReturn( pArray ) );
         }; break;
      }
   }
}

HB_FUNC( CHARLIST )
{
   do_list( DO_LIST_CHARLIST );
}

HB_FUNC( CHARSLIST )
{
   do_list( DO_LIST_CHARSLIST );
}

HB_FUNC( CHARNOLIST )
{
   do_list( DO_LIST_CHARNOLIST );
}

HB_FUNC( CHARHIST )
{
   do_list( DO_LIST_CHARHIST );
}

