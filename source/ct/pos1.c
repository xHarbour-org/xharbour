/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   POSALPHA(), POSLOWER(), POSRANGE() and POSUPPER() CT3 string functions
 *
 * POSUPPER() Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *
 * POSALPHA(), POSLOWER(), POSRANGE()
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
#include <ctype.h>

#ifndef HB_CDP_SUPPORT_OFF
  #include "hbapicdp.h"
  #if 0
  #define ISUPPER( c )  ( ( hb_cdppage() )->nChars ? HB_ISUPPER( ( UCHAR ) c ) || strchr( ( hb_cdppage() )->CharsUpper, ( UCHAR ) c ) != NULL : HB_ISUPPER( ( UCHAR ) c ) )
  #define ISLOWER( c )  ( ( hb_cdppage() )->nChars ? HB_ISLOWER( ( UCHAR ) c ) || strchr( ( hb_cdppage() )->CharsLower, ( UCHAR ) c ) != NULL : HB_ISLOWER( ( UCHAR ) c ) )
  #define ISALPHA( c )  ( ( hb_cdppage() )->nChars ? HB_ISALPHA( ( UCHAR ) c ) || strchr( ( hb_cdppage() )->CharsUpper, ( UCHAR ) c ) != NULL || strchr( hb_cdp_page->CharsLower, c ) != NULL : HB_ISALPHA( ( UCHAR ) c ) )
  #endif
  #define ISUPPER( c )  __HB_ISUPPER( ( UCHAR ) c )
  #define ISLOWER( c )  __HB_ISLOWER( ( UCHAR ) c )
  #define ISALPHA( c )  __HB_ISALPHA( ( UCHAR ) c )
#else
  #define ISUPPER( c )  HB_ISUPPER( ( UCHAR ) c )
  #define ISLOWER( c )  HB_ISLOWER( ( UCHAR ) c )
  #define ISALPHA( c )  HB_ISALPHA( ( UCHAR ) c )
#endif


/* defines */
#define DO_POS1_POSALPHA   0
#define DO_POS1_POSLOWER   1
#define DO_POS1_POSRANGE   2
#define DO_POS1_POSUPPER   3

#ifndef HB_CDP_SUPPORT_OFF

static BOOL __HB_ISUPPER( UCHAR c )
{
   PHB_CODEPAGE __hb_cdp_page = hb_cdppage();

   return __hb_cdp_page->nChars ? HB_ISUPPER( c ) || strchr( __hb_cdp_page->CharsUpper, c ) != NULL : HB_ISUPPER( c );
}

static BOOL __HB_ISLOWER( UCHAR c )
{
   PHB_CODEPAGE __hb_cdp_page = hb_cdppage();

   return __hb_cdp_page->nChars ? HB_ISLOWER( c ) || strchr( __hb_cdp_page->CharsLower, c ) != NULL : HB_ISLOWER( c );
}

static BOOL __HB_ISALPHA( UCHAR c )
{
   PHB_CODEPAGE __hb_cdp_page = hb_cdppage();

   return __hb_cdp_page->nChars ? HB_ISALPHA( c ) || strchr( __hb_cdp_page->CharsUpper, c ) != NULL || strchr( __hb_cdp_page->CharsLower, c ) != NULL : HB_ISALPHA( c );
}

#endif /* HB_CDP_SUPPORT_OFF */

/* helper function for the posxxx() functions */
static void do_pos1( int iSwitch )
{

   if( ( ISCHAR( 1 ) )                       /* all functions need string as 1st param */
       &&
       ( ( iSwitch != DO_POS1_POSRANGE )     /* that's the only condition for all funcs _except_ POSRANGE */
         ||
         ( ( iSwitch == DO_POS1_POSRANGE )   /* In addition, POSRANGE needs .. */
          &&
          ( ISCHAR( 2 ) )                    /* .. string as 2nd .. */
          &&
          ( ISCHAR( 3 ) )                    /* .. and 3rd param */
         )
       )
       )
   {

      unsigned char *   pcString;
      size_t            sStrLen;
      unsigned char *   puc, ucChar1 = ' ', ucChar2 = ' ';
      int               iMode;
      size_t            sIgnore;
      int               iParamShift = 0;

      if( iSwitch == DO_POS1_POSRANGE )
      {

         if( hb_parclen( 1 ) == 0 )
         {
            hb_retnl( 0 );
            return;
         }
         else
         {
            ucChar1 = *( hb_parc( 1 ) );
         }

         if( hb_parclen( 2 ) == 0 )
         {
            hb_retnl( 0 );
            return;
         }
         else
         {
            ucChar2 = *( hb_parc( 2 ) );
         }

         iParamShift += 2;
      }

      pcString = ( unsigned char * ) hb_parc( iParamShift + 1 );
      sStrLen  = ( size_t ) hb_parclen( iParamShift + 1 );

      if( ISLOG( iParamShift + 2 ) )
         iMode = hb_parl( iParamShift + 2 );
      else
         iMode = 0;

      if( ISNUM( iParamShift + 3 ) )
         sIgnore = ( size_t ) hb_parnl( iParamShift + 3 );
      else
         sIgnore = 0;

      for( puc = pcString + sIgnore; puc < pcString + sStrLen; puc++ )
      {
         int iDoRet = 0;
         switch( iSwitch )
         {
            case DO_POS1_POSALPHA:
            {
               iDoRet = ISALPHA( *puc );
            }; break;

            case DO_POS1_POSLOWER:
            {
               iDoRet = ISLOWER( *puc );
            }; break;

            case DO_POS1_POSRANGE:
            {
               iDoRet = ( ( ucChar1 <= *puc ) && ( ucChar2 >= *puc ) );
            }; break;

            case DO_POS1_POSUPPER:
            {
               iDoRet = ISUPPER( *puc );
            }; break;
         }

         if( ( iMode && ! iDoRet ) || ( ! iMode && iDoRet ) )
         {
            hb_retns( puc - pcString + 1 );
            return;
         }
      }

      hb_retnl( 0 );

   }
   else /* ISCHAR (1) etc. */
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         switch( iSwitch )
         {
            case DO_POS1_POSALPHA:
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSALPHA,
                                        NULL, "POSALPHA", 0, EF_CANSUBSTITUTE, 3,
                                        hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
            }; break;

            case DO_POS1_POSLOWER:
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSLOWER,
                                        NULL, "POSLOWER", 0, EF_CANSUBSTITUTE, 3,
                                        hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
            }; break;

            case DO_POS1_POSRANGE:
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSRANGE,
                                        NULL, "POSRANGE", 0, EF_CANSUBSTITUTE, 5,
                                        hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
                                        hb_paramError( 4 ), hb_paramError( 5 ) );
            }; break;

            case DO_POS1_POSUPPER:
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSUPPER,
                                        NULL, "POSUPPER", 0, EF_CANSUBSTITUTE, 3,
                                        hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
            }; break;
         }
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnl( 0 );
      }
   }
}

HB_FUNC( POSALPHA )
{
   do_pos1( DO_POS1_POSALPHA );
}

HB_FUNC( POSLOWER )
{
   do_pos1( DO_POS1_POSLOWER );
}

HB_FUNC( POSRANGE )
{
   do_pos1( DO_POS1_POSRANGE );
}

HB_FUNC( POSUPPER )
{
   do_pos1( DO_POS1_POSUPPER );
}
