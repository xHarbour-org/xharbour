/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - TOKEN()
 *     - NUMTOKEN()
 *     - ATTOKEN()
 *     - TOKENLOWER()
 *     - TOKENUPPER()
 *     - TOKENSEP()
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
#include <ctype.h>

#ifndef HB_CDP_SUPPORT_OFF
  #include "hbapicdp.h"
  #if 0
  #define TOUPPER( c )  ( ( hb_cdppage() )->nChars ? ( hb_cdppage() )->s_upper[ ( UCHAR ) c ] : HB_TOUPPER( ( UCHAR ) c ) )
  #define TOLOWER( c )  ( ( hb_cdppage() )->nChars ? ( hb_cdppage() )->s_lower[ ( UCHAR ) c ] : HB_TOLOWER( ( UCHAR ) c ) )
  #endif
  #define TOUPPER( c )  ct__HB_TOUPPER( ( UCHAR ) c )
  #define TOLOWER( c )  ct__HB_TOLOWER( ( UCHAR ) c )
#else
  #define TOUPPER( c )  HB_TOUPPER( ( UCHAR ) c )
  #define TOLOWER( c )  HB_TOLOWER( ( UCHAR ) c )
#endif

#ifndef HB_CDP_SUPPORT_OFF

static UCHAR ct__HB_TOUPPER( UCHAR c )
{
   PHB_CODEPAGE __hb_cdp_page = hb_cdppage();

   return __hb_cdp_page->nChars ? __hb_cdp_page->s_upper[ c ] : ( UCHAR ) HB_TOUPPER( c );
}

static UCHAR ct__HB_TOLOWER( UCHAR c )
{
   PHB_CODEPAGE __hb_cdp_page = hb_cdppage();

   return __hb_cdp_page->nChars ? __hb_cdp_page->s_lower[ c ] : ( UCHAR ) HB_TOLOWER( c );
}

#endif

/* static const data */
static const char *  spcSeparatorStr   = "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";
static const size_t  ssSeparatorStrLen = 26;

/* static data */
/* even if these are chars, variable must be int, since we need an extra -1 */
static int  siPreSeparator    = -1; /* TODO: make this threadsafe */
static int  siPostSeparator   = -1; /* TODO: make this threadsafe */

/* defines */
#define DO_TOKEN1_TOKEN       0
#define DO_TOKEN1_NUMTOKEN    1
#define DO_TOKEN1_ATTOKEN     2
#define DO_TOKEN1_TOKENLOWER  3
#define DO_TOKEN1_TOKENUPPER  4

/* helper function for the token function group I */
static void do_token1( int iSwitch )
{
   int   iParamCheck = 0;
   int   iNoRef      = ct_getref() && ISBYREF( 1 );

   switch( iSwitch )
   {
      case DO_TOKEN1_TOKEN:
      {
         siPreSeparator = siPostSeparator = -1;
      }; /* no "break" here !! */
      /* fallthrough */
      case DO_TOKEN1_ATTOKEN:
      case DO_TOKEN1_NUMTOKEN:
      case DO_TOKEN1_TOKENLOWER:
      case DO_TOKEN1_TOKENUPPER:
      {
         iParamCheck = ( ISCHAR( 1 ) );

         break;
      }
   }

   if( iParamCheck )
   {
      const char *   pcString       = hb_parc( 1 );
      HB_SIZE        sStrLen        = hb_parclen( 1 );
      const char *   pcSeparatorStr;
      HB_SIZE        sSeparatorStrLen;
      ULONG          ulTokenCounter = 0;
      ULONG          ulSkip;

      const char *   pcSubStr;
      char *         pcRet       = NULL;
      HB_SIZE        sSubStrLen;
      HB_SIZE        sRetStrLen  = 0;

      ULONG          ulToken     = 0;
      ULONG          ulSkipCnt;
      const char *   pc;

      /* separator string */
      if( ISCHAR( 2 ) && ( ( sSeparatorStrLen = hb_parclen( 2 ) ) != 0 ) )
      {
         pcSeparatorStr = hb_parc( 2 );
      }
      else
      {
         pcSeparatorStr   = ( char * ) spcSeparatorStr;
         sSeparatorStrLen = ssSeparatorStrLen;
      }

      /* token counter */
      if( iSwitch != DO_TOKEN1_NUMTOKEN )
      {
         if( ISNUM( 3 ) )
            ulTokenCounter = hb_parnl( 3 );
         else
            ulTokenCounter = 0;
      }
      if( ulTokenCounter == 0 )
         ulTokenCounter = HB_MKULONG( 255, 255, 255, 255 );

      /* skip width */
      if( iSwitch == DO_TOKEN1_NUMTOKEN )
      {
         if( ISNUM( 3 ) )
            ulSkip = hb_parnl( 3 );
         else
            ulSkip = HB_MKULONG( 255, 255, 255, 255 );
      }
      else
      {
         if( ISNUM( 4 ) )
            ulSkip = hb_parnl( 4 );
         else
            ulSkip = HB_MKULONG( 255, 255, 255, 255 );
      }
      if( ulSkip == 0 )
         ulSkip = HB_MKULONG( 255, 255, 255, 255 );

      /* prepare return value for TOKENUPPER/TOKENLOWER */
      if( ( iSwitch == DO_TOKEN1_TOKENLOWER ) || ( iSwitch == DO_TOKEN1_TOKENUPPER ) )
      {
         if( sStrLen == 0 )
         {
            if( iNoRef )
               hb_retl( 0 );
            else
               hb_retc( "" );

            return;
         }
         pcRet = ( char * ) hb_xgrab( sRetStrLen = sStrLen );
         hb_xmemcpy( pcRet, pcString, (size_t) sRetStrLen );
      }

      /* find the <ulTokenCounter>th token */
      pcSubStr    = pcString;
      sSubStrLen  = sStrLen;

      /* scan start condition */
      pc          = pcSubStr - 1;

      while( ulToken < ulTokenCounter )
      {
         HB_SIZE sMatchedPos = sSeparatorStrLen;

         /* Skip the left ulSkip successive separators */
         ulSkipCnt = 0;
         do
         {
            sSubStrLen  -= ( pc - pcSubStr ) + 1;
            pcSubStr    = pc + 1;
            pc          = ct_at_charset_forward( pcSubStr, sSubStrLen,
                                                 pcSeparatorStr, sSeparatorStrLen,
                                                 &sMatchedPos );
            if( iSwitch == DO_TOKEN1_TOKEN )
            {
               siPreSeparator = siPostSeparator;
               if( sMatchedPos < sSeparatorStrLen )
                  siPostSeparator = pcSeparatorStr[ sMatchedPos ];
               else
                  siPostSeparator = -1;
            }

            ulSkipCnt++;
         }
         while( ( ulSkipCnt < ulSkip ) && ( pc == pcSubStr ) );

         if( sSubStrLen == 0 )
         {
            /* string ends with tokenizer (null string after tokenizer at
               end of string is not a token) */
            switch( iSwitch )
            {
               case DO_TOKEN1_TOKEN:
               {
                  char cRet;

                  hb_retc( "" );
                  if( ISBYREF( 5 ) )
                  {
                     cRet = ( char ) siPreSeparator;
                     hb_storclen( &cRet, ( siPreSeparator != -1 ? 1 : 0 ), 5 );
                  }
                  if( ISBYREF( 6 ) )
                  {
                     cRet = ( char ) siPostSeparator;
                     hb_storclen( &cRet, ( siPostSeparator != -1 ? 1 : 0 ), 6 );
                  }

                  break;
               }

               case DO_TOKEN1_NUMTOKEN:
               {
                  hb_retnl( ulToken );

                  break;
               }

               case DO_TOKEN1_ATTOKEN:
               {
                  hb_retnl( 0 );

                  break;
               }

               case DO_TOKEN1_TOKENLOWER:
               case DO_TOKEN1_TOKENUPPER:
               {
                  if( ! iNoRef )
                  {
                     hb_retclen( pcRet, sRetStrLen );
                  }
                  else
                  {
                     hb_retl( 0 );
                  }
                  if( ISBYREF( 1 ) )
                  {
                     hb_storclen( pcRet, sRetStrLen, 1 );
                  }
                  hb_xfree( pcRet );

                  break;
               }
            }
            return;
         }

         switch( iSwitch )
         {
            case DO_TOKEN1_TOKEN:
            case DO_TOKEN1_NUMTOKEN:
            case DO_TOKEN1_ATTOKEN:
               break;

            case DO_TOKEN1_TOKENLOWER:
            {
               if( pcSubStr != pc ) /* letters can be tokenizers, too,
                                          but they should not be lowercase'd */
                  *( pcRet + ( pcSubStr - pcString ) ) = TOLOWER( *pcSubStr );

               break;
            }

            case DO_TOKEN1_TOKENUPPER:
            {
               if( pcSubStr != pc ) /* letters can be tokenizers, too,
                                          but they should not be uppercase'd */
                  *( pcRet + ( pcSubStr - pcString ) ) = TOUPPER( *pcSubStr );

               break;
            }

            default:
               break;
         }

         ulToken++;

         if( pc == NULL )
         {
            pc = pcSubStr + sSubStrLen;   /* little trick for return values */
            break;                        /* we must leave the while loop even if we have not
                                             yet found the <ulTokenCounter>th token */
         }

         /* should we find the last token, but string ends with tokenizer, i.e.
            pc points to the last character at the moment ?
            -> break here ! */
         if( ulTokenCounter == HB_MKULONG( 255, 255, 255, 255 ) )
         {
            if( ulSkip == HB_MKULONG( 255, 255, 255, 255 ) )
            {
               const char *   t;
               BOOL           bLast = TRUE;

               for( t = pc + 1; t < pcString + sStrLen; t++ )
               {
                  if( ! memchr( pcSeparatorStr, *t, (size_t) sSeparatorStrLen ) )
                  {
                     bLast = FALSE;
                     break;
                  }
               }
               if( bLast )
               {
                  break;
               }
            }
            else if( pc + 1 == pcString + sStrLen )
            {
               break;
            }
         }

      } /* while (ulToken < ulTokenCounter) */

      switch( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            char cRet;

            if( ( ulTokenCounter == HB_MKULONG( 255, 255, 255, 255 ) ) ||
                ( ulToken == ulTokenCounter ) )
               hb_retclen( pcSubStr, pc - pcSubStr );
            else
               hb_retc( "" );

            if( ISBYREF( 5 ) )
            {
               cRet = ( char ) siPreSeparator;
               hb_storclen( &cRet, ( siPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( ISBYREF( 6 ) )
            {
               cRet = ( char ) siPostSeparator;
               hb_storclen( &cRet, ( siPostSeparator != -1 ? 1 : 0 ), 6 );
            }

            break;
         }

         case DO_TOKEN1_NUMTOKEN:
         {
            hb_retnl( ulToken );

            break;
         }

         case DO_TOKEN1_ATTOKEN:
         {
            if( ( ulTokenCounter == HB_MKULONG( 255, 255, 255, 255 ) ) ||
                ( ulToken == ulTokenCounter ) )
               hb_retnl( ( LONG ) ( pcSubStr - pcString + 1 ) );
            else
               hb_retnl( 0 );

            break;
         }

         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
         {
            if( ! iNoRef )
            {
               hb_retclen( pcRet, sRetStrLen );
            }
            else
            {
               hb_retl( 0 );
            }
            if( ISBYREF( 1 ) )
            {
               hb_storclen( pcRet, sRetStrLen, 1 );
            }
            hb_xfree( pcRet );

            break;
         }
      }

   }
   else /* iParamCheck */
   {
      switch( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            PHB_ITEM pSubst         = NULL;
            int      iArgErrorMode  = ct_getargerrormode();
            char     cRet;

            if( ISBYREF( 5 ) )
            {
               cRet = ( char ) siPreSeparator;
               hb_storclen( &cRet, ( siPreSeparator != -1 ? 1 : 0 ),
                            5 );
            }
            if( ISBYREF( 6 ) )
            {
               cRet = ( char ) siPostSeparator;
               hb_storclen( &cRet, ( siPostSeparator != -1 ? 1 : 0 ),
                            6 );
            }

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKEN,
                                        NULL, "TOKEN", 0, EF_CANSUBSTITUTE, 6,
                                        hb_paramError( 1 ), hb_paramError( 2 ),
                                        hb_paramError( 3 ), hb_paramError( 4 ),
                                        hb_paramError( 5 ), hb_paramError( 6 ) );
            }

            if( pSubst != NULL )
            {
               hb_itemRelease( hb_itemReturnForward( pSubst ) );
            }
            else
            {
               if( ! iNoRef )
               {
                  hb_retc( "" );
               }
               else
               {
                  hb_retl( 0 );
               }
            }

            break;
         }


         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
         {
            PHB_ITEM pSubst         = NULL;
            int      iArgErrorMode  = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                        ( iSwitch == DO_TOKEN1_TOKENLOWER ? CT_ERROR_TOKENLOWER : CT_ERROR_TOKENUPPER ),
                                        NULL,
                                        ( iSwitch == DO_TOKEN1_TOKENLOWER ? "TOKENLOWER" : "TOKENUPPER" ),
                                        0, EF_CANSUBSTITUTE, 4,
                                        hb_paramError( 1 ), hb_paramError( 2 ),
                                        hb_paramError( 3 ), hb_paramError( 4 ) );
            }

            if( pSubst != NULL )
            {
               hb_itemRelease( hb_itemReturnForward( pSubst ) );
            }
            else
            {
               if( ! iNoRef )
               {
                  hb_retc( "" );
               }
               else
               {
                  hb_retl( 0 );
               }
            }

            break;
         }

         case DO_TOKEN1_NUMTOKEN:
         case DO_TOKEN1_ATTOKEN:
         {
            PHB_ITEM pSubst         = NULL;
            int      iArgErrorMode  = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                        ( iSwitch == DO_TOKEN1_NUMTOKEN ? CT_ERROR_NUMTOKEN : CT_ERROR_ATTOKEN ),
                                        NULL,
                                        ( iSwitch == DO_TOKEN1_NUMTOKEN ? "NUMTOKEN" : "ATTOKEN" ),
                                        0, EF_CANSUBSTITUTE,
                                        ( iSwitch == DO_TOKEN1_NUMTOKEN ? 3 : 4 ),
                                        hb_paramError( 1 ), hb_paramError( 2 ),
                                        hb_paramError( 3 ), hb_paramError( 4 ) );
            }

            if( pSubst != NULL )
            {
               hb_itemRelease( hb_itemReturnForward( pSubst ) );
            }
            else
            {
               hb_retnl( 0 );
            }

            break;
         }
      }
   }

}

HB_FUNC( ATTOKEN )
{
   do_token1( DO_TOKEN1_ATTOKEN );
}

HB_FUNC( TOKEN )
{
   do_token1( DO_TOKEN1_TOKEN );
}

HB_FUNC( NUMTOKEN )
{
   do_token1( DO_TOKEN1_NUMTOKEN );
}

HB_FUNC( TOKENLOWER )
{
   do_token1( DO_TOKEN1_TOKENLOWER );
}

HB_FUNC( TOKENUPPER )
{
   do_token1( DO_TOKEN1_TOKENUPPER );
}

HB_FUNC( TOKENSEP )
{
   char cRet;

   if( ISLOG( 1 ) && hb_parl( 1 ) )
   {
      /* return the separator char BEHIND the last token */
      if( siPostSeparator != -1 )
      {
         cRet = ( char ) siPostSeparator;
         hb_retclen( &cRet, 1 );
      }
      else
         hb_retc( "" );
   }
   else
   {
      /* return the separator char BEFORE the last token */
      if( siPreSeparator != -1 )
      {
         cRet = ( char ) siPreSeparator;
         hb_retclen( &cRet, 1 );
      }
      else
      {
         hb_retc( "" );
      }
   }
}
