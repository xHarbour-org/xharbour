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

/*
 * 2002-Dec-13 Ron Pinkas added POSIX REG_STAREND support.
 */

/*
 * 2002-Dec-13 Ron Pinkas added xHarbour wrapper:
 *
 *    HB_FUNC( HB_ATX )
 *
 */

/*
 * 2003-Feb-3 Giancarlo Niccolai added the wrappers
 *
 *    HB_FUNC( HB_REGEX* )
 *
 */

#if defined(__WATCOMC__)
   #pragma disable_message ( 124 )
#elif defined(__POCC__)
   #pragma warn (disable:2130)
#endif

#define HB_THREAD_OPTIMIZE_STACK
#define _HB_API_INTERNAL_

#include "hbdefs.h"
#include "hbstack.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#include "hbregex.h"

/*
 * It will be good to separate RegEx internals from xHarbour API
 * and define our own meta functions so C modules which use regex
 * will not have to include regex.h and we will be free to make
 * any modifications in internal structures (also use native
 * platform regex library which exists in most of *nixes), Druzus
 */
#if ! defined( HB_ALLOC_ALIGNMENT ) || ( HB_ALLOC_ALIGNMENT + 1 == 1 )
#  define HB_ALLOC_ALIGNMENT 8
#endif

#define hb_isregexstring( x )  ( ( x->item.asString.length > 3 && memcmp( x->item.asString.value, "***", 3 ) == 0 ) )

HB_EXTERN_BEGIN
extern void HB_PCREPOS_LIBRARY( void );
HB_EXTERN_END

regex_t * hb_getregex( PHB_ITEM pRegEx, BOOL lIgnCase, BOOL lNL, BOOL * fFree )
{
   char *      szRegEx  = hb_itemGetCPtr( pRegEx );
   regex_t *   pRetReg  = NULL;

   *fFree = FALSE;
   if( szRegEx && *szRegEx )
   {
      if( memcmp( szRegEx, "***", 3 ) == 0 )
      {
         pRetReg           = ( regex_t * ) ( szRegEx + HB_ALLOC_ALIGNMENT );
         pRetReg->re_pcre  = ( pcre * ) ( ( BYTE * ) pRetReg + sizeof( regex_t ) );
      }
      else
      {
         int CFlags = REG_EXTENDED | ( lIgnCase ? REG_ICASE : 0 ) |
                      ( lNL ? REG_NEWLINE : 0 );

         pRetReg = ( regex_t * ) hb_xgrab( sizeof( regex_t ) );

         if( regcomp( pRetReg, szRegEx, CFlags ) == 0 )
            *fFree = TRUE;
         else
         {
            hb_xfree( pRetReg );
            pRetReg = NULL;
            hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid Regular expression", "Regex subsystem", 1, pRegEx );
         }
      }
   }

   return pRetReg;
}

void hb_freeregex( regex_t * pReg )
{
   regfree( pReg );
   hb_xfree( pReg );
}

BOOL hb_regexCompile( PHB_REGEX pRegEx, const char * szRegEx, int iCFlags, int iEFlags )
{
   pRegEx->pReg      = NULL;
   pRegEx->fFree     = FALSE;
   pRegEx->iCFlags   = REG_EXTENDED | iCFlags;
   pRegEx->iEFlags   = iEFlags;

   if( szRegEx && *szRegEx )
   {
      pRegEx->pReg = ( regex_t * ) hb_xgrab( sizeof( regex_t ) );
      if( regcomp( pRegEx->pReg, szRegEx, pRegEx->iCFlags ) == 0 )
      {
         pRegEx->fFree = TRUE;
         return TRUE;
      }
      hb_xfree( pRegEx->pReg );
      pRegEx->pReg = NULL;
   }
   return FALSE;
}

BOOL hb_regexGet( PHB_REGEX pRegEx, PHB_ITEM pRegExItm, int iCFlags, int iEFlags )
{
   char *   szRegEx  = hb_itemGetCPtr( pRegExItm );
   BOOL     fResult  = FALSE;

   if( szRegEx && *szRegEx )
   {
      if( memcmp( szRegEx, "***", 3 ) == 0 )
      {
         pRegEx->pReg            = ( regex_t * ) ( szRegEx + HB_ALLOC_ALIGNMENT );
         pRegEx->pReg->re_pcre   = ( pcre * ) ( ( BYTE * ) pRegEx->pReg + sizeof( regex_t ) );
         pRegEx->fFree           = FALSE;
         pRegEx->iCFlags         = REG_EXTENDED | iCFlags;
         pRegEx->iEFlags         = iEFlags;
         fResult                 = TRUE;
      }
      else
         fResult = hb_regexCompile( pRegEx, hb_itemGetCPtr( pRegExItm ), iCFlags, iEFlags );
   }

   if( ! fResult )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid Regular expression", "Regex subsystem", 1, pRegExItm );

   return fResult;
}

void hb_regexFree( PHB_REGEX pRegEx )
{
   if( pRegEx )
   {
      if( pRegEx->fFree )
      {
         regfree( pRegEx->pReg );
         hb_xfree( pRegEx->pReg );
         pRegEx->pReg   = NULL;
         pRegEx->fFree  = FALSE;
      }
   }
}

BOOL hb_regexMatch( PHB_REGEX pRegEx, const char * szString, BOOL fFull )
{
   BOOL fMatch;

   fMatch = regexec( pRegEx->pReg, szString, 1, pRegEx->aMatches, pRegEx->iEFlags ) == 0;

   return fMatch && ( ! fFull ||
                      ( pRegEx->aMatches[ 0 ].rm_so == 0 &&
                        pRegEx->aMatches[ 0 ].rm_eo == ( int ) strlen( szString ) ) );
}

/*
 *  This is a worker function may be called by PRG Wrappers HB_REGEX and HB_REGEXMATCH
 *  in such case paramaters ara on the HB_BM_STACK, or it may be called directly by the
 *  HVM when executing the HB_P_MATCH and HB_P_LIKE operator. In such case the operands
 *  are passed directly.
 */
BOOL hb_regex( char cRequest, PHB_ITEM pRegEx, PHB_ITEM pString )
{
   #ifndef REGEX_MAX_GROUPS
      #define REGEX_MAX_GROUPS 16
   #endif

   regex_t *   pReg;
   regmatch_t  aMatches[ REGEX_MAX_GROUPS ];
   int         EFlags         = 0; /* REG_BACKR; */
   int         i, iMatches    = 0;
   int         iMaxMatch      = REGEX_MAX_GROUPS;

   PHB_ITEM    pCaseSensitive = hb_param( 3, HB_IT_LOGICAL );
   PHB_ITEM    pNewLine       = hb_param( 4, HB_IT_LOGICAL );
   BOOL        fFree          = FALSE;

   HB_ITEM_NEW( pRetArray );

   if( pRegEx == NULL )
   {
      pRegEx   = hb_param( 1, HB_IT_STRING );
      pString  = hb_param( 2, HB_IT_STRING );
   }

   if( pRegEx == NULL || pString == NULL || ( ! HB_IS_STRING( pRegEx ) ) || ( ! HB_IS_STRING( pString ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameters", "Regex subsystem", 4, pRegEx, pString, hb_paramError( 3 ), hb_paramError( 4 ) );
      return FALSE;
   }

   pReg = hb_getregex( pRegEx,
                       pCaseSensitive && ! pCaseSensitive->item.asLogical.value,
                       pNewLine != NULL && pNewLine->item.asLogical.value,
                       &fFree );

   if( pReg == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid regular expression", "Regex subsystem", 4, pRegEx, pString, hb_paramError( 3 ), hb_paramError( 4 ) );
      return FALSE;
   }

   if( cRequest != 0 && cRequest != 4 && cRequest != 5 )
      iMaxMatch = 1;

   aMatches[ 0 ].rm_so  = 0;
   aMatches[ 0 ].rm_eo  = ( regoff_t ) pString->item.asString.length;

   if( regexec( pReg, pString->item.asString.value, iMaxMatch, aMatches, EFlags ) == 0 )
   {
      switch( cRequest )
      {
         case 0: /* Wants Results */
            /* Count sucessful matches */
            for( i = 0; i < iMaxMatch; i++ )
            {
               if( aMatches[ i ].rm_eo != -1 )
                  iMatches = i;
            }

            iMatches++;
            hb_arrayNew( &pRetArray, iMatches );

            for( i = 0; i < iMatches; i++ )
            {
               if( aMatches[ i ].rm_eo > -1 )
               {
                  hb_arraySetCL( &pRetArray, i + 1,
                                pString->item.asString.value + aMatches[i].rm_so,
                                aMatches[i].rm_eo - aMatches[i].rm_so );
               }
               else
                  hb_arraySetCL( &pRetArray, i + 1, "", 0 );
            }

            if( fFree )
               hb_freeregex( pReg );

            hb_itemReturnForward( &pRetArray );

            return TRUE;

         case 1: /* HB_P_LIKE */
            if( fFree )
               /* TraceLog( NULL, "%s like %s\n", pString->item.asString.value, pRegEx->item.asString.value );
                */
               hb_freeregex( pReg );

            return aMatches[ 0 ].rm_so == 0 && ( ULONG ) ( aMatches[ 0 ].rm_eo ) == pString->item.asString.length;

         case 2: /* HB_P_MATCH */
            if( fFree )
               /* TraceLog( NULL, ">%s< has >%s<\n", pString->item.asString.value, pRegEx->item.asString.value );
                */
               hb_freeregex( pReg );

            return TRUE;

         case 3: /* Split */
         {
            HB_ITEM_NEW( Match );
            char *str         = pString->item.asString.value;
            int      iMax     = hb_parni( 5 );
            int      iCount   = 1;

            hb_arrayNew( &pRetArray, 0 );

            do
            {
               hb_itemPutCL( &Match, str, aMatches[ 0 ].rm_so );
               hb_arrayAddForward( &pRetArray, &Match );
               str += aMatches[ 0 ].rm_eo;
               iCount++;
            }
            while( *str && ( iMax == 0 || iMax > iCount ) && regexec( pReg, str, 1, aMatches, EFlags ) == 0 );

            /* last match must be done also in case that str is empty; this would
               mean an empty split field at the end of the string */
            hb_itemPutCL( &Match, str, strlen( str ) );
            hb_arrayAddForward( &pRetArray, &Match );

            if( fFree )
               hb_freeregex( pReg );

            hb_itemReturnForward( &pRetArray );

            return TRUE;
         }

         case 4: /* Wants Results AND positions */
         {
            HB_ITEM aSingleMatch;

            /* Count sucessful matches */
            for( i = 0; i < iMaxMatch; i++ )
            {
               if( aMatches[ i ].rm_eo != -1 )
                  iMatches = i;
            }

            iMatches++;
            hb_arrayNew( &pRetArray, iMatches );

            for( i = 0; i < iMatches; i++ )
            {
               aSingleMatch.type = HB_IT_NIL;
               hb_arrayNew( &aSingleMatch, 3 );

               if( aMatches[ i ].rm_eo != -1 )
               {
                  /* matched string */
                  hb_arraySetCL( &aSingleMatch, 1, pString->item.asString.value + aMatches[i].rm_so, aMatches[i].rm_eo - aMatches[i].rm_so );

                  /* begin of match */
                  hb_arraySetNI( &aSingleMatch, 2, aMatches[ i ].rm_so + 1 );

                  /* End of match */
                  hb_arraySetNI( &aSingleMatch, 3, aMatches[ i ].rm_eo );
               }
               else
               {
                  hb_arraySetCL( &aSingleMatch, 1, "", 0 );
                  hb_arraySetNI( &aSingleMatch, 2, 0 );
                  hb_arraySetNI( &aSingleMatch, 3, 0 );
               }

               hb_arraySetForward( &pRetArray, i + 1, &aSingleMatch );
            }

            if( fFree )
               hb_freeregex( pReg );

            hb_itemReturnForward( &pRetArray );

            return TRUE;
         }

         case 5: /* Wants *ALL* Results AND positions */
         {
            HB_ITEM_NEW( pAtxArray );
            HB_ITEM  aSingleMatch;

            char *   str         = pString->item.asString.value;
            int      iMax        = hb_parni( 5 );  /* max nuber of matches I want, 0 = unlimited */
            int      iGetMatch   = hb_parni( 6 );  /* Gets if want only one single match or a sub-match */
            BOOL     fOnlyMatch  = TRUE;           /* if TRUE returns only matches and sub-matches, not positions */
            ULONG    ulOffSet    = 0;
            int      iCount      = 1;

            if( hb_parinfo( 7 ) & HB_IT_LOGICAL )
               fOnlyMatch = hb_parl( 7 );

            /* Set new array */
            hb_arrayNew( &pRetArray, 0 );

            do
            {
               /* Count sucessful matches */
               for( i = 0; i < iMaxMatch; i++ )
               {
                  if( aMatches[ i ].rm_eo != -1 )
                     iMatches = i;
                     /* TraceLog( NULL, "iMatches = %i, Pos = %i\n\r", iMatches, aMatches[i].rm_eo );
                      */
               }

               iMatches++;

               /* TraceLog( NULL, "iMatches %i\n\r", iMatches ); */

               /* If I want all matches */
               if( iGetMatch == 0 ||  /* Check boundaries */
                   ( iGetMatch < 0 || iGetMatch > iMatches )
                   )
               {
                  hb_arrayNew( &pAtxArray, iMatches );

                  for( i = 0; i < iMatches; i++ )
                  {
                     aSingleMatch.type = HB_IT_NIL;
                     hb_arrayNew( &aSingleMatch, 3 );

                     if( ! fOnlyMatch )
                     {
                        if( aMatches[ i ].rm_eo != -1 )
                        {
                           /* matched string */
                           hb_arraySetCL( &aSingleMatch, 1, str + aMatches[ i ].rm_so, ( aMatches[ i ].rm_eo - aMatches[ i ].rm_so ) );

                           /* begin of match */
                           hb_arraySetNI( &aSingleMatch, 2, ulOffSet + aMatches[ i ].rm_so + 1 );

                           /* End of match */
                           hb_arraySetNI( &aSingleMatch, 3, ulOffSet + aMatches[ i ].rm_eo );
                        }
                        else
                        {
                           hb_arraySetCL( &aSingleMatch, 1, "", 0 );
                           hb_arraySetNI( &aSingleMatch, 2, 0 );
                           hb_arraySetNI( &aSingleMatch, 3, 0 );
                        }
                     }
                     else
                     {
                        if( aMatches[ i ].rm_eo != -1 )
                           /* matched string */
                           hb_itemPutCL( &aSingleMatch, str + aMatches[ i ].rm_so, ( aMatches[ i ].rm_eo - aMatches[ i ].rm_so ) );
                        else
                           hb_itemPutCL( &aSingleMatch, "", 0 );
                     }

                     hb_arraySetForward( &pAtxArray, i + 1, &aSingleMatch );
                  }

                  hb_arrayAddForward( &pRetArray, &pAtxArray );

               }
               else /* Here I get only single matches */
               {
                  i                 = iGetMatch - 1;
                  aSingleMatch.type = HB_IT_NIL;

                  if( ! fOnlyMatch )
                  {
                     hb_arrayNew( &aSingleMatch, 3 );

                     if( aMatches[ i ].rm_eo != -1 )
                     {
                        /* matched string */
                        hb_arraySetCL( &aSingleMatch, 1, str + aMatches[ i ].rm_so, ( aMatches[ i ].rm_eo - aMatches[ i ].rm_so ) );

                        /* begin of match */
                        hb_arraySetNI( &aSingleMatch, 2, ulOffSet + aMatches[ i ].rm_so + 1 );

                        /* End of match */
                        hb_arraySetNI( &aSingleMatch, 3, ulOffSet + aMatches[ i ].rm_eo );
                     }
                     else
                     {
                        hb_arraySetCL( &aSingleMatch, 1, "", 0 );
                        hb_arraySetNI( &aSingleMatch, 2, 0 );
                        hb_arraySetNI( &aSingleMatch, 3, 0 );
                     }
                  }
                  else
                  {
                     if( aMatches[ i ].rm_eo != -1 )
                        /* matched string */
                        hb_itemPutCL( &aSingleMatch, str + aMatches[ i ].rm_so, ( aMatches[ i ].rm_eo - aMatches[ i ].rm_so ) );
                     else
                        hb_itemPutCL( &aSingleMatch, "", 0 );
                  }

                  hb_arrayAddForward( &pRetArray, &aSingleMatch );
               }

               str      += aMatches[ 0 ].rm_eo;
               /* TraceLog( NULL, "iCount, ulOffSet, aMatches[0].rm_so, aMatches[0].rm_eo: %i, %i, %i, %i \n\r", iCount, ulOffSet, aMatches[0].rm_so, aMatches[0].rm_eo );
                */
               ulOffSet += aMatches[ 0 ].rm_eo;
               iCount++;
            }
            while( *str && ( iMax == 0 || iMax >= iCount ) && regexec( pReg, str, iMaxMatch, aMatches, EFlags ) == 0 );

            if( fFree )
               hb_freeregex( pReg );

            hb_itemReturnForward( &pRetArray );

            return TRUE;
         }
      }
   }

   /* If we have no match, we must anyway return an array of one element
      for request kind == 3 (split) */
   if( fFree )
      hb_freeregex( pReg );

   if( cRequest == 3 )
   {
      hb_arrayNew( &pRetArray, 1 );
      hb_arraySet( &pRetArray, 1, pString );
      hb_itemReturnForward( &pRetArray );
      return TRUE;
   }

   return FALSE;
}

/*
   Converts OS Directory Wild Mask to an equivalent RegEx
   Caller must allocate sRegEx with enough space for conversion.
   returns the length of the resulting RegEx.
 */
int Wild2RegEx( const char * sWild, char * sRegEx, BOOL bMatchCase )
{
   char     cChar;
   HB_SIZE  iLen = strlen( sWild );
   int      i, iLenResult = 0;

   if( bMatchCase == FALSE )
   {
      hb_strncpy( sRegEx, "(?i)", 4 );
      iLenResult = 4;
   }

   hb_strncpy( sRegEx + iLenResult, "\\b", 2 );
   iLenResult += 2;

   for( i = 0; i < (int) iLen; i++ )
   {
      cChar = sWild[ i ];

      switch( cChar )
      {
         case '*':
            /* Space NOT allowed as 1st. character. */
            if( i == 0 )
            {
               hb_strncpy( sRegEx + iLenResult, "[^ ]", 4 );
               iLenResult += 4;
            }

            hb_strncpy( sRegEx + iLenResult, ".*", 2 );
            iLenResult += 2;
            break;

         case '?':
            hb_strncpy( sRegEx + iLenResult, ".?", 2 );
            iLenResult += 2;
            break;

         case '.':
            hb_strncpy( sRegEx + iLenResult, "\\.", 2 );
            iLenResult += 2;
            break;

         default:
            sRegEx[ iLenResult++ ] = cChar;
      }
   }

   hb_strncpy( sRegEx + iLenResult, "\\b", 2 );
   iLenResult           += 2;

   sRegEx[ iLenResult ] = '\0';

   return iLenResult;
}

/*
   Converts Clipper Symbol Mask to an equivalent RegEx
   Caller must allocate sRegEx with enough space for conversion.
   returns the length of the resulting RegEx.
 */
int Mask2RegEx( const char * sWild, char * sRegEx, BOOL bMatchCase )
{
   char     cChar;
   HB_SIZE  iLen        = strlen( sWild );
   HB_SIZE  i;
   int      iLenResult  = 0;

   if( bMatchCase == FALSE )
   {
      hb_strncpy( sRegEx, "(?i)", 4 );
      iLenResult = 4;
   }

   for( i = 0; i < iLen; i++ )
   {
      cChar = sWild[ i ];

      switch( cChar )
      {
         case '*':
            hb_strncpy( sRegEx + iLenResult, ".*", 2 );
            iLenResult  += 2;
            i           = iLen; /* Skip rest if any! */
            break;

         case '?':
            hb_strncpy( sRegEx + iLenResult, ".?", 2 );
            iLenResult += 2;
            break;

         default:
            sRegEx[ iLenResult++ ] = cChar;
      }
   }

   sRegEx[ iLenResult ] = '\0';

   return iLenResult;
}

HB_FUNC( HB_ATX )
{
   HB_THREAD_STUB_API
   #define REGEX_MAX_GROUPS 16
   regex_t * pReg;
   regmatch_t  aMatches[ REGEX_MAX_GROUPS ];
   int         EFlags = 0; /* REG_BACKR; */
   ULONG       ulLen;
   BOOL        fFree;

   PHB_ITEM    pRegEx         = hb_param( 1, HB_IT_STRING );
   PHB_ITEM    pString        = hb_param( 2, HB_IT_STRING );
   PHB_ITEM    pCaseSensitive = hb_param( 3, HB_IT_LOGICAL );
//    HB_SIZE     lStart         = hb_parnl( 4 ), lEnd = hb_parnl( 5 );
   HB_SIZE     lStart         = hb_parns( 4 ), lEnd = hb_parns( 5 );

   if( pRegEx && pString && lStart <= pString->item.asString.length )
   {
      pReg = hb_getregex( pRegEx,
                          pCaseSensitive && ! pCaseSensitive->item.asLogical.value,
                          FALSE, &fFree );

      if( pReg )
      {
         /* Validate and convert to ZERO based */
         if( (int)lStart > 0 )
            lStart--;
         else if( (int)lStart < 0 )
         {
            lStart += pString->item.asString.length - 1;
            /* lStart += hb_itemGetCLen( pString ) - 1; */

            if( (int)lStart < 0 )
               lStart = 0;
         }

         /* Validate and convert to ZERO based */
         if( (int)lEnd > 0 )
         {
            lEnd--;

            if( lEnd > pString->item.asString.length )
               lEnd = lStart ? pString->item.asString.length : 0;
         }
         else if( (int)lEnd < 0 )
         {
            lEnd += pString->item.asString.length;

            /* Let it fail for: lStart > lEnd
             * if( lEnd < 0 )
             *    lEnd = lStart ? pString->item.asString.length : 0;
             */
         }
         else
         {
            if( lStart )
               lEnd = pString->item.asString.length;
         }

         if( lStart > lEnd )
         {
            if( fFree )
               hb_freeregex( pReg );

            if( hb_pcount() > 3 )
               hb_stornl( 0, 4 );

            if( hb_pcount() > 4 )
               hb_stornl( 0, 5 );

            return;
         }

         /* lEnd ALWAYS set if lStart is set*/
         if( /*lStart ||*/ lEnd )
         {
            EFlags               |= REG_STARTEND;

            aMatches[ 0 ].rm_so  = ( regoff_t ) lStart;
            aMatches[ 0 ].rm_eo  = ( regoff_t ) lEnd;
         }

         if( regexec( pReg, pString->item.asString.value, REGEX_MAX_GROUPS, aMatches, EFlags ) == 0 )
         {
            ulLen = aMatches[ 0 ].rm_eo - aMatches[ 0 ].rm_so;

            if( hb_pcount() > 3 )
               hb_stornl( ( LONG ) ( aMatches[ 0 ].rm_so + 1 + lStart ), 4 );

            if( hb_pcount() > 4 )
               hb_stornl( aMatches[ 0 ].rm_eo - aMatches[ 0 ].rm_so, 5 );

            hb_retclen( pString->item.asString.value + aMatches[0].rm_so + lStart, ulLen );

            if( fFree )
               hb_freeregex( pReg );

            return;
         }

         if( fFree )
            hb_freeregex( pReg );
      }
   }

   if( hb_pcount() > 3 )
      hb_stornl( 0, 4 );

   if( hb_pcount() > 4 )
      hb_stornl( 0, 5 );
}

HB_FUNC( WILD2REGEX )
{
   HB_THREAD_STUB_API

   char  sRegEx[ HB_PATH_MAX ];
   int   iLen = Wild2RegEx( hb_parcx( 1 ), sRegEx, hb_parl( 2 ) );

   hb_retclen( sRegEx, iLen );
}

/* Returns array of Match + Sub-Matches. */
HB_FUNC( HB_REGEX )
{
   hb_regex( 0, NULL, NULL );
}

/* Returns array of { Match, start, end}, { Sub-Matches, start, end} */
HB_FUNC( HB_REGEXATX )
{
   hb_regex( 4, NULL, NULL );
}

/*
 * 2005-12-16 - Francesco Saverio Giudice
 * HB_RegExAll( cRegex, cString, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch, lOnlyMatch ) -> aAllRegexMatches
 *
 * This function return all matches from a Regex search.
 * It is a mix from hb_RegEx() and hb_RegExAtX()
 *
 * PARAMETERS:
 *  cRegex         - Regex pattern string or precompiled Regex
 *  cString        - The string you want to search
 *  lCaseSensitive - default = FALSE
 *  lNewLine       - default = FALSE
 *  nMaxMatches    - default = unlimited, this limit number of matches that have to return
 *  nGetMatch      - default = unlimited, this returns only one from Match + Sub-Matches
 *  lOnlyMatch     - default = TRUE, if TRUE returns Matches, otherwise it returns also start and end positions
 *
 */
HB_FUNC( HB_REGEXALL )
{
   hb_regex( 5, NULL, NULL );
}

/* Returns just .T. if match found or .F. otherwise. */
HB_FUNC( HB_REGEXMATCH )
{
   HB_THREAD_STUB_API

   hb_retl( hb_regex( hb_parl( 3 ) ? 1 /* LIKE */ : 2 /* HAS */, NULL, NULL ) );
}

/* Splits the string in an array of matched expressions */
HB_FUNC( HB_REGEXSPLIT )
{
   hb_regex( 3, NULL, NULL );
}

HB_FUNC( HB_REGEXCOMP )
{
   HB_THREAD_STUB_API

   regex_t  re;
   char *   cRegex;
   int      CFlags         = REG_EXTENDED;

   PHB_ITEM pRegEx         = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pCaseSensitive = hb_param( 2, HB_IT_LOGICAL );
   PHB_ITEM pNewLine       = hb_param( 3, HB_IT_LOGICAL );

   if( pRegEx == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter count/type",
                            NULL,
                            2, hb_param( 1, HB_IT_ANY ), hb_param( 2, HB_IT_ANY ) );
      return;
   }

   if( pCaseSensitive != NULL && pCaseSensitive->item.asLogical.value == ( int )FALSE )
      CFlags |= REG_ICASE;

   if( pNewLine != NULL && pNewLine->item.asLogical.value == ( int )TRUE )
      CFlags |= REG_NEWLINE;

   if( regcomp( &re, pRegEx->item.asString.value, CFlags ) == 0 )
   {
      ULONG nSize = ( ( real_pcre * ) re.re_pcre )->size;
      cRegex      = ( char * ) hb_xgrab( HB_ALLOC_ALIGNMENT + sizeof( re ) + nSize );
      HB_MEMCPY( cRegex + HB_ALLOC_ALIGNMENT + sizeof( re ), re.re_pcre, nSize );
      ( pcre_free ) ( re.re_pcre );
      re.re_pcre  = ( pcre * ) ( cRegex + HB_ALLOC_ALIGNMENT + sizeof( re ) );
      HB_MEMCPY( cRegex, "***", 3 );
      HB_MEMCPY( cRegex + HB_ALLOC_ALIGNMENT, &re, sizeof( re ) );
      hb_retclenAdoptRaw( cRegex, HB_ALLOC_ALIGNMENT + sizeof( re ) + nSize );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid Regular expression",
                            "Regex subsystem",
                            3, pRegEx, hb_paramError( 3 ), hb_paramError( 4 ) );
}

HB_FUNC( HB_PCRE_VERSION )
{
   HB_THREAD_STUB_API

   hb_retc( pcre_version() );
}

HB_FUNC( HB_ISREGEXSTRING )
{
   HB_THREAD_STUB_API

   PHB_ITEM pRegEx = hb_param( 1, HB_IT_STRING );

   hb_retl( pRegEx && hb_isregexstring( pRegEx ) );
}

HB_FUNC( HB_FORCELINK_PCREPOS )
{
   HB_PCREPOS_LIBRARY();
}
