/*
 * $Id: txtline.c,v 1.1 2004/09/06 19:25:53 mlombardo Exp $
 */

/*
 * xHarbour Project source code:
 * Text line functions like memoedit(), mlCount(), etc.
 *
 * Copyright 2004 Marcelo Lombardo - lombardo@uol.com.br
 * http://www.xharbour.org
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapifs.h"
#include "hbfast.h"
#include "hbset.h"
#include "hbapiitm.h"
#include "hbapierr.h"

void hb_readLine( char * szText, ULONG ulTextLen, ULONG uiLineLen, USHORT uiTabLen, BOOL bWrap, char ** Term, int * iTermSizes, USHORT uiTerms, BOOL * bFound, BOOL * bEOF, LONG * lEnd, ULONG * ulEndOffset )
{
   USHORT uiPosTerm, uiPosition;
   ULONG ulPos, ulCurrCol, ulLastBlk;
   BOOL bBreak = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_readLine(%p, %i, %i, %i, %i, %p, %p, %i, %i, %i, %i, %i)", szText, ulTextLen, uiLineLen, uiTabLen, bWrap, ** Term, * iTermSizes, uiTerms, *bFound, *bEOF, *lEnd, *ulEndOffset ));

   *bFound    = 0;
   *bEOF      = 0;
   *lEnd      = 0;
   ulCurrCol  = 0;
   ulLastBlk  = 0;

   if(ulTextLen <= 0)
   {
      *lEnd        = -1;
      *ulEndOffset = 0;
      *bEOF        = 1;
      return;
   }

   if( !uiTabLen )
   {
      uiTabLen = 4;
   }

   for( ulPos=0; ulPos<ulTextLen; ulPos++ )
   {

      // Check for line terminators

      for( uiPosTerm=0; uiPosTerm < uiTerms; uiPosTerm++ )
      {
         if( szText[ulPos] == Term[uiPosTerm][0] && (ulPos + iTermSizes[uiPosTerm] - 1) < ulTextLen )
         {
            *bFound = 1;
            for(uiPosition=1; uiPosition < iTermSizes[uiPosTerm]; uiPosition++)
            {
               if(Term[uiPosTerm][uiPosition] != szText[ ulPos+uiPosition ])
               {
                  *bFound = 0;
                  break;
               }
            }

            if( *bFound )
            {
               if( ulPos == 0 )
               {
                  *lEnd = -1;
                  *ulEndOffset = iTermSizes[uiPosTerm];
               }
               else
               {
                  *lEnd = ulPos-1;
                  *ulEndOffset = ulPos+ iTermSizes[uiPosTerm];
               }
               break;
            }
         }
      }

      if(  szText[ulPos] == HB_CHAR_HT )
      {
         ulCurrCol += uiTabLen - ( ulCurrCol % uiTabLen );
      }
      else
      {
         ulCurrCol++;
      }

      if(*bFound)
      {
         break;
      }

      if( szText[ulPos] == ' ' || szText[ulPos] == HB_CHAR_HT )
      {
         ulLastBlk = ulPos;
      }

      if( ulCurrCol > uiLineLen )
      {
         if( (!bWrap) || ( bWrap && ulLastBlk == 0 ) )
         {
            *lEnd = ulPos-1;
            *ulEndOffset = ulPos;
            bBreak = 1;
            break;
         }
         else if( bWrap && ulLastBlk != 0 )
         {
            *lEnd = ulLastBlk;
            *ulEndOffset = ulLastBlk + 1;
            bBreak = 1;
            break;
         }
      }
   }

   if((!(*bFound)) && (!bBreak))
   {
      *lEnd        = ulTextLen-1;
      *ulEndOffset = ulTextLen-1;
      *bEOF        = 1;
   }
}

// HB_READLINE( <cText>, [<aTerminators | cTerminator>], <nLineLen>, <nTabLen>, <lWrap>, [<nStartOffset>], @nOffSet, @nEnd, @lFound, @lEOF )

HB_FUNC( HB_READLINE )
{
   PHB_ITEM pTerm1;
   char * szText  = hb_parcx( 1 );
   char ** Term;
   int * iTermSizes;
   USHORT uiTabLen, uiTerms;
   ULONG ulLineSize = hb_parni(3);
   USHORT i;
   BOOL bWrap = hb_parl(5);
   BOOL bFound, bEOF;
   ULONG ulStartOffset;
   ULONG ulEndOffset, ulTextLen;
   LONG lEnd;
   HB_ITEM Opt;

   if( !ISCHAR( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "READLINE", 9, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4),  hb_paramError(5), hb_paramError(6), hb_paramError(7), hb_paramError(8), hb_paramError(9), hb_paramError(10) );
      return;
   }

   ulTextLen = hb_parclen(1);
   uiTabLen  = hb_parclen(4);

   if( ISNUM( 6 ) )
   {
      ulStartOffset = hb_parnl( 6 );
   }
   else
   {
      ulStartOffset = 0;
   }

   if( !(ISARRAY( 2 ) || ISCHAR( 2 )) )
   {
      if( !hb_set.HB_SET_EOL )
      {
         hb_set.HB_SET_EOL = hb_itemPutC( NULL, hb_conNewLine() );
      }
      pTerm1 = hb_set.HB_SET_EOL;
   }
   else
   {
      pTerm1 = hb_param( 2, HB_IT_ANY );
   }

   Opt.type = HB_IT_NIL;

   if( HB_IS_ARRAY( pTerm1 ) )
   {
      uiTerms = (USHORT) pTerm1->item.asArray.value->ulLen;
      Term  = (char**) hb_xgrab( sizeof(char*) * uiTerms );
      iTermSizes = (int *) hb_xgrab( sizeof(int) * uiTerms );

      for(i=0;i<uiTerms;i++)
      {
         hb_arrayGet( pTerm1, i + 1, &Opt );
         Term[i]       = (char *) (&Opt)->item.asString.value;
         iTermSizes[i] = (&Opt)->item.asString.length;
      }
   }
   else
   {
      Term          = (char**) hb_xgrab( sizeof(char*) );
      iTermSizes    = (int *) hb_xgrab( sizeof(int) );
      Term[0]       = (char *) pTerm1->item.asString.value;
      iTermSizes[0] = pTerm1->item.asString.length;
      uiTerms       = 1;
   }

   ulStartOffset--;

   hb_readLine( szText+ulStartOffset, ulTextLen - ulStartOffset, ulLineSize, uiTabLen, bWrap, Term, iTermSizes, uiTerms, &bFound, &bEOF, &lEnd, &ulEndOffset );

   hb_storl( bFound, 7 );
   hb_storl( bEOF, 8 );
   hb_stornl( lEnd+ulStartOffset+1, 9 );
   hb_stornl( ulEndOffset+ulStartOffset+1, 10 );

   hb_xfree( Term );
   hb_xfree( iTermSizes );
}

HB_FUNC( MLCOUNT )
{
   char * pszString    = ISCHAR( 1 ) ? hb_parcx( 1 ) : "";
   ULONG ulLineSize    = ISNUM( 2 ) ? hb_parnl( 2 ) : 79;
   USHORT uiTabLen     = ISNUM( 3 ) ? (USHORT) hb_parni( 3 ) : 4;
   BOOL bLongLines     = ISLOG( 5 ) ? hb_parl( 5 ) : 0;
   BOOL bWrap          = ISLOG( 4 ) ? hb_parl( 4 ) : TRUE;
   BOOL bEOF           = 0;
   ULONG ulCurLength   = 0;
   ULONG ulTextLen     = hb_parclen( 1 );
   ULONG ulLines       = 0;
   ULONG ulStartOffset = 0;
   ULONG ulEndOffset;
   LONG lEnd;
   BOOL  bFound;
   USHORT uiTerms, i;
   char ** Term;
   int * iTermSizes;
   HB_ITEM Opt;
   PHB_ITEM pTerm1;

   if( ulLineSize < 4 || (bLongLines ? 0 : ulLineSize > 254) )
   {
      ulLineSize = 79;
   }

   if( (ULONG)uiTabLen > ulLineSize )
   {
      uiTabLen = ( ulLineSize - 1 > 0 ? ulLineSize - 1 : 1 );
   }

   // Check for EOL police

   if( !(ISARRAY( 2 ) || ISCHAR( 2 )) )
   {
      if( !hb_set.HB_SET_EOL )
      {
         hb_set.HB_SET_EOL = hb_itemPutC( NULL, hb_conNewLine() );
      }
      pTerm1 = hb_set.HB_SET_EOL;
   }
   else
   {
      pTerm1 = hb_param( 2, HB_IT_ANY );
   }

   Opt.type = HB_IT_NIL;

   if( HB_IS_ARRAY( pTerm1 ) )
   {
      uiTerms = (USHORT) pTerm1->item.asArray.value->ulLen;
      Term  = (char**) hb_xgrab( sizeof(char*) * uiTerms );
      iTermSizes = (int *) hb_xgrab( sizeof(int) * uiTerms );

      for(i=0;i<uiTerms;i++)
      {
         hb_arrayGet( pTerm1, i + 1, &Opt );
         Term[i]       = (char *) (&Opt)->item.asString.value;
         iTermSizes[i] = (&Opt)->item.asString.length;
      }
   }
   else
   {
      Term          = (char**) hb_xgrab( sizeof(char*) );
      iTermSizes    = (int *) hb_xgrab( sizeof(int) );
      Term[0]       = (char *) pTerm1->item.asString.value;
      iTermSizes[0] = pTerm1->item.asString.length;
      uiTerms       = 1;
   }

   while( !bEOF )    // All the job is done here
   {
      hb_readLine( pszString+ulStartOffset, ulTextLen - ulStartOffset, ulLineSize, uiTabLen, bWrap, Term, iTermSizes, uiTerms, &bFound, &bEOF, &lEnd, &ulEndOffset );
      ulStartOffset += ulEndOffset;
      if( !((!bFound) &&  bEOF && lEnd == -1L ) )   
      { 
         ulLines++;
      } 
   }

   if( ulCurLength > 0 )
   {
      ulLines++;
   }

   hb_xfree( Term );
   hb_xfree( iTermSizes );

   hb_retnl( ulLines );
}

HB_FUNC( MEMOLINE )
{
   char * pszString    = ISCHAR( 1 ) ? hb_parcx( 1 ) : "";
   ULONG ulLineSize    = ISNUM( 2 ) ? hb_parni( 2 ) : 79;
   ULONG ulLineNumber  = ISNUM( 3 ) ? hb_parni( 3 ) : 1;
   USHORT uiTabLen     = ISNUM( 4 ) ? (USHORT) hb_parni( 4 ) : 4;
   BOOL  bWrap         = ISLOG( 5 ) ? hb_parl( 5 ) : TRUE;
   BOOL  bLongLines    = ISLOG( 6 ) ? hb_parl( 6 ) : 0;
   ULONG ulTextLen     = hb_parclen( 1 );
   ULONG ulLines       = 0;
   ULONG ulStartOffset = 0;
   ULONG ulEndOffset;
   LONG lEnd;
   BOOL bFound, bEOF = FALSE, bLineFound = FALSE;
   USHORT uiTerms, i;
   char ** Term;
   int * iTermSizes;
   HB_ITEM Opt;
   PHB_ITEM pTerm1;
   char * szRet; 

   if( ulLineSize < 4 || (bLongLines ? 0 : ulLineSize > 254) )
   {
      ulLineSize = 79;
   }

   if( (ULONG)uiTabLen > ulLineSize )
   {
      uiTabLen = ( ulLineSize - 1 > 0 ? ulLineSize - 1 : 1 );
   }

   // Check for EOL police

   if( !(ISARRAY( 2 ) || ISCHAR( 2 )) )
   {
      if( !hb_set.HB_SET_EOL )
      {
         hb_set.HB_SET_EOL = hb_itemPutC( NULL, hb_conNewLine() );
      }
      pTerm1 = hb_set.HB_SET_EOL;
   }
   else
   {
      pTerm1 = hb_param( 2, HB_IT_ANY );
   }

   Opt.type = HB_IT_NIL;

   if( HB_IS_ARRAY( pTerm1 ) )
   {
      uiTerms = (USHORT) pTerm1->item.asArray.value->ulLen;
      Term  = (char**) hb_xgrab( sizeof(char*) * uiTerms );
      iTermSizes = (int *) hb_xgrab( sizeof(int) * uiTerms );

      for(i=0;i<uiTerms;i++)
      {
         hb_arrayGet( pTerm1, i + 1, &Opt );
         Term[i]       = (char *) (&Opt)->item.asString.value;
         iTermSizes[i] = (&Opt)->item.asString.length;
      }
   }
   else
   {
      Term          = (char**) hb_xgrab( sizeof(char*) );
      iTermSizes    = (int *) hb_xgrab( sizeof(int) );
      Term[0]       = (char *) pTerm1->item.asString.value;
      iTermSizes[0] = pTerm1->item.asString.length;
      uiTerms       = 1;
   }

   szRet = (char *) hb_xgrab( ulLineSize + 1 ); 
   memset( szRet, ' ', ulLineSize );
   szRet[ulLineSize] = HB_CHAR_NUL;

   while( !bEOF )
   {
      hb_readLine( pszString+ulStartOffset, ulTextLen - ulStartOffset, ulLineSize, uiTabLen, bWrap, Term, iTermSizes, uiTerms, &bFound, &bEOF, &lEnd, &ulEndOffset );
      if( !((!bFound) &&  bEOF && lEnd == -1L ) )    
      { 
         ulLines++;
      } 
      if (ulLines == ulLineNumber)
      {
         LONG lPos, lSpAdded = 0;

         for( lPos = 0; lPos <= lEnd; lPos++ )
         {
            if( pszString[ ulStartOffset + lPos ] == HB_CHAR_HT )
            {
               lSpAdded += uiTabLen - ( (lPos+lSpAdded) % uiTabLen ) - 1;
            }
            else
            {
               * ( szRet + lPos + lSpAdded ) = * ( pszString + ulStartOffset + lPos );
            }
         }
         hb_retclenAdopt( szRet, ulLineSize );
         bLineFound = TRUE;
         break;
      }
      ulStartOffset += ulEndOffset;
   }

   if( ulLines+1 < ulLineNumber )
   {
      hb_retc( "" );
      hb_xfree( szRet ); 
   }
   else if( !bLineFound)
   {
      hb_retclenAdopt( szRet, ulLineSize );  
   }

   hb_xfree( Term );
   hb_xfree( iTermSizes );
}

HB_FUNC( MLPOS ) 
{
   char * pszString    = ISCHAR( 1 ) ? hb_parcx( 1 ) : "";
   ULONG ulLineSize    = ISNUM( 2 ) ? hb_parni( 2 ) : 79;
   ULONG ulLineNumber  = ISNUM( 3 ) ? hb_parni( 3 ) : 1;
   USHORT uiTabLen     = ISNUM( 4 ) ? (USHORT) hb_parni( 4 ) : 4;
   BOOL  bWrap         = ISLOG( 5 ) ? hb_parl( 5 ) : TRUE;
   BOOL  bLongLines    = ISLOG( 6 ) ? hb_parl( 6 ) : 0;
   ULONG ulTextLen     = hb_parclen( 1 );
   ULONG ulLines       = 0;
   ULONG ulStartOffset = 0;
   ULONG ulEndOffset;
   LONG lEnd;
   BOOL bFound, bEOF = FALSE, bLineFound = FALSE;
   USHORT uiTerms, i;
   char ** Term;
   int * iTermSizes;
   HB_ITEM Opt;
   PHB_ITEM pTerm1;

   if( ulLineSize < 4 || (bLongLines ? 0 : ulLineSize > 254) )
   {
      ulLineSize = 79;
   }

   if( (ULONG)uiTabLen > ulLineSize )
   {
      uiTabLen = ( ulLineSize - 1 > 0 ? ulLineSize - 1 : 1 );
   }

   // Check for EOL police

   if( !(ISARRAY( 2 ) || ISCHAR( 2 )) )
   {
      if( !hb_set.HB_SET_EOL )
      {
         hb_set.HB_SET_EOL = hb_itemPutC( NULL, hb_conNewLine() );
      }
      pTerm1 = hb_set.HB_SET_EOL;
   }
   else
   {
      pTerm1 = hb_param( 2, HB_IT_ANY );
   }

   Opt.type = HB_IT_NIL;

   if( HB_IS_ARRAY( pTerm1 ) )
   {
      uiTerms = (USHORT) pTerm1->item.asArray.value->ulLen;
      Term  = (char**) hb_xgrab( sizeof(char*) * uiTerms );
      iTermSizes = (int *) hb_xgrab( sizeof(int) * uiTerms );

      for(i=0;i<uiTerms;i++)
      {
         hb_arrayGet( pTerm1, i + 1, &Opt );
         Term[i]       = (char *) (&Opt)->item.asString.value;
         iTermSizes[i] = (&Opt)->item.asString.length;
      }
   }
   else
   {
      Term          = (char**) hb_xgrab( sizeof(char*) );
      iTermSizes    = (int *) hb_xgrab( sizeof(int) );
      Term[0]       = (char *) pTerm1->item.asString.value;
      iTermSizes[0] = pTerm1->item.asString.length;
      uiTerms       = 1;
   }

   while( !bEOF )
   {
      hb_readLine( pszString+ulStartOffset, ulTextLen - ulStartOffset, ulLineSize, uiTabLen, bWrap, Term, iTermSizes, uiTerms, &bFound, &bEOF, &lEnd, &ulEndOffset );
      if( !((!bFound) &&  bEOF && lEnd == -1L ) )    
      { 
         ulLines++;
      } 
      if (ulLines == ulLineNumber)
      {
         hb_retni( ulStartOffset + 1 ); 
         bLineFound = TRUE;
         break;
      }
      ulStartOffset += ulEndOffset;
   }

   if( !bLineFound)
   {
      hb_retni( ulTextLen ); 
   }

   hb_xfree( Term );
   hb_xfree( iTermSizes );
}
 
