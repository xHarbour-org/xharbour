/*
 * $Id: arrayshb.c,v 1.16 2002/07/23 01:36:43 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * The Array API (Harbour level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org
 *
 * Copyright 2001 Ron Pinkas <ron@profit-master.com>
 * HB_FUNC( HB_APARAMS )
 * HB_FUNC( HB_AEXPRESSIONS )
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "cstruct.ch"

/* This function creates an array item using 'iDimension' as an index
 * to retrieve the number of elements from the parameter list.
 */
static void hb_arrayNewRagged( PHB_ITEM pArray, int iDimension )
{
   ULONG ulElements;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayNewRagged(%p, %d)", pArray, iDimension));

   ulElements = ( ULONG ) hb_parnl( iDimension );

   /* create an array */
   hb_arrayNew( pArray, ulElements );

   if( ++iDimension <= hb_pcount() )
   {
      /* call self recursively to create next dimensions
       */
      while( ulElements )
         hb_arrayNewRagged( hb_arrayGetItemPtr( pArray, ulElements-- ), iDimension );
   }
}

HB_FUNC( ARRAY )
{
   int iPCount = hb_pcount();

   if( iPCount > 0 )
   {
      BOOL bError = FALSE;
      int iParam;

      for( iParam = 1; iParam <= iPCount; iParam++ )
      {
         if( ! ISNUM( iParam ) )
         {
            bError = TRUE;
            break;
         }

         if( hb_parnl( iParam ) < 0 ) /* || hb_parnl( iParam ) <= 4096 */
         {
            hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ), 1, hb_paramError( 1 ) );
            bError = TRUE;
            break;
         }
      }

      if( ! bError )
         hb_arrayNewRagged( &hb_stack.Return, 1 );
   }
}

HB_FUNC( AADD )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

      if( pValue && hb_arrayAdd( pArray, pValue ) )
         hb_itemCopy( &hb_stack.Return, pValue );
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, "AADD", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "AADD", 2, hb_paramError(1), hb_paramError( 2 ) );
}

HB_FUNC( HB_ARRAYID )  /* for debugging: returns the array's "address" so dual references to same array can be seen */
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( HB_IS_ARRAY(pArray) )
      hb_retnl( (long) pArray->item.asArray.value );
   else
      hb_retnl( -1 );
}


/* NOTE: CA-Cl*pper 5.3 and older will return NIL on bad parameter, 5.3a,b
         will throw a runtime error. [vszakats] */

HB_FUNC( ASIZE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && ISNUM( 2 ) )
   {
      long lSize = hb_parnl( 2 );

      hb_arraySize( pArray, HB_MAX( lSize, 0 ) );

      hb_itemCopy( &hb_stack.Return, pArray ); /* ASize() returns the array itself */
   }
#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ASIZE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
#endif
}

HB_FUNC( ATAIL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, &hb_stack.Return );
}

HB_FUNC( AINS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
    #ifndef HB_C52_STRICT
      PHB_ITEM pExtend = hb_param( 4, HB_IT_LOGICAL );

      if( pExtend && pExtend->item.asLogical.value )
      {
         hb_arraySize( pArray, pArray->item.asArray.value->ulLen + 1 );
      }
    #endif

      if( ISNUM( 2 ) )
      {
         hb_arrayIns( pArray, hb_parnl( 2 ) );
      }

    #ifndef HB_C52_STRICT
      if( hb_pcount() >= 3 )
      {
         hb_arraySet( pArray, hb_parnl( 2 ), hb_param( 3, HB_IT_ANY ) );
      }
    #endif

      hb_itemCopy( &hb_stack.Return, pArray ); /* AIns() returns the array itself */
   }
}

HB_FUNC( ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      if( ISNUM( 2 ) )
      {
         hb_arrayDel( pArray, hb_parnl( 2 ) );
      }
      else
      {
         hb_arrayDel( pArray, 1 );
      }

      hb_itemCopy( &hb_stack.Return, pArray ); /* ADel() returns the array itself */
   }
}

HB_FUNC( AFILL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

      if( pValue )
      {
         long ulStart = hb_parnl( 3 );
         long ulCount = hb_parnl( 4 );

         /* Explicy ulCount of 0 - Nothing to do! */
         if( ISNUM(4) && ulCount == 0 )
         {
            hb_itemCopy( &hb_stack.Return, pArray ); /* AFill() returns the array itself */
            return;
         }

         if( ulStart == 0 )
         {
            /* Clipper allows Start to be of wrong type, or 0, and corrects it to 1. */
            ulStart = 1;
         }
         else if( ulStart < 0 )
         {
            /* Clipper aborts if negative start. */
            hb_itemCopy( &hb_stack.Return, pArray ); /* AFill() returns the array itself */
            return;
         }

         if( ulCount == 0 )
         {
            /* Clipper allows the Count to be of wrong type, and corrects it to maximum elements. */
            ulCount = pArray->item.asArray.value->ulLen;
         }
         else if( ulCount < 0 )
         {
            if( ulStart == 1 )
            {
               /* Clipper allows the Count to be negative, if start is 1, and corrects it to maximum elements. */
               ulCount = pArray->item.asArray.value->ulLen;
            }
            else
            {
               /* Clipper aborts if negative count and start is not at 1. */
               hb_itemCopy( &hb_stack.Return, pArray ); /* AFill() returns the array itself */
               return;
            }
         }

         hb_arrayFill( pArray, pValue, (ULONG) ulStart, (ULONG) ulCount );
      }

      hb_itemCopy( &hb_stack.Return, pArray ); /* AFill() returns the array itself */
   }
   else
   {
      #ifdef HB_C52_STRICT
        /* NOTE: In CA-Cl*pper AFILL() is written in a manner that it will
               call AEVAL() to do the job, so the error (if any) will also be
               thrown by AEVAL().  [vszakats] */
        hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", 4, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
      #else
        hb_errRT_BASE( EG_ARG, 9999, NULL, "AFILL", 4, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
      #endif
   }
}

HB_FUNC( ASCAN )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pArray && pValue )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );

      hb_retnl( hb_arrayScan( pArray,
                              pValue,
                              ISNUM( 3 ) ? &ulStart : NULL,
                              ISNUM( 4 ) ? &ulCount : NULL ) );
   }
   else
      hb_retnl( 0 );
}

/* TODO: In Xbase++ fifth parameter determines whether array elements
         are passed by reference to the code block. [vszakats] */

HB_FUNC( AEVAL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

   if( pArray && pBlock )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );

      hb_arrayEval( pArray,
                    pBlock,
                    ISNUM( 3 ) ? &ulStart : NULL,
                    ISNUM( 4 ) ? &ulCount : NULL );

      hb_itemCopy( &hb_stack.Return, hb_stackItemFromBase( 1 ) ); /* AEval() returns the array itself */
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", 4, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
}

HB_FUNC( ACOPY )
{
   PHB_ITEM pSrcArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pDstArray = hb_param( 2, HB_IT_ARRAY );

   if( pSrcArray && pDstArray )
   {
      /* CA-Cl*pper works this way. */
      if( ! hb_arrayIsObject( pSrcArray ) && ! hb_arrayIsObject( pDstArray ) )
      {
         ULONG ulStart = hb_parnl( 3 );
         ULONG ulCount = hb_parnl( 4 );
         ULONG ulTarget = hb_parnl( 5 );

         hb_arrayCopy( pSrcArray,
                       pDstArray,
                       ISNUM( 3 ) ? &ulStart : NULL,
                       ISNUM( 4 ) ? &ulCount : NULL,
                       ISNUM( 5 ) ? &ulTarget : NULL );
      }

      hb_itemCopy( &hb_stack.Return, hb_stackItemFromBase( 2 ) ); /* ACopy() returns the target array */
   }
}

/* NOTE: Clipper will return NIL if the parameter is not an array. [vszakats] */

HB_FUNC( ACLONE )
{
   PHB_ITEM pSrcArray = hb_param( 1, HB_IT_ARRAY );

   if( pSrcArray && ! hb_arrayIsObject( pSrcArray ) )
      hb_itemRelease( hb_itemReturn( hb_arrayClone( pSrcArray, NULL ) ) ); /* AClone() returns the new array */
}

HB_FUNC( HB_APARAMS )
{
   PHB_ITEM * pBase = hb_stack.pBase;

   pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackbase;

   hb_itemRelease( hb_itemReturn( hb_arrayFromParams( pBase ) ) );
}

HB_FUNC( HB_AEXPRESSIONS )
{
   PHB_ITEM pArray = &hb_stack.Return;
   PHB_ITEM pLine  = hb_param( 1, HB_IT_STRING );
   size_t i, iOffset = 0;
   int iParans = 0, iArrays = 0, iIndexs = 0;
   BOOL bArray = FALSE;

   if( pLine == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "HB_AEXPRESSIONS", 1, hb_paramError(1) );
      return;
   }

   hb_arrayNew( pArray, 0 );

   for( i = 0; i < pLine->item.asString.length; i++ )
   {
      switch( pLine->item.asString.value[i] )
      {
         case '(' :
            iParans++;
            bArray = FALSE;
            break;

         case ')' :
            iParans--;
            bArray = TRUE;
            break;

         case '{' :
            iArrays++;
            bArray = FALSE;
            break;

         case '}' :
            iArrays--;
            bArray = TRUE;
            break;

         case '[' :
            if( bArray || ( i && isalnum((int) pLine->item.asString.value[i - 1] ) ) )
            {
               iIndexs++;
            }
            else
            {
               while( ++i < pLine->item.asString.length  && pLine->item.asString.value[i] != ']'  );
            }
            bArray = FALSE;
            break;

         case ']' :
            iIndexs--;
            bArray = TRUE;
            break;

         case '"' :
            while( ++i < pLine->item.asString.length && pLine->item.asString.value[i] != '"'  );
            bArray = FALSE;
            break;

         case '\'' :
            while( ++i < pLine->item.asString.length && pLine->item.asString.value[i] != '\''  );
            bArray = FALSE;
            break;

         case ',' :
            if( iParans == 0 && iArrays == 0 && iIndexs == 0 )
            {
               PHB_ITEM pExp = hb_itemNew( NULL );

               hb_arrayAdd( pArray, hb_itemPutCL( pExp, pLine->item.asString.value + iOffset, i - iOffset ) );
               iOffset = i + 1;

               hb_itemRelease( pExp );
            }
            bArray = FALSE;
            break;

         default :
            bArray = FALSE;
            break;
      }
   }

   if( iOffset < pLine->item.asString.length - 1 )
   {
      PHB_ITEM pExp = hb_itemNew( NULL );

      hb_arrayAdd( pArray, hb_itemPutCL( pExp, pLine->item.asString.value + iOffset, pLine->item.asString.length - iOffset ) );

      hb_itemRelease( pExp );
   }
}

HB_FUNC( HB_ATOKENS )
{
   PHB_ITEM pLine  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pDelim = hb_param( 2, HB_IT_STRING );

   if( pLine )
   {
      PHB_ITEM pArray = &hb_stack.Return;
      PHB_ITEM pToken = hb_itemNew( NULL );
      char cDelimiter = pDelim ? pDelim->item.asString.value[0] : 32;
      size_t i, iOffset = 0;

      hb_arrayNew( pArray, 0 );

      for( i = 0; i < pLine->item.asString.length; i++ )
      {
         if( pLine->item.asString.value[i] == cDelimiter )
         {
            hb_arrayAddForward( pArray, hb_itemPutCL( pToken, pLine->item.asString.value + iOffset, i - iOffset ) );

            iOffset = i + 1;
         }
      }
      if( iOffset < pLine->item.asString.length )
      {
         hb_arrayAddForward( pArray, hb_itemPutCL( pToken, pLine->item.asString.value + iOffset, pLine->item.asString.length - iOffset ) );
      }

      hb_itemRelease( pToken );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "HB_ATOKENS", 2, hb_paramError(1), hb_paramError(2) );
      return;
   }
}

unsigned int SizeOfCStructure( PHB_ITEM aDef, unsigned int uiAlign )
{
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   ULONG ulLen = pBaseDef->ulLen;
   ULONG ulIndex;
   unsigned int uiSize = 0, uiMemberSize;
   unsigned char cShift;

   for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
   {
      if( ( pBaseDef->pItems + ulIndex )->type != HB_IT_INTEGER )
      {
         hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
         return 0;
      }

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR : // char
         case CTYPE_UNSIGNED_CHAR : // unsigned char
            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR : // char *
         case CTYPE_UNSIGNED_CHAR_PTR : // unsigned char *
            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT : // short
         case CTYPE_UNSIGNED_SHORT : // unsigned short
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR : // short
         case CTYPE_UNSIGNED_SHORT_PTR : // unsigned short
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT : // int
         case CTYPE_UNSIGNED_INT : // unsigned int
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR : // int *
         case CTYPE_UNSIGNED_INT_PTR : // unsigned int *
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG : // long
         case CTYPE_UNSIGNED_LONG : // unsigned long
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR : // long *
         case CTYPE_UNSIGNED_LONG_PTR : // unsigned long *
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT : // float
            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR : // float *
            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE : // double
            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR : // double *
            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR : // void * (pointer)
            uiMemberSize = sizeof( void * );
            break;

         default:
         {
            PHB_ITEM pID = hb_itemPutNI( NULL, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof( void * );
            }
            else if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "ACTYPES", 0 );

                  uiMemberSize = SizeOfCStructure( &hb_stack.Return, uiAlign  );
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
               return 0;
            }
         }
      }

      if( uiSize )
      {
         unsigned int uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( uiSize % uiPad ) ) > 0 )
         {
            uiSize += ( uiPad - cShift );
         }
      }

      //printf( "Size: %i Offset: %i\n", uiMemberSize, uiSize );

      uiSize += uiMemberSize;
   }

   if( ( cShift = ( uiSize % uiAlign ) ) > 0 )
   {
      uiSize += ( uiAlign - cShift );
   }

   return uiSize;
}

HB_FUNC( HB_SIZEOFCSTRUCTURE )
{
   PHB_ITEM aDef = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pAlign = hb_param( 1, HB_IT_INTEGER );
   unsigned int uiAlign;

   if( aDef )
   {
      if( pAlign )
      {
         uiAlign = (unsigned char) pAlign->item.asInteger.value;
      }
      else
      {
         uiAlign = 4;
      }

      hb_retni( SizeOfCStructure( aDef, uiAlign ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

BYTE * ArrayToStructure( PHB_ITEM aVar, PHB_ITEM aDef, unsigned int uiAlign, unsigned int * puiSize )
{
   PHB_BASEARRAY pBaseVar = aVar->item.asArray.value;
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   ULONG ulLen = pBaseDef->ulLen;
   ULONG ulIndex;
   BYTE  *Buffer;
   unsigned int uiOffset = 0, uiMemberSize;
   unsigned char cShift;

   *puiSize = SizeOfCStructure( aDef, uiAlign ) ;

   //printf( "Size: %i\n", *puiSize );

   Buffer = (BYTE *) hb_xgrab( *puiSize );

   for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
   {
      //printf( "#: %i\n", ulIndex );

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR : // char
         case CTYPE_UNSIGNED_CHAR : // unsigned char
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_INTEGER )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR : // char *
         case CTYPE_UNSIGNED_CHAR_PTR : // unsigned char *
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_STRING )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;;
            }

            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT : // short
         case CTYPE_UNSIGNED_SHORT : // unsigned short
            // Type check performed in actual translation...
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR : // short *
         case CTYPE_UNSIGNED_SHORT_PTR : // unsigned short *
            // Type check performed in actual translation...
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT : // int
         case CTYPE_UNSIGNED_INT : // unsigned int
            // Type check performed in actual translation...
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR : // int *
         case CTYPE_UNSIGNED_INT_PTR : // unsigned int *
            // Type check performed in actual translation...
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG : // long
         case CTYPE_UNSIGNED_LONG : // unsigned long
            // Type check performed in actual translation...
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR : // long *
         case CTYPE_UNSIGNED_LONG_PTR : // unsigned long *
            // Type check performed in actual translation...
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT : // float
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR : // float *
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE : // double
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR : // double *
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR : // void * (pointer)
            if( ( pBaseVar->pItems + ulIndex  )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            uiMemberSize = sizeof( void * );
            break;

         default:
         {
            PHB_ITEM pID = hb_itemPutNI( NULL, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof( void * );
            }
            else if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "ACTYPES", 0 );

                  uiMemberSize = SizeOfCStructure( &hb_stack.Return, uiAlign  );
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
               return 0;
            }
         }
      }

      if( uiOffset )
      {
         unsigned int uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( uiOffset % uiPad ) ) > 0 )
         {
            uiOffset += ( uiPad - cShift );
         }
      }

      //printf( "* Size: %i Offset: %i\n", uiMemberSize, uiOffset );

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR : // char
            *( (char *) ( Buffer + uiOffset ) ) = (char) ( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            break;

         case CTYPE_UNSIGNED_CHAR : // unsigned char
            *( (unsigned char *) ( Buffer + uiOffset ) ) = (unsigned char) ( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            break;

         case CTYPE_CHAR_PTR : // char *
            *( (char **) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex  )->item.asString.value;
            break;

         case CTYPE_UNSIGNED_CHAR_PTR : // unsigned char *
            *( (unsigned char **) ( Buffer + uiOffset ) ) = (unsigned char *) ( ( pBaseVar->pItems + ulIndex  )->item.asString.value );
            break;

         case CTYPE_SHORT : // short
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (short *) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex  )->item.asInteger.value;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_SHORT : // unsigned short
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (unsigned short *) ( Buffer + uiOffset ) ) = (unsigned short) ( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (unsigned short *) ( Buffer + uiOffset ) ) = (unsigned short) ( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_SHORT_PTR : // short *
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (short **) ( Buffer + uiOffset ) ) = (short *) &( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_SHORT_PTR : // unsigned short *
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (unsigned short **) ( Buffer + uiOffset ) ) = (unsigned short *) &( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (unsigned short **) ( Buffer + uiOffset ) ) = (unsigned short *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_INT : // int
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (int *) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex  )->item.asInteger.value;
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (int *) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex  )->item.asLong.value;
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_INT : // unsigned int
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (unsigned int *) ( Buffer + uiOffset ) ) = (unsigned int) ( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (unsigned int *) ( Buffer + uiOffset ) ) = (unsigned int) ( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_DOUBLE )
            {
               *( (unsigned int *) ( Buffer + uiOffset ) ) = (unsigned int) ( ( pBaseVar->pItems + ulIndex  )->item.asDouble.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            break;

         case CTYPE_INT_PTR : // int *
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (int **) ( Buffer + uiOffset ) ) = (int *) &( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (int **) ( Buffer + uiOffset ) ) = (int *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }

            break;

         case CTYPE_UNSIGNED_INT_PTR : // unsigned int *
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (unsigned int **) ( Buffer + uiOffset ) ) = (unsigned int *) &( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (unsigned int **) ( Buffer + uiOffset ) ) = (unsigned int *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_DOUBLE )
            {
               unsigned long ulTemp = (unsigned int) ( pBaseVar->pItems + ulIndex  )->item.asDouble.value;

               ( pBaseVar->pItems + ulIndex  )->item.asLong.value = ulTemp;
               ( pBaseVar->pItems + ulIndex  )->type = HB_IT_LONG;
               *( (unsigned int **) ( Buffer + uiOffset ) ) = (unsigned int *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_LONG : // long
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (long *) ( Buffer + uiOffset ) ) = (long) ( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (long *) ( Buffer + uiOffset ) ) = (long) ( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_LONG : // unsigned long
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (unsigned long *) ( Buffer + uiOffset ) ) = (unsigned long) ( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (unsigned long *) ( Buffer + uiOffset ) ) = (unsigned long) ( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_DOUBLE )
            {
               *( (unsigned long *) ( Buffer + uiOffset ) ) = (unsigned long) ( ( pBaseVar->pItems + ulIndex  )->item.asDouble.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_LONG_PTR : // long *
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (long **) ( Buffer + uiOffset ) ) = (long *) &( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (long **) ( Buffer + uiOffset ) ) = (long *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_LONG_PTR : // unsigned long *
            if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_INTEGER )
            {
               *( (unsigned long **) ( Buffer + uiOffset ) ) = (unsigned long *) &( ( pBaseVar->pItems + ulIndex  )->item.asInteger.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_LONG )
            {
               *( (unsigned long **) ( Buffer + uiOffset ) ) = (unsigned long *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else if( ( pBaseVar->pItems + ulIndex  )->type == HB_IT_DOUBLE )
            {
               unsigned long ulTemp = (unsigned int) ( pBaseVar->pItems + ulIndex  )->item.asDouble.value;

               ( pBaseVar->pItems + ulIndex  )->item.asLong.value = ulTemp;
               ( pBaseVar->pItems + ulIndex  )->type = HB_IT_LONG;
               *( (unsigned long **) ( Buffer + uiOffset ) ) = (unsigned long *) &( ( pBaseVar->pItems + ulIndex  )->item.asLong.value );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return NULL;
            }
            break;

         case CTYPE_FLOAT : // float
            *( (double *) ( Buffer + uiOffset ) ) = (float) ( pBaseVar->pItems + ulIndex  )->item.asDouble.value;
            break;

         case CTYPE_FLOAT_PTR : // float *
            *( (float **) ( Buffer + uiOffset ) ) = (float *) &( ( pBaseVar->pItems + ulIndex  )->item.asDouble.value );
            break;

         case CTYPE_DOUBLE : // double
            *( (double *) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex  )->item.asDouble.value;
            break;

         case CTYPE_DOUBLE_PTR : // double *
            *( (double **) ( Buffer + uiOffset ) ) = (double *) &( ( pBaseVar->pItems + ulIndex  )->item.asDouble.value );
            break;

         case CTYPE_VOID_PTR : // void *
            *( (void **) ( Buffer + uiOffset ) ) = (void *) hb_itemGetNL( pBaseVar->pItems + ulIndex  );
            break;

         default:
         {
            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = pBaseVar->pItems + ulIndex;

               if( HB_IS_NIL( pStructure ) )
               {
                  if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                  {
                     *( (void **) ( Buffer + uiOffset ) ) = NULL;
                  }
                  else
                  {
                     printf( "Empty Inplace\n" );
                     memset( (void *) ( Buffer + uiOffset ), 0, uiMemberSize );
                  }
               }
               else if( strncmp( hb_objGetClsName( pStructure ), "C Structure", 11 ) == 0 )
               {
                  PHB_BASEARRAY pBaseStructure = pStructure->item.asArray.value;
                  PHB_ITEM pInternalBuffer = pBaseStructure->pItems + pBaseStructure->ulLen - 1;

                  hb_objSendMsg( pStructure, "VALUE", 0 );

                  if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                  {
                     *( (void **) ( Buffer + uiOffset ) ) = (void *) pInternalBuffer->item.asString.value;
                  }
                  else
                  {
                     memcpy( (void *) ( Buffer + uiOffset ), (void *) pInternalBuffer->item.asString.value, uiMemberSize );
                  }
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
            }
         }
      }

      //printf( "Wrote %i bytes at Offset %i\n", uiMemberSize, uiOffset );

      uiOffset += uiMemberSize;
   }

   return Buffer;
}

HB_FUNC( HB_ARRAYTOSTRUCTURE )
{
   PHB_ITEM aVar = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM aDef = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pAlign = hb_param( 3, HB_IT_INTEGER );

   if( aVar && aDef )
   {
      unsigned int uiSize;
      unsigned int uiAlign;
      BYTE *Buffer;

      if( pAlign )
      {
         uiAlign = (unsigned char) pAlign->item.asInteger.value;
      }
      else
      {
         uiAlign = 4;
      }

      Buffer = ArrayToStructure( aVar, aDef, uiAlign, &uiSize );

      hb_itemPutCRaw( &hb_stack.Return, (char *) Buffer, uiSize );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
   }
}

PHB_ITEM StructureToArray( BYTE* Buffer, PHB_ITEM aDef, unsigned int uiAlign )
{
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   ULONG ulLen = pBaseDef->ulLen;
   ULONG ulIndex;
   unsigned int uiOffset, uiMemberSize;
   unsigned char cShift;
   PHB_ITEM pRet = hb_itemNew( NULL );
   PHB_BASEARRAY pBaseVar;

   hb_arrayNew( pRet, ulLen );
   pBaseVar = pRet->item.asArray.value;

   uiOffset = 0;
   for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
   {
      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR : // char
         case CTYPE_UNSIGNED_CHAR : // unsigned char
            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR : // char *
         case CTYPE_UNSIGNED_CHAR_PTR : // unsigned char *
            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT : // short
         case CTYPE_UNSIGNED_SHORT : // unsigned short
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR : // short *
         case CTYPE_UNSIGNED_SHORT_PTR : // unsigned short *
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT : // int
         case CTYPE_UNSIGNED_INT : // unsigned int
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR : // int *
         case CTYPE_UNSIGNED_INT_PTR : // unsigned int *
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG : // long
         case CTYPE_UNSIGNED_LONG : // unsigned long
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR : // long *
         case CTYPE_UNSIGNED_LONG_PTR : // unsigned long *
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT : // float
            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR : // float *
            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE : // double
            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR : // double *
            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR : // void * (pointer)
            uiMemberSize = sizeof( void * );
            break;

         default:
         {
            PHB_ITEM pID = hb_itemPutNI( NULL, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof( void * );
            }
            else if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "ACTYPES", 0 );

                  uiMemberSize = SizeOfCStructure( &hb_stack.Return, uiAlign  );
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 1, hb_paramError( 1 ) );
                  return pRet;;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
               return pRet;;
            }
         }
      }

      if( uiOffset )
      {
         unsigned int uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( uiOffset % uiPad ) ) > 0 )
         {
            uiOffset += ( uiPad - cShift );
         }
      }

      //printf( "* Size: %i Offset: %i\n", uiMemberSize, uiOffset );

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR : // char
            hb_itemPutNI( pBaseVar->pItems + ulIndex , (int) *( (char *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_CHAR : // unsigned char
            hb_itemPutNI( pBaseVar->pItems + ulIndex , (int) *( (unsigned char *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_CHAR_PTR : // char *
            hb_itemPutC( pBaseVar->pItems + ulIndex , *( (char **) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_CHAR_PTR : // unsigned char *
            hb_itemPutC( pBaseVar->pItems + ulIndex , (char *) *( (unsigned char **) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_SHORT : // short
            hb_itemPutNI( pBaseVar->pItems + ulIndex , *( (short *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_SHORT : // unsigned short
            hb_itemPutNI( pBaseVar->pItems + ulIndex , (short) *( (unsigned short *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_SHORT_PTR : // short *
            hb_itemPutNI( pBaseVar->pItems + ulIndex , *( *( (short **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_UNSIGNED_SHORT_PTR : // unsigned short *
            hb_itemPutNI( pBaseVar->pItems + ulIndex , (short) *( *( (unsigned short **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_INT : // int
            hb_itemPutNI( pBaseVar->pItems + ulIndex , *( (int *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_INT : // unsigned int
            hb_itemPutNI( pBaseVar->pItems + ulIndex , (int) *( (unsigned int *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_INT_PTR : // int *
            hb_itemPutNI( pBaseVar->pItems + ulIndex , *( *( (int **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_UNSIGNED_INT_PTR : // unsigned int *
            hb_itemPutNI( pBaseVar->pItems + ulIndex , (int) *( *( (unsigned int **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_LONG : // long
            hb_itemPutNL( pBaseVar->pItems + ulIndex , *( (long *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_LONG : // unsigned long
            hb_itemPutNL( pBaseVar->pItems + ulIndex , (long) *( (unsigned long *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_LONG_PTR : // long *
            hb_itemPutNL( pBaseVar->pItems + ulIndex , *( *( (long **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_UNSIGNED_LONG_PTR : // unsigned long *
            hb_itemPutNL( pBaseVar->pItems + ulIndex , (long) *( *( (unsigned long **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_FLOAT : // float
            hb_itemPutND( pBaseVar->pItems + ulIndex , (double) *( (float *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_FLOAT_PTR : // float *
            hb_itemPutND( pBaseVar->pItems + ulIndex , (double) *( *( (float **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_DOUBLE : // double
            hb_itemPutND( pBaseVar->pItems + ulIndex , *( (double *) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_DOUBLE_PTR : // double *
            hb_itemPutND( pBaseVar->pItems + ulIndex , *( *( (double **) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_VOID_PTR : // void *
            hb_itemPutNL( pBaseVar->pItems + ulIndex , (long) (void *) ( Buffer + uiOffset ) );
            break;

         default:
         {
            PHB_ITEM pID = hb_itemPutNI( NULL, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );
            PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );
            unsigned int uiNestedSize, uiNestedAlign;

            hb_objSendMsg( pStructure, "NALIGN", 0 );
            uiNestedAlign = ( &hb_stack.Return )->item.asInteger.value;

            hb_objSendMsg( pStructure, "ACTYPES", 0 );
            uiNestedSize = SizeOfCStructure( &hb_stack.Return, uiNestedAlign );

            if( HB_IS_OBJECT( pStructure ) )
            {
               if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
               {

                  //printf( "Offset %i Pointer: %p\n", uiOffset, *(char **) ( (long ** )( Buffer + uiOffset ) ) );

                  if( *(char **) ( (long ** )( Buffer + uiOffset ) ) )
                  {
                     PHB_BASEARRAY pBaseStructure = pStructure->item.asArray.value;
                     PHB_ITEM pInternalBuffer = pBaseStructure->pItems + pBaseStructure->ulLen - 1;

                     hb_itemPutCRaw( pInternalBuffer, *(char **) ( (long **)( Buffer + uiOffset ) ), uiNestedSize );

                     hb_objSendMsg( pStructure, "DEVALUE", 0 );
                  }
                  else
                  {
                     //hb_objSendMsg( pStructure, "RESET", 0 );
                     hb_itemClear( pStructure );
                  }
               }
               else
               {
                  PHB_BASEARRAY pBaseStructure = pStructure->item.asArray.value;
                  PHB_ITEM pInternalBuffer = pBaseStructure->pItems + pBaseStructure->ulLen - 1;

                  hb_itemPutCRawStatic( pInternalBuffer, (char *) (unsigned char *)( Buffer + uiOffset ), uiNestedSize );

                  hb_objSendMsg( pStructure, "DEVALUE", 0 );
               }

               hb_itemForwardValue( pBaseVar->pItems + ulIndex, pStructure );

               hb_itemRelease( pID );
               hb_itemRelease( pStructure );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               return pRet;
            }
         }
      }

      uiOffset += uiMemberSize;
   }

   return pRet;
}

HB_FUNC( HB_STRUCTURETOARRAY )
{
   PHB_ITEM Structure = hb_param( 1, HB_IT_STRING );
   PHB_ITEM aDef = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pAlign = hb_param( 3, HB_IT_INTEGER );

   if( Structure && aDef )
   {
      PHB_ITEM pRet;
      BYTE  *Buffer = (BYTE *) Structure->item.asString.value;
      unsigned int uiAlign;

      if( pAlign )
      {
         uiAlign = (unsigned char) pAlign->item.asInteger.value;
      }
      else
      {
         uiAlign = 4;
      }

      hb_itemForwardValue( &hb_stack.Return, pRet = StructureToArray( Buffer, aDef, uiAlign ) );

      hb_itemRelease( pRet );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}
