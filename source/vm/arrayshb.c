/*
 * $Id$
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
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *
 */

#include <ctype.h>

#include "hbvmopt.h"
#include "hbvm.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "cstruct.h"
#include "hbset.h"

/* This function creates an array item using 'iDimension' as an index
 * to retrieve the number of elements from the parameter list.
 */
static void hb_arrayNewRagged( PHB_ITEM pArray, int iDimension )
{
   HB_SIZE ulElements;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayNewRagged(%p, %d)", pArray, iDimension ) );

   ulElements = hb_parns( iDimension );

   /* create an array
    */
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
      BOOL  bError = FALSE;
      int   iParam;

      for( iParam = 1; iParam <= iPCount; iParam++ )
      {
         if( ! hb_param( iParam, HB_IT_NUMERIC ) )
         {
            /* if( ! ISNUM( iParam ) )
             */
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
         hb_arrayNewRagged( hb_stackReturnItem(), 1 );
   }
}

HB_FUNC( AADD )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

      if( pValue && hb_arrayAdd( pArray, pValue ) )
      {
         if( hb_stackItemFromBase( 2 )->type & HB_IT_BYREF )
            hb_itemCopy( hb_stackReturnItem(), pValue );
         else
            hb_itemForwardValue( hb_stackReturnItem(), pValue );
      }
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, "AADD", HB_MIN( hb_pcount(), 2 ), hb_paramError( 1 ), hb_paramError( 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "AADD", HB_MIN( hb_pcount(), 2 ), hb_paramError( 1 ), hb_paramError( 2 ) );
}

/* for debugging: returns the array's "address" so dual references to same
 * array can be seen
 */
HB_FUNC( HB_ARRAYID )
{
   hb_retptr( hb_arrayId( hb_param( 1, HB_IT_ARRAY ) ) );
}

/* Programmer responability that the ID is of a [still] VALID Array!!!
 */
HB_FUNC( HB_THISARRAY )
{
   PHB_ITEM pArrayID = hb_param( 1, HB_IT_POINTER | HB_IT_NUMINT );
   PHB_ITEM pArray   = hb_stackReturnItem();

   hb_itemClear( pArray );

   if( pArrayID )
   {
      pArray->type = HB_IT_ARRAY;

      if( HB_IS_POINTER( pArrayID ) )
         pArray->item.asArray.value = ( PHB_BASEARRAY ) hb_itemGetPtr( pArrayID );
      else
         pArray->item.asArray.value = ( PHB_BASEARRAY ) hb_itemGetNS( pArrayID );

#ifdef HB_ARRAY_USE_COUNTER
      HB_ATOMIC_INC( pArray->item.asArray.value->ulHolders );
#else
      hb_arrayRegisterHolder( pArray->item.asArray.value, ( void * ) pArray );
#endif
   }
}

/* NOTE: CA-Cl*pper 5.3 and older will return NIL on bad parameter, 5.3a,b
         will throw a runtime error. [vszakats] */

HB_FUNC( ASIZE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && ISNUM( 2 ) )
   {
      LONG lSize = hb_parnl( 2 );

      hb_arraySize( pArray, HB_MAX( lSize, 0 ) );

      /* ASize() returns the array itself
       */
      if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
         hb_itemCopy( hb_stackReturnItem(), pArray );
      else
         hb_itemForwardValue( hb_stackReturnItem(), pArray );
   }
#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ASIZE", HB_MIN( hb_pcount(), 2 ), hb_paramError( 1 ), hb_paramError( 2 ) );
#endif
}

HB_FUNC( ASIZEALLOC )
{
   PHB_ITEM pArray      = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pPreAlloc   = hb_param( 2, HB_IT_NUMERIC );

   if( pArray && pPreAlloc )
   {
      pArray->item.asArray.value->ulBlock = hb_itemGetNS( pPreAlloc );

      /* returns the array itself
       */
      if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
         hb_itemCopy( hb_stackReturnItem(), pArray );
      else
         hb_itemForwardValue( hb_stackReturnItem(), pArray );
   }
}

HB_FUNC( ALENALLOC )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_retnl( ( LONG ) pArray->item.asArray.value->ulBlock );
}

HB_FUNC( ATAIL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, hb_stackReturnItem() );
}

HB_FUNC( AINS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
#ifndef HB_C52_STRICT
      PHB_ITEM pExtend = hb_param( 4, HB_IT_LOGICAL );

      if( pExtend && pExtend->item.asLogical.value )
         hb_arraySize( pArray, pArray->item.asArray.value->ulLen + 1 );
#endif

      if( ISNUM( 2 ) )
      {
         HB_ISIZ lIndex = hb_parns( 2 );

#ifndef HB_C52_STRICT
         if( lIndex < 0 )
            lIndex += pArray->item.asArray.value->ulLen + 1;
#endif
         hb_arrayIns( pArray, lIndex );

#ifndef HB_C52_STRICT

         if( hb_pcount() >= 3 )
            hb_arraySet( pArray, hb_parnl( 2 ), hb_param( 3, HB_IT_ANY ) );
#endif
      }
      /* AIns() returns the array itself
       */
      if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
         hb_itemCopy( hb_stackReturnItem(), pArray );
      else
         hb_itemForwardValue( hb_stackReturnItem(), pArray );
   }
}

HB_FUNC( HB_AINS )
{
   HB_FUNC_EXEC( AINS )
}

HB_FUNC( ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      if( pArray->item.asArray.value->ulLen )
      {
         HB_ISIZ lIndex;

         if( ISNUM( 2 ) )
         {
            lIndex = hb_parns( 2 );

#ifndef HB_C52_STRICT
            if( lIndex < 0 )
               lIndex += pArray->item.asArray.value->ulLen + 1;
#endif
         }
         else
            lIndex = 1;

         if( hb_arrayDel( pArray, lIndex ) )
         {
#ifndef HB_C52_STRICT
            PHB_ITEM pShrink = hb_param( 3, HB_IT_LOGICAL );

            if( pShrink && pShrink->item.asLogical.value )
               hb_arraySize( pArray, pArray->item.asArray.value->ulLen - 1 );
#endif
         }
      }

      /* ADel() returns the array itself
       */
      if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
         hb_itemCopy( hb_stackReturnItem(), pArray );
      else
         hb_itemForwardValue( hb_stackReturnItem(), pArray );
   }
}

HB_FUNC( AFILL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue;

   if( pArray )
   {
      pValue = hb_param( 2, HB_IT_ANY );

      if( pValue )
      {
         HB_ISIZ  ulStart  = hb_parnl( 3 );
         HB_ISIZ  ulCount  = hb_parnl( 4 );

         /* Explicy ulCount of 0 - Nothing to do!
          */
         if( ISNUM( 4 ) && ulCount == 0 )
         {
            /* AFill() returns the array itself
             */
            if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
               hb_itemCopy( hb_stackReturnItem(), pArray );
            else
               hb_itemForwardValue( hb_stackReturnItem(), pArray );

            return;
         }

         if( ulStart == 0 )
            /* Clipper allows Start to be of wrong type, or 0, and corrects it to 1.
             */
            ulStart = 1;
         /* Clipper aborts if negative start.
          */
         else if( ulStart < 0 )
         {
            /* AFill() returns the array itself
             */
            if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
               hb_itemCopy( hb_stackReturnItem(), pArray );
            else
               hb_itemForwardValue( hb_stackReturnItem(), pArray );

            return;
         }

         if( ulCount == 0 )
         {
            /* Clipper allows the Count to be of wrong type, and corrects it to maximum elements.
             */
            ulCount = pArray->item.asArray.value->ulLen;
         }
         else if( ulCount < 0 )
         {
            if( ulStart == 1 )
               /* Clipper allows the Count to be negative, if start is 1, and corrects it to maximum elements.
                */
               ulCount = pArray->item.asArray.value->ulLen;

            /* Clipper aborts if negative count and start is not at 1.
             */
            else
            {
               /* AFill() returns the array itself
                */
               if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
                  hb_itemCopy( hb_stackReturnItem(), pArray );
               else
                  hb_itemForwardValue( hb_stackReturnItem(), pArray );

               return;
            }
         }

         hb_arrayFill( pArray, pValue, ulStart, ulCount );
      }

      /* AFill() returns the array itself
       */
      if( hb_stackItemFromBase( 1 )->type & HB_IT_BYREF )
         hb_itemCopy( hb_stackReturnItem(), pArray );
      else
         hb_itemForwardValue( hb_stackReturnItem(), pArray );
   }
   else
#ifdef HB_C52_STRICT
      /* NOTE: In CA-Cl*pper AFILL() is written in a manner that it will
       *       call AEVAL() to do the job, so the error (if any) will also be
       *       thrown by AEVAL().  [vszakats]
       */
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", HB_MIN( hb_pcount(), 4 ), hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
#else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "AFILL", HB_MIN( hb_pcount(), 4 ), hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
#endif
}

HB_FUNC( ASCAN )
{
   PHB_ITEM pArray   = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue   = hb_param( 2, HB_IT_ANY );

   if( pArray && pValue )
   {
      HB_SIZE  ulStart  = hb_parnl( 3 );
      HB_SIZE  ulCount  = hb_parnl( 4 );

      hb_retns( hb_arrayScan( pArray, pValue, ISNUM( 3 ) ? &ulStart : NULL, ISNUM( 4 ) ? &ulCount : NULL, hb_parl( 5 ), hb_parl( 6 ) ) );
   }
   else
      hb_retns( 0 );
}

/* TODO: In Xbase++ fifth parameter determines whether array elements
  *      are passed by reference to the code block. [vszakats]
 */

HB_FUNC( AEVAL )
{
   PHB_ITEM pArray   = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pBlock   = hb_param( 2, HB_IT_BLOCK );

   if( pArray && pBlock )
   {
      HB_SIZE  ulStart  = hb_parnl( 3 );
      HB_SIZE  ulCount  = hb_parnl( 4 );

      hb_arrayEval( pArray,
                    pBlock,
                    ISNUM( 3 ) ? &ulStart : NULL,
                    ISNUM( 4 ) ? &ulCount : NULL );

      hb_itemReturn( hb_stackItemFromBase( 1 ) ); /* AEval() returns the array itself */
   }
   else
   {
      HB_ITEM_NEW( Err1 );
      HB_ITEM_NEW( Err2 );

      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", 4, pArray ? hb_paramError( 1 ) : &Err1, pBlock ? hb_paramError( 2 ) : &Err2, hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

HB_FUNC( ACOPY )
{
   PHB_ITEM pSrcArray   = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pDstArray   = hb_param( 2, HB_IT_ARRAY );

   if( pSrcArray && pDstArray )
   {
      /* CA-Cl*pper works this way. */
      if( ! hb_arrayIsObject( pSrcArray ) && ! hb_arrayIsObject( pDstArray ) )
      {
         HB_SIZE  ulStart  = hb_parnl( 3 );
         HB_SIZE  ulCount  = hb_parnl( 4 );
         HB_SIZE  ulTarget = hb_parnl( 5 );

         hb_arrayCopy( pSrcArray,
                       pDstArray,
                       ISNUM( 3 ) ? &ulStart : NULL,
                       ISNUM( 4 ) ? &ulCount : NULL,
                       ISNUM( 5 ) ? &ulTarget : NULL );
      }

      /* ACopy() returns the target array
       */
      if( hb_stackItemFromBase( 2 )->type & HB_IT_BYREF )
         hb_itemCopy( hb_stackReturnItem(), pDstArray );
      else
         hb_itemForwardValue( hb_stackReturnItem(), pDstArray );
   }
}

/* NOTE: Clipper will return NIL if the parameter is not an array. [vszakats] */

HB_FUNC( ACLONE )
{
   PHB_ITEM pSrcArray = hb_param( 1, HB_IT_ARRAY );

   if( pSrcArray && ! hb_arrayIsObject( pSrcArray ) )
      hb_itemRelease( hb_itemReturnForward( hb_arrayClone( pSrcArray, NULL ) ) ); /* AClone() returns the new array */
}

HB_FUNC( HB_APARAMS )
{
   PHB_ITEM * pBase = hb_stackGetBase( hb_parnl( 1 ) + 1 );

   if( pBase )
      hb_itemRelease( hb_itemReturnForward( hb_arrayFromParams( pBase ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, "HB_APARAMS", 1, hb_paramError( 1 ) );
}

HB_FUNC( HB_AEXPRESSIONS )
{
   PHB_ITEM pLine    = hb_param( 1, HB_IT_STRING );
   size_t   i, iOffset = 0;
   int      iParans  = 0, iArrays = 0, iIndexs = 0;
   BOOL     bArray   = FALSE;

   if( pLine == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "HB_AEXPRESSIONS", 1, hb_paramError( 1 ) );
      return;
   }

   hb_arrayNew( hb_stackReturnItem(), 0 );

   for( i = 0; i < pLine->item.asString.length; i++ )
   {
      switch( pLine->item.asString.value[ i ] )
      {
         case '(':
            iParans++;
            bArray = FALSE;
            break;

         case ')':
            iParans--;
            bArray = TRUE;
            break;

         case '{':
            iArrays++;
            bArray = FALSE;
            break;

         case '}':
            iArrays--;
            bArray = TRUE;
            break;

         case '[':
            if( bArray || ( i && HB_ISALNUM( ( BYTE ) pLine->item.asString.value[ i - 1 ] ) ) )
               iIndexs++;
            else
            {
               while( ++i < pLine->item.asString.length && pLine->item.asString.value[ i ] != ']' )
               {
               }
            }
            bArray = FALSE;
            break;

         case ']':
            iIndexs--;
            bArray = TRUE;
            break;

         case '"':
            while( ++i < pLine->item.asString.length && pLine->item.asString.value[ i ] != '"' )
            {
            }
            bArray = FALSE;
            break;

         case '\'':
            while( ++i < pLine->item.asString.length && pLine->item.asString.value[ i ] != '\'' )
            {
            }
            bArray = FALSE;
            break;

         case ',':
            if( iParans == 0 && iArrays == 0 && iIndexs == 0 )
            {
               HB_ITEM_NEW( Exp );

               hb_arrayAddForward( &( HB_VM_STACK ).Return, hb_itemPutCL( &Exp, pLine->item.asString.value + iOffset, i - iOffset ) );
               iOffset = i + 1;

            }
            bArray = FALSE;
            break;

         default:
            bArray = FALSE;
            break;
      }
   }

   if( iOffset < pLine->item.asString.length - 1 )
   {
      HB_ITEM_NEW( Exp );

      hb_arrayAddForward( &( HB_VM_STACK ).Return, hb_itemPutCL( &Exp, pLine->item.asString.value + iOffset, pLine->item.asString.length - iOffset ) );

   }
}

unsigned int SizeOfCStructure( PHB_ITEM aDef, unsigned int uiAlign )
{
   PHB_BASEARRAY  pBaseDef = aDef->item.asArray.value;
   HB_SIZE        ulLen    = pBaseDef->ulLen;
   HB_SIZE        ulIndex;
   unsigned int   uiSize   = 0, uiMemberSize;
   BYTE           cShift;
   unsigned int   uiPad;

   for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
   {
      if( ( pBaseDef->pItems + ulIndex )->type != HB_IT_INTEGER )
      {
         hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
         return 0;
      }

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:           /* char */
         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR:            /* short */
         case CTYPE_UNSIGNED_SHORT_PTR:   /* unsigned short */
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT:            /* int */
         case CTYPE_UNSIGNED_INT:   /* unsigned int */
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR:           /* int * */
         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG:           /* long */
         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT:  /* float */
            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE:  /* double */
            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            uiMemberSize = sizeof( void * );
            break;

         default:
         {
            HB_ITEM_NEW( ID );

            hb_itemPutNI( &ID, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
               uiMemberSize = sizeof( void * );
            else if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, &ID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "SizeOf", 0 );
                  uiMemberSize = ( unsigned int ) hb_itemGetNL( &HB_VM_STACK.Return );
                  hb_itemRelease( pStructure );
               }
               else
               {
                  hb_itemRelease( pStructure );
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 1, hb_paramError( 1 ) );
                  return 0;
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
         uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( BYTE ) ( uiSize % uiPad ) ) > 0 )
            uiSize += ( uiPad - cShift );
      }

      uiSize += uiMemberSize;

      /* printf( "#%lu Size: %u Align: %u Pad: %u Shift %i Size: %u\n", ulIndex, uiMemberSize, uiAlign, uiPad, cShift, uiSize );
       */
   }

   if( ( cShift = ( BYTE ) ( uiSize % uiAlign ) ) > 0 )
      uiSize += ( uiAlign - cShift );

   /* printf( "#%lu Size: %u Align: %u Pad: %u Shift %i Size: %u\n", ulIndex, uiMemberSize, uiAlign, uiPad, cShift, uiSize );
    */

   return uiSize;
}

HB_FUNC( HB_SIZEOFCSTRUCTURE )
{
   PHB_ITEM  aDef     = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM  pAlign   = hb_param( 2, HB_IT_INTEGER );

   if( aDef )
      hb_retni( SizeOfCStructure( aDef, ( unsigned int ) ( pAlign ? pAlign->item.asInteger.value : 8 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "SizeOfCStructure", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

BYTE * ArrayToStructure( PHB_ITEM aVar, PHB_ITEM aDef, unsigned int uiAlign, unsigned int * puiSize )
{
   PHB_BASEARRAY  pBaseVar = aVar->item.asArray.value;
   PHB_BASEARRAY  pBaseDef = aDef->item.asArray.value;
   HB_SIZE        ulLen    = pBaseDef->ulLen;
   HB_SIZE        ulIndex;
   BYTE *         Buffer;
   unsigned int   uiOffset = 0, uiMemberSize;
   BYTE           cShift;

   *puiSize = SizeOfCStructure( aDef, uiAlign );

   /* printf( "Size: %i\n", *puiSize );
    */

   Buffer = ( BYTE * ) hb_xgrab( *puiSize );

   for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
   {
      /* printf( "#: %i\n", ulIndex );
       */

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:           /* char */
         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            if( ( pBaseVar->pItems + ulIndex )->type && ! HB_IS_NUMERIC( pBaseVar->pItems + ulIndex  ) )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_STRING
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;;
            }

            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR:            /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR:   /* unsigned short * */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;;
            }

            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT:            /* int */
         case CTYPE_UNSIGNED_INT:   /* unsigned int */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR:           /* int * */
         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;;
            }

            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG:           /* long */
         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;;
            }

            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT:  /* float */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_DOUBLE
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE:  /* double */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_DOUBLE
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            if( ( pBaseVar->pItems + ulIndex )->type && ( pBaseVar->pItems + ulIndex )->type != HB_IT_POINTER
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_LONG
                && ( pBaseVar->pItems + ulIndex )->type != HB_IT_STRING )
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            uiMemberSize = sizeof( void * );
            break;

         default:
         {
            HB_ITEM_NEW( ID );

            hb_itemPutNI( &ID, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
               uiMemberSize = sizeof( void * );
            else if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, &ID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "SizeOf", 0 );
                  uiMemberSize = ( unsigned int ) hb_itemGetNL( &HB_VM_STACK.Return );
                  hb_itemRelease( pStructure );
               }
               else
               {
                  hb_itemRelease( pStructure );
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
                  return NULL;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
         }
      }

      if( uiOffset )
      {
         unsigned int uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( BYTE ) ( uiOffset % uiPad ) ) > 0 )
            uiOffset += ( uiPad - cShift );
      }

      /* printf( "* Size: %i Offset: %i\n", uiMemberSize, uiOffset );
       */

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:  /* char */
            if( ( pBaseVar->pItems + ulIndex )->type )
               *( ( char * ) ( Buffer + uiOffset ) ) = ( char ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else
               *( ( char * ) ( Buffer + uiOffset ) ) = 0;

            break;

         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            if( ( pBaseVar->pItems + ulIndex )->type )
               *( ( BYTE * ) ( Buffer + uiOffset ) ) = ( BYTE ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else
               *( ( BYTE * ) ( Buffer + uiOffset ) ) = 0;

            break;

         case CTYPE_CHAR_PTR:  /* char * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_STRING:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex )->item.asString.value;
                  break;

               case HB_IT_POINTER:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( char * ) ( pBaseVar->pItems + ulIndex )->item.asPointer.value;
                  break;
#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( char * ) ( HB_PTRUINT ) ( pBaseVar->pItems + ulIndex )->item.asInteger.value;
                  break;
#endif
               case HB_IT_LONG:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( char * ) ( HB_PTRUINT ) ( pBaseVar->pItems + ulIndex )->item.asLong.value;
                  break;

               default:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_CHAR_PTR:  /* unsigned char * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_STRING:
                  *( ( BYTE ** ) ( Buffer + uiOffset ) ) = ( BYTE * ) ( ( pBaseVar->pItems + ulIndex )->item.asString.value );
                  break;

               case HB_IT_POINTER:
                  *( ( BYTE ** ) ( Buffer + uiOffset ) ) = ( BYTE * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( BYTE ** ) ( Buffer + uiOffset ) ) = ( BYTE * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( BYTE ** ) ( Buffer + uiOffset ) ) = ( BYTE * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( BYTE ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_SHORT:  /* short */
            if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_INTEGER )
               *( ( short * ) ( Buffer + uiOffset ) ) = ( short ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_LONG )
               *( ( short * ) ( Buffer + uiOffset ) ) = ( short ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_DOUBLE )
               *( ( short * ) ( Buffer + uiOffset ) ) = ( short ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_NIL )
               *( ( short * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_SHORT:  /* unsigned short */
            if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_INTEGER )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = ( unsigned short ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_LONG )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = ( unsigned short ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_DOUBLE )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = ( unsigned short ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_NIL )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_SHORT_PTR:  /* short * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_SHORT_PTR:  /* unsigned short * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_INT:  /* int */
            if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_INTEGER )
               *( ( int * ) ( Buffer + uiOffset ) ) = ( int ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_LONG )
               *( ( int * ) ( Buffer + uiOffset ) ) = ( int ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_DOUBLE )
               *( ( int * ) ( Buffer + uiOffset ) ) = ( int ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_NIL )
               *( ( int * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_INT:  /* unsigned int */
            if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_INTEGER )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = ( unsigned int ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_LONG )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = ( unsigned int ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_DOUBLE )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = ( unsigned int ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_NIL )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }

            break;

         case CTYPE_INT_PTR:  /* int * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_LONG:  /* long */
            if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_INTEGER )
               *( ( long * ) ( Buffer + uiOffset ) ) = ( long ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_LONG )
               *( ( long * ) ( Buffer + uiOffset ) ) = ( long ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_DOUBLE )
               *( ( long * ) ( Buffer + uiOffset ) ) = ( long ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_NIL )
               *( ( long * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_INTEGER )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = ( unsigned long ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_LONG )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = ( unsigned long ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_DOUBLE )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = ( unsigned long ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + ulIndex )->type == HB_IT_NIL )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return NULL;
            }
            break;

         case CTYPE_LONG_PTR:  /* long * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_LONG_PTR:  /* unsigned long * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_FLOAT:  /* float */
            if( ( pBaseVar->pItems + ulIndex )->type )
               *( ( float * ) ( Buffer + uiOffset ) ) = ( float ) ( pBaseVar->pItems + ulIndex )->item.asDouble.value;
            else
               *( ( float * ) ( Buffer + uiOffset ) ) = 0;
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               /* Is this correct??? IMHO It's a bug */
               case HB_IT_DOUBLE:
                  **( ( float ** ) ( Buffer + uiOffset ) ) = ( float ) ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
                  break;

               default:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_DOUBLE:  /* double */
            if( ( pBaseVar->pItems + ulIndex )->type )
               *( ( double * ) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + ulIndex )->item.asDouble.value;
            else
               *( ( double * ) ( Buffer + uiOffset ) ) = 0;
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               /* Is this correct??? IMHO It's a bug */
               case HB_IT_DOUBLE:
                  **( ( double ** ) ( Buffer + uiOffset ) ) = ( ( pBaseVar->pItems + ulIndex )->item.asDouble.value );
                  break;

               default:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         case CTYPE_VOID_PTR:  /* void * */
            switch( ( pBaseVar->pItems + ulIndex )->type )
            {
               case HB_IT_POINTER:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( ( pBaseVar->pItems + ulIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case HB_IT_INTEGER:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asInteger.value );
                  break;
#endif
               case HB_IT_LONG:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRUINT ) ( ( pBaseVar->pItems + ulIndex )->item.asLong.value );
                  break;

               default:
                  *( ( void ** ) ( Buffer + uiOffset ) ) = NULL;
                  break;
            }
            break;

         default:
            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = pBaseVar->pItems + ulIndex;

               if( HB_IS_LONG( pStructure ) )
               {
                  if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRUINT ) pStructure->item.asLong.value;
                  else
                     HB_MEMCPY( ( void * ) ( Buffer + uiOffset ), ( void * ) ( HB_PTRUINT ) pStructure->item.asLong.value, uiMemberSize );
               }
#if UINT_MAX == ULONG_MAX
               else if( HB_IS_INTEGER( pStructure ) )
               {
                  if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) ( HB_PTRUINT ) pStructure->item.asInteger.value;
                  else
                     HB_MEMCPY( ( void * ) ( Buffer + uiOffset ), ( void * ) ( HB_PTRUINT ) pStructure->item.asInteger.value, uiMemberSize );
               }
#endif
               else if( HB_IS_NIL( pStructure ) )
               {
                  if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( ( void ** ) ( Buffer + uiOffset ) ) = NULL;
                  else
                  {
                     /* TraceLog( NULL, "ArrayToStructure() - Empty Inplace\n" );
                      */
                     memset( ( void * ) ( Buffer + uiOffset ), 0, uiMemberSize );
                  }
               }
               else if( strncmp( hb_objGetClsName( pStructure ), "C Structure", 11 ) == 0 )
               {
                  PHB_BASEARRAY  pBaseStructure    = pStructure->item.asArray.value;
                  PHB_ITEM       pInternalBuffer   = pBaseStructure->pItems + pBaseStructure->ulLen - 1;

                  hb_objSendMsg( pStructure, "VALUE", 0 );

                  if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( ( void ** ) ( Buffer + uiOffset ) ) = ( void * ) pInternalBuffer->item.asString.value;
                  else
                     HB_MEMCPY( ( void * ) ( Buffer + uiOffset ), ( void * ) pInternalBuffer->item.asString.value, uiMemberSize );
               }
               else
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
            }
            else
               hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      }

      /* printf( "Wrote %i bytes at Offset %i\n", uiMemberSize, uiOffset );
       */

      uiOffset += uiMemberSize;
   }

   return Buffer;
}

HB_FUNC( HB_ARRAYTOSTRUCTURE )
{
   PHB_ITEM aVar     = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM aDef     = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pAlign   = hb_param( 3, HB_IT_INTEGER );

   if( aVar && aDef )
   {
      unsigned int   uiSize;
      unsigned int   uiAlign = pAlign ? ( BYTE ) pAlign->item.asInteger.value : 8;
      BYTE *         Buffer  = ArrayToStructure( aVar, aDef, uiAlign, &uiSize );

      hb_itemPutCRaw( hb_stackReturnItem(), ( char * ) Buffer, uiSize );
   }
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ArrayToStructure", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
}

PHB_ITEM StructureToArray( BYTE * Buffer, HB_SIZE ulBufferLen, PHB_ITEM aDef, unsigned int uiAlign, BOOL bAdoptNested, PHB_ITEM pRet )
{
   PHB_BASEARRAY  pBaseDef = aDef->item.asArray.value;
   HB_SIZE        ulLen    = pBaseDef->ulLen;
   HB_SIZE        ulIndex;
   unsigned int   uiOffset, uiMemberSize;
   BYTE           cShift;
   /* PHB_ITEM pRet = hb_itemNew( NULL );
    */
   PHB_BASEARRAY  pBaseVar;

   /* TraceLog( NULL, "StructureToArray(%p, %p, %u, %i) ->%u\n", Buffer, aDef, uiAlign, bAdoptNested, ulLen );
    */

   /* hb_arrayNew( pRet, ulLen );
    */
   pBaseVar = pRet->item.asArray.value;

   uiOffset = 0;
   for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
   {
      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:           /* char */
         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            uiMemberSize = sizeof( char );
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            uiMemberSize = sizeof( char * );
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            uiMemberSize = sizeof( short );
            break;

         case CTYPE_SHORT_PTR:            /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR:   /* unsigned short * */
            uiMemberSize = sizeof( short * );
            break;

         case CTYPE_INT:            /* int */
         case CTYPE_UNSIGNED_INT:   /* unsigned int */
            uiMemberSize = sizeof( int );
            break;

         case CTYPE_INT_PTR:           /* int * */
         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            uiMemberSize = sizeof( int * );
            break;

         case CTYPE_LONG:           /* long */
         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            uiMemberSize = sizeof( long );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            uiMemberSize = sizeof( long * );
            break;

         case CTYPE_FLOAT:  /* float */
            uiMemberSize = sizeof( float );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            uiMemberSize = sizeof( float * );
            break;

         case CTYPE_DOUBLE:  /* double */
            uiMemberSize = sizeof( double );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            uiMemberSize = sizeof( double * );
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            uiMemberSize = sizeof( void * );
            break;

         default:
         {
            HB_ITEM_NEW( ID );

            hb_itemPutNI( &ID, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
               uiMemberSize = sizeof( void * );
            else if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, &ID );

               if( HB_IS_OBJECT( pStructure ) )
               {
                  hb_objSendMsg( pStructure, "SizeOf", 0 );
                  uiMemberSize = ( unsigned int ) hb_itemGetNL( &HB_VM_STACK.Return );
                  hb_itemRelease( pStructure );
               }
               else
               {
                  hb_itemRelease( pStructure );
                  hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 1, hb_paramError( 1 ) );
                  return pRet;;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               return pRet;;
            }
         }
      }

      if( uiOffset )
      {
         unsigned int uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = ( BYTE ) ( uiOffset % uiPad ) ) > 0 )
            uiOffset += ( uiPad - cShift );

         /* TraceLog( NULL, "* Size: %i Offset: %i Pad: %i\n", uiMemberSize, uiOffset, uiPad );
          */
      }
      else
      {
         /* TraceLog( NULL, "* Size: %i Offset: %i\n", uiMemberSize, uiOffset );
          */
      }

      if( ( uiOffset + uiMemberSize ) > ulBufferLen )
         break;

      switch( ( pBaseDef->pItems + ulIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:  /* char */
            hb_itemPutNI( pBaseVar->pItems + ulIndex, ( int ) *( ( char * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            hb_itemPutNI( pBaseVar->pItems + ulIndex, ( int ) *( ( BYTE * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_CHAR_PTR:  /* char * */
            if( HB_IS_STRING( pBaseVar->pItems + ulIndex ) && ( pBaseVar->pItems + ulIndex )->item.asString.value == *( ( char ** ) ( Buffer + uiOffset ) ) )
            {
               /* TraceLog( NULL, "IDENTICAL: %s\n", *( (char **) ( Buffer + uiOffset ) ) );
                */
            }
            else if( bAdoptNested )
               /* TraceLog( NULL, "Adopt: %s\n", *( (char **) ( Buffer + uiOffset ) ) );
                */
               hb_itemPutC( pBaseVar->pItems + ulIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            else
               /* TraceLog( NULL, "Static: %s\n", *( (char **) ( Buffer + uiOffset ) ) );
                */
               hb_itemPutCStatic( pBaseVar->pItems + ulIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );

            break;

         case CTYPE_UNSIGNED_CHAR_PTR:  /* unsigned char * */
            if( HB_IS_STRING( pBaseVar->pItems + ulIndex ) && ( pBaseVar->pItems + ulIndex )->item.asString.value == *( ( char ** ) ( Buffer + uiOffset ) ) )
            {
               /* TraceLog( NULL, "IDENTICAL: %s\n", *( (char **) ( Buffer + uiOffset ) ) );
                */
            }
            else if( bAdoptNested )
               /* TraceLog( NULL, "Adopt: %s\n", *( (char **) ( Buffer + uiOffset ) ) );
                */
               hb_itemPutC( pBaseVar->pItems + ulIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            else
               /* TraceLog( NULL, "Static: %s\n", *( (char **) ( Buffer + uiOffset ) ) );
                */
               hb_itemPutCStatic( pBaseVar->pItems + ulIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );

            break;

         case CTYPE_SHORT:  /* short */
            hb_itemPutNI( pBaseVar->pItems + ulIndex, *( ( short * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_SHORT:  /* unsigned short */
            hb_itemPutNI( pBaseVar->pItems + ulIndex, ( short ) *( ( unsigned short * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_SHORT_PTR:            /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR:   /* unsigned short * */
            hb_itemPutPtr( pBaseVar->pItems + ulIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_INT:  /* int */
            hb_itemPutNI( pBaseVar->pItems + ulIndex, *( ( int * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_INT:  /* unsigned int */
            hb_itemPutNI( pBaseVar->pItems + ulIndex, ( int ) *( ( unsigned int * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_INT_PTR:           /* int * */
         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            hb_itemPutPtr( pBaseVar->pItems + ulIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_LONG:  /* long */
            hb_itemPutNL( pBaseVar->pItems + ulIndex, *( ( long * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            hb_itemPutNL( pBaseVar->pItems + ulIndex, ( long ) *( ( unsigned long  * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            hb_itemPutPtr( pBaseVar->pItems + ulIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_FLOAT:  /* float */
            hb_itemPutND( pBaseVar->pItems + ulIndex, ( double ) *( ( float * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            hb_itemPutPtr( pBaseVar->pItems + ulIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         case CTYPE_DOUBLE:  /* double */
            hb_itemPutND( pBaseVar->pItems + ulIndex, *( ( double * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
         case CTYPE_VOID_PTR:    /* void * */
            hb_itemPutPtr( pBaseVar->pItems + ulIndex, ( void * ) ( Buffer + uiOffset ) );
            break;

         default:
         {
            HB_ITEM_NEW( ID );
            PHB_ITEM       pStructure;
            unsigned int   uiNestedSize /*, uiNestedAlign */;

            hb_itemPutNI( &ID, ( pBaseDef->pItems + ulIndex )->item.asInteger.value );

            pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, &ID );

            if( ! HB_IS_OBJECT( pStructure ) )
            {
               hb_itemRelease( pStructure );
               hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               return pRet;
            }

            hb_objSendMsg( pStructure, "NALIGN", 0 );
            /* uiNestedAlign = ( hb_stackReturnItem() )->item.asInteger.value;
             */

            hb_objSendMsg( pStructure, "SizeOf", 0 );
            uiNestedSize = ( unsigned int ) hb_itemGetNL( &HB_VM_STACK.Return );

            /* TraceLog( NULL, "* NestedSize: %i Offset: %i\n", uiNestedSize, uiOffset );
             */

            if( ( pBaseDef->pItems + ulIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
            {
               /* printf( "Offset %i Pointer: %p\n", uiOffset, *(char **) ( (long ** )( Buffer + uiOffset ) ) );
                */

               if( *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ) )
               {
                  PHB_BASEARRAY  pBaseStructure    = pStructure->item.asArray.value;
                  PHB_ITEM       pInternalBuffer   = pBaseStructure->pItems + pBaseStructure->ulLen - 1;

                  if( bAdoptNested )
                     hb_itemPutCRaw( pInternalBuffer, *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ), uiNestedSize );
                  else
                     hb_itemPutCRawStatic( pInternalBuffer, *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ), uiNestedSize );

                  hb_objSendMsg( pStructure, "DEVALUE", 0 );
               }
               else
                  /* hb_objSendMsg( pStructure, "RESET", 0 );
                   */
                  hb_itemClear( pStructure );
            }
            else
            {
               PHB_BASEARRAY  pBaseStructure    = pStructure->item.asArray.value;
               PHB_ITEM       pInternalBuffer   = pBaseStructure->pItems + pBaseStructure->ulLen - 1;
               HB_ITEM        Adopt;

               Adopt.type                 = HB_IT_LOGICAL;
               Adopt.item.asLogical.value = bAdoptNested;

               /* TraceLog( NULL, "Before Devalue\n" );
                */

               hb_itemPutCRawStatic( pInternalBuffer, ( char * ) ( BYTE * ) ( Buffer + uiOffset ), uiNestedSize );

               hb_objSendMsg( pStructure, "DEVALUE", 1, &Adopt );

               /* TraceLog( NULL, "After Devalue\n" );
                */
            }

            hb_itemForwardValue( pBaseVar->pItems + ulIndex, pStructure );

            hb_itemRelease( pStructure );
         }
      }

      uiOffset += uiMemberSize;

      /* TraceLog( NULL, "AFTER Size: %i Offset: %i\n", uiMemberSize, uiOffset );
       */
   }

   return pRet;
}

HB_FUNC( HB_STRUCTURETOARRAY )
{
   PHB_ITEM Structure   = hb_param( 1, HB_IT_STRING );
   PHB_ITEM aDef        = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pAlign      = hb_param( 3, HB_IT_INTEGER );
   PHB_ITEM pAdopt      = hb_param( 4, HB_IT_LOGICAL );
   PHB_ITEM pRet        = hb_param( 5, HB_IT_ARRAY );
   BOOL     bAdopt;

   if( Structure && aDef )
   {
      BYTE *         Buffer = ( BYTE * ) Structure->item.asString.value;
      unsigned int   uiAlign = pAlign ? ( BYTE ) pAlign->item.asInteger.value : 8;

      bAdopt = pAdopt ? pAdopt->item.asLogical.value : FALSE;

      hb_itemForwardValue( hb_stackReturnItem(), StructureToArray( Buffer, Structure->item.asString.length, aDef, uiAlign, bAdopt, pRet ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "StructureToArray", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

HB_FUNC( RASCAN )  /* Reverse AScan... no hashes supported :( */
{
   PHB_ITEM pArray   = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue   = hb_param( 2, HB_IT_ANY );

   if( pArray && pValue )
   {
      HB_ISIZ  ulCount;
      BOOL     bExact      = hb_parl( 5 );
      BOOL     bAllowChar  = hb_parl( 6 );
      PHB_ITEM pItems      = pArray->item.asArray.value->pItems;
      HB_ISIZ  ulLen       = pArray->item.asArray.value->ulLen;
      /* sanitize scan range */
      HB_ISIZ  ulStart     = ( ISNUM( 3 ) && hb_parns( 3 ) >= 1 ) ? hb_parns( 3 ) : ulLen;

      if( ulStart > ulLen )
      {
         hb_retnl( 0 );
         return;
      }

      ulCount = ( ISNUM( 4 ) && ( hb_parns( 4 ) <= ulStart ) ) ? hb_parns( 4 ) : ulStart;

      if( ulCount > ulStart )
         ulCount = ulStart;

      /* Make separate search loops for different types to find, so that
         the loop can be faster. */

      if( HB_IS_BLOCK( pValue ) )
      {
         for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pValue );
            hb_vmPush( pItems + ulStart );
            hb_vmPushSize( ulStart + 1 );
            hb_vmSend( 2 );

            if( HB_IS_LOGICAL( hb_stackReturnItem() ) && HB_VM_STACK.Return.item.asLogical.value )
            {
               hb_retns( ulStart + 1 );             /* arrays start from 1 */
               return;
            }
         }
      }
      else if( HB_IS_STRING( pValue ) ) /* Must precede HB_IS_NUMERIC() */
      {
         if( ! bAllowChar || ! HB_IS_NUMERIC( pValue ) )
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
            {
               PHB_ITEM pItem = pItems + ulStart;

               /* NOTE: The order of the pItem and pValue parameters passed to
                        hb_itemStrCmp() is significant, please don't change it. [vszakats] */
               if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, bExact ) == 0 )
               {
                  hb_retns( ulStart + 1 );             /* arrays start from 1 */
                  return;
               }
            }
         }
         else
         {
            double dValue = hb_itemGetND( pValue );

            for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
            {
               PHB_ITEM pItem = pItems + ulStart;

               /* NOTE: The order of the pItem and pValue parameters passed to
                        hb_itemStrCmp() is significant, please don't change it. [vszakats] */
               if( HB_IS_STRING( pItem ) )
               {
                  if( hb_itemStrCmp( pItem, pValue, bExact ) == 0 )
                  {
                     hb_retns( ulStart + 1 );          /* arrays start from 1 */
                     return;
                  }
               }
               else if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
               {
                  hb_retns( ulStart + 1 );             /* arrays start from 1 */
                  return;
               }
            }
         }
      }
      else if( pValue->type == HB_IT_DATE ) /* Must precede HB_IS_NUMERIC() */
      {
         HB_ISIZ lValue = pValue->item.asDate.value;

         for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
         {
            PHB_ITEM pItem = pItems + ulStart;

            if( pItem->type == HB_IT_DATE && pItem->item.asDate.value == lValue &&
                pValue->item.asDate.time == pItem->item.asDate.time )
            {
               hb_retns( ulStart + 1 );             /* arrays start from 1 */
               return;
            }
         }
      }
      else if( HB_IS_NUMERIC( pValue ) )
      {
         double dValue = hb_itemGetND( pValue );

         for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
         {
            PHB_ITEM pItem = pItems + ulStart;

            if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue && ( bAllowChar || ! HB_IS_STRING( pItem ) ) )
            {
               hb_retns( ulStart + 1 );             /* arrays start from 1 */
               return;
            }
         }
      }
      else if( HB_IS_LOGICAL( pValue ) )
      {
         BOOL bValue = hb_itemGetL( pValue );

         for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
         {
            PHB_ITEM pItem = pItems + ulStart;

            if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
            {
               hb_retns( ulStart + 1 );             /* arrays start from 1 */
               return;
            }
         }
      }
      else if( HB_IS_NIL( pValue ) )
      {
         for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
         {
            if( HB_IS_NIL( pItems + ulStart ) )
            {
               hb_retns( ulStart + 1 );             /* arrays start from 1 */
               return;
            }
         }
      }
      else if( bExact && pValue->type == HB_IT_ARRAY )
      {
         for( ulStart--; ulCount > 0; ulCount--, ulStart-- )
         {
            PHB_ITEM pItem = pItems + ulStart;

            if( pItem->type == HB_IT_ARRAY && pItem->item.asArray.value == pValue->item.asArray.value )
            {
               hb_retns( ulStart + 1 );             /* arrays start from 1 */
               return;
            }
         }
      }
   }

   hb_retnl( 0 );
}

/* aSplice( <aArray> [, <nPos>] [, <nCount>] [,<xVal1>] [, ...] [, <xValN>]  ) => <aDeleted>
 * Removes elements and return them as array, optionally add items
 */
HB_FUNC( ASPLICE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_SIZE  ulStart, ulRemove, ulIndex, ulAdd;
      HB_SIZE  ulLen    = hb_arrayLen( pArray );
      PHB_ITEM pReturn  = hb_stackReturnItem();

      if( ulLen == 0 )
      {
         hb_arrayNew( pReturn, 0 );
         return;
      }

      if( ISNUM( 2 ) )
         ulStart = hb_parns( 2 );
      else
         ulStart = ulLen + ( hb_pcount() > 3 && ! ISNUM( 3 ) ? 1 : 0 );

      if( ISNUM( 3 ) )
         ulRemove = hb_parns( 3 );
      else
         ulRemove = ( hb_pcount() > 3 && ulStart == ulLen + 1 ) ? 0 : 1;

      if( ulStart == 0 || ulStart > ulLen )
      {
         if( ! ( ulStart == ulLen + 1 && hb_pcount() > 3 && ulRemove == 0 ) )
         {
            hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }
      }

      if( ulStart + ulRemove - 1 > ulLen )
         ulRemove = ulLen - ulStart + 1;

      hb_arrayNew( pReturn, ulRemove );

      /* 0 Based */
      ulStart--;

      for( ulIndex = ulStart + 1; ( ulIndex - ulStart ) <= ulRemove; ulIndex++ )
         hb_itemForwardValue( hb_arrayGetItemPtr( pReturn, ulIndex - ulStart ),
                              hb_arrayGetItemPtr( pArray, ulIndex ) );

      if( hb_pcount() > 3 )
      {
         HB_SIZE ulNew = 0;
         ulAdd = hb_pcount() - 3;

         if( ulAdd > ulRemove )
         {
            HB_SIZE  ulMore   = ulAdd - ulRemove;
            HB_SIZE  ulShift  = ulLen - ( ulStart + ulRemove );

            hb_arraySize( pArray, ulLen + ulMore );

            /* Shift right BEFORE adding, so that new items will not override existing values. */
            for( ulIndex = ulLen; ulIndex && ulShift--; --ulIndex )
               hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex + ulMore ),
                                    hb_arrayGetItemPtr( pArray, ulIndex ) );

            /* Now insert new values into emptied space. */
            for( ulIndex = ulStart; ++ulNew <= ulAdd; ulIndex++ )
               hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex + 1 ),
                                    hb_param( 3 + ( int ) ulNew, HB_IT_ANY ) );
         }
         else
         {
            /* Insert over the space emptied by removed items */
            for( ulIndex = ulStart; ++ulNew <= ulAdd; ulIndex++ )
               hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex + 1 ), hb_param( 3 + ( int ) ulNew, HB_IT_ANY ) );

            if( ulRemove > ulAdd )
            {
               ulRemove -= ulAdd;

               /* Shift left to compact the emptied hole. */
               for( ulIndex = ulStart + ulAdd + 1; ulIndex + ulRemove <= ulLen; ulIndex++ )
                  hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex ),
                                       hb_arrayGetItemPtr( pArray, ulIndex + ulRemove ) );
            }
         }
      }
      else
      {
         for( ulIndex = ulStart + 1; ulIndex + ulRemove <= ulLen; ulIndex++ )
            hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex ),
                                 hb_arrayGetItemPtr( pArray, ulIndex + ulRemove ) );

         hb_arraySize( pArray, ulLen - ulRemove );
      }
   }
}

/* Synonym of aSplice() Xbase++ compatability (extended with optional
 * replacemenet values)
 */
HB_FUNC( AREMOVE )
{
   HB_FUNC_EXEC( ASPLICE )
}

/* aMerge( <aTarget>, <aSource> [, <nPos>] ) => aTarget */
HB_FUNC( AMERGE )
{
   PHB_ITEM pArray1  = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pArray2  = hb_param( 2, HB_IT_ARRAY );

   if( pArray1 && pArray2 )
   {
      HB_SIZE  ulLen = hb_arrayLen( pArray1 );
      HB_SIZE  ulAdd = hb_arrayLen( pArray2 );
      HB_SIZE  ulIndex, ulStart;

      hb_arraySize( pArray1, ulLen + ulAdd );

      if( ISNUM( 3 ) )
      {
         ulStart = hb_parnl( 3 ) - 1;

         if( ulStart > ulLen )
         {
            hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }

         /* Shift right BEFORE merging, so that merged items will not override
          * existing values.
          */
         for( ulIndex = ulLen; ulIndex > ulStart; --ulIndex )
            hb_itemForwardValue( hb_arrayGetItemPtr( pArray1, ulIndex + ulAdd ),
                                 hb_arrayGetItemPtr( pArray1, ulIndex ) );
      }
      else
         ulStart = ulLen;

      for( ulIndex = 1; ulIndex <= ulAdd; ulIndex++ )
         hb_itemCopy( hb_arrayGetItemPtr( pArray1, ulStart + ulIndex ),
                      hb_arrayGetItemPtr( pArray2, ulIndex ) );

      hb_itemCopy( hb_stackReturnItem(), pArray1 );
   }
   else
      hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
