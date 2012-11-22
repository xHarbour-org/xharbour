/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Eval API
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
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_itemDo() ( based on HB_DO() by Ryszard Glab )
 *    hb_itemDoC() ( based on HB_DO() by Ryszard Glab )
 *
 * Copyright 2003-.. Giancarlo Niccolai <viktor.szakats@syenar.hu>
 *    HB_FUNC( HB_EXECFROMARRAY )
 *    hb_execFromArray( PHB_ITEM pExecArray )
 *    HB_FUNC( EVAL ) - Movde here from hvm.c - copyright as hvm.c
 *
 * See doc/license.txt for licensing terms.
 *
 */
#if defined( _MSC_VER ) && defined( HB_OS_WIN_64 )
   #pragma warning ( disable:4334 )
   /* '<<' : result of 32-bit shift implicitly converted to 64 bits (was 64-bit shift intended?)
    * yes, intended.
   */
#endif

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbfast.h"
#include "hbvm.h"
#include "classes.h"

BOOL hb_evalNew( PHB_EVALINFO pEvalInfo, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_evalNew(%p, %p)", pEvalInfo, pItem ) );

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( EVALINFO ) );
      pEvalInfo->pItems[ 0 ]  = pItem;
      pEvalInfo->paramCount   = 0;

      return TRUE;
   }

   return FALSE;
}

/* NOTE: CA-Cl*pper is buggy and will not check if more parameters are
         added than the maximum (9). [vszakats] */

/* NOTE: CA-Cl*pper NG suggest that the Items passed as parameters should/may
         be released by the programmer explicitly. But in fact hb_evalRelease()
         will automatically release all of them. The sample programs in the
         NG are doing it that way. Releasing the parameters explicitly in
         Harbour will cause an internal error, while it will be silently
         ignored (?) in CA-Cl*pper. This is due to the different internal
         handling of the Items, but IIRC it causes leak in CA-Clipper. All in
         all, don't release the eval parameter Items explicitly to make both
         Harbour and CA-Clipper happy. [vszakats] */

BOOL hb_evalPutParam( PHB_EVALINFO pEvalInfo, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_evalPutParam(%p, %p)", pEvalInfo, pItem ) );

   if( pEvalInfo && pItem && pEvalInfo->paramCount < HB_EVAL_PARAM_MAX_ )
   {
      pEvalInfo->pItems[ ++pEvalInfo->paramCount ] = pItem;

      return TRUE;
   }

   return FALSE;
}

PHB_ITEM hb_evalLaunch( PHB_EVALINFO pEvalInfo )
{
   HB_THREAD_STUB

   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_evalLaunch(%p)", pEvalInfo ) );

   if( pEvalInfo )
   {
      register USHORT uiParam = 1;

      if( HB_IS_STRING( pEvalInfo->pItems[ 0 ] ) )
      {
         const char * ptr = pEvalInfo->pItems[ 0 ]->item.asString.value;

         hb_vmPushSymbol( hb_dynsymFindName( ptr )->pSymbol );
         hb_vmPushNil();

         while( uiParam <= pEvalInfo->paramCount )
            hb_vmPush( pEvalInfo->pItems[ uiParam++ ] );

         hb_vmDo( pEvalInfo->paramCount );

         pResult = hb_itemNew( NULL );
         hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );
      }
      else if( HB_IS_BLOCK( pEvalInfo->pItems[ 0 ] ) )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pEvalInfo->pItems[ 0 ] );

         while( uiParam <= pEvalInfo->paramCount )
            hb_vmPush( pEvalInfo->pItems[ uiParam++ ] );

         hb_vmSend( pEvalInfo->paramCount );

         pResult = hb_itemNew( NULL );
         hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );
      }
   }

   return pResult;
}

/* NOTE: CA-Clipper NG states that hb_evalLaunch() must be called at least
 *       once and only once before calling hb_evalRelease(). Harbour doesn't
 *       have these requirements. [vszakats]
 */

BOOL hb_evalRelease( PHB_EVALINFO pEvalInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_evalRelease(%p)", pEvalInfo ) );

   if( pEvalInfo )
   {
      register USHORT   uiParam;
      register USHORT   uiParamCount;

      uiParamCount = pEvalInfo->paramCount;
      for( uiParam = 0; uiParam <= uiParamCount; uiParam++ )
      {
         hb_itemRelease( pEvalInfo->pItems[ uiParam ] );
         pEvalInfo->pItems[ uiParam ] = NULL;
      }
      pEvalInfo->paramCount = 0;

      return TRUE;
   }

   return FALSE;
}

/* NOTE: Same purpose as hb_evalLaunch(), but simpler, faster and more flexible.
 *       It can be used to call symbols, functions names, or blocks, the items
 *       don't need to be duplicated when passed as argument, one line is
 *       enough to initiate a call, the number of parameters is not limited.
 *       [vszakats]
 */

PHB_ITEM hb_itemDo( PHB_ITEM pItem, HB_SIZE ulPCount, ... )
{
   HB_THREAD_STUB

   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemDo(%p, %hu, ...)", pItem, ulPCount ) );

   if( pItem )
   {
      PHB_SYMB pSymbol = NULL;

      if( HB_IS_STRING( pItem ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFindName( pItem->item.asString.value );

         if( pDynSym )
            pSymbol = pDynSym->pSymbol;
      }
      else if( HB_IS_POINTER( pItem ) )
         pSymbol = ( PHB_SYMB ) pItem->item.asPointer.value;
      else if( HB_IS_SYMBOL( pItem ) )
         pSymbol = pItem->item.asSymbol.value;

      if( pSymbol )
      {
         hb_vmPushState();

         hb_vmPushSymbol( pSymbol );
         hb_vmPushNil();

         if( ulPCount )
         {
            register ULONG ulParam;
            va_list        va;

            va_start( va, ulPCount );
            for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
               hb_vmPush( va_arg( va, PHB_ITEM ) );
            va_end( va );
         }

         hb_vmDo( ( USHORT ) ulPCount );

         pResult = hb_itemNew( NULL );
         hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );

         hb_vmPopState();
      }
      else if( HB_IS_BLOCK( pItem ) )
      {
         hb_vmPushState();

         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pItem );

         if( ulPCount )
         {
            register ULONG ulParam;
            va_list        va;

            va_start( va, ulPCount );
            for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
               hb_vmPush( va_arg( va, PHB_ITEM ) );
            va_end( va );
         }

         hb_vmSend( ( USHORT ) ulPCount );

         pResult = hb_itemNew( NULL );
         hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );

         hb_vmPopState();
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         hb_vmPushState();
         if( hb_execFromArray( pItem ) )
         {
            pResult = hb_itemNew( NULL );
            hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );
         }
         hb_vmPopState();
      }
   }

   return pResult;
}

/* NOTE: Same as hb_itemDo(), but even simpler, since the function name can be
         directly passed as a zero terminated string. [vszakats]
 */
PHB_ITEM hb_itemDoC( const char * szFunc, HB_SIZE ulPCount, ... )
{
   HB_THREAD_STUB

   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemDoC(%s, %hu, ...)", szFunc, ulPCount ) );

   if( szFunc )
   {
      PHB_DYNS pDynSym;

      pDynSym = hb_dynsymFindName( szFunc );

      if( pDynSym )
      {
         hb_vmPushState();

         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();

         if( ulPCount )
         {
            register ULONG ulParam;
            va_list        va;

            va_start( va, ulPCount );
            for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
               hb_vmPush( va_arg( va, PHB_ITEM ) );
            va_end( va );
         }

         hb_vmDo( ( USHORT ) ulPCount );

         pResult = hb_itemNew( NULL );
         hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );

         hb_vmPopState();
      }
   }

   return pResult;
}

/* NOTE: Same as hb_itemDoC(), but has additional second parameter
         which set the reference mask for 1-st 32/64 parametes [druzus]
 */
PHB_ITEM hb_itemDoCRef( char * szFunc, HB_SIZE ulRefMask, HB_SIZE ulPCount, ... )
{
   HB_THREAD_STUB

   PHB_ITEM pResult = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemDoCRef(%s, %hu, %hu, ...)", szFunc, ulRefMask, ulPCount ) );

   if( szFunc )
   {
      PHB_DYNS pDynSym;

      pDynSym = hb_dynsymFindName( szFunc );

      if( pDynSym )
      {
         hb_vmPushState();
         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();

         if( ulPCount )
         {
            PHB_ITEM          pItemRefBuf[ sizeof( HB_SIZE ) * 8 ];
            HB_ITEM           itmRef;
            register HB_SIZE  ulParam;
            HB_SIZE           ulRef = 0;
            va_list           va;
            PHB_ITEM *        pRefBase;
            PHB_ITEM          pParam;

            pRefBase                                  = pItemRefBuf;
            /* initialize the reference item */
            itmRef.type                               = HB_IT_BYREF;
            itmRef.item.asRefer.offset                = -1;
            itmRef.item.asRefer.BasePtr.itemsbasePtr  = &pRefBase;

            va_start( va, ulPCount );
            for( ulParam = 0; ulParam < ulPCount; ulParam++ )
            {
               pParam = va_arg( va, PHB_ITEM );
               if( ulRefMask & ( 1L << ulParam ) )
               {
                  /* when item is passed by reference then we have to put
                   * the reference on the stack instead of the item itself
                   */
                  pItemRefBuf[ ulRef++ ]     = pParam;
                  itmRef.item.asRefer.value  = ( LONG ) ulRef;
                  /* hb_vmPush( &itmRef ); */
                  pParam                     = &itmRef;
               }
               hb_vmPush( pParam );
            }
            va_end( va );
         }

         hb_vmDo( ( USHORT ) ulPCount );

         pResult = hb_itemNew( NULL );
         hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );

         hb_vmPopState();
      }
   }

   return pResult;
}

/*
 * Notice that these two functions place the result at HB_VM_STACK.Return,
 * that you may access its value using a _par...( -1 ).
 */

/* undocumented Clipper _cEval0() */
void hb_evalBlock0( PHB_ITEM pCodeBlock )
{
   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pCodeBlock );
   hb_vmSend( 0 );
}

/* undocumented Clipper _cEval1() */
void hb_evalBlock1( PHB_ITEM pCodeBlock, PHB_ITEM pParam )
{
   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pCodeBlock );
   hb_vmPush( pParam );
   hb_vmSend( 1 );
}

/* same functionality but with a NULL terminated list of parameters */
void hb_evalBlock( PHB_ITEM pCodeBlock, ... )
{
   va_list  args;
   UINT     uiParams = 0;
   PHB_ITEM pParam;

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pCodeBlock );

   va_start( args, pCodeBlock );
   while( ( pParam = va_arg( args, PHB_ITEM ) ) != NULL )
   {
      hb_vmPush( pParam );
      ++uiParams;
   }
   va_end( args );

   hb_vmSend( ( USHORT ) uiParams );
}

/**********************************************************
 * Indirect execution section
 * HB_ExecFromArray() & relative API
 ***********************************************************/


/* JC1: HB_ExecFromArray executes a function using contents of an array as parameter list
 * Format is:
 * HB_ExecFromArray( aArray ) // aArray = { @Func(),...} or any other format
 * HB_ExecFromArray( @Func(), aArray )
 * HB_ExecFromArray( oObject, @Method(), aArray )
 *
 * 18/11/2003 - FSG - changed to support this new syntax with optional params
 *
 * HB_ExecFromArray( "Func" )
 * HB_ExecFromArray( "Func", aParams )
 * HB_ExecFromArray( @Func() )
 * HB_ExecFromArray( @Func(), aParams )
 * HB_ExecFromArray( bCode )
 * HB_ExecFromArray( bCode, aParams )
 * HB_ExecFromArray( oObject, @Method() )
 * HB_ExecFromArray( oObject, "Method" )
 * HB_ExecFromArray( oObject, @Method(), aArray )
 * HB_ExecFromArray( oObject, "Method", aArray )
 * HB_ExecFromArray( aArray )
 */

HB_FUNC( HB_EXECFROMARRAY )
{
   HB_THREAD_STUB_API

   register ULONG i;
   HB_SIZE        ulLen    = 0;
   ULONG          ulStart  = 1;
   UINT           uiPcount = ( UINT ) hb_pcount();
   PHB_ITEM       pFirst   = hb_param( 1, HB_IT_ANY );
   PHB_ITEM       pArgs    = NULL;
   PHB_ITEM       pSelf    = NULL;
   PHB_ITEM       pString;
   PHB_DYNS       pExecSym = NULL;
   PHB_SYMB       pSymbol  = NULL;

   if( HB_IS_OBJECT( pFirst ) && uiPcount == 2 )  /* hb_ExecFromArray( oObject, cMessage | pMessage )  */
   {
      pSelf    = pFirst;
      pString  = hb_param( 2, HB_IT_ANY );

      if( pString->type == HB_IT_STRING )
         pExecSym = hb_dynsymFindName( pString->item.asString.value );
      else if( pString->type == HB_IT_POINTER )
         pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pString );
   }
   else if( HB_IS_OBJECT( pFirst ) && uiPcount == 3 ) /* hb_ExecFromArray( oObject, cMessage | pMessage, { params,... } )  */
   {
      pSelf    = pFirst;
      pString  = hb_param( 2, HB_IT_ANY );

      if( pString->type == HB_IT_STRING )
         pExecSym = hb_dynsymGet( pString->item.asString.value );
      else if( pString->type == HB_IT_POINTER )
         pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pString );

      pArgs = hb_param( 3, HB_IT_ARRAY );
   }
   else if( pFirst->type == HB_IT_STRING && uiPcount == 1 ) /* hb_ExecFromArray( cFunc )  */
      pExecSym = hb_dynsymFindName( pFirst->item.asString.value );
   else if( pFirst->type == HB_IT_STRING && uiPcount == 2 ) /* hb_ExecFromArray( cFunc, { params,... } )  */
   {
      pExecSym = hb_dynsymFindName( pFirst->item.asString.value );
      pArgs    = hb_param( 2, HB_IT_ARRAY );
   }
   else if( pFirst->type == HB_IT_POINTER && uiPcount == 1 )  /* hb_ExecFromArray( pFunc )  */
      pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pFirst );
   else if( pFirst->type == HB_IT_POINTER && uiPcount == 2 )  /* hb_ExecFromArray( pFunc, { params,... } )  */
   {
      pSymbol  = ( PHB_SYMB ) hb_itemGetPtr( pFirst );
      pArgs    = hb_param( 2, HB_IT_ARRAY );
   }
   else if( HB_IS_BLOCK( pFirst ) && uiPcount == 1 )       /* hb_ExecFromArray( bCode )  */
   {
      pSymbol  = &hb_symEval;
      pSelf    = pFirst;
   }
   else if( HB_IS_BLOCK( pFirst ) && uiPcount == 2 )      /* hb_ExecFromArray( bCode, { params,... } )  */
   {
      pSymbol  = &hb_symEval;
      pSelf    = pFirst;
      pArgs    = hb_param( 2, HB_IT_ARRAY );
   }
   else if( HB_IS_ARRAY( pFirst ) )                       /* hb_ExecFromArray( aArray )  */
   {
      pString  = hb_arrayGetItemPtr( pFirst, 1 );
      pArgs    = pFirst;

      if( HB_IS_OBJECT( pString ) && hb_arrayLen( pFirst ) >= 2 )
      {
         pSelf    = pString;
         pString  = hb_arrayGetItemPtr( pFirst, 2 );

         if( pString->type == HB_IT_STRING )
            pExecSym = hb_dynsymFindName( pString->item.asString.value );
         else if( pString->type == HB_IT_POINTER )
            pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pString );

         ulStart = 3;
      }
      else if( pString->type == HB_IT_STRING )
      {
         pExecSym = hb_dynsymFindName( pString->item.asString.value );
         ulStart  = 2;
      }
      else if( pString->type == HB_IT_POINTER )
      {
         pSymbol  = ( PHB_SYMB ) hb_itemGetPtr( pString );
         ulStart  = 2;
      }
      else if( HB_IS_BLOCK( pString ) )
      {
         pSelf    = pString;
         pSymbol  = &hb_symEval;
         ulStart  = 2;
      }
   }

   if( pExecSym )
      pSymbol = pExecSym->pSymbol;

   if( pSymbol == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_ExecFromArray", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }

   hb_vmPushSymbol( pSymbol );

   if( pSelf )
      hb_vmPush( pSelf );
   else
      hb_vmPushNil();

   if( pArgs )
      ulLen = hb_arrayLen( pArgs );

   /* pushing the contents of the array
    */
   for( i = ulStart; i <= ulLen; i++ )
      hb_vmPush( hb_arrayGetItemPtr( pArgs, i ) );

   if( pSelf )
      hb_vmSend( ( USHORT ) ( ulLen - ulStart + 1 ) );
   else
      hb_vmDo( ( USHORT ) ( ulLen - ulStart + 1 ) );
}

/* JC1: To reduce OH of using the HB_FUN_ version of hb_execFromArray()
   as just a wrapper, here is a "reduced" version of hb_execFromArray
   available for c programs, accepting only the format:

   hb_execFromArray( aArray ) // aArray = { @Func(),...} or any other format
   Returns TRUE if the routine call is sucessful (object returned by
   the called xharbour routine is in HB_VM_STACK.Return), and false if
   call is not possible (e.g. incorrect callable array)
 */

BOOL hb_execFromArray( PHB_ITEM pFirst )
{
   register ULONG i;
   HB_SIZE        ulLen;
   ULONG          ulStart  = 1;
   PHB_ITEM       pArgs;
   PHB_ITEM       pString;
   PHB_ITEM       pSelf    = NULL;
   PHB_DYNS       pExecSym = NULL;
   PHB_SYMB       pSymbol  = NULL;

   if( pFirst == NULL || pFirst->type != HB_IT_ARRAY )
      return FALSE;

   pString  = hb_arrayGetItemPtr( pFirst, 1 );
   pArgs    = pFirst;

   if( HB_IS_OBJECT( pString ) && hb_arrayLen( pFirst ) >= 2 )
   {
      pSelf    = pString;
      pString  = hb_arrayGetItemPtr( pFirst, 2 );

      if( pString->type == HB_IT_STRING )
         pExecSym = hb_dynsymFindName( pString->item.asString.value );
      else if( pString->type == HB_IT_POINTER )
         pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pString );

      ulStart = 3;
   }
   else if( pString->type == HB_IT_STRING )
   {
      pExecSym = hb_dynsymFindName( pString->item.asString.value );
      ulStart  = 2;
   }
   else if( pString->type == HB_IT_POINTER )
   {
      pSymbol  = ( PHB_SYMB ) hb_itemGetPtr( pString );
      ulStart  = 2;
   }
   else if( HB_IS_BLOCK( pString ) )
   {
      pSymbol  = &hb_symEval;
      ulStart  = 2;
   }

   if( pExecSym )
      pSymbol = pExecSym->pSymbol;

   if( pSymbol == NULL )
      return FALSE;

   hb_vmPushSymbol( pSymbol );

   if( pSelf )
      hb_vmPush( pSelf );
   else
      hb_vmPushNil();

   ulLen = hb_arrayLen( pArgs );

   /* pushing the contents of the array
    */
   for( i = ulStart; i <= ulLen; i++ )
      hb_vmPush( hb_arrayGetItemPtr( pArgs, i ) );

   if( pSelf )
      hb_vmSend( ( USHORT ) ( ulLen - ulStart + 1 ) );
   else
      hb_vmDo( ( USHORT ) ( ulLen - ulStart + 1 ) );

   return TRUE;
}

/******************************************************
* EXEC function
******************************************************/
HB_FUNC( HB_EXEC )
{
   HB_THREAD_STUB

   PHB_ITEM pPointer = hb_param( 1, HB_IT_POINTER );

   if( pPointer )
   {
      PHB_SYMB pSymbol  = ( PHB_SYMB ) hb_itemGetPtr( pPointer );
      int      iParams;
      BOOL     bSend    = FALSE;

      if( pSymbol )
      {
         iParams = hb_pcount() - 1;

         if( iParams >= 1 )
         {
            if( hb_param( 2, HB_IT_ANY )->type )
               bSend = TRUE;

            iParams--;
         }
         else
            hb_vmPushNil();
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_Exec", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         return;
      }

      /* printf( "Sym: '%s' Params: %i\n", pSymbol->szName, iParams );
       */

      /* Changing the Pointer item to a Symbol Item, so that we don't have to re-push paramters.
       */
      hb_itemPutSymbol( pPointer, pSymbol );
      HB_MEMCPY( pPointer->item.asSymbol.pCargo, hb_stackBaseItem()->item.asSymbol.pCargo, sizeof( HB_SYMBCARGO ) );
      pPointer->item.asSymbol.pCargo->stackbase    = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );
      pPointer->item.asSymbol.pCargo->uiSuperClass = 0;

      if( bSend )
         hb_vmSend( ( USHORT ) iParams );
      else
         hb_vmDo( ( USHORT ) iParams );

      return;
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_Exec", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}
