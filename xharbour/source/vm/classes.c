/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base-routines for OOPS system
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
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    :CLASSSEL()
 *    __clsDelMsg()
 *    __clsModMsg()
 *    __clsInstSuper()
 *    __cls_CntClsData()
 *    __cls_CntData()
 *    __cls_DecData()
 *    __cls_IncData()
 *    __objClone()
 *    __objHasMsg()
 *    __objSendMsg()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb___msgEval()
 *    __CLASSNEW()
 *    __CLASSINSTANCE()
 *    __CLASSADD()
 *    __CLASSNAME()
 *    __CLASSSEL() (based on hb___msgClsSel())
 *
 * Copyright 1999 Janica Lubos <janica@fornax.elf.stuba.sk>
 *    hb_clsDictRealloc()
 *
 * Copyright 2000 ( ->07/2000 ) JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <cakiral@altern.org
 *    Multiple inheritence fully implemented
 *    Forwarding, delegating
 *    Data initialisation & Autoinit for Bool and Numeric
 *    Scoping : Protected / exported
 *
 * Copyright 2000 ( 08/2000-> ) JF. Lefebvre <jfl@mafact.com>
 *    hb_clsDictRealloc()   New version
 *    Now support of shared and not shared class data
 *    Multiple datas declaration fully supported
 *
 *    2000 RGlab
 *    Garbage collector fixe
 *
 * Copyright 2001 JF. Lefebvre <jfl@mafact.com>
 *    Super msg corrected
 *    Scoping : working for protected, hidden and readonly
 *    To Many enhancement and correction to give a full list :-)
 *    Improved class(y) compatibility
 *    Improved TopClass compatibility
 *    __CLS_PAR00() (Allow the creation of class wich not autoinherit of the default HBObject)
 *    Adding HB_CLS_ENFORCERO FLAG to disable Write access to RO VAR
 *    outside of Constructors /!\ Could be related to some incompatibility
 *    Added hb_objGetRealClsName to keep a full class tree ( for 99% cases )
 *    Fixed hb_clsIsParent
 *
 * Copyright 2005 Walter Negro <anegro@overnet.com.ar>
 *    New engine, based in a list of the value of pMessage ordered.
 *    hb_clsFindMethod() search the method using binary search.
 *    hb_clsSaveMethod() insert new method in the list.
 *    hb_clsDelMethod() delete a method from list.
 *    hb_objGetpMthd() return a pointer to pMethod and replace to old
 *    search procedure. (uiAt, uiMask, uiLimit)
 *    With new engine the memory used by pMethods is only the necessary.
 *    Old procedure consume 3, 5 o more memory than new.
 *    - MsgToNum()
 *    - hb_clsDictRealloc()
 *    + hb_clsFindMethod()
 *    + hb_clsSaveMethod()
 *    + hb_clsDelMethod()
 *    + hb_objGetpMthd()
 *
 *    hb_objGetMthd() & __CLSADDMSG modified to translate the followings operators
 *
   "+"     = __OpPlus
   "-"     = __OpMinus
   "*"     = __OpMult
   "/"     = __OpDivide
   "%"     = __OpMod
   "^"     = __OpPower
   "**"    = __OpPower
   "++"    = __OpInc
   "--"    = __OpDec
   "=="    = __OpExactEqual
   "="     = __OpEqual
   "!="    = __OpNotEqual
   "<>"    = __OpNotEqual (same as "!=")
   "#"     = __OpNotEqual (same as "!=")
   "<"     = __OpLess
   "<="    = __OpLessEqual
   ">"     = __OpGreater
   ">="    = __OpGreaterEqual
   "$"     = __OpInstring
   "!"     = __OpNot
   ".NOT." = __OpNot (same as "!")
   ".AND." = __OpAnd
   ".OR."  = __OpOr
   ":="    = __OpAssign   ... not tested ...

 * Implemented by Walter Negro
   "&"     = __OpBitAnd
   "|"     = __OpBitOr
   "^^"    = __OpBitXor
   ">>"    = __OpBitShiftR
   "<<"    = __OpBitShiftL

 * ...
   "$$"    = __OpContains

 *
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include <math.h>

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hboo.ch"
#include "classes.h"
#include "hashapi.h"

#ifdef HB_THREAD_SUPPORT
#include "thread.h"
#endif

#include <ctype.h>             /* For HB_TOUPPER() */

/* DEBUG only */
/* #include <windows.h> */

/* #define DEBUG_HASH */

#if ( defined( HB_OPT_CON ) || defined( HB_OPT_GUI ) ) && ! defined( HB_NO_PROFILER )
#define HB_NO_PROFILER
#endif

#ifndef HB_NO_PROFILER
extern BOOL       hb_bProfiler; /* profiler activity status */
#endif

static PHB_DYNS   s_msgClassName       = NULL;

static PHB_DYNS   s_msgClassH          = NULL;
static PHB_DYNS   s_msgEval            = NULL;
static PHB_DYNS   s_msgClassSel        = NULL;
static PHB_DYNS   s_msgClassFullSel    = NULL;
static PHB_DYNS   s_msgClsParent       = NULL;

static BOOL       s_bClsScope          = TRUE;
static BOOL       s_bClsAutoInit       = TRUE;
static BOOL       s_AllowDestructors   = TRUE;

/* static PHB_DYNS s_msgClass     = NULL; */

/* All functions contained in classes.c */

void            hb_clsInst( USHORT uiClass, PHB_ITEM pSelf );
static BOOL     hb_clsValidScope( PHB_ITEM pObject, PMETHOD pMethod, int iOptimizedSend );

static USHORT   hb_clsFindMethod( PHB_DYNS pMsg, PCLASS pClass, int * piPos );
static void     hb_clsSaveMethod( PHB_DYNS pMsg, int iPivot, PCLASS pClass, USHORT uiAt );
static void     hb_clsDelMethod( PCLASS pClass, int iPos, USHORT uiAt );

static PHB_FUNC hb_objGetMessage( PHB_ITEM pObject, const char * szString, PHB_DYNS * ppDynSym );

static void     hb_clsClear( PCLASS );
static void     hb_clsRelease( PCLASS );

static void     hb___msgClsH( void );
static void     hb___msgClsName( void );
static void     hb___msgClsFullSel( void );
static void     hb___msgClsSel( void );
/* static void  hb___msgClass( void ); */
static void     hb___msgSuper( void );
static void     hb___msgEvalInline( void );
static void     hb___msgClsParent( void );
#if 0
static void     hb___msgEval( void );
#endif
static void     hb___msgVirtual( void );
static void     hb___msgDelegate( void );

USHORT hb_cls_uiArrayClass    = 0, hb_cls_uiBlockClass = 0, hb_cls_uiCharacterClass = 0, hb_cls_uiDateClass = 0,
       hb_cls_uiTimeFlagClass = 0,
       hb_cls_uiLogicalClass  = 0, hb_cls_uiNilClass = 0, hb_cls_uiNumericClass = 0, hb_cls_uiPointerClass = 0,
       hb_cls_uiHashClass     = 0;

/* ================================================ */

/*
 * Class definition holder
 */
/* In MT mode we are allocating array big enough to hold all
 * class definitions so we do not have to worry about runtime
 * s_pClasses reallocation, [druzus]
 */
#if defined( HB_THREAD_SUPPORT )

#  define HB_CLASS_POOL_SIZE  16384
#  define HB_CLASS_LOCK       HB_CRITICAL_LOCK( s_clsMtx );
#  define HB_CLASS_UNLOCK     HB_CRITICAL_UNLOCK( s_clsMtx );
static HB_CRITICAL_T s_clsMtx;

#else

#  define HB_CLASS_POOL_SIZE  0
#  define HB_CLASS_LOCK
#  define HB_CLASS_UNLOCK

#endif

#define HB_CLASS_POOL_RESIZE  16

static PCLASS  s_pClasses  = NULL;
static USHORT  s_uiClsSize = 0;
static USHORT  s_uiClasses = 0;

/* ================================================ */

static BOOL hb_clsHasParent( PCLASS pClass, PHB_DYNS pParentSym )
{
   USHORT uiAt = hb_clsFindMethod( pParentSym, pClass, NULL );

   if( uiAt )
      return pClass->pMethods[ uiAt - 1 ].pFunction == hb___msgSuper;

   return FALSE;
}

static BOOL hb_clsIsFriendSymbol( PCLASS pClass, PHB_SYMB pSym )
{
   USHORT uiCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsIsFriendSymbol(%p,%p)", pClass, pSym ) );

   if( pSym >= pClass->pFriendModule &&
       pSym < pClass->pFriendModule + pClass->uiFriendModule )
      return TRUE;

   for( uiCount = 0; uiCount < pClass->uiFriendSyms; ++uiCount )
   {
      if( pClass->pFriendSyms[ uiCount ] == pSym )
         return TRUE;
   }

   return FALSE;
}

static void hb_clsAddFriendSymbol( PCLASS pClass, PHB_SYMB pSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsAddFriendSymbol(%p,%p)", pClass, pSym ) );

   if( ! hb_clsIsFriendSymbol( pClass, pSym ) )
   {
      if( pClass->uiFriendSyms == 0 )
      {
         pClass->pFriendSyms        = ( PHB_SYMB * ) hb_xgrab( sizeof( PHB_SYMB ) );
         pClass->pFriendSyms[ 0 ]   = pSym;
         pClass->uiFriendSyms++;
      }
      else
      {
         pClass->pFriendSyms                             = ( PHB_SYMB * ) hb_xrealloc( pClass->pFriendSyms,
                                                                                       ( pClass->uiFriendSyms + 1 ) * sizeof( PHB_SYMB ) );
         pClass->pFriendSyms[ pClass->uiFriendSyms++ ]   = pSym;
      }
   }
}

/*
 * initialize Classy/OO system at HVM startup
 */
void hb_clsInit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsInit()" ) );

   s_uiClsSize = HB_CLASS_POOL_SIZE;
   s_uiClasses = 0;
   s_pClasses  = ( PCLASS ) hb_xgrab( sizeof( CLASS ) * ( ( ULONG ) s_uiClsSize + 1 ) );

#ifdef HB_THREAD_SUPPORT
   HB_CRITICAL_INIT( s_clsMtx );
#endif
}

/*
 * hb_clsRelease( <pClass> )
 *
 * Release a class from memory
 */
static void hb_clsRelease( PCLASS pClass )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsRelease(%p)", pClass ) );

   if( pClass->pInitValues )
      hb_xfree( pClass->pInitValues );

   if( pClass->szName )
      hb_xfree( ( void * ) pClass->szName );

   if( pClass->pMethods )
      hb_xfree( pClass->pMethods );

   if( pClass->uiFriendSyms )
      hb_xfree( pClass->pFriendSyms );

   hb_xfree( pClass->pMethDyn );

   if( pClass->pMtxSync )
      hb_itemRelease( pClass->pMtxSync );

/*
   if( pClass->pFunError )
   {
      if( ( pClass->uiScope & HB_OO_CLS_ONERROR_SYMB ) == HB_OO_CLS_ONERROR_SYMB )
      {
         if( ( pClass->uiScope & HB_OO_CLS_ONERROR_SUPER ) != HB_OO_CLS_ONERROR_SUPER )
         {
            hb_xfree( pClass->pFunError );
         }
      }
   }
 */
}

/*
 * hb_clsClearClassDatas( <pClass> )
 *
 * Reset all classdata held by class definition
 */
static void hb_clsClearClassDatas( PCLASS pClass )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsClearClassDatas(%p)", pClass ) );

   hb_arrayFill( pClass->pClassDatas, ( *HB_VM_STACK.pPos ), 1, pClass->pClassDatas->item.asArray.value->ulLen );
}

/*
 * hb_clsClear( <pClass> )
 *
 * Release all data held by class definition
 */
static void hb_clsClear( PCLASS pClass )
{
   USHORT   uiAt;
   PMETHOD  pMeth = pClass->pMethods;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsClear(%p)", pClass ) );

   for( uiAt = pClass->uiMethods + 1; --uiAt; pMeth++ )
   {
      if( pMeth->pInitValue )
      {
         hb_itemRelease( pMeth->pInitValue );
         pMeth->pInitValue = NULL;
      }
   }

   /*
      if( pClass->pMtxSync )
      {
      hb_itemRelease( pClass->pMtxSync );
      pClass->pMtxSync = NULL;
      }
    */

   hb_itemRelease( pClass->pClassDatas );
   pClass->pClassDatas  = NULL;

   hb_itemRelease( pClass->pInlines );
   pClass->pInlines     = NULL;
}

void hb_clsDisableDestructors( void )
{
   s_AllowDestructors = FALSE;
}

BOOL hb_clsDestructorsAllowed( void )
{
   return s_AllowDestructors;
}

/*
 * hb_clsClearAllClassDatas( <pClass> )
 *
 * Reset all classdata held by ALL class definition
 */
void hb_clsClearAllClassDatas( void )
{
   SHORT uiClass;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsClearAll()" ) );

   for( uiClass = 0; uiClass < s_uiClasses; uiClass++ )
      hb_clsClearClassDatas( s_pClasses + uiClass );
}

void hb_clsClearAll( void )
{
   SHORT uiClass;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsClearAll()" ) );

   for( uiClass = 0; uiClass < s_uiClasses; uiClass++ )
      hb_clsClear( s_pClasses + uiClass );
}

/*
 * hb_clsReleaseAll()
 *
 * Release all classes
 */
void hb_clsReleaseAll( void )
{
   SHORT uiClass;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsReleaseAll()" ) );

   for( uiClass = 0; uiClass < s_uiClasses; uiClass++ )
      hb_clsRelease( s_pClasses + uiClass );

   if( s_pClasses )
      hb_xfree( s_pClasses );

   s_pClasses        = NULL;
   s_uiClasses       = 0;
   s_msgClassName    = NULL;

   s_msgClassH       = NULL;
   s_msgEval         = NULL;
   s_msgClassSel     = NULL;
   s_msgClassFullSel = NULL;
   s_msgClsParent    = NULL;
   s_bClsScope       = TRUE;
   s_bClsAutoInit    = TRUE;
}

/* Mark all internal data as used so it will not be released by the
 * garbage collector
 */

void hb_clsIsClassRef( void )
{
   PCLASS   pClass   = s_pClasses;
   PMETHOD  pMeth;
   USHORT   uiClass  = s_uiClasses;
   USHORT   uiAt;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsIsClassRef()" ) );

   while( uiClass-- )
   {
      if( pClass->pInlines )
         hb_gcItemRef( pClass->pInlines );

      if( pClass->pClassDatas )
         hb_gcItemRef( pClass->pClassDatas );

      pMeth = pClass->pMethods;

      for( uiAt = pClass->uiMethods + 1; --uiAt; pMeth++ )
      {
         if( pMeth->pInitValue )
            hb_gcItemRef( pMeth->pInitValue );
      }

      ++pClass;
   }
}

static BOOL hb_clsValidScope( PHB_ITEM pObject, PMETHOD pMethod, int iOptimizedSend )
{
   USHORT uiScope;

   /* Checking scope. s_bClsScope is defined FALSE to retrieve values without scope violation
    * i.e. during debug or in designer mode
    */
   if( ( ! s_bClsScope ) || pObject->type != HB_IT_ARRAY || pObject->item.asArray.value->uiClass == 0 )
      return TRUE;

   uiScope = pMethod->uiScope;

   /* #define DEBUG_SCOPE
    */

   if( uiScope & ( HB_OO_CLSTP_PROTECTED | HB_OO_CLSTP_HIDDEN | HB_OO_CLSTP_READONLY ) )
   {
      if( ( uiScope & ( HB_OO_CLSTP_PROTECTED |
                        HB_OO_CLSTP_HIDDEN |
                        HB_OO_CLSTP_READONLY ) ) == HB_OO_CLSTP_READONLY &&
          iOptimizedSend != 2 && pMethod->pMessage->pSymbol->szName[ 0 ] != '_' )
         /* Allow anyway if all we do is read a property.
          */
         return TRUE;
      else
      {
         HB_THREAD_STUB

         PHB_ITEM * pBase    = HB_VM_STACK.pBase;
         PHB_ITEM   pCaller;
         PCLASS     pClass   = s_pClasses + ( pObject->item.asArray.value->uiClass - 1 ), pRealClass = pClass;
         PHB_SYMB   pSym;

         /* ----------------- Get the Caller Symbol ----------------- */
         if( iOptimizedSend == 0 )
            /* Outer function level.
             */
            pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;

         if( strcmp( ( *pBase )->item.asSymbol.value->szName, "__OBJSENDMSG" ) == 0 ||
             strcmp( ( *pBase )->item.asSymbol.value->szName, "ASCAN" ) == 0 )
            /* Backtrack 1 level.
             */
            pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;

         /* ----------------- Get the Caller Self ----------------- */

         if( strcmp( ( *pBase )->item.asSymbol.value->szName, "AEVAL" ) == 0 )
            pCaller = *( pBase + 1 + 2 );
         else
            pCaller = *( pBase + 1 );

         /* ----------------- Get the Real Module ----------------- */

         /*
            If Method is Inerited then the Module Name may be SAME because compared to THIS INHERTED Object
            but the PARENT Class may be defined in a SEPERATE Module
          */
         if( uiScope & HB_OO_CLSTP_SUPER )
         {
            USHORT uiSuperClass = hb_objGetRealCls( pObject, pMethod->pMessage->pSymbol->szName );

            if( uiSuperClass )
               pRealClass = s_pClasses + ( uiSuperClass - 1 );
         }

         /* ----------------- Compare Modules ----------------- */
         if( HB_IS_OBJECT( pCaller ) )
            pSym = s_pClasses[ pCaller->item.asArray.value->uiClass - 1 ].pClassFuncSym;
         else if( HB_IS_BLOCK( pCaller ) )
            pSym = pCaller->item.asBlock.value->symbol;
         else
            pSym = ( *pBase )->item.asSymbol.value;

         if( hb_clsIsFriendSymbol( pRealClass, hb_vmGetRealFuncSym( pSym ) ) )
            return TRUE;

         /* ----------------- Validate Scope ----------------- */

#ifdef DEBUG_SCOPE
         printf( "Method: '%s' Scope: %i\n\r", pMethod->pMessage->pSymbol->szName, uiScope );
#endif

#ifdef CLASSY_SCOPE
         /* Outer while in Inline, Eval() or aEval().
          */
         while( ( HB_IS_BLOCK( *( pBase + 1 ) ) || strcmp( ( *pBase )->item.asSymbol.value->szName, "AEVAL" ) == 0 ) )
         {
            pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
         }
#else
         if( HB_IS_BLOCK( pCaller ) )
         {
            if( pCaller->item.asBlock.value->uiClass == 0 )
            {
               const char * szCaller = pCaller->item.asBlock.value->symbol->szName;
               const char * pAt      = strchr( szCaller, ':' );

               if( pAt )
               {
                  /* Same class.
                   */
                  if( strncmp( szCaller, pClass->szName, pAt - szCaller ) == 0 )
                     return TRUE;
               }
               else
               {
                  /* Block is a Data of Object initialized at Class Creation Function.
                   */
                  if( strcmp( szCaller, pClass->szName ) == 0 )
                     return TRUE;
               }
            }
            else
            {
               /* pObject and the HB_QSelf() of block are same class.
                */
               if( pCaller->item.asBlock.value->uiClass == pObject->item.asArray.value->uiClass )
                  return TRUE;
            }

            /* Either NOT same Class, or Block was created from NON Method, or INLINE Method!
             */
            if( pBase != HB_VM_STACK.pItems )
            {
               do
               {
                  /* Backtrack 1 level incase it's INLINE Method.
                   */
                  pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
               }
               while( ( HB_IS_BLOCK( *( pBase + 1 ) ) && pBase != HB_VM_STACK.pItems ) );

               pCaller = *( pBase + 1 );
            }
         }
#endif

         if( HB_IS_OBJECT( pCaller ) )
         {
#ifdef DEBUG_SCOPE
            printf( "Object: %s, Caller: %s\n", pClass->szName, ( s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 ) )->szName );
#endif

            /* This is an Inherited Method
             */
            if( uiScope & HB_OO_CLSTP_SUPER )
            {
               const char * szCallerMessage = ( *pBase )->item.asSymbol.value->szName;

#ifdef DEBUG_SCOPE
               printf( "Object: %s, Message: %s, RealClass: %s, Caller: %s, CallerMessage: %s, CallerMessageClass: %s\n",
                       pClass->szName,
                       pMethod->pMessage->pSymbol->szName,
                       hb_objGetRealClsName( pObject, pMethod->pMessage->pSymbol->szName ),
                       ( s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 ) )->szName,
                       szCallerMessage,
                       hb_objGetRealClsName( pCaller, szCallerMessage ) );
#endif

               /* It's possible that the Caller Message is a Super Messge, and Super is also where this Derived  Method is defined.
                */
               if( hb_objGetRealCls( pObject, pMethod->pMessage->pSymbol->szName ) == hb_objGetRealCls( pCaller, szCallerMessage ) )
                  return TRUE;

               /* It's possible that caller Method is Derived from a Class which also implements the Message we now validate.
                * This means the validated Message IS avialable within the scope of the Methods that calls this Message.
                */
               if( hb_clsHasMsg( hb_objGetRealCls( pCaller, szCallerMessage ), pMethod->pMessage->pSymbol->szName ) )
                  return TRUE;

               /* HIDDEN Method can't be called from subclass.
                */
               if( uiScope & HB_OO_CLSTP_HIDDEN )
                  goto ScopeError;

               /* PROTECTED + READONLY can NOT be written from subclass.
                */
               if( ( uiScope & HB_OO_CLSTP_PROTECTED ) && ( uiScope & HB_OO_CLSTP_READONLY ) )
                  goto ScopeError;
               else
               {
                  if( uiScope & HB_OO_CLSTP_READONLY )
                     /* Derived READONLY Message ( NOT PROTCTED ) allowed.
                     */
                     return TRUE;
                  else
                  {
                     /* If we got here, this MUST be a Derived NON HIDDEN, NON READONLY Message, thus a Derived PROTECTED Method.
                      */
                     const char * szClassOfMessage = hb_objGetRealClsName( pObject, pMethod->pMessage->pSymbol->szName );

#ifdef DEBUG_SCOPE
                     printf( "Defined in: %s\n", szClassOfMessage );
#endif

                     /* Is the Caller derived from the Object?
                      */
                     if( hb_clsIsParent( pCaller->item.asArray.value->uiClass, szClassOfMessage ) )
                        /* Derived class - allow access to PROTECTED. */
                        return TRUE;
                  }
               }
            }
            else
            {
               /* NON Inherited Message, and Caller is same class as the object, so all scopes allowed.
                */
               if( pCaller->item.asArray.value->uiClass == pObject->item.asArray.value->uiClass )
                  return TRUE;
               else
               {
                  const char * szObjectClass = pClass->szName;

                  /* Is the Caller derived from the Object?
                   */
                  if( hb_clsIsParent( pCaller->item.asArray.value->uiClass, szObjectClass ) )
                  {
                     if( uiScope & HB_OO_CLSTP_PROTECTED )
                     {
                        if( uiScope & HB_OO_CLSTP_READONLY )
                           /* PROTECTED + READONLY can NOT be written from subclass. */
                           goto ScopeError;
                        else
                           /* PROTECTED (NON READONLY) can be written from subclass. */
                           return TRUE;
                     }

                     /* All other scopes are NOT allowed even in derived class. */
                     return TRUE;
                  }
               }
               /* This is NOT an Inherted Method, and Caller is Different Class - No restricted scope could be valid (excpet READONLY addressed above)!
                */
            }
         }

         /* All else is not allowed.
          */
         ScopeError:
         {
            char szScope[ 64 ];

            szScope[ 0 ] = '\0';

            hb_xstrcat( szScope, "Scope Violation <", 0 );

            if( uiScope & HB_OO_CLSTP_HIDDEN )
               hb_xstrcat( szScope, "HIDDEN+", 0 );

            if( uiScope & HB_OO_CLSTP_PROTECTED )
               hb_xstrcat( szScope, "PROTECTED+", 0 );

            if( uiScope & HB_OO_CLSTP_READONLY )
               hb_xstrcat( szScope, "READONLY+", 0 );

            szScope[ strlen( szScope ) - 1 ] = '>';

            hb_errRT_BASE( EG_NOMETHOD, 1004, szScope, pMethod->pMessage->pSymbol->szName, 1, pObject );

            return FALSE;
         }
      }
   }

   return TRUE;
}

BOOL hb_clsIsParent( USHORT uiClass, const char * szParentName )
{
   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      if( strcmp( pClass->szName, szParentName ) == 0 )
         return TRUE;
      else
      {
         PHB_DYNS pMsg = hb_dynsymFindName( szParentName );

         if( pMsg )
            return hb_clsHasParent( pClass, pMsg );
      }
   }

   return FALSE;
}

/* ================================================ */

/*
 * Get the class name of an object
 */
const char * hb_objGetClsName( PHB_ITEM pObject )
{
   const char * szClassName;
   USHORT       uiClass = hb_objClassH( pObject );

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetClsName(%p)", pObject ) );

   if( uiClass > 0 )
      szClassName = ( s_pClasses + uiClass - 1 )->szName;
   else                                         /* built in types */
   {
      switch( pObject->type )
      {
         case HB_IT_NIL:
            szClassName = "NIL";
            break;

         case HB_IT_ARRAY:
            szClassName = "ARRAY";
            break;

         case HB_IT_STRING:
            szClassName = "CHARACTER";
            break;

         case HB_IT_BLOCK:
            szClassName = "BLOCK";
            break;

         case HB_IT_SYMBOL:
            szClassName = "SYMBOL";
            break;

         case HB_IT_DATE:
            szClassName = "DATE";
            break;

         case HB_IT_TIMEFLAG:
            szClassName = "TIMEFLAG";
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            szClassName = "NUMERIC";
            break;

         case HB_IT_LOGICAL:
            szClassName = "LOGICAL";
            break;

         case HB_IT_POINTER:
            szClassName = "POINTER";
            break;

         case HB_IT_HASH:
            szClassName = "HASH";
            break;

         default:
            szClassName = "UNKNOWN";
            break;
      }
   }

   return szClassName;
}

const char * hb_clsName( USHORT uiClass )
{
   return ( uiClass && ( uiClass <= s_uiClasses ) ) ? s_pClasses[ uiClass - 1 ].szName : NULL;
}

/*
 * <szName> = ( pObject )
 *
 * Get the real class name of an object message
 * Will return the class name from wich the message is inherited in case
 * of inheritance.
 *
 */
USHORT hb_objGetRealCls( PHB_ITEM pObject, const char * szName )
{
   HB_THREAD_STUB

   PHB_DYNS pMsg    = hb_dynsymFindName( szName );
   PHB_ITEM pBase   = hb_stackBaseItem();
   USHORT   uiClass = 0;
   USHORT   uiCurCls;

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetRealCls(%p, %s)", pObject, szName ) );

   if( ! pMsg )
      return 0;

   uiCurCls = ( USHORT ) pBase->item.asSymbol.pCargo->uiSuperClass;

   if( uiCurCls == 0 )
      uiCurCls = hb_objClassH( pObject );

   if( uiCurCls && uiCurCls <= s_uiClasses )
   {
      PMETHOD pMethod = hb_objGetpMthd( pMsg, uiCurCls );

      if( pMethod && pMethod->uiSprClass )
         uiClass = pMethod->uiSprClass;
   }

   return uiClass;
}

const char * hb_objGetRealClsName( PHB_ITEM pObject, const char * szName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetrealClsName(%p, %s)", pObject, szName ) );

   if( HB_IS_ARRAY( pObject ) )
   {
      if( ! pObject->item.asArray.value->uiClass )
         return "ARRAY";
      else
      {
         USHORT uiClass = hb_objGetRealCls( pObject, szName );

         if( uiClass && uiClass <= s_uiClasses )
            return ( s_pClasses + uiClass - 1 )->szName;
         else
            return "";
      }
   }
   else                                         /* built in types */
   {
      switch( pObject->type )
      {
         case HB_IT_NIL:
            return "NIL";

         case HB_IT_STRING:
            return "CHARACTER";

         case HB_IT_BLOCK:
            return "BLOCK";

         case HB_IT_SYMBOL:
            return "SYMBOL";

         case HB_IT_DATE:
            return "DATE";

         case HB_IT_TIMEFLAG:
            return "TIMEFLAG";

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            return "NUMERIC";

         case HB_IT_LOGICAL:
            return "LOGICAL";
      }
   }

   return "UNKNOWN";
}

HB_SIZE hb_objGetOpOver( const PHB_ITEM pObject )
{
   USHORT uiClass = hb_objClassH( pObject );

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetOpOver(%p)", pObject ) );

   if( uiClass )
      return ( s_pClasses + ( uiClass - 1 ) )->fOpOver;

   return 0;
}

PHB_SYMB hb_objGetClsSymbol( const PHB_ITEM pObject )
{
   USHORT uiClass = hb_objClassH( pObject );

   return uiClass ? ( s_pClasses + ( uiClass - 1 ) )->pClassFuncSym : NULL;
}

static USHORT hb_clsFindMethod( PHB_DYNS pMsg, PCLASS pClass, int * piPos )
{
   int iMin       = 0;
   int iPivot     = 0;
   int iLen       = ( int ) pClass->uiMethods;
   int iMax       = iLen - 1;
   PMETHDYN pDict = pClass->pMethDyn;

   while( iMin <= iMax )
   {
      iPivot = ( iMin + iMax ) >> 1;

      if( pMsg == pDict[ iPivot ].pMessage )
      {
         if( piPos )
            *piPos = iPivot;

         return pDict[ iPivot ].uiAt;
      }
      else if( pMsg < pDict[ iPivot ].pMessage )
         iMax = iPivot - 1;
      else
         iMin = iPivot + 1;
   }

   if( piPos )
   {
      if( iMax < iPivot )
         iPivot = iMax + 1;
      else if( iMin > iPivot )
         iPivot = iMin;

      *piPos = iPivot;
   }

   return 0;
}

static void hb_clsSaveMethod( PHB_DYNS pMsg, int iPivot, PCLASS pClass, USHORT uiAt )
{
   int      iLen  = ( int ) uiAt;
   PMETHDYN pDict = pClass->pMethDyn;

   if( iPivot + 1 < iLen )
      memmove( pDict + iPivot + 1, pDict + iPivot, ( iLen - ( iPivot + 1 ) ) * sizeof( METHDYN ) );

   pDict[ iPivot ].pMessage   = pMsg;
   pDict[ iPivot ].uiAt       = uiAt;
}

static void hb_clsDelMethod( PCLASS pClass, int iPos, USHORT uiAt )
{
   if( iPos >= 0 && iPos < pClass->uiMethods )
   {
      PMETHDYN pDict = pClass->pMethDyn;
      UINT     i;

      if( iPos < pClass->uiMethods - 1 )
         memmove( pDict + iPos, pDict + iPos + 1, ( pClass->uiMethods - iPos - 1 ) * sizeof( METHDYN ) );

      pClass->pMethDyn = ( PMETHDYN ) hb_xrealloc( pClass->pMethDyn, ( pClass->uiMethods - 1 ) * sizeof( METHDYN ) );

      for( pDict = pClass->pMethDyn, i = pClass->uiMethods; --i; pDict++ )
      {
         if( pDict->uiAt > uiAt )
            pDict->uiAt--;
      }
   }
}

static void hb_clsMakeSuperObject( PHB_ITEM pDest, PHB_ITEM pObject, USHORT uiSuperClass )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_clsMakeSuperObject(%p, %p, %hu)", pDest, pObject, uiSuperClass ) );

   /* create a fake object array */
   hb_arrayNew( pDest, 1 );
   /* Now save the Self object as the 1st elem. */
   hb_arraySet( pDest, 1, pObject );
   /* And transform it into a fake object */
   /* backup of actual handel */
   pDest->item.asArray.value->uiPrevCls   = hb_objClassH( pObject );
   /* superclass handel casting */
   pDest->item.asArray.value->uiClass     = uiSuperClass;
}

/*
 * <pFunc> = hb_objGetMethod( <pObject>, <pMessage> )
 *
 * Internal function to the function pointer of a message of an object
 */
PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   BOOL     bSymbol;
   PHB_FUNC pFunc = hb_objGetMthd( ( PHB_ITEM ) pObject, ( PHB_SYMB ) pMessage, TRUE, NULL, FALSE, &bSymbol );

   if( bSymbol )
   {
      PHB_SYMB pSym = ( PHB_SYMB ) pFunc;
      pFunc = pSym->value.pFunPtr;

      if( pSym->scope.value & HB_FS_CLSERROR )
         hb_xfree( ( void * ) pSym );
   }

   return pFunc;
}

PHB_FUNC hb_objGetMthd( PHB_ITEM pObject, PHB_SYMB pMessage, BOOL lAllowErrFunc, BOOL * bConstructor, int iOptimizedSend, BOOL * bSymbol )
{
   USHORT   uiClass = hb_objClassH( pObject );
   PHB_DYNS pMsg    = pMessage->pDynSym;
   PHB_FUNC pFunction;
   PMETHOD  pMethod;

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetMthd(%p, '%s', %i)", pObject, pMsg->pSymbol->szName, lAllowErrFunc, bConstructor ) );

   /* TraceLog( NULL, "Type: %i Class: %s Message: %s -> hb_objGetMthd(%p, '%s', %i)\n", pObject->type, hb_objGetClsName( pObject ), pMessage->szName, pObject, pMsg->pSymbol->szName, lAllowErrFunc, bConstructor );
    */

   if( uiClass )
   {
      pMethod = hb_objGetpMthd( pMsg, uiClass );

      if( pMethod )
      {
         pFunction   = pMethod->pFunction;
         *bSymbol    = ( pMethod->uiScope & HB_OO_CLSTP_SYMBOL ) > 0;

         if( HB_IS_OBJECTDESTROYED( pObject ) || ! hb_clsValidScope( pObject, pMethod, iOptimizedSend ) )
         {
            /* Force NO execution incase error was bypassed.
             */
            pFunction   = hb___msgVirtual;
            *bSymbol    = FALSE;
         }

#if 0
         else if( *bSymbol )
         {
            /* TraceLog( NULL, "FOUND Method: [%s]%s:%s Defined in: %s\n", hb_objGetRealClsName( pObject, pMessage->szName ), s_pClasses[ uiClass - 1 ].szName, pMessage->szName, pMethod->pModuleSymbols ? pMethod->pModuleSymbols->szModuleName : "" );
             */
         }
#endif
         ( HB_VM_STACK.pMethod ) = pMethod;

#ifndef HB_NO_PROFILER
         if( hb_bProfiler )
            pMethod->ulCalls++;    /* Profiler */
#endif

         if( bConstructor )
            *bConstructor = ( pMethod->uiScope & HB_OO_CLSTP_CTOR );

         return pFunction;
      }
   }

   ( HB_VM_STACK.pMethod ) = NULL;

   if( bConstructor )
      *bConstructor = FALSE;

   /* Default message here */
   if( s_msgClassName == NULL )
   {
      s_msgClassName    = hb_dynsymGet( "CLASSNAME" );   /* Standard messages        */
      s_msgClassH       = hb_dynsymGet( "CLASSH" );      /* Not present in classdef. */
      s_msgClassSel     = hb_dynsymGet( "CLASSSEL" );
      s_msgClassFullSel = hb_dynsymGet( "CLASSFULLSEL" );
      s_msgEval         = hb_dynsymGet( "EVAL" );
      s_msgClsParent    = hb_dynsymGet( "ISDERIVEDFROM" );
      /*s_msgClass      = hb_dynsymGet( "CLASS" );*/
   }

   *bSymbol = FALSE;

   if( pMsg == s_msgClassName )
      return hb___msgClsName;
   else if( pMsg == s_msgClassH )
      return hb___msgClsH;
   else if( pMsg == s_msgClassSel )
      return hb___msgClsSel;
   else if( pMsg == s_msgClassFullSel )
      return hb___msgClsFullSel;
   /* Eval message for blocks is handled at VM level.
        else if( pMsg == s_msgEval )
        {
            return hb___msgEval;
        }
    */
   else if( pMsg == s_msgClsParent )
      return hb___msgClsParent;
   /* else if( pMsg == s_msgClass )
      {
       return hb___msgClass;
      }
    */

   if( lAllowErrFunc && uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      if( pClass->pFunError )
      {
         *bSymbol = ( pClass->uiScope & HB_OO_CLS_ONERROR_SYMB ? TRUE : FALSE );

         if( *bSymbol )
         {
            PHB_SYMB pSymCloned = ( PHB_SYMB ) hb_xgrab( sizeof( HB_SYMB ) );

            HB_MEMCPY( pSymCloned, ( void * ) pClass->pFunError, sizeof( HB_SYMB ) );

            pSymCloned->szName      = pMessage->szName;
            pSymCloned->scope.value |= HB_FS_CLSERROR;

            return ( PHB_FUNC ) pSymCloned;
            /* ( (PHB_SYMB) pClass->pFunError)->szName = pMessage->szName;
            */
         }

         return pClass->pFunError;
      }
   }

   return NULL;
}

BOOL hb_clsHasMsg( USHORT uiClass, const char * szMsg )
{
   PHB_DYNS pMsg = hb_dynsymFindName( szMsg );

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsHasMsg(%i, %s)", uiClass, szMsg ) );

   if( pMsg && uiClass && uiClass <= s_uiClasses )
   {
      PMETHOD pMethod = hb_objGetpMthd( pMsg, uiClass );

      if( pMethod && pMethod->pMessage == pMsg )
      {
         if( pMethod->uiScope & HB_OO_CLSTP_SUPER )
         {
            if( pMethod->uiScope & HB_OO_CLSTP_HIDDEN )
               return FALSE;

            if( pMethod->uiScope & HB_OO_CLSTP_READONLY )
            {
               if( pMethod->uiScope && HB_OO_CLSTP_PROTECTED )
                  return FALSE;
            }
         }

         /* printf( "EXISTs Message: %s in Class %s\n", szMsg, pClass->szName );
          */
         return TRUE;
      }
   }

   return FALSE;
}

PMETHOD hb_objGetpMthd( PHB_DYNS pMsg, USHORT uiClass )
{
   PCLASS   pClass = s_pClasses + ( uiClass - 1 );
   USHORT   uiAt   = hb_clsFindMethod( pMsg, pClass, NULL );

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetpMthd(%p, %u)", pMsg, uiClass ) );

   return uiAt ? pClass->pMethods + uiAt - 1 : NULL;
}

PMETHOD hb_objGetpMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   USHORT uiClass = hb_objClassH( pObject );

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetpMethod(%p, %p)", pObject, pMessage ) );

   return uiClass ? hb_objGetpMthd( pMessage->pDynSym, uiClass ) : NULL;
}

/*
 * <uPtr> = hb_objHasMsg( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
PHB_FUNC hb_objHasMsg( PHB_ITEM pObject, const char * szString )
{
   return hb_objGetMessage( pObject, szString, NULL );
}

BOOL hb_objHasMessage( PHB_ITEM pObject, PHB_DYNS pMessage )
{
   BOOL     bSymbol  = FALSE;
   PHB_FUNC pFunc    = hb_objGetMthd( pObject, pMessage->pSymbol, FALSE, NULL, FALSE, &bSymbol );

   /* Without hb_xfree() because lAllowErrFunc == FALSE
    */
   return bSymbol ? ( ( PHB_SYMB ) pFunc )->value.pFunPtr != NULL : pFunc != NULL;
}

static PHB_FUNC hb_objGetMessage( PHB_ITEM pObject, const char * szString, PHB_DYNS * ppDynSym )
{
   PHB_DYNS pDynSym = hb_dynsymFindName( szString );

   HB_TRACE( HB_TR_DEBUG, ( "hb_objGetMessage(%p, %s, %p)", pObject, szString, ppDynSym ) );

   if( ppDynSym )
      *ppDynSym = pDynSym;

   if( pDynSym )
   {
      BOOL     bSymbol;
      PHB_FUNC pFunc = hb_objGetMthd( pObject, pDynSym->pSymbol, FALSE, NULL, FALSE, &bSymbol );

      if( bSymbol )
         /* Without hb_xfree() because lAllowErrFunc == FALSE
          */
         return ( ( PHB_SYMB ) pFunc )->value.pFunPtr;

      return pFunc;
   }

   return NULL;
}

/* Worker function for HB_FUNC( __CLSADDMSG ).
 */
static BOOL hb_clsAddMsg( USHORT uiClass, const char * szMessage,
                          void * pFunc_or_BlockPointer, USHORT uiID, USHORT wType, USHORT uiSprClass, USHORT uiScope, BOOL bPersistent, PHB_ITEM pMsgIs, PHB_ITEM pInit, BOOL bCheckPrefix, BOOL bCase )
{
   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      PHB_DYNS pMessage = NULL;
      USHORT   uiAt;
      PMETHOD  pNewMeth;
      ULONG    fOpOver  = 0;
      int      iPos;

      switch( strlen( szMessage ) )
      {
         case 1:
            if( strcmp( "+", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpPlus" );
               fOpOver  = HB_CLASS_OP_PLUS;
            }
            else if( strcmp( "-", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpMinus" );
               fOpOver  = HB_CLASS_OP_MINUS;
            }
            else if( strcmp( "*", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpMult" );
               fOpOver  = HB_CLASS_OP_MULT;
            }
            else if( strcmp( "/", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpDivide" );
               fOpOver  = HB_CLASS_OP_DIVIDE;
            }
            else if( strcmp( "%", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpMod" );
               fOpOver  = HB_CLASS_OP_MOD;
            }
            else if( strcmp( "^", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpPower" );
               fOpOver  = HB_CLASS_OP_POWER;
            }
            else if( strcmp( "=", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpEqual" );
               fOpOver  = HB_CLASS_OP_EQUAL;
            }
            else if( strcmp( "#", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpNotEqual" );
               fOpOver  = HB_CLASS_OP_NOTEQUAL;
            }
            else if( strcmp( "<", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpLess" );
               fOpOver  = HB_CLASS_OP_LESS;
            }
            else if( strcmp( ">", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpGreater" );
               fOpOver  = HB_CLASS_OP_GREATER;
            }
            else if( strcmp( "$", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpInstring" );
               fOpOver  = HB_CLASS_OP_INSTRING;
            }
            else if( strcmp( "!", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpNot" );
               fOpOver  = HB_CLASS_OP_NOT;
            }
            else if( strcmp( "&", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpBitAnd" );
               fOpOver  = HB_CLASS_OP_BITAND;
            }
            else if( strcmp( "|", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpBitOr" );
               fOpOver  = HB_CLASS_OP_BITOR;
            }
            break;

         case 2:
            if( strcmp( "**", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpPower" );
               fOpOver  = HB_CLASS_OP_POWER;
            }
            else if( strcmp( "++", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpInc" );
               fOpOver  = HB_CLASS_OP_INC;
            }
            else if( strcmp( "--", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpDec" );
               fOpOver  = HB_CLASS_OP_DEC;
            }
            else if( strcmp( "==", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpExactEqual" );
               fOpOver  = HB_CLASS_OP_EXACTEQUAL;
            }
            else if( strcmp( "!=", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpNotEqual" );
               fOpOver  = HB_CLASS_OP_NOTEQUAL;
            }
            else if( strcmp( "<>", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpNotEqual" );
               fOpOver  = HB_CLASS_OP_NOTEQUAL;
            }
            else if( strcmp( "<=", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpLessEqual" );
               fOpOver  = HB_CLASS_OP_LESSEQUAL;
            }
            else if( strcmp( ">=", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpGreaterEqual" );
               fOpOver  = HB_CLASS_OP_GREATEREQUAL;
            }
            else if( strcmp( ":=", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpAssign" );
               fOpOver  = HB_CLASS_OP_ASSIGN;
            }
            else if( strcmp( "[]", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpArrayIndex" );
               fOpOver  = HB_CLASS_OP_ARRAYINDEX;
            }
            else if( strcmp( "^^", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpBitXor" );
               fOpOver  = HB_CLASS_OP_BITXOR;
            }
            else if( strcmp( ">>", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpBitShiftR" );
               fOpOver  = HB_CLASS_OP_BITSHIFTR;
            }
            else if( strcmp( "<<", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpBitShiftL" );
               fOpOver  = HB_CLASS_OP_BITSHIFTL;
            }
            else if( strcmp( "$$", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpContains" );
               fOpOver  = HB_CLASS_OP_CONTAINS;
            }
            break;

         case 3:
            break;

         case 4:
            if( strcmp( ".OR.", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpOr" );
               fOpOver  = HB_CLASS_OP_OR;
            }
            break;

         case 5:
            if( strcmp( ".NOT.", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpNot" );
               fOpOver  = HB_CLASS_OP_NOT;
            }
            else if( strcmp( ".AND.", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpAnd" );
               fOpOver  = HB_CLASS_OP_AND;
            }
            break;

         case 8:
            if( strcmp( "FOR EACH", szMessage ) == 0 )
            {
               pMessage = hb_dynsymGet( "__OpForEach" );
               fOpOver  = HB_CLASS_OP_FOREACH;
            }
            break;
      }

      if( ! fOpOver )
         pMessage = bCase ? hb_dynsymGetCase( szMessage ) : hb_dynsymGet( szMessage );

      if( wType == ( USHORT ) HB_OO_MSG_INLINE && pFunc_or_BlockPointer == NULL )
         hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSADDMSG", 0 );

      uiAt = hb_clsFindMethod( pMessage, pClass, &iPos );

      if( uiAt )
         pNewMeth = pClass->pMethods + uiAt - 1;
      else
      {
         uiAt = ++pClass->uiMethods;  /* One more message */

         if( pClass->uiReserved < pClass->uiMethods )
         {
            pClass->pMethods  = ( PMETHOD ) hb_xrealloc( pClass->pMethods, uiAt * sizeof( METHOD ) );
            pNewMeth          = pClass->pMethods + uiAt - 1;
            memset( pNewMeth, 0, sizeof( METHOD ) );
            pClass->pMethDyn  = ( PMETHDYN ) hb_xrealloc( pClass->pMethDyn, uiAt * sizeof( METHDYN ) );
         }
         else
            pNewMeth = pClass->pMethods + uiAt - 1;

         pNewMeth->pMessage = pMessage;
         hb_clsSaveMethod( pMessage, iPos, pClass, uiAt );
      }

      pClass->fOpOver               |= fOpOver;
      pNewMeth->uiSprClass          = uiClass;  /* now used !! */
      pNewMeth->bClsDataInitiated   = 0;        /* reset state */
#ifndef HB_NO_PROFILER
      pNewMeth->ulCalls             = 0;
      pNewMeth->ulTime              = 0;
      pNewMeth->ulRecurse           = 0;
#endif
      pNewMeth->bIsPersistent       = bPersistent;
      pNewMeth->uiType              = wType;

      /* in case of re-used message */
      if( pNewMeth->pInitValue )
      {
         if( pNewMeth->pFunction == hb___msgSetData || pNewMeth->pFunction == hb___msgGetData )
         {
            UINT      uiLen  = pClass->uiDataInitiated + 1;
            PCLSDINIT pInit  = pClass->pInitValues;
            PHB_ITEM  pValue = pNewMeth->pInitValue;

            for(; --uiLen; pInit++ )
            {
               if( pInit->pInitValue == pValue )
               {
                  if( uiLen > 1 )
                     memmove( pInit, pInit + 1, ( uiLen - 1 ) * sizeof( CLSDINIT ) );

                  /* without realloc, only free if uiDataInitiated is 0 */
                  if( --pClass->uiDataInitiated == 0 )
                  {
                     hb_xfree( pClass->pInitValues );
                     pClass->pInitValues = NULL;
                  }
                  break;
               }
            }
         }

         hb_itemRelease( pNewMeth->pInitValue );
         pNewMeth->pInitValue = NULL;
      }

      switch( wType )
      {
         case HB_OO_MSG_METHOD:
            pNewMeth->pFunction  = ( PHB_FUNC ) pFunc_or_BlockPointer;
            /* pNewMeth->uiScope = uiScope | HB_OO_CLSTP_SYMBOL;
             */
            pNewMeth->uiData     = 0;

            if( uiScope & HB_OO_CLSTP_PFUNC )
               pNewMeth->uiScope &= ~( ( USHORT ) HB_OO_CLSTP_PFUNC );
            else
               pNewMeth->uiScope = uiScope | HB_OO_CLSTP_SYMBOL;

            pClass->uiScope |= ( uiScope & HB_OO_CLSTP_CLASSCTOR );
            break;

         case HB_OO_MSG_DATA:
            pNewMeth->uiData  = uiID;
            pNewMeth->uiScope = uiScope;
            pNewMeth->uiScope &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );

            if( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' )
               pNewMeth->pFunction = hb___msgSetData;
            else
            {
               pNewMeth->pFunction = hb___msgGetData;

               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  if( HB_IS_ARRAY( pInit ) )
                  {
                     if( pInit->item.asArray.value->uiClass )
                     {
                        PCLASS pClass = s_pClasses + pInit->item.asArray.value->uiClass - 1;

                        if( pClass->pDestructor && pClass->uiScope & HB_OO_CLS_DESTRUC_SYMB )
                        {
                           hb_errRT_BASE( EG_ARG, 3009, "Init value can not contain an object with a destructor", szMessage, 1, pInit );
                           return FALSE;
                        }

                        pNewMeth->pInitValue = hb_objClone( pInit );
                     }
                     else
                        pNewMeth->pInitValue = hb_arrayClone( pInit, NULL );
                  }
                  else if( HB_IS_HASH( pInit ) )
                     pNewMeth->pInitValue = hb_hashClone( pInit, NULL );
                  else
                     pNewMeth->pInitValue = hb_itemNew( pInit );

                  if( ! pClass->pInitValues )
                  {
                     pClass->pInitValues     = ( PCLSDINIT ) hb_xgrab( sizeof( CLSDINIT ) );
                     pClass->uiDataInitiated = 1;
                     pClass->uiScope         |= HB_OO_CLS_REALLOCINIT;
                  }
                  else
                  {
                     pClass->uiDataInitiated++;

                     if( pClass->uiScope & ( HB_OO_CLS_INSTANCED | HB_OO_CLS_REALLOCINIT ) )
                        pClass->pInitValues = ( PCLSDINIT ) hb_xrealloc( pClass->pInitValues, pClass->uiDataInitiated * sizeof( CLSDINIT ) );
                     /* else wihtout realloc. Allocated in __clsnew or __incdata */
                  }

                  pClass->pInitValues[ pClass->uiDataInitiated - 1 ].uiData      = uiID;
                  pClass->pInitValues[ pClass->uiDataInitiated - 1 ].uiAt        = uiAt;
                  pClass->pInitValues[ pClass->uiDataInitiated - 1 ].pInitValue  = pNewMeth->pInitValue;
               }
            }

            break;

         case HB_OO_MSG_CLASSDATA:
            pNewMeth->uiData        = uiID;
            pNewMeth->uiDataShared  = pNewMeth->uiData;
            pNewMeth->uiScope       = uiScope;
            pNewMeth->uiScope       &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );

            if( ( USHORT ) pClass->pClassDatas->item.asArray.value->ulLen < pNewMeth->uiData )
               hb_arraySize( pClass->pClassDatas, pNewMeth->uiData );

            if( ! ( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' ) )
            {
               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  if( HB_IS_ARRAY( pInit ) )
                  {
                     if( pInit->item.asArray.value->uiClass )
                     {
                        PCLASS pClass = s_pClasses + pInit->item.asArray.value->uiClass - 1;

                        if( pClass->pDestructor && pClass->uiScope & HB_OO_CLS_DESTRUC_SYMB )
                        {
                           hb_errRT_BASE( EG_ARG, 3009, "Class Var's init value can not contain an object with a destructor", szMessage, 1, pInit );
                           return FALSE;
                        }

                        pNewMeth->pInitValue = hb_objClone( pInit );
                     }
                     else
                        pNewMeth->pInitValue = hb_arrayClone( pInit, NULL );
                  }
                  else if( HB_IS_HASH( pInit ) )
                     pNewMeth->pInitValue = hb_hashClone( pInit, NULL );
                  else
                     pNewMeth->pInitValue = hb_itemNew( pInit );

                  if( pClass->uiScope & HB_OO_CLS_INSTANCED )
                  {
                     PHB_ITEM pData;

                     if( HB_IS_ARRAY( pInit ) )
                        pData = hb_arrayClone( pInit, NULL );
                     else if( HB_IS_HASH( pInit ) )
                        pData = hb_hashClone( pInit, NULL );
                     else
                        pData = hb_itemNew( pInit );

                     hb_arraySetForward( pClass->pClassDatas, pNewMeth->uiData, pData );
                     hb_itemRelease( pData );
                  }
               }
            }

            if( ( pNewMeth->uiScope & HB_OO_CLSTP_SHARED ) != HB_OO_CLSTP_SHARED )
            {
               if( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' )
               {
                  pNewMeth->pFunction = hb___msgSetClsData;
                  pClass->uiDatasShared++;
               }
               else
                  pNewMeth->pFunction = hb___msgGetClsData;
            }
            else
            {
               if( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' )
               {
                  pNewMeth->pFunction = hb___msgSetShrData;
                  pClass->uiDatasShared++;
               }
               else
                  pNewMeth->pFunction = hb___msgGetShrData;
            }

            break;

         case HB_OO_MSG_INLINE:
            pNewMeth->uiData     = ( USHORT ) pClass->pInlines->item.asArray.value->ulLen + 1;
            pNewMeth->uiScope    = uiScope;
            pNewMeth->uiScope    &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );

            ( ( PHB_ITEM ) pFunc_or_BlockPointer )->item.asBlock.value->uiClass  = uiClass;

            hb_arraySize( pClass->pInlines, pNewMeth->uiData );
            hb_arraySet( pClass->pInlines, pNewMeth->uiData, ( PHB_ITEM ) pFunc_or_BlockPointer );
            pNewMeth->pFunction  = hb___msgEvalInline;
            pClass->uiScope      |= ( uiScope & HB_OO_CLSTP_CLASSCTOR );
            break;

         case HB_OO_MSG_VIRTUAL:
            pNewMeth->uiScope    &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );
            pNewMeth->pFunction  = hb___msgVirtual;
            break;

         case HB_OO_MSG_SUPER:
            pNewMeth->uiData     = uiID;
            pNewMeth->uiSprClass = ( USHORT ) uiSprClass; /* store the super handle */
            pNewMeth->uiScope    = uiScope;
            pNewMeth->uiScope    &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );
            pNewMeth->pFunction  = hb___msgSuper;
            break;

         case HB_OO_MSG_ONERROR:
            pNewMeth->pFunction  = ( PHB_FUNC ) pFunc_or_BlockPointer;
            pNewMeth->uiScope    |= HB_OO_CLSTP_SYMBOL;

            pClass->pFunError    = ( PHB_FUNC ) pFunc_or_BlockPointer;
            pClass->uiScope      |= HB_OO_CLS_ONERROR_SYMB;
            break;

         case HB_OO_MSG_DESTRUCTOR:
            pNewMeth->pFunction  = ( PHB_FUNC ) pFunc_or_BlockPointer;
            pNewMeth->uiScope    |= HB_OO_CLSTP_SYMBOL;

            pClass->pDestructor  = pNewMeth;
            pClass->uiScope      |= HB_OO_CLS_DESTRUC_SYMB;
            break;

         case HB_OO_MSG_DELEGATE:
            if( pMsgIs && HB_IS_STRING( pMsgIs ) )
               pNewMeth->pMsgIs = hb_dynsymGet( pMsgIs->item.asString.value )->pSymbol;
            else
               pNewMeth->pMsgIs = NULL;

            if( pInit && HB_IS_STRING( pInit ) )
               pNewMeth->pMsgTo = hb_dynsymGet( pInit->item.asString.value )->pSymbol;
            else
               pNewMeth->pMsgTo = NULL;

            if( pInit && HB_IS_OBJECT( pInit ) )
               pNewMeth->pInitValue = hb_itemNew( pInit );

            pNewMeth->uiScope    = uiScope;
            pNewMeth->uiScope    &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );
            pNewMeth->pFunction  = hb___msgDelegate;
            pClass->uiScope      |= ( uiScope & HB_OO_CLSTP_CLASSCTOR );
            break;

         default:
            hb_errInternal( HB_EI_CLSINVMETHOD, NULL, "__clsAddMsg", NULL );
      }

      /*
         Clone the execution symbol, into a new symbol with the correct Message Name.
         This way we'll have the correct execution context as well as correct symbolic name.
       */
      if( pNewMeth->uiScope & HB_OO_CLSTP_SYMBOL )
      {
         if( wType != HB_OO_MSG_ONERROR )
         {
            PHB_SYMB       pFunc = ( PHB_SYMB ) pFunc_or_BlockPointer;
            PHB_SYMB       pMsg  = hb_symbolNew( pMessage->pSymbol->szName );
            const char *   szMsg = pMsg->szName;

            HB_MEMCPY( pMsg, pFunc, sizeof( HB_SYMB ) );

            pMsg->szName         = szMsg;
            pNewMeth->pFunction  = ( PHB_FUNC ) pMsg;
         }
      }

#ifdef HB_THREAD_SUPPORT
      if( ( uiScope & HB_OO_CLSTP_SYNC ) && ( wType == HB_OO_MSG_METHOD || wType == HB_OO_MSG_INLINE || wType == HB_OO_MSG_DELEGATE ) && pClass->pMtxSync == NULL )
         /* Create mutex.
          */
         pClass->pMtxSync = hb_threadMutexCreate( NULL );
#endif
   }
   return TRUE;
}

/*
 *               1                  2            3                     4         5                  6           7                8
 * __clsAddMsg( <hClass/pObject>,  <cMessage>, <Func_or_Block_or_ID>, <nType>, [<Super_or_Init>], [<nScope>], [<lPersistent>], [<lCase> ] )
 *
 * Add a message to the class.
 *
 * <hClass>    Class handle
 * <cMessage>  Message
 * <pFunction> HB_OO_MSG_METHOD    : Pointer to function
 *             HB_OO_MSG_DATA      : Index number in array
 *             HB_OO_MSG_CLASSDATA : Index number in array
 *             HB_OO_MSG_INLINE    : Code block
 *             HB_OO_MSG_SUPER     : Handle of super class
 *             HB_OO_MSG_DELEGATE  : Handle of delegate class
 *
 * <nType>     see HB_OO_MSG_*
 *
 * <xInit>     HB_OO_MSG_DATA      : Optional initializer for DATA
 *             HB_OO_MSG_CLASSDATA : Optional initializer for DATA
 *             HB_OO_MSG_SUPER     : Index number in array (for instance SuperObject)
 *             HB_OO_MSG_DELEGATE  : Name of method in delegate class
 *
 * <uiScope>   HB_OO_CLSTP_EXPORTED        1 : default for data and method
 *             HB_OO_CLSTP_PUBLISHED       2 : method or data published
 *             HB_OO_CLSTP_PROTECTED       4 : method or data protected
 *             HB_OO_CLSTP_HIDDEN          8 : method or data hidden
 *             HB_OO_CLSTP_CTOR           16 : method constructor
 *             HB_OO_CLSTP_READONLY       32 : data read only
 *             HB_OO_CLSTP_SHARED         64 : (method or) data shared
 *             HB_OO_CLSTP_CLASS         128 : message is the name of a superclass
 *             HB_OO_CLSTP_SUPER         256 : message is herited
 *             HB_OO_CLSTP_CLASSCTOR     512 : Class method constructor
 *             HB_OO_CLSTP_SYNC         1024 : Sync method
 *             HB_OO_CLSTP_CLASSMETH    2048 : Class method
 */
HB_FUNC( __CLSADDMSG )
{
   USHORT       uiClass, uiScope, wType, uiSprClass = 0, uiID;
   const char * szMessage;
   char         szAssign[ HB_SYMBOL_NAME_LEN + 1 ];
   void *       pFunc_or_BlockPointer;
   PHB_ITEM     pInit    = NULL;
   PHB_ITEM     pMsgIs   = NULL;
   BOOL         bPersistent, bCase;

   /* 1 */
   uiClass = ISARRAY( 1 ) ? hb_param( 1, HB_IT_ARRAY )->item.asArray.value->uiClass : ( USHORT ) hb_parni( 1 );

   /* 2 */
   szMessage               = hb_parcx( 2 );

   /* 3 */
   pFunc_or_BlockPointer   = ( void * ) hb_param( 3, HB_IT_BLOCK );
   if( pFunc_or_BlockPointer == NULL )
   {
      pFunc_or_BlockPointer = hb_parptr( 3 );
      uiID = ( pFunc_or_BlockPointer != NULL ) ? ( ( PHB_SYMB ) pFunc_or_BlockPointer )->scope.value : ( USHORT ) hb_parni( 3 );
   }
   else
      /* 3 Alternate */
      uiID = ( USHORT ) hb_parni( 3 );

   /* 4 */
   wType = ( USHORT ) hb_parni( 4 );

   /* 5 Follows below because might be using 6, 7 and 8.
    */

   /* 6 */
   uiScope     = ( USHORT ) ( ISNUM( 6 ) ? hb_parni( 6 ) : HB_OO_CLSTP_EXPORTED );

   /* 7 */
   bPersistent = hb_parl( 7 );

   /* 8 */
   bCase       = hb_parl( 8 );

   /* 5 */
   switch( wType )
   {
      case HB_OO_MSG_DATA:
      case HB_OO_MSG_CLASSDATA:
         pInit = hb_param( 5, HB_IT_ANY );
         break;

      case HB_OO_MSG_PROPERTY:
      case HB_OO_MSG_CLASSPROPERTY:
         wType -= HB_OO_PROPERTY;

         hb_clsAddMsg( uiClass, szMessage, NULL, uiID, wType, 0, uiScope, bPersistent, NULL, hb_param( 5, HB_IT_ANY ), FALSE, bCase );

         /* Remove HB_OO_CLSTP_PUBLISHED flag if present.
          */
         uiScope        &= ~HB_OO_CLSTP_PUBLISHED;

         szAssign[ 0 ]  = '_';
         szAssign[ 1 ]  = '\0';
         hb_xstrcat( ( char * ) szAssign, szMessage, 0 );
         szMessage      = ( char * ) szAssign;
         bPersistent    = FALSE;
         break;

      case HB_OO_MSG_DELEGATE:
         pMsgIs   = hb_param( 3, HB_IT_STRING );
         pInit    = hb_param( 5, HB_IT_STRING | HB_IT_OBJECT );
         break;

      default:
         uiSprClass = ( USHORT ) hb_parnl( 5 ); /* store the super handle */
   }

   /* Call worker function.
    */
   hb_clsAddMsg( uiClass, szMessage, pFunc_or_BlockPointer, uiID, wType, uiSprClass, uiScope, bPersistent, pMsgIs, pInit, TRUE, bCase );
}

USHORT __cls_CntMethods( USHORT uiClass, PHB_FUNC pFunction )
{
   PCLASS   pClass      = s_pClasses + ( uiClass - 1 );
   USHORT   uiMethods   = pClass->uiMethods + 1, uiRet = 0;
   PMETHOD  pMethod     = pClass->pMethods;

   for(; --uiMethods; pMethod++ )
   {
      if( pMethod->pFunction == pFunction )
         uiRet++;
   }

   return uiRet;
}

/*
 * Create a new class
 *
 * <szClassName>     Name of the class
 * <uiDatas>         Number of DATAs in the class
 * <uiMethods>       Number of additional Methods in the class
 * <pSuperArray>     Optional array with handle(s) of superclass(es)
 * <pClassFunc>      Class function symbol, when NULL public function
 *                   with the same name as szClassName is used
 * <fModuleFriendly> when true all functions and classes from the same
 *                   module as pClassFunc are defined as friends
 */
static USHORT hb_clsNew( const char * szClassName, USHORT uiDatas,
                         USHORT uiMethods,
                         PHB_ITEM pSuperArray, PHB_SYMB pClassFunc,
                         BOOL fModuleFriendly )
{
   PCLASS  pNewCls;
   PMETHOD pMethod;
   USHORT  i, uiSuperCls;
   USHORT  uiClass, j, uiAt;
   USHORT  uiKnownMethods = uiDatas * 2 + uiMethods + 1; /* add 1 for self class casting */
   int     iPos;
   USHORT  uiSuper = ( USHORT ) ( pSuperArray ? hb_arrayLen( pSuperArray ) : 0 );

   pClassFunc  = hb_vmGetRealFuncSym( pClassFunc );

   HB_CLASS_LOCK

   if( s_uiClasses == s_uiClsSize )
   {
      s_uiClsSize += HB_CLASS_POOL_RESIZE;
      s_pClasses  = ( PCLASS ) hb_xrealloc( s_pClasses, sizeof( CLASS ) * ( ( ULONG ) s_uiClsSize + 1 ) );
   }
   uiClass  = ++s_uiClasses;
   pNewCls  = s_pClasses + uiClass - 1;
   memset( pNewCls, 0, sizeof( CLASS ) );

   HB_CLASS_UNLOCK

   pNewCls->szName    = hb_strdup( szClassName );
   pNewCls->pClassSym = hb_dynsymGet( pNewCls->szName );

   if( ! pClassFunc )
      pClassFunc = hb_vmGetRealFuncSym( pNewCls->pClassSym->pSymbol );

   pNewCls->pClassFuncSym  = pClassFunc;

   if( fModuleFriendly )
      hb_vmFindModuleSymbols( pClassFunc, &pNewCls->pFriendModule,
                              &pNewCls->uiFriendModule );

   pNewCls->bActive  = TRUE;
   pNewCls->uiDatas  = uiDatas;

   if( uiSuper )
   {
      ULONG    ulSize   = 0; /* USHORT is small. Maximum 409 methods. In some
                                cases it is enough. This eliminate random GPFs
                                in this function for big classes */
      ULONG    nLenClsDatas;
      ULONG    nLenInlines;
      PCLASS   pSprCls  = NULL; /* To void incorrect warning; */
      PMETHOD  pSprMethod;
      USHORT   uiInit   = 0;

      for( i = 1; i <= uiSuper; i++ )
      {
         uiSuperCls = ( USHORT ) hb_arrayGetNI( pSuperArray, i );
         if( uiSuperCls && uiSuperCls < s_uiClasses )
         {
            pSprCls           = s_pClasses + uiSuperCls - 1;
            ulSize            += ( ULONG ) pSprCls->uiMethods;
            pNewCls->uiDatas  += pSprCls->uiDatas;
         }
      }
      if( pNewCls->uiDatas )
      {
         pNewCls->pInitValues = ( PCLSDINIT ) hb_xgrab( sizeof( CLSDINIT ) * pNewCls->uiDatas );
         memset( pNewCls->pInitValues, 0, sizeof( CLSDINIT ) * pNewCls->uiDatas );
      }

      pNewCls->uiScope     |= pSprCls->uiScope & ~( ( USHORT ) HB_OO_CLS_INSTANCED );
      ulSize               += uiKnownMethods;
      pNewCls->uiReserved  = ( USHORT ) ulSize;
      pNewCls->pMethDyn    = ( PMETHDYN ) hb_xgrab( ulSize * sizeof( METHDYN ) );
      ulSize               *= sizeof( METHOD );
      pNewCls->pMethods    = ( PMETHOD ) hb_xgrab( ulSize );
      memset( pNewCls->pMethods, 0, ulSize );

      nLenClsDatas         = 0;
      nLenInlines          = 0;
      ulSize               = 0;

      for( i = 1; i <= uiSuper; i++ )
      {
         uiSuperCls = ( USHORT ) hb_arrayGetNI( pSuperArray, i );
         if( uiSuperCls && uiSuperCls < s_uiClasses )
         {
            PCLASS pSprCls;

            pSprCls = s_pClasses + uiSuperCls - 1;
            if( i == 1 )
            {
               if( pSprCls->pFunError )
               {
                  pNewCls->pFunError   = pSprCls->pFunError;
                  pNewCls->uiScope     |= HB_OO_CLS_ONERROR_SUPER;
               }

               pNewCls->pDestructor    = pSprCls->pDestructor;

               /* CLASS DATA Not Shared ( new array, new value ) */
               pNewCls->pClassDatas    = hb_arrayClone( pSprCls->pClassDatas, NULL );
               pNewCls->pInlines       = hb_arrayClone( pSprCls->pInlines, NULL );
               pNewCls->uiDatasShared  = pSprCls->uiDatasShared;
               HB_MEMCPY( pNewCls->pMethDyn, pSprCls->pMethDyn, pSprCls->uiMethods * sizeof( METHDYN ) );
            }
            else if( ! hb_clsHasParent( pNewCls, pSprCls->pClassSym ) )
            {
               USHORT   uiNew, uiAdd = 0;
               USHORT   nLen, ui;
               PHB_ITEM pClsAnyTmp;

               /* Ok add now the previous len to the offset */
               nLenClsDatas   = ( USHORT ) hb_itemSize( pNewCls->pClassDatas );
               nLenInlines    = ( USHORT ) hb_itemSize( pNewCls->pInlines );

               /* ClassDatas */
               pClsAnyTmp     = hb_arrayClone( pSprCls->pClassDatas, NULL );

               nLen           = ( USHORT ) hb_itemSize( pClsAnyTmp );
               ui             = ( USHORT ) hb_itemSize( pNewCls->pClassDatas );
               nLen           += ui;
               hb_arraySize( pNewCls->pClassDatas, nLen );

               for( uiNew = ui + 1; uiNew <= nLen; uiNew++ )
               {
                  hb_arrayGet( pClsAnyTmp, ++uiAdd, hb_arrayGetItemPtr( pNewCls->pClassDatas, uiNew ) );
               }

               hb_itemRelease( pClsAnyTmp );

               /* SharedDatas */
               pNewCls->uiDatasShared  += pSprCls->uiDatasShared;

               /* Inlines */
               pClsAnyTmp              = hb_arrayClone( pSprCls->pInlines, NULL );
               ui                      = ( USHORT ) pNewCls->pInlines->item.asArray.value->ulLen;
               nLen                    = ( USHORT ) ( pClsAnyTmp )->item.asArray.value->ulLen;
               nLen                    += ui;
               hb_arraySize( pNewCls->pInlines, nLen );
               uiAdd                   = 0;

               for( uiNew = ui + 1; uiNew <= nLen; uiNew++ )
               {
                  hb_arrayGet( pClsAnyTmp, ++uiAdd, hb_arrayGetItemPtr( pNewCls->pInlines, uiNew ) );
               }

               hb_itemRelease( pClsAnyTmp );
            }

            pSprMethod  = pSprCls->pMethods;
            j           = pSprCls->uiMethods + 1;

            for(; --j; pMethod++, pSprMethod++ )
            {
               uiAt = hb_clsFindMethod( pSprMethod->pMessage, pNewCls, &iPos );

               if( uiAt )
               {
                  pMethod = pNewCls->pMethods + uiAt - 1;
                  hb_xmemcpy( pMethod, pSprMethod, sizeof( METHOD ) );
               }
               else
               {
                  pNewCls->uiMethods++;
                  pMethod  = pNewCls->pMethods + ( USHORT ) ulSize;
                  hb_xmemcpy( pMethod, pSprMethod, sizeof( METHOD ) );
                  ++ulSize;
                  uiAt     = ( USHORT ) ulSize;
                  hb_clsSaveMethod( pMethod->pMessage, iPos, pNewCls, ( USHORT ) ulSize );
               }

               /* TraceLog( NULL, "NEW INHERITED Method: %s:%s from CLASS %s Module: %s Pos: %i %i\n", pNewCls->szName, pNewMethod->pMessage->pSymbol->szName, pSprCls->szName, pNewMethod->pModuleSymbols->szModuleName, iPos, ulSize );
                */

               if( pMethod->pFunction == hb___msgSetClsData ||
                   pMethod->pFunction == hb___msgGetClsData )
                  pMethod->uiData += ( USHORT ) nLenClsDatas;
               else if( pMethod->pFunction == hb___msgSetData ||
                        pMethod->pFunction == hb___msgGetData )
                  pMethod->uiData += pNewCls->uiDataFirst;
               else if( pMethod->pFunction == hb___msgEvalInline )
                  pMethod->uiData += ( USHORT ) nLenInlines;

               pMethod->uiScope |= HB_OO_CLSTP_SUPER;

               if( pMethod->pInitValue )
               {
                  if( HB_IS_ARRAY( pMethod->pInitValue ) )
                     pMethod->pInitValue = hb_arrayClone( pMethod->pInitValue, NULL );
                  else if( HB_IS_HASH( pMethod->pInitValue ) )
                     pMethod->pInitValue = hb_hashClone( pMethod->pInitValue, NULL );
                  else
                     pMethod->pInitValue = hb_itemNew( pMethod->pInitValue );

                  if( pMethod->pFunction == hb___msgSetData ||
                      pMethod->pFunction == hb___msgGetData )
                  {
                     pNewCls->pInitValues[ uiInit ].uiData     = pMethod->uiData;
                     pNewCls->pInitValues[ uiInit ].uiAt       = uiAt;
                     pNewCls->pInitValues[ uiInit ].pInitValue = pMethod->pInitValue;
                     uiInit++;
                  }
               }
            }
            pNewCls->fOpOver     |= pSprCls->fOpOver;
            pNewCls->uiDataFirst += pSprCls->uiDatas;
         }
      }
      pNewCls->uiMethods         = ( USHORT ) ulSize;
      pNewCls->uiDataInitiated   = uiInit;
      pNewCls->uiScope           &= ~( ( USHORT ) HB_OO_CLS_INSTANCED );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "Class '%s' Known: %i Datas: %i Extras %i\n", pNewCls->szName, uiKnownMethods, pNewCls->uiDatas, uiMethods ) );

      pNewCls->uiReserved  = ( USHORT ) uiKnownMethods;
      pNewCls->pMethods    = ( PMETHOD ) hb_xgrab( uiKnownMethods * sizeof( METHOD ) );
      memset( pNewCls->pMethods, 0, uiKnownMethods * sizeof( METHOD ) );
      pNewCls->pMethDyn    = ( PMETHDYN ) hb_xgrab( uiKnownMethods * sizeof( METHDYN ) );

      pNewCls->pClassDatas = hb_itemArrayNew( 0 );
      pNewCls->pInlines    = hb_itemArrayNew( 0 );

      if( pNewCls->uiDatas )
      {
         pNewCls->pInitValues = ( PCLSDINIT ) hb_xgrab( sizeof( CLSDINIT ) * pNewCls->uiDatas );
         memset( pNewCls->pInitValues, 0, sizeof( CLSDINIT ) * pNewCls->uiDatas );
      }
   }

   /* add self class casting */
   if( hb_stricmp( pNewCls->szName, pNewCls->pClassSym->pSymbol->szName ) == 0 )
   {
      uiAt = hb_clsFindMethod( pNewCls->pClassSym, pNewCls, &iPos );
      if( ! uiAt )
      {
         pMethod              = pNewCls->pMethods + pNewCls->uiMethods;
         pMethod->pMessage    = pNewCls->pClassSym;
         pMethod->uiSprClass  = uiClass;
         pMethod->uiScope     = HB_OO_CLSTP_EXPORTED;
         pMethod->pFunction   = hb___msgSuper;
         pNewCls->uiMethods++;
         hb_clsSaveMethod( pMethod->pMessage, iPos, pNewCls, pNewCls->uiMethods );
      }
   }

   HB_TRACE( HB_TR_DEBUG, ( "Finalized: '%s' Known: %i\n", pNewCls->szName, uiKnownMethods ) );

   if( strcmp( pNewCls->szName, "ARRAY" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiArrayClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "BLOCK" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiBlockClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "CHARACTER" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiCharacterClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "DATE" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiDateClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "TIMEFLAG" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiTimeFlagClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "LOGICAL" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiLogicalClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "NIL" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiNilClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "NUMERIC" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiNumericClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "POINTER" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiPointerClass = uiClass;
   }
   else if( strcmp( pNewCls->szName, "HASH" ) == 0 )
   {
      if( __cls_CntMethods( uiClass, hb___msgGetData ) )
         hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      else
         hb_cls_uiHashClass = uiClass;
   }

   return uiClass;
}

HB_FUNC( __CLSNEW )
{
   HB_THREAD_STUB_API

   const char* szClassName = hb_parc( 1 );
   PHB_ITEM pDatas         = hb_param( 2, HB_IT_ANY );
   PHB_ITEM pMethods       = hb_param( 3, HB_IT_ANY );
   PHB_ITEM pSuperArray    = hb_param( 4, HB_IT_ANY );
   PHB_ITEM pBaseLevel     = hb_param( 5, HB_IT_ANY );

   if( pSuperArray && HB_IS_NIL( pSuperArray ) )
      pSuperArray = NULL;

   if( szClassName &&
       ( ! pDatas || HB_IS_NUMERIC( pDatas ) ) &&
       ( ! pMethods || HB_IS_NUMERIC( pMethods ) ) &&
       ( ! pSuperArray || HB_IS_ARRAY( pSuperArray ) ) &&
       ( ! pBaseLevel || HB_IS_NUMERIC( pBaseLevel ) ) )
   {
      USHORT   uiClass;
      PHB_SYMB pClassFunc = NULL;

      if( pBaseLevel )
         pClassFunc = ( *hb_stackGetBase( hb_itemGetNI( pBaseLevel ) + 1 ) )->item.asSymbol.value;

      uiClass = hb_clsNew( szClassName, ( USHORT ) hb_itemGetNI( pDatas ),
                           ( USHORT ) hb_itemGetNI( pMethods ),
                           pSuperArray, pClassFunc, TRUE );
      hb_retni( uiClass );
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

BOOL hb_clsDeactiveClass( PSYMBOLS pModule )
{
   PCLASS   pClass   = s_pClasses;
   UINT     uiPos    = s_uiClasses;
   BOOL     bFound   = FALSE;
   PHB_SYMB pThisModuleSymbols;
   PHB_SYMB pModuleSymbols = pModule->pSymbolTable;
   USHORT   uiSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsDeactiveClass(%p)", pModule ) );

   while( uiPos )
   {
      if( hb_vmFindModuleSymbols( pClass->pClassFuncSym, &pThisModuleSymbols, &uiSymbols )
          && pThisModuleSymbols == pModuleSymbols )
      {
         pClass->bActive   = FALSE;
         bFound            = TRUE;
      }
      uiPos--;
      pClass++;
   }
   return bFound;
}

HB_FUNC( __CLSISACTIVE )
{
   HB_THREAD_STUB_API

   USHORT uiClass  = ( USHORT ) hb_parni( 1 );
   BOOL   bActive  = FALSE;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      bActive = pClass->bActive;
   }
   hb_retl( bActive );
}

HB_FUNC( __CLSACTIVE )
{
   HB_THREAD_STUB_ANY

   USHORT     uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM * pBase   = hb_stackGetBase( 1 );

   if( pBase && uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      if( pClass->pClassFuncSym == ( *pBase )->item.asSymbol.value )
      {
         pClass->bActive = TRUE;
         hb_retl( TRUE );
         return;
      }
   }
   hb_retl( FALSE );
}

HB_FUNC( __CLSSETMODULE )
{
   HB_THREAD_STUB_ANY

   USHORT     uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM * pBase   = hb_stackGetBase( 1 );

   if( pBase && uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      if( ! pClass->bActive )
      {
         pClass->pClassFuncSym = ( *pBase )->item.asSymbol.value;
         hb_retl( TRUE );
         return;
      }
   }
   hb_retl( FALSE );
}

/*
 * __clsDelMsg( <oObj>, <cMessage> )
 *
 * Delete message (only for INLINE and METHOD)
 *
 * <oObj>     Object
 * <cMessage> Message
 */
HB_FUNC( __CLSDELMSG )
{
   USHORT   uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass = s_pClasses + ( uiClass - 1 );
         PMETHOD  pMethod;
         int      iPos;
         USHORT   uiAt = hb_clsFindMethod( pMsg, pClass, &iPos );

         if( uiAt )
         {
            PHB_FUNC pFunc;

            pMethod  = pClass->pMethods + uiAt - 1;
            pFunc    = pMethod->pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method deleted */
            {
               /* Can NOT be deleted or else refernce by number to other Inline blocks will break.
                  hb_arrayDel( pClass->pInlines, pClass->pMethods[ uiAt ].uiData );
               */
               hb_itemClear( pClass->pInlines->item.asArray.value->pItems + pMethod->uiData - 1  );
            }
            else if( ( pFunc == hb___msgSetData ) || ( pFunc == hb___msgGetData ) ) /* Not allowed for DATA     */
            {
               hb_errRT_BASE( EG_ARG, 3004, "Cannot delete a DATA item", "__CLSDELMSG", 0 );
               return;
            }

            hb_clsDelMethod( pClass, iPos, uiAt );

            if( pClass->uiMethods > uiAt )
               memmove( pMethod, pMethod + 1, ( pClass->uiMethods - uiAt ) * sizeof( METHOD ) );

            pClass->uiMethods--;  /* Decrease number messages */
            pClass->uiReserved--;
         }
      }
   }
}

/*
 * <oNewObject> := __clsInst( <hClass> )
 *
 * Create a new object from class definition <hClass>
 */
HB_FUNC( __CLSINST )
{
   HB_THREAD_STUB_STACK

   hb_clsInst( ( USHORT ) hb_parni( 1 ), hb_stackReturnItem() );
}

/*
 * [<o(Super)Object>] := hb_clsInst( <hClass> )
 *
 * Create a (super)object from class definition <hClass>
 */
void hb_clsInst( USHORT uiClass, PHB_ITEM pSelf )
{
   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );
      USHORT   uiAt;

      hb_arrayNew( pSelf, pClass->uiDatas );

      pSelf->item.asArray.value->uiClass     = uiClass;
      pSelf->item.asArray.value->uiPrevCls   = 0;
      /* pSelf->item.asArray.value->puiClsTree   = ( USHORT * ) hb_xgrab( sizeof( USHORT ) ); */
      /* pSelf->item.asArray.value->puiClsTree[0]=0; */

      if( pClass->pInitValues )
      {
         if( pClass->uiDataInitiated > 0 )
         {
            PCLSDINIT pDataInit = pClass->pInitValues;
            PHB_ITEM  pInitValue;

            for( uiAt = pClass->uiDataInitiated + 1; --uiAt; pDataInit++ )
            {
               if( HB_IS_ARRAY( pDataInit->pInitValue ) )
                  pInitValue = hb_arrayClone( pDataInit->pInitValue, NULL );
               else if( HB_IS_HASH( pDataInit->pInitValue ) )
                  pInitValue = hb_hashClone( pDataInit->pInitValue, NULL );
               else
               {
                  pInitValue = hb_itemNew( NULL );
                  hb_itemCopy( pInitValue, pDataInit->pInitValue );

                  if( HB_IS_BLOCK( pInitValue ) )
                     pInitValue->item.asBlock.value->uiClass = pSelf->item.asArray.value->uiClass;
               }

               hb_arraySetForward( pSelf, pDataInit->uiData, pInitValue );
               hb_itemRelease( pInitValue );
            }
         }
      }

      if( ! ( pClass->uiScope & HB_OO_CLS_INSTANCED ) )
      {
         if( pClass->uiDatasShared )
         {
            PMETHOD pMeth;

            /* Initialise value if initialisation was requested */
            pMeth = pClass->pMethods;
            for( uiAt = pClass->uiMethods + 1; --uiAt; pMeth++ )
            {
               /* Init Classdata (inherited and not) if needed */
               if( pMeth->pInitValue )
               {
                  if( pMeth->pFunction == hb___msgGetClsData && ! ( pMeth->bClsDataInitiated ) )
                  {
                     PHB_ITEM pInit;

                     if( hb_arrayGetItemPtr( pClass->pClassDatas, pMeth->uiData )->type == HB_IT_NIL )
                     {
                        if( HB_IS_ARRAY( pMeth->pInitValue ) )
                           pInit = hb_arrayClone( pMeth->pInitValue, NULL );
                        else if( HB_IS_HASH( pMeth->pInitValue ) )
                           pInit = hb_hashClone( pMeth->pInitValue, NULL );
                        else
                        {
                           pInit = hb_itemNew( NULL );
                           hb_itemCopy( pInit, pMeth->pInitValue );

                           if( HB_IS_BLOCK( pInit ) )
                              pInit->item.asBlock.value->uiClass = pSelf->item.asArray.value->uiClass;
                        }

                        hb_arraySetForward( pClass->pClassDatas, pMeth->uiData, pInit );
                        hb_itemRelease( pInit );
                        pMeth->bClsDataInitiated = 1;
                     }
                  }
                  else if( pMeth->pFunction == hb___msgGetShrData && ! ( pMeth->bClsDataInitiated ) )
                  {
                     /* Init Shared Classdata as needed, we only need to init the first */
                     /* not inherited classdata array where all shared will point to    */
                     PHB_ITEM pInit;

                     if( hb_arrayGetItemPtr( pClass->pClassDatas, pMeth->uiData )->type == HB_IT_NIL )
                     {
                        /* TraceLog(NULL,"Inicializando la posición #%d del array de pClassDatas GetShrData() con un valor tipo: %x\n", pMeth->uiData,pMeth->pInitValue->type);
                         */
                        if( HB_IS_ARRAY( pMeth->pInitValue ) )
                           pInit = hb_arrayClone( pMeth->pInitValue, NULL );
                        else if( HB_IS_HASH( pMeth->pInitValue ) )
                           pInit = hb_hashClone( pMeth->pInitValue, NULL );
                        else
                        {
                           pInit = hb_itemNew( NULL );
                           hb_itemCopy( pInit, pMeth->pInitValue );

                           if( HB_IS_BLOCK( pInit ) )
                              pInit->item.asBlock.value->uiClass = pSelf->item.asArray.value->uiClass;
                        }

                        hb_arraySetForward( pClass->pClassDatas, pMeth->uiData, pInit );
                        hb_itemRelease( pInit );
                        pMeth->bClsDataInitiated = 1;
                     }
                  }
               }
            }
         }
      }

      if( pClass->uiScope & HB_OO_CLSTP_CLASSCTOR )
      {
         PMETHOD  pMethod = pClass->pMethods;
         PHB_ITEM pArray  = hb_itemArrayNew( 3 );

         hb_arraySetNI( pArray, 3, HB_OO_MCLSCTOR_INSTANCE );
         hb_arraySetForward( pArray, 1, pSelf );

         for( uiAt = pClass->uiMethods + 1; --uiAt; pMethod++ )
         {
            if( pMethod->uiScope & HB_OO_CLSTP_CLASSCTOR )
            {
               hb_arraySetC( pArray, 2, pMethod->pMessage->pSymbol->szName );
               hb_execFromArray( pArray );
            }
         }

         hb_itemForwardValue( pSelf, hb_arrayGetItemPtr( pArray, 1 ) );

         hb_itemRelease( pArray );
      }

      pClass->uiScope |= HB_OO_CLS_INSTANCED;
   }
   else
   {
      HB_ITEM_NEW( Class );

      hb_itemPutNI( &Class, ( int ) uiClass );
      hb_errRT_BASE( EG_ARG, 3006, "Invalid class handle", "hb_clsInst()", 1, &Class );
   }
}

/*
 * __clsModMsg( <oObj>, <cMessage>, <pFuncOrBlock> [, nScope ] )
 *
 * Modify message (only for INLINE and METHOD)
 */
HB_FUNC( __CLSMODMSG )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );
   USHORT   uiClass = pObject ? pObject->item.asArray.value->uiClass : ( USHORT ) hb_parnl( 1 );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass = s_pClasses + ( uiClass - 1 );
         USHORT   uiAt   = hb_clsFindMethod( pMsg, pClass, NULL );

         if( uiAt )
         {
            PMETHOD  pMethod = pClass->pMethods + uiAt - 1;
            USHORT   uiScope = ( USHORT ) ( ISNUM( 4 ) ? hb_parni( 4 ) : HB_OO_CLSTP_EXPORTED );

            /* pFunc   = pMethod->pFunction;
             */

            switch( pMethod->uiType )
            {
               case HB_OO_MSG_METHOD:
               case HB_OO_MSG_VIRTUAL:
               {
                  PHB_FUNC pFunc = ( PHB_FUNC ) hb_parptr( 3 );

                  if( pFunc )
                  {
                     pMethod->pFunction   = pFunc;
                     pMethod->uiScope     = uiScope | HB_OO_CLSTP_SYMBOL;
                     pMethod->uiType      = HB_OO_MSG_METHOD;
                  }
                  else /* Convert to INLINE. */
                  {
                     PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );

                     if( pBlock )
                     {
                        pMethod->pFunction   = hb___msgEvalInline;
                        pMethod->uiScope     = uiScope;
                        pMethod->uiScope     &= ~( ( USHORT ) HB_OO_CLSTP_SYMBOL );
                        hb_arrayAdd( pClass->pInlines, pBlock );
                        pMethod->uiData      = ( USHORT ) pClass->pInlines->item.asArray.value->ulLen;
                        pMethod->uiType      = HB_OO_MSG_INLINE;
                     }
                     else
                        hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSMODMSG", 0 );
                  }
                  break;
               }
               case HB_OO_MSG_INLINE:
               {
                  PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );

                  if( pBlock == NULL )
                  {
                     PHB_FUNC pFunc = ( PHB_FUNC ) hb_parptr( 3 );

                     if( pFunc ) /* Convert to Method. */
                     {
                        pMethod->pFunction   = pFunc;
                        pMethod->uiScope     = uiScope | HB_OO_CLSTP_SYMBOL;
                        pMethod->uiType      = HB_OO_MSG_METHOD;

                        /* Clear the inline - can NOT be deleted or else refrence by number to other Inline methods will break.
                         */
                        hb_itemClear( pClass->pInlines->item.asArray.value->pItems + pMethod->uiData - 1  );
                     }
                     else
                        hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSMODMSG", 0 );
                  }
                  else
                     hb_arraySet( pClass->pInlines, pMethod->uiData, pBlock );

                  break;
               }
               case HB_OO_MSG_DATA:
                  hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", "__CLSMODMSG", 0 );
                  break;

               case HB_OO_MSG_CLASSDATA:
                  hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a CLASSDATA item", "__CLSMODMSG", 0 );
                  break;

               case HB_OO_MSG_SUPER:
                  hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a SUPER item", "__CLSMODMSG", 0 );
                  break;

               case HB_OO_MSG_ONERROR:
                  hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a ONERROR method", "__CLSMODMSG", 0 );
                  break;

               case HB_OO_MSG_DESTRUCTOR:
                  hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DESTRUCTOR method", "__CLSMODMSG", 0 );
                  break;
            }
         }
      }
   }
}

HB_FUNC( __CLSMSGASSIGNED )
{
   HB_THREAD_STUB_API

   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );
   BOOL     fResult = FALSE;
   USHORT   uiClass = pObject ? pObject->item.asArray.value->uiClass : 0;

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PMETHOD pMethod = hb_objGetpMthd( pMsg, uiClass );

         if( pMethod && pMethod->pFunction != hb___msgVirtual )         /* NON Virtual method */
            fResult = TRUE;
      }
   }
   hb_retl( fResult );
}

/*
 * <cClassName> := ClassName( <xObj> )
 *
 * Returns class name of any type
 *
 * See __ClsName() for HANDLE based argument
 */
HB_FUNC( __OBJGETCLSNAME )
{
   HB_THREAD_STUB_API

   hb_retc( hb_objGetClsName( hb_param( 1, HB_IT_ANY ) ) );
}

/*
 * <lRet> := __clsHasMsg( <oObj | hClass>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj | hClass>
 */
HB_FUNC( __CLSHASMSG )
{
   HB_THREAD_STUB_API

   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );
   USHORT   uiClass = pObject ? pObject->item.asArray.value->uiClass : ( USHORT ) hb_parnl( 1 );

   if( uiClass && uiClass <= s_uiClasses && pString )
      hb_retl( hb_clsHasMsg( uiClass, pString->item.asString.value ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "__ClsHasMsg", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/*
 * <lRet> := __objHasMsg( <oObj>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HB_FUNC( __OBJHASMSG )
{
   HB_THREAD_STUB_API

   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( pObject && pString )
      hb_retl( hb_objHasMsg( pObject, pString->item.asString.value ) != NULL );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "__ObjHasMsg", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/*
 * <oNew> := __objClone( <oOld> )
 *
 * Clone an object. Note the similarity with aClone ;-)
 */

PHB_ITEM hb_objClone( PHB_ITEM pSrcObject )
{
   if( pSrcObject && HB_IS_OBJECT( pSrcObject ) )
   {
      PHB_ITEM pDstObject = hb_arrayClone2( pSrcObject, NULL );
      PCLASS   pClass     = s_pClasses + pDstObject->item.asArray.value->uiClass - 1;

      if( pClass->uiScope & HB_OO_CLSTP_CLASSCTOR )
      {
         PMETHOD  pMethod = pClass->pMethods;
         USHORT   uiAt    = pClass->uiMethods + 1;
         PHB_ITEM pArray  = hb_itemArrayNew( 3 );

         hb_arraySetNI( pArray, 3, HB_OO_MCLSCTOR_CLONE );
         hb_arraySet( pArray, 1, pDstObject );

         for(; --uiAt; pMethod++ )
         {
            if( pMethod->uiScope & HB_OO_CLSTP_CLASSCTOR )
            {
               hb_arraySetC( pArray, 2, pMethod->pMessage->pSymbol->szName );
               hb_execFromArray( pArray );
            }
         }

         hb_itemRelease( pArray );
      }

      /* pDstObject->item.asArray.value->puiClsTree = NULL; */
      /* pDstObject->item.asArray.value->puiClsTree = ( USHORT * ) hb_xgrab( sizeof( USHORT ) ); */
      /* pDstObject->item.asArray.value->puiClsTree[0]=0; */

      return pDstObject;
   }
   return hb_itemNew( NULL );
}

HB_FUNC( __OBJCLONE )
{
   PHB_ITEM pSrcObject = hb_param( 1, HB_IT_OBJECT );

   if( pSrcObject )
      hb_itemRelease( hb_itemReturnForward( hb_objClone( pSrcObject ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, "__OBJCLONE", 0 );
}

PHB_ITEM hb_objSendMsg( PHB_ITEM pObj, const char * sMsg, HB_SIZE ulArg, ... )
{
   HB_THREAD_STUB_STACK

   PHB_DYNS pMsgSym = hb_dynsymFindName( sMsg );
   PHB_SYMB pSymbol;

   /* printf( "%s %p\n", sMsg, pMsgSym );
    */

   if( pMsgSym )
   {
      pSymbol = pMsgSym->pSymbol;

      hb_vmPushSymbol( pSymbol );
      hb_vmPush( pObj );

      if( ulArg )
      {
         ULONG    i;
         va_list  ap;

         va_start( ap, ulArg );

         for( i = 0; i < ulArg; i++ )
            hb_vmPush( va_arg( ap, PHB_ITEM ) );

         va_end( ap );
      }

      hb_vmSend( ( USHORT ) ulArg );
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__ObjSendMsg()", 0 );

   return hb_stackReturnItem();
}

PHB_ITEM hb_objSendSymbol( PHB_ITEM pObj, PHB_SYMB pSymbol, HB_SIZE ulArg, ... )
{
   HB_THREAD_STUB_STACK

   hb_vmPushSymbol( pSymbol );
   hb_vmPush( pObj );

   if( ulArg )
   {
      ULONG    i;
      va_list  ap;

      va_start( ap, ulArg );

      for( i = 0; i < ulArg; i++ )
         hb_vmPush( va_arg( ap, PHB_ITEM ) );

      va_end( ap );
   }

   hb_vmSend( ( USHORT ) ulArg );

   return hb_stackReturnItem();
}

void hb_objSendMessage( PHB_ITEM pObject, PHB_DYNS pMsgSym, HB_SIZE ulArg, ... )
{
   hb_vmPushSymbol( pMsgSym->pSymbol );
   hb_vmPush( pObject );

   if( ulArg )
   {
      ULONG    i;
      va_list  ap;

      va_start( ap, ulArg );

      for( i = 0; i < ulArg; i++ )
         hb_vmPush( va_arg( ap, PHB_ITEM ) );

      va_end( ap );
   }

   hb_vmSend( ( USHORT ) ulArg );
}

/*
 * <xRet> = __objSendMsg( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HB_FUNC( __OBJSENDMSG )
{
   HB_THREAD_STUB_ANY

   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   int      iPCount = hb_pcount();

   if( iPCount >= 2 && pObject )                            /* Object & message passed */
   {
      /*hb_dynsymFindName( hb_parcx(2) );*/
      PHB_DYNS pMsg = hb_dynsymGet( hb_parcx( 2 ) );

      if( pMsg )
      {
         int iParam;

         hb_vmPushSymbol( pMsg->pSymbol );                  /* Push char symbol as message  */
         hb_vmPush( pObject );                              /* Push object */

         for( iParam = 3; iParam <= iPCount; iParam++ )     /* Push arguments on stack */
         {
            /* NOTE: hb_param() cannot be used here, because it dereferences the parameters
             */
            hb_vmPush( hb_stackItemFromBase( iParam ) );
         }

         hb_vmSend( ( USHORT ) ( iPCount - 2 ) );           /* Execute message */
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJSENDMSG", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * <xRet> = __objSendMsgCase( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a case sensitive message to an object
 */
HB_FUNC( __OBJSENDMSGCASE )
{
   HB_THREAD_STUB

   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   int      iPCount = hb_pcount();

   if( iPCount >= 2 && pObject )                            /* Object & message passed */
   {
      /*hb_dynsymFindName( hb_parcx(2) );*/
      PHB_DYNS pMsg;

      pMsg = hb_dynsymGetCase( hb_parcx( 2 ) );

      if( pMsg )
      {
         int iParam;

         hb_vmPushSymbol( pMsg->pSymbol );                  /* Push char symbol as message  */
         hb_vmPush( pObject );                              /* Push object */

         for( iParam = 3; iParam <= iPCount; iParam++ )     /* Push arguments on stack */
         {
            /* NOTE: hb_param() cannot be used here, because it dereferences the parameters
             */
            hb_vmPush( hb_stackItemFromBase( iParam ) );
         }

         hb_vmSend( ( USHORT ) ( iPCount - 2 ) );           /* Execute message */
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJSENDMSGCASE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/*
 * <hClass> := __clsInstSuper( <cName> )
 *
 * Instance super class and return class handle
 */
HB_FUNC( __CLSINSTSUPER )
{
   BOOL bFound = FALSE;

   HB_THREAD_STUB_STACK

   if( hb_pcount() >= 1 )
   {
      const char * szString = hb_parcx( 1 );
      PHB_DYNS     pDynSym  = hb_dynsymFind( szString );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
      {
         USHORT uiClass;

         hb_vmPushSymbol( pDynSym->pSymbol );               /* Push function name  */
         hb_vmPushNil();
         hb_vmDo( 0 );                                      /* Execute super class */

         if( HB_IS_OBJECT( hb_stackReturnItem() ) )
         {
            for( uiClass = 0; ! bFound && uiClass < s_uiClasses; uiClass++ ) /* Locate the entry */
            {
               if( hb_stricmp( szString, s_pClasses[ uiClass ].szName ) == 0 )
               {
                  hb_retni( uiClass + 1 );                  /* Entry + 1 = hb___msgClsH */
                  bFound = TRUE;
               }
            }
         }
         else
            hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", "__CLSINSTSUPER", 1, hb_paramError( 1 ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 3003, "Cannot find super class", "__CLSINSTSUPER", 1, hb_paramError( 1 ) );
   }

   if( ! bFound )
      hb_retni( 0 );
}

/*
 * <nSeq> = __cls_CntClsData( <hClass> )
 *
 * Return number of class datas
 */
HB_FUNC( __CLS_CNTCLSDATA )
{
   HB_THREAD_STUB_API

   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      hb_retns( pClass->pClassDatas->item.asArray.value->ulLen );
   }
   else
      hb_retni( 0 );
}

/*
 * <nSeq> = __cls_CntShrData( <hClass> )
 *
 * Return number of class shared datas
 */
HB_FUNC( __CLS_CNTSHRDATA )
{
   HB_THREAD_STUB_API

   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   hb_retni( uiClass ? s_pClasses[ uiClass - 1 ].uiDatasShared : 0 );
}

/*
 * <nSeq> = __cls_CntData( <hClass> )
 *
 * Return number of datas
 */
HB_FUNC( __CLS_CNTDATA )
{
   HB_THREAD_STUB_API

   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
      hb_retni( uiClass != 0 ? s_pClasses[ uiClass - 1 ].uiDatas : 0 );
}

/*
 * <nSeq> = __cls_DecData( <hClass> )
 *
 * Return number of datas and decrease
 */
HB_FUNC( __CLS_DECDATA )
{
   HB_THREAD_STUB_API

   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
      hb_retni( s_pClasses[ uiClass - 1 ].uiDatas-- );
}

/*
 * <nSeq> = __cls_IncData( <hClass> )
 *
 * Return number of datas and increase
 */
HB_FUNC( __CLS_INCDATA )
{
   HB_THREAD_STUB_API

   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      ++pClass->uiDatas;

      if( ! ( pClass->uiScope & HB_OO_CLS_INSTANCED ) )
         pClass->pInitValues = ( PCLSDINIT ) hb_xrealloc( pClass->pInitValues, pClass->uiDatas * sizeof( CLSDINIT ) );

      hb_retni( pClass->uiDatas );
   }
}

/* NOTE: Undocumented Clipper function */

/* Implemented as PRG in TClass.prg for parameter compatibility with Clipper.
   HB_FUNC( __CLASSNEW )
   {
   HB_FUNCNAME( __CLSNEW )();
   }
 */

/* NOTE: Undocumented Clipper function */
/* Implemented as PRG in TClass.prg for parameter compatibility with Clipper.
HB_FUNC( __CLASSINS )
{
   HB_FUNCNAME( __CLSINST )();
}
*/

/* NOTE: Undocumented Clipper function */
/* Implemented as PRG in TClass.prg for parameter compatibility with Clipper.
HB_FUNC( __CLASSADD )
{
   HB_FUNCNAME( __CLSADDMSG )();
}
*/

/* NOTE: Undocumented Clipper function */

HB_FUNC( __CLASSNAME )
{
   HB_THREAD_STUB_API

   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
      hb_retcStatic( s_pClasses[ uiClass - 1 ].szName );
   else
      hb_retclen( NULL, 0 );
}

/* NOTE: Undocumented Clipper function */
/* NOTE: Based on hb___msgClsSel() */

HB_FUNC( __CLASSSEL )
{
   USHORT   uiClass = ( USHORT ) hb_parni( 1 );
   HB_ITEM  Return;

   Return.type = HB_IT_NIL;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      USHORT   uiAt     = pClass->uiMethods, uiPos = 0;
      PMETHOD  pMeth    = pClass->pMethods;

      hb_arrayNew( &Return, uiAt );
      /* Create a transfer array  */
      for( uiAt++; --uiAt; pMeth++ )
         /* Add to array */
         hb_arraySetC( &Return, ++uiPos, pMeth->pMessage->pSymbol->szName );
   }

   hb_itemReturnForward( &Return );
}

HB_FUNC( __GETMEMBERS )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 0 );
   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );
   USHORT   nParam  = ( USHORT ) hb_parni( 2 );
   USHORT   uiScope = ( USHORT ) hb_parni( 3 );
   USHORT   uiType  = ( USHORT ) hb_parni( 4 );
   USHORT   uiClass = hb_objClassH( pObject );

   if( ( ! uiClass ) && HB_IS_BYREF( pObject ) )
   {
      PHB_ITEM pItemRef = hb_itemUnRef( pObject );

      if( HB_IS_ARRAY( pItemRef ) )
         uiClass = pItemRef->item.asArray.value->uiClass;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );
      USHORT   uiPos  = 0;
      USHORT   uiAt   = pClass->uiMethods;
      PMETHOD  pMeth  = pClass->pMethods;
      PHB_ITEM pItem  = hb_itemNew( NULL );

      hb_arraySize( pReturn, uiAt );

      for( uiAt++; --uiAt; pMeth++ )
      {
         if( ( ( nParam == HB_MSGLISTALL ) ||
               ( ( nParam == HB_MSGLISTCLASS ) &&
                 ( ( pMeth->pFunction == hb___msgSetClsData ) ||
                   ( pMeth->pFunction == hb___msgGetClsData ) ||
                   ( pMeth->pFunction == hb___msgSetShrData ) ||
                   ( pMeth->pFunction == hb___msgGetShrData ) )
               ) ||
               ( ( nParam == HB_MSGLISTPURE ) &&
                 ( ( ! ( pMeth->pFunction == hb___msgSetClsData ) ) &&
                   ( ! ( pMeth->pFunction == hb___msgGetClsData ) ) &&
                   ( ! ( pMeth->pFunction == hb___msgSetShrData ) ) &&
                   ( ! ( pMeth->pFunction == hb___msgGetShrData ) ) )
               )
               ) &&
             ( uiScope == 0 || pMeth->uiScope & uiScope ) &&
             ( uiType == 0 || pMeth->uiType == uiType ) &&
             pMeth->pMessage->pSymbol->szName[ 0 ] != '_'
             )
            hb_arraySetForward( pReturn, ++uiPos, hb_itemPutC( pItem, pMeth->pMessage->pSymbol->szName ) );
      }

      hb_itemRelease( pItem );
      hb_arraySize( pReturn, uiPos );
   }

   hb_itemRelease( hb_itemReturnForward( pReturn ) );
}

/* to be used from Classes ERROR HANDLER method */
HB_FUNC( __GETMESSAGE )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pBase = hb_stackItem( ( hb_stackBaseItem() )->item.asSymbol.pCargo->stackbase );

   hb_retcStatic( pBase->item.asSymbol.value->szName );
}

HB_FUNC( __CLSPARENT )
{
   HB_THREAD_STUB_API

   hb_retl( hb_clsIsParent( ( USHORT ) hb_parni( 1 ), hb_parcx( 2 ) ) );
}

HB_FUNC( __SENDER )
{
   HB_THREAD_STUB

   PHB_ITEM * pBase   = HB_VM_STACK.pBase;
   PHB_ITEM   oSender = NULL;
   USHORT     iLevel  = 3;

   while( iLevel > 0 && pBase != HB_VM_STACK.pItems )
   {
      pBase    = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
      oSender  = *( pBase + 1 );

      if( ( iLevel-- == 2 && oSender->type != HB_IT_BLOCK ) || oSender->type == HB_IT_NIL )
         break;
   }

   if( iLevel == 0 && oSender != NULL && oSender->type == HB_IT_OBJECT )
      hb_itemCopy( hb_stackReturnItem(), oSender );
}

/*
 * Added by RâC&JfL
 *
 * based on hb___msgClsH( void )
 */
USHORT hb_objClassH( PHB_ITEM pObject )
{
   USHORT uiClass;

   switch( pObject->type )
   {
      case HB_IT_ARRAY:
         uiClass = pObject->item.asArray.value->uiClass ? pObject->item.asArray.value->uiClass : hb_cls_uiArrayClass;
         break;

      case HB_IT_BLOCK:
         uiClass = hb_cls_uiBlockClass;
         break;

      case HB_IT_STRING:
         uiClass = hb_cls_uiCharacterClass;
         break;

      case HB_IT_DATE:
         uiClass = hb_cls_uiDateClass;
         break;

      case HB_IT_TIMEFLAG:
         uiClass = hb_cls_uiTimeFlagClass;
         break;

      case HB_IT_LOGICAL:
         uiClass = hb_cls_uiLogicalClass;
         break;

      case HB_IT_NIL:
         uiClass = hb_cls_uiNilClass;
         break;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         uiClass = hb_cls_uiNumericClass;
         break;

      case HB_IT_POINTER:
         uiClass = hb_cls_uiPointerClass;
         break;

      case HB_IT_HASH:
         uiClass = hb_cls_uiHashClass;
         break;

      default:
         uiClass = 0;
   }

   return uiClass && uiClass <= s_uiClasses ? uiClass : 0;
}

HB_FUNC( __CLASSH )
{
   HB_THREAD_STUB_API

   hb_retni( hb_objClassH( hb_param( 1, HB_IT_ANY ) ) );
}

/*
 * based on hb___msgEval( void )
 */
HB_FUNC( __EVAL )
{
   HB_THREAD_STUB_API

   PHB_ITEM pObject = hb_param( 1, HB_IT_BLOCK );
   int      iPCount = hb_pcount();

   if( pObject )
   {
      int iParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pObject );                      /* Push block  */

      for( iParam = 1; iParam <= iPCount; iParam++ )
         hb_vmPush( hb_param( iParam, HB_IT_ANY ) );

      hb_vmSend( ( USHORT ) iPCount );           /* Self is also an argument */
   }
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL", 0 );
}

/* ================================================ */

/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
static void hb___msgClsH( void )
{
   HB_THREAD_STUB_STACK

   hb_retni( hb_objClassH( hb_stackSelfItem() ) );
}

/* Added by JfL&RaC
 * <logical> <= <obj>:IsDerivedFrom( xParam )
 *
 * Return true if <obj> is derived from xParam.
 * xParam can be either an obj or a classname
 */
static void hb___msgClsParent( void )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pItemRef   = HB_IS_BYREF( hb_stackSelfItem() ) ? hb_itemUnRef( hb_stackSelfItem() ) : hb_stackSelfItem();
   USHORT   uiClass    = hb_objClassH( pItemRef );
   PHB_ITEM pItemParam = hb_stackItemFromBase( 1 );

   if( HB_IS_OBJECT( pItemParam ) )
      hb_retl( hb_clsIsParent( uiClass, hb_objGetClsName( pItemParam ) ) );
   else if( HB_IS_STRING( pItemParam ) )
   {
      UINT     i;
      char *   szParentName = hb_itemGetC( pItemParam );

      for( i = 0; szParentName[ i ] != '\0'; i++ )
         szParentName[ i ] = ( char ) HB_TOUPPER( szParentName[ i ] );

      hb_retl( hb_clsIsParent( uiClass, szParentName ) );

      hb_itemFreeC( szParentName );
   }
}

/*
 * <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
static void hb___msgClsName( void )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pItemRef = hb_stackSelfItem();

   pItemRef = hb_itemUnRef( pItemRef );

   hb_retcStatic( hb_objGetClsName( pItemRef ) );
}

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * <aMessages> := <obj>:ClassFullSel()
 *
 * Returns all the messages in <obj> as ClassSel plus scope
 */
static void hb___msgClsFullSel( void )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pSelf   = hb_stackSelfItem();
   USHORT   uiClass = hb_objClassH( pSelf );
   HB_ITEM  Return;
   USHORT   nParam  = ( USHORT ) hb_parni( 1 ), uiScope = ( USHORT ) hb_parni( 2 );

   Return.type = HB_IT_NIL;

   if( ( ! uiClass ) && HB_IS_BYREF( pSelf ) )
   {
      PHB_ITEM pItemRef = hb_itemUnRef( pSelf ); /* Is it possible? */

      if( HB_IS_ARRAY( pItemRef ) )
         uiClass = pItemRef->item.asArray.value->uiClass;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      USHORT   uiPos    = 0;
      USHORT   uiAt     = pClass->uiMethods;
      PMETHOD  pMeth    = pClass->pMethods;

      hb_arrayNew( &Return, uiAt );
      /* Create a transfer array  */
      for( uiAt++; --uiAt; pMeth++ )
      {
         if( ( nParam == HB_MSGLISTALL ) ||
             ( ( nParam == HB_MSGLISTCLASS ) &&
               ( ( pMeth->pFunction == hb___msgSetClsData ) ||
                 ( pMeth->pFunction == hb___msgGetClsData ) ||
                 ( pMeth->pFunction == hb___msgSetShrData ) ||
                 ( pMeth->pFunction == hb___msgGetShrData ) )
             ) ||
             ( ( nParam == HB_MSGLISTPURE ) &&
               ( ( ! ( pMeth->pFunction == hb___msgSetClsData ) ) &&
                 ( ! ( pMeth->pFunction == hb___msgGetClsData ) ) &&
                 ( ! ( pMeth->pFunction == hb___msgSetShrData ) ) &&
                 ( ! ( pMeth->pFunction == hb___msgGetShrData ) ) )
             )
             )
         {
            if( uiScope == 0 || pMeth->uiScope & uiScope )
            {
               HB_ITEM SubArray;

               SubArray.type = HB_IT_NIL;

               hb_arrayNew( &SubArray, 4 );

               hb_arraySetC( &SubArray, HB_OO_DATA_SYMBOL, pMeth->pMessage->pSymbol->szName );

               /* 2 is also HB_OO_DATA_VALUE which seems to not be used so overlapped!
                */
               hb_arraySetPtr( &SubArray, HB_OO_DATA_SYMBOL_PTR, ( void * ) pMeth->pMessage->pSymbol );

               hb_arraySetNI( &SubArray, HB_OO_DATA_TYPE, pMeth->uiType );
               hb_arraySetNI( &SubArray, HB_OO_DATA_SCOPE, pMeth->uiScope );

               hb_arraySetForward( &Return, ++uiPos, &SubArray );
            }
         }
      }

      hb_arraySize( &Return, uiPos );
   }

   hb_itemReturnForward( &Return );
}

/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static void hb___msgClsSel( void )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pSelf   = hb_stackSelfItem();
   USHORT   uiClass = hb_objClassH( pSelf );
   HB_ITEM  Return;
   USHORT   nParam  = ( USHORT ) hb_parni( 1 ), uiScope = ( USHORT ) hb_parni( 2 );

   Return.type = HB_IT_NIL;

   if( ( ! uiClass ) && HB_IS_BYREF( pSelf ) )
   {
      PHB_ITEM pItemRef = hb_itemUnRef( pSelf ); /* Is it possible? */

      if( HB_IS_ARRAY( pItemRef ) )
         uiClass = pItemRef->item.asArray.value->uiClass;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS  pClass = s_pClasses + ( uiClass - 1 );
      USHORT  uiPos  = 0;
      USHORT  uiAt   = pClass->uiMethods;
      PMETHOD pMeth  = pClass->pMethods;

      Return.type = HB_IT_NIL;
      hb_arrayNew( &Return, uiAt );

      /* Create a transfer array  */
      for( uiAt++; --uiAt; pMeth++ )
      {
         if( ( nParam == HB_MSGLISTALL ) ||
             ( ( nParam == HB_MSGLISTCLASS ) &&
               ( ( pMeth->pFunction == hb___msgSetClsData ) ||
                 ( pMeth->pFunction == hb___msgGetClsData ) ||
                 ( pMeth->pFunction == hb___msgSetShrData ) ||
                 ( pMeth->pFunction == hb___msgGetShrData ) )
             ) ||
             ( ( nParam == HB_MSGLISTPURE ) &&
               ( ( ! ( pMeth->pFunction == hb___msgSetClsData ) ) &&
                 ( ! ( pMeth->pFunction == hb___msgGetClsData ) ) &&
                 ( ! ( pMeth->pFunction == hb___msgSetShrData ) ) &&
                 ( ! ( pMeth->pFunction == hb___msgGetShrData ) ) )
             )
             )
         {
            if( uiScope == 0 || pMeth->uiScope & uiScope )
               hb_arraySetC( &Return, ++uiPos, pMeth->pMessage->pSymbol->szName );
         }
      }

      hb_arraySize( &Return, uiPos );
   }

   hb_itemReturnForward( &Return );
}

/*
 * __msgEvalInline()
 *
 * Internal function executed for inline methods
 */
static void hb___msgEvalInline( void )
{
   HB_THREAD_STUB

   int      iParam, iPCount = hb_pcount();
   PHB_ITEM pSelf   = hb_stackSelfItem();
   USHORT   uiClass = hb_objClassH( pSelf );

   hb_vmPushSymbol( &hb_symEval );
   hb_stackItemFromTop( -1 )->item.asSymbol.pCargo->uiSuperClass  = hb_stackBaseItem()->item.asSymbol.pCargo->uiSuperClass;
   hb_vmPush( hb_arrayGetItemPtr( s_pClasses[ uiClass - 1 ].pInlines, ( HB_VM_STACK.pMethod )->uiData ) );
   hb_vmPush( pSelf );                        /* Push self */

   for( iParam = 1; iParam <= iPCount; iParam++ )
      hb_vmPush( hb_stackItemFromBase( iParam ) );

   hb_vmSend( ( USHORT ) ( iPCount + 1 ) );   /* Self is also an argument */

/* Release block
   * hb_codeblockDelete( &block );
   */
}

/*
 * __msgEval()
 *
 * Internal function for the internal EVAL method.
 */
#if 0
/* Eval message for blocks is handled at VM level.
 */
static void hb___msgEval( void )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pSelf = hb_stackSelfItem();

   if( HB_IS_BLOCK( pSelf ) )
   {
      int iParam, iPCount = hb_pcount();

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pSelf );                         /* Push block               */

      for( iParam = 1; iParam <= iPCount; iParam++ )
      {
         hb_vmPush( hb_param( iParam, HB_IT_ANY ) );
      }

      hb_vmSend( ( USHORT ) iPCount );            /* Self is also an argument */
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL", 0 );
   }
}
#endif

/*
 * __msgSuper()
 *
 * Internal function to return a superobject
 */
static void hb___msgSuper( void )
{
   HB_THREAD_STUB

   hb_clsMakeSuperObject( hb_stackReturnItem(), hb_stackSelfItem(),
                          ( HB_VM_STACK.pMethod )->uiSprClass );
}

/*
 * __msgClass()
 *
 * Internal function to return Self at Self:Class call (classy compatibility)
 */
/*
   static void hb___msgClass( void )
   {
   hb_itemCopy( hb_stackReturnItem(), hb_stackSelfItem() );
   }
 */

/*
 * __msgGetClsData()
 *
 * Internal function to return a CLASSDATA
 */
void hb___msgGetClsData( void )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pObject = hb_stackSelfItem();
   USHORT   uiClass = hb_objClassH( pObject );

   if( uiClass )
      hb_arrayGet( s_pClasses[ uiClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiData, hb_stackReturnItem() );
}

/*
 * __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
void hb___msgSetClsData( void )
{
   HB_THREAD_STUB

   PHB_ITEM pObject = hb_stackSelfItem();
   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );
   USHORT   uiClass = hb_objClassH( pObject );

   if( uiClass )
      hb_arraySet( s_pClasses[ uiClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiData, pReturn );

   hb_itemCopy( hb_stackReturnItem(), pReturn );
}

/*
 * __msgGetShrData()
 *
 * Internal function to return a SHAREDDATA
 */
void hb___msgGetShrData( void )
{
   HB_THREAD_STUB

   USHORT uiSprCls = ( HB_VM_STACK.pMethod )->uiSprClass;

   if( uiSprCls && uiSprCls <= s_uiClasses )
      hb_arrayGet( s_pClasses[ uiSprCls - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiDataShared, hb_stackReturnItem() );
}

/*
 * __msgSetShrData()
 *
 * Internal function to set a SHAREDDATA
 */
void hb___msgSetShrData( void )
{
   HB_THREAD_STUB

   USHORT   uiSprCls = ( HB_VM_STACK.pMethod )->uiSprClass;
   PHB_ITEM pReturn  = hb_stackItemFromBase( 1 );

   if( uiSprCls && uiSprCls <= s_uiClasses )
      hb_arraySet( s_pClasses[ uiSprCls - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiDataShared, pReturn );

   hb_itemCopy( hb_stackReturnItem(), pReturn );
}

/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
void hb___msgGetData( void )
{
   HB_THREAD_STUB

   PHB_ITEM pObject = hb_stackSelfItem();
   USHORT   uiIndex = ( HB_VM_STACK.pMethod )->uiData;

   /* will arise only if the class has been modified after first instance */
   if( uiIndex > ( USHORT ) pObject->item.asArray.value->ulLen )  /* Resize needed */
      hb_arraySize( pObject, uiIndex );                           /* Make large enough */

   hb_arrayGet( pObject, uiIndex, hb_stackReturnItem() );
}

/*
 * __msgSetData()
 *
 * Internal function to set a DATA
 */
void hb___msgSetData( void )
{
   HB_THREAD_STUB

   PHB_ITEM pObject = hb_stackSelfItem();
   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );
   USHORT   uiIndex = ( HB_VM_STACK.pMethod )->uiData;

   /* will arise only if the class has been modified after first instance */
   if( uiIndex > ( USHORT ) pObject->item.asArray.value->ulLen )  /* Resize needed ? */
      hb_arraySize( pObject, uiIndex );                           /* Make large enough */

   hb_arraySet( pObject, uiIndex, pReturn );
   hb_itemReturnForward( pReturn );
}

/* No comment :-) */
static void hb___msgVirtual( void )
{
   /* hb_ret(); */ /* NOTE: It's safe to comment this out */
   ;
}

/*
 * __msgDelegate()
 *
 * Internal function to execute a delegate method
 */
static void hb___msgDelegate( void )
{
   HB_THREAD_STUB

   PHB_SYMB pMsgIs   = ( HB_VM_STACK.pMethod )->pMsgIs;
   PHB_SYMB pMsgTo   = ( HB_VM_STACK.pMethod )->pMsgTo;
   PHB_ITEM pSelf    = hb_stackSelfItem();
   int      i, iPCount = hb_pcount();

   if( ! pMsgIs )
      pMsgIs = ( HB_VM_STACK.pMethod )->pMessage->pSymbol;

   if( pMsgTo )
   {
      hb_vmPushSymbol( pMsgTo );
      hb_vmPush( pSelf );
      hb_vmSend( 0 );

      pSelf = &( HB_VM_STACK.Return );
   }
   else if( ( HB_VM_STACK.pMethod )->pInitValue )
      pSelf = ( HB_VM_STACK.pMethod )->pInitValue;

   if( pSelf )
   {
      hb_vmPushSymbol( pMsgIs );
      hb_vmPush( pSelf );

      for( i = 1; i <= iPCount; i++ )
      {
         hb_vmPush( hb_stackItemFromBase( i ) );
      }
      hb_vmSend( ( USHORT ) iPCount );
   }
}

PCLASS hb_clsClassesArray( void )
{
   return s_pClasses;
}

USHORT hb_clsMaxClasses( void )
{
   return s_uiClasses;
}

HB_FUNC( __GETMSGPRF ) /* profiler: returns a method called and consumed times */
{                      /* ( nClass, cMsg ) --> aMethodInfo { nTimes, nTime } */
   HB_THREAD_STUB_API

#ifndef HB_NO_PROFILER
   PHB_DYNS pMsg = hb_dynsymFindName( hb_parcx( 2 ) );
   PMETHOD pMethod = hb_objGetpMthd( pMsg, ( USHORT ) hb_parni( 1 ) );

   hb_reta( 2 );
   hb_stornl( 0, -1, 1 );
   hb_stornl( 0, -1, 2 );

   if( pMethod )
   {
      hb_stornl( pMethod->ulCalls, -1, 1 );
      hb_stornl( pMethod->ulTime, -1, 2 );
      return;
   }
#else
   hb_reta( 2 );
   hb_stornl( 0, -1, 1 );
   hb_stornl( 0, -1, 2 );
#endif
}

void hb_mthAddTime( PMETHOD pMethod, ULONG ulClockTicks )
{
#ifndef HB_NO_PROFILER
   if( pMethod != NULL )
      pMethod->ulTime += ulClockTicks;
#else
   HB_SYMBOL_UNUSED( pMethod );
   HB_SYMBOL_UNUSED( ulClockTicks );
#endif
}

#ifdef HB_THREAD_SUPPORT
void hb_clsPutSyncID( USHORT uiClass )
{
   HB_THREAD_STUB

   PSYNCID pSyncId = HB_VM_STACK.pSyncId;
   ULONG   ulCount;

   if( ! pSyncId )
   {
      HB_VM_STACK.pSyncId = pSyncId = ( PSYNCID ) hb_xalloc( sizeof( SYNCID ) * 2 );

      pSyncId->uiClass = 0xFFFF;
      pSyncId->ulCount = 2;
      pSyncId++;

      pSyncId->uiClass = uiClass;
      pSyncId->ulCount = 1;
   }
   else
   {
      ULONG ulPos = 1;

      ulCount = pSyncId->ulCount;
      pSyncId++;

      do
      {
         if( pSyncId->uiClass >= uiClass )
         {
            if( pSyncId->uiClass == uiClass )
            {
               pSyncId->ulCount++;
               break;
            }
            else
            {
               HB_VM_STACK.pSyncId  = pSyncId = ( PSYNCID ) hb_xrealloc( HB_VM_STACK.pSyncId, sizeof( SYNCID ) * ( ulCount + 1 ) );
               pSyncId->ulCount++;

               pSyncId              += ulPos;

               memmove( pSyncId + 1, pSyncId, sizeof( SYNCID ) * ( ulCount - ulPos ) );

               pSyncId->uiClass  = uiClass;
               pSyncId->ulCount  = 1;
               break;
            }
         }
         pSyncId++;
         ulPos++;
      }
      while( ulPos < ulCount );

      if( ulPos == ulCount )
      {
         HB_VM_STACK.pSyncId  = pSyncId = ( PSYNCID ) hb_xrealloc( HB_VM_STACK.pSyncId, sizeof( SYNCID ) * ( ulCount + 1 ) );
         pSyncId->ulCount++;

         pSyncId              += ulPos;

         pSyncId->uiClass     = uiClass;
         pSyncId->ulCount     = 1;
      }
   }

   if( pSyncId->ulCount == 1 )
      hb_threadMutexLock( ( s_pClasses + uiClass - 1 )->pMtxSync, FALSE );
}

ULONG hb_clsDelSyncID( USHORT uiClass )
{
   HB_THREAD_STUB

   PSYNCID pSyncId = HB_VM_STACK.pSyncId;
   ULONG   ulCount = 0, ulPos;

   if( pSyncId )
   {
      ulCount  = pSyncId->ulCount;
      pSyncId++;
      ulPos    = 1;

      while( ulCount < ulPos && pSyncId->uiClass < uiClass )
      {
         pSyncId++;
         ulPos++;
      }

      if( pSyncId->uiClass == uiClass && pSyncId->ulCount > 0 )
         ulCount = --pSyncId->ulCount;
      else
         ulCount = 0;
   }

   if( pSyncId->ulCount == 0 )
      hb_threadMutexUnlock( ( s_pClasses + uiClass - 1 )->pMtxSync, FALSE );

   return ulCount;
}

void hb_clsUnmutexSync( void )
{
   HB_THREAD_STUB

   PSYNCID pSyncId = HB_VM_STACK.pSyncId;
   ULONG   ulCount;

   if( pSyncId )
   {
      ulCount = pSyncId->ulCount;
      pSyncId++;

      while( --ulCount )
      {
         if( pSyncId->ulCount > 0 )
            hb_threadMutexUnlock( ( s_pClasses + pSyncId->uiClass - 1 )->pMtxSync, FALSE );

         pSyncId++;
      }
   }
}

void hb_clsRemutexSync( void )
{
   HB_THREAD_STUB

   PSYNCID pSyncId = HB_VM_STACK.pSyncId;
   ULONG   ulCount;

   if( pSyncId )
   {
      ulCount = pSyncId->ulCount;
      pSyncId++;

      while( --ulCount )
      {
         if( pSyncId->ulCount > 0 )
            hb_threadMutexLock( ( s_pClasses + pSyncId->uiClass - 1 )->pMtxSync, FALSE );

         pSyncId++;
      }
   }
}

#endif /* HB_THREAD_SUPPORT */

void hb_clsFinalize( PHB_ITEM pObject )
{
   HB_THREAD_STUB

   USHORT uiClass;

   if( HB_IS_OBJECT( pObject ) )
      uiClass = pObject->item.asArray.value->uiClass;
   else
      /* TODO Error!
       */
      return;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      if( pClass->pDestructor )
      {
         if( pClass->uiScope & HB_OO_CLS_DESTRUC_SYMB )
         {
            assert( hb_stack_ready );

            if( s_AllowDestructors /* && hb_stack_ready */ )
            {
               if( HB_IS_SYMBOL( hb_stackBaseItem() ) &&
                   ( hb_stackBaseItem()->item.asSymbol.pCargo->uiSuperClass == 0 || ( USHORT ) hb_stackBaseItem()->item.asSymbol.pCargo->uiSuperClass != uiClass ||
                     ( HB_IS_ARRAY( hb_stackSelfItem() ) && hb_stackSelfItem()->item.asArray.value != pObject->item.asArray.value ) ) && strcmp( hb_stackBaseItem()->item.asSymbol.value->szName, "__CLSINSTSUPER" ) )
               {
                  /* To DISABLE GC here where no refernce to this object will cause GPF for double release!
                   */
                  BOOL     bCollecting = hb_gcSetCollecting( TRUE ), bPop = TRUE;
                  PHB_SYMB pDestructor = pClass->pDestructor->pMessage->pSymbol;

                  /* Save the existing Return Value and Top Item if any.
                   */
                  if( HB_IS_ARRAY( &HB_VM_STACK.Return ) && HB_VM_STACK.Return.item.asArray.value == pObject->item.asArray.value )
                  {
                     /* Don't process HB_VM_STACK.Return!
                      */
                     bPop = FALSE;

                     /* Save top item which can be processed at this moment
                      */
                     hb_stackPush();
                  }
                  else
                     hb_vmPushState();

                  hb_vmPushSymbol( pDestructor );
                  hb_vmPush( pObject ); /* Do NOT Forward!!! */
                  hb_vmSend( 0 );

                  /* Restore the existing Return Value and Top Item if any.
                   */
                  if( bPop )
                     hb_vmPopState();
                  else
                     /* Restore top item
                      */
                     hb_stackDec();

                  hb_gcSetCollecting( bCollecting );
               }
            }
            else
               hb_errInternal( HB_EI_ERRUNRECOV, "Destructors disabled! Destructor of class: '%s' can't be executed.", pClass->szName, NULL );
         }
      }
   }
}

/* __ClsGetProperties( nClassHandle ) --> aPropertiesNames
 * Notice that this function works quite similar to __CLASSSEL()
 * except that just returns the name of the datas and methods
 * that have been declared as PROPERTY (or PERSISTENT) */

HB_FUNC( __CLSGETPROPERTIES )
{
   USHORT  uiClass = ( USHORT ) hb_parni( 1 );
   HB_ITEM Return;

   Return.type = HB_IT_NIL;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      USHORT   uiAt     = pClass->uiMethods;
      PMETHOD  pMeth    = pClass->pMethods;
      HB_ITEM  Item;

      Item.type = HB_IT_NIL;

      hb_arrayNew( &Return, 0 );
      /* Create a transfer array  */
      for( uiAt++; --uiAt; pMeth++ )
      {
         if( pMeth->bIsPersistent )
            hb_arrayAddForward( &Return, hb_itemPutC( &Item, pMeth->pMessage->pSymbol->szName ) );
      }
   }

   hb_itemReturnForward( &Return );
}

HB_FUNC( __CLSGETPROPERTIESANDVALUES )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ANY );
   USHORT   uiClass;

   uiClass = hb_objClassH( pObject );

   if( uiClass )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );
      USHORT   uiAt   = pClass->uiMethods;
      PMETHOD  pMeth  = pClass->pMethods;
      PHB_ITEM pTemp;
      HB_ITEM  SubArray, Return;

      Return.type    = HB_IT_NIL;
      SubArray.type  = HB_IT_NIL;

      hb_arrayNew( &Return, 0 );

      for( uiAt++; --uiAt; pMeth++ )
      {
         /* if( pMessage && pClass->pMethods[ uiAt ].bIsPersistent && pClass->pMethods[ uiAt ].uiData )
          */
         if( ( pMeth->uiScope & HB_OO_CLSTP_PUBLISHED || pMeth->bIsPersistent ) && pMeth->uiData )
         {
            hb_arrayNew( &SubArray, 2 );

            pTemp = hb_arrayGetItemPtr( pObject, pMeth->uiData );

            if( pTemp )
               hb_arraySet( &SubArray, 2, pTemp );

            hb_arraySetC( &SubArray, 1, pMeth->pMessage->pSymbol->szName );

            hb_arrayAddForward( &Return, &SubArray );
         }
      }

      hb_itemReturnForward( &Return );
   }
}

HB_FUNC( __CLSGETIVARNAMESANDVALUES )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ANY );
   USHORT   uiScope = ( USHORT ) hb_parni( 2 );
   USHORT   uiClass;

   uiClass = hb_objClassH( pObject );

   if( uiClass )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );
      USHORT   uiAt   = pClass->uiMethods;
      PMETHOD  pMeth  = pClass->pMethods;
      PHB_ITEM pTemp;

      HB_ITEM  Return;
      HB_ITEM  SubArray;

      Return.type   = HB_IT_NIL;
      SubArray.type = HB_IT_NIL;

      hb_arrayNew( &Return, 0 );

      for( uiAt++; --uiAt; pMeth++ )
      {
         if( pMeth->uiData && ( uiScope == 0 || pMeth->uiScope & uiScope ) )
         {
            if( pMeth->pFunction == hb___msgGetData ||
                pMeth->pFunction == hb___msgGetClsData ||
                pMeth->pFunction == hb___msgGetShrData )
            {
               hb_arrayNew( &SubArray, 2 );
               hb_arraySetC( &SubArray, 1, pMeth->pMessage->pSymbol->szName );

               pTemp = hb_arrayGetItemPtr( pObject, pMeth->uiData );

               if( pTemp )
                  hb_arraySet( &SubArray, 2, pTemp );

               hb_arrayAddForward( &Return, &SubArray );
            }
         }
      }

      hb_itemReturnForward( &Return );
   }
}

PHB_DYNS hb_clsSymbolFromFunction( PHB_ITEM pObject, PHB_FUNC pFunction )
{
   USHORT uiClass;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsSymbolFromFunction(%p, %p)", pObject, pFunction ) );

   uiClass = hb_objClassH( pObject );

   if( uiClass )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      USHORT   ui       = pClass->uiMethods;
      PMETHOD  pMethod  = pClass->pMethods;

      for( ui++; --ui; pMethod++ )
      {
         if( pMethod->pFunction == pFunction )
            /* printf( "Function %i Name: %s\n", pFunction, pClass->pMethods[ ui ].pMessage->pSymbol->szName );
             */
            return pMethod->pMessage;
      }
   }

   return NULL;
}

/*
 * <nPtr> := HB_ObjMsgPtr( <oObj>, <cMessage> )
 */
HB_FUNC( HB_OBJMSGPTR )
{
   HB_THREAD_STUB_API

   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( pObject && pString )
   {
      PHB_DYNS pDynSym = NULL;

      hb_objGetMessage( pObject, pString->item.asString.value, &pDynSym );

      hb_retptr( ( void * ) pDynSym->pSymbol );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_ObjMsgPtr", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/* ================================================ */

/* JC1: get class handle from name
 *
 * Param: the name of the class
 * output: the position of the class in the array of classes, or nothing.
 */
UINT hb_clsGetHandleFromName( const char * szClassName )
{
   PCLASS   start = s_pClasses;
   USHORT   uPos  = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_clsGetHandleFromName(%s)", szClassName ) );

   while( s_uiClasses > uPos && strcmp( start->szName, szClassName ) != 0 )
   {
      uPos++;
      start++;
   }

   if( uPos == s_uiClasses )
      return 0;

   return uPos + 1;
}

HB_FUNC( __CLSGETHANDLEFROMNAME )
{
   HB_THREAD_STUB_API

   char * szClass = hb_strUpperCopy( ( char * ) hb_parcx( 1 ), hb_parclen( 1 ) );
   USHORT uiClass = ( USHORT ) hb_clsGetHandleFromName( szClass );

   if( uiClass == 0 )
   {
      if( ( strcmp( szClass, "ARRAY" ) == 0 ) ||
          ( strcmp( szClass, "BLOCK" ) == 0 ) ||
          ( strcmp( szClass, "CHARACTER" ) == 0 ) ||
          ( strcmp( szClass, "DATE" ) == 0 ) ||
          ( strcmp( szClass, "LOGICAL" ) == 0 ) ||
          ( strcmp( szClass, "NIL" ) == 0 ) ||
          ( strcmp( szClass, "NUMERIC" ) == 0 ) ||
          ( strcmp( szClass, "POINTER" ) == 0 ) )
      {
         /* szClass is already pushed on stack as Param #1, so this hack is possible.
          * __clsNew( szClass, 0, 0, 0 );
          */
         HB_FUNCNAME( __CLSNEW ) ();
         uiClass = s_uiClasses;
      }
   }

   hb_xfree( szClass );

   hb_retni( uiClass );
}

void hb_clsSetModule( USHORT uiClass )
{
   HB_THREAD_STUB

   if( uiClass && uiClass <= s_uiClasses )
      ( s_pClasses + ( uiClass - 1 ) )->pClassFuncSym = HB_GETSYM();
}

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * Activate Class Scoping
 *       TRUE  - Class Scoping is active (Default)
 *       FALSE - for debugging purpose and to retrieve data without get scope error
 *
 * <bOldClsScope> := __SetClassScope( <bNewClsScope> )
 */
HB_FUNC( __SETCLASSSCOPE )
{
   HB_THREAD_STUB_API

   BOOL bOldClsScope = s_bClsScope;

   if( ISLOG( 1 ) )
      s_bClsScope = hb_parl( 1 );

   hb_retl( bOldClsScope );
}

BOOL hb_clsSetScope( BOOL bClsScope )
{
   BOOL bOldClsScope = s_bClsScope;

   s_bClsScope = bClsScope;
   return bOldClsScope;
}

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * Auto Initialize Class Flag
 *
 * <bOldClsAutoInit> := __SetClassAutoInit( <bNewClsAutoInit> )
 *
 * <bNewClsAutoInit> =
 *       TRUE  - (Default) Auto initialize class with its default constructor
 *               If true class will be initialized calling its first constructor method
 *               i.e.: oWin := TWindow() is equivalent to oWin := TWindow():New()
 *       FALSE - No auto initialization
 *
 */
HB_FUNC( __SETCLASSAUTOINIT )
{
   HB_THREAD_STUB_API

   BOOL bOldClsAutoInit = s_bClsAutoInit;

   if( ISLOG( 1 ) )
      s_bClsAutoInit = hb_parl( 1 );

   hb_retl( bOldClsAutoInit );
}

static BOOL __clsAddFriend( USHORT uiClass, PHB_ITEM pFriend )
{
   if( uiClass <= s_uiClasses && pFriend )
   {
      PCLASS   pClass      = s_pClasses + uiClass - 1;
      PHB_SYMB pFriendSym  = NULL;

      if( HB_IS_OBJECT( pFriend ) )
      {
         USHORT uiFriendClass = ( USHORT ) pFriend->item.asArray.value->uiClass;

         if( uiFriendClass <= s_uiClasses )
            pFriendSym = s_pClasses[ uiFriendClass - 1 ].pClassFuncSym;
      }
      else if( HB_IS_STRING( pFriend ) )
      {
         char *   szFriendClass  = hb_strUpperCopy( hb_itemGetCPtr( pFriend ), hb_itemGetCLen( pFriend ) );
         USHORT   uiFriendClass  = ( USHORT ) hb_clsGetHandleFromName( szFriendClass );

         hb_xfree( szFriendClass );

         if( uiFriendClass <= s_uiClasses )
            pFriendSym = s_pClasses[ uiFriendClass - 1 ].pClassFuncSym;
      }
      else if( HB_IS_POINTER( pFriend ) )
         pFriendSym = ( PHB_SYMB ) pFriend->item.asPointer.value;

      if( pFriendSym )
      {
         hb_clsAddFriendSymbol( pClass, hb_vmGetRealFuncSym( pFriendSym ) );
         return TRUE;
      }
   }
   return FALSE;
}

HB_FUNC( __CLSADDFRIEND )
{
   __clsAddFriend( ( USHORT ) hb_parni( 1 ), hb_param( 2, HB_IT_OBJECT | HB_IT_POINTER | HB_IT_STRING ) );
}

/* __ClsFriendly( oObj, oFriend | @FriendFunc() ) -> aFriendSymbols  Is it really useful?
 */
HB_FUNC( __CLSFRIENDLY )
{
   HB_THREAD_STUB_API

   PHB_ITEM pObj    = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pFriend = hb_param( 2, HB_IT_OBJECT | HB_IT_POINTER );

   if( pObj && HB_IS_OBJECT( pObj ) )
   {
      USHORT   uiClass  = pObj->item.asArray.value->uiClass;
      PCLASS   pClass   = s_pClasses + uiClass - 1;

      __clsAddFriend( uiClass, pFriend );

      if( uiClass <= s_uiClasses )
      {
         hb_reta( pClass->uiFriendSyms );

         if( pClass->uiFriendSyms )
         {
            PHB_ITEM pArray  = hb_param( -1, HB_IT_ANY );
            PHB_ITEM pHClass = hb_itemPutNI( NULL, 0 );
            USHORT   ui;

            for( ui = 1; ui <= pClass->uiFriendSyms; ui++ )
            {
               hb_itemPutSymbol( pHClass, pClass->pFriendSyms[ ui - 1 ] );
               hb_arraySet( pArray, ui, pHClass );
            }

            hb_itemRelease( pHClass );
         }
      }
   }
}

HB_FUNC( __CLSASSOCTYPE )
{
   USHORT         uiClass  = ( USHORT ) hb_parni( 1 );
   const char *   szType   = hb_parcx( 2 );

   if( __cls_CntMethods( uiClass, hb___msgGetData ) )
   {
      hb_errRT_BASE( EG_ARG, 3005, "Scalar class can not contain any datas", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   if( strcmp( szType, "ARRAY" ) == 0 )
      hb_cls_uiArrayClass = uiClass;
   else if( strcmp( szType, "BLOCK" ) == 0 )
      hb_cls_uiBlockClass = uiClass;
   else if( strcmp( szType, "CHARACTER" ) == 0 )
      hb_cls_uiCharacterClass = uiClass;
   else if( strcmp( szType, "DATE" ) == 0 )
      hb_cls_uiDateClass = uiClass;
   else if( strcmp( szType, "TIMEFLAG" ) == 0 )
      hb_cls_uiTimeFlagClass = uiClass;
   else if( strcmp( szType, "LOGICAL" ) == 0 )
      hb_cls_uiLogicalClass = uiClass;
   else if( strcmp( szType, "NIL" ) == 0 )
      hb_cls_uiNilClass = uiClass;
   else if( strcmp( szType, "NUMERIC" ) == 0 )
      hb_cls_uiNumericClass = uiClass;
   else if( strcmp( szType, "POINTER" ) == 0 )
      hb_cls_uiPointerClass = uiClass;
   else if( strcmp( szType, "HASH" ) == 0 )
      hb_cls_uiHashClass = uiClass;
   else
      hb_errRT_BASE( EG_ARG, 3005, "Unsupported scalar type", "__CLSASSOCTYPE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

HB_FUNC( __CLSCNTCLASSES )
{
   HB_THREAD_STUB_API

   hb_retni( ( int ) s_uiClasses );
}

HB_FUNC( __CLSINSTNAME )
{
   HB_THREAD_STUB_STACK

   const char * szClassName = hb_parc( 1 );

   if( szClassName )
   {
      USHORT   uiClass;
      PHB_DYNS pDynSym;

      uiClass = ( USHORT ) hb_clsGetHandleFromName( szClassName );
      /* TraceLog( NULL, "uiClass: %i\n", uiClass );
       */

      if( uiClass && uiClass <= s_uiClasses )
      {
         hb_clsInst( uiClass, hb_stackReturnItem() );
         /* TraceLog( NULL, "INSTANCIATE uiClass: %i Return: %i\n", uiClass, hb_stackReturnItem()->type );
          */
         return;
      }

      pDynSym = hb_dynsymFind( szClassName );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
      {
         /* TraceLog( NULL, "Class Function: %s\n", szClassName );
          */
         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();
         hb_vmDo( 0 );
      }
      else
         hb_errRT_BASE( EG_ARG, 3008, "Class name is not defined and is not a public function", "__ClsInstName()", 1, hb_paramError( 1 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3007, "Invalid class name", "__ClsInstName()", 1, hb_paramError( 1 ) );
}

/* Harbour equivalent for Clipper internal __mdCreate() */
USHORT hb_clsCreate( USHORT usSize, const char * szClassName )
{
   return hb_clsNew( szClassName, usSize, 0, NULL, NULL, TRUE );
}

/* Harbour equivalent for Clipper internal __mdAdd() */
void hb_clsAdd( USHORT usClassH, const char * szMethodName, PHB_FUNC pFuncPtr )
{
   /* PHB_SYMB pExecSym;
    */

   /*
    * We can use empty name "" for this symbol in hb_symbolNew()
    * It's only envelop for function with additional execution
    * information for HVM not registered symbol. [druzus]
    */
   /*pExecSym = hb_symbolNew( "" );
      pExecSym->value.pFunPtr = pFuncPtr;*/

   hb_clsAddMsg( usClassH, szMethodName, ( void * ) pFuncPtr /*pExecSym*/, 0,
                 HB_OO_MSG_METHOD, 0, HB_OO_CLSTP_PFUNC, FALSE, NULL, NULL,
                 TRUE, FALSE );
}

void hb_clsAddData( USHORT usClassH, const char * szDataName, USHORT uiID )
{
   hb_clsAddMsg( usClassH, szDataName, NULL, uiID, HB_OO_MSG_DATA, 0,
                 HB_OO_CLSTP_EXPORTED, FALSE, NULL, NULL, TRUE, FALSE );
}

/* Harbour equivalent for Clipper internal __mdAssociate() */
void hb_clsAssociate( USHORT usClassH )
{
   HB_THREAD_STUB_API

   hb_clsInst( usClassH, hb_stackReturnItem() );
}

void hb_clsAddDataInit( USHORT usClassH, const char * szDataName, USHORT uiID, PHB_ITEM pInit )
{
   hb_clsAddMsg( usClassH, szDataName, NULL, uiID, HB_OO_MSG_DATA, 0,
                 HB_OO_CLSTP_EXPORTED, FALSE, NULL, pInit, TRUE, FALSE );
}
