/*
 * $Id: classes.c,v 1.81 2003/11/08 00:14:22 jonnymind Exp $
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
 * Copyright 2003 Giancarlo Niccolai <antispam /at/ niccolai [dot] ws>
 *    hb_objGetPropValue()
 *    hb_objSetPropValue()
 *      Functions to mask HB_VM_STACK.Return in MT programming.
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
 "=="    = __OpEqual
 "="     = __OpEqual (same as "==")
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
 *
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include <math.h>

#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hboo.ch"
#include "classes.h"

#include <ctype.h>             /* For toupper() */

/* DEBUG only*/
/* #include <windows.h> */

//#define DEBUG_HASH

#define BUCKET                   5
#define BASE_METHODS   BUCKET * 20  /* Incerement unit of number of messages */
#define HASH_KEY       ( BASE_METHODS / BUCKET )

#ifndef HB_NO_PROFILER
   extern BOOL hb_bProfiler; /* profiler activity status */
#endif

static PCLASS   s_pClasses     = NULL;
static USHORT   s_uiClasses    = 0;
static PHB_DYNS s_msgClassName = NULL;

static PHB_DYNS s_msgClassH    = NULL;
static PHB_DYNS s_msgEval      = NULL;
static PHB_DYNS s_msgClassSel  = NULL;
static PHB_DYNS s_msgClassFullSel  = NULL;
static PHB_DYNS s_msgClsParent = NULL;
static BOOL     s_bClsScope    = TRUE;
static BOOL     s_bClsAutoInit = TRUE;
/* static PHB_DYNS s_msgClass     = NULL; */

/* All functions contained in classes.c */

static PHB_ITEM hb_clsInst( USHORT uiClass );
static BOOL     hb_clsValidScope( PHB_ITEM pObject, PMETHOD pMethod, int iOptimizedSend );
BOOL     hb_clsIsParent( USHORT uiClass, char * szParentName );
static void     hb_clsDictRealloc( PCLASS pClass );
static void     hb_clsRelease( PCLASS );

static HARBOUR  hb___msgClsH( void );
static HARBOUR  hb___msgClsName( void );
static HARBOUR  hb___msgClsFullSel( void );
static HARBOUR  hb___msgClsSel( void );
/* static HARBOUR  hb___msgClass( void ); */
static HARBOUR  hb___msgSuper( void );
static HARBOUR  hb___msgEvalInline( void );
static HARBOUR  hb___msgClsParent( void );
static HARBOUR  hb___msgEval( void );
static HARBOUR  hb___msgVirtual( void );

HARBOUR  hb___msgGetClsData( void );
HARBOUR  hb___msgSetClsData( void );
HARBOUR  hb___msgGetShrData( void );
HARBOUR  hb___msgSetShrData( void );
HARBOUR  hb___msgGetData( void );
HARBOUR  hb___msgSetData( void );

USHORT MsgToNum( const char *ptr, USHORT uiHashKey )
{
   int val = 0;

   while( *ptr )
   {
      int tmp;

      val = ( val << 4 ) + *ptr;

      if( ( tmp = ( val & 0xf0000000 ) ) != 0 )
      {
         val = val ^ ( tmp >> 24 );
         val = val ^ tmp;
      }

      ptr++;
   }

   if( val < 0 )
   {
      val = -val;
   }

   //printf( "Val: %i Mult: %f Mod: %f Floor: %f Key: %i\n", val, val * 0.618, fmod( (double) ( val * 0.618 ), (double) 1 ), floor( fmod( (double) ( val * 0.618 ), (double) 1 ) * uiHashKey ), uiHashKey );

   return val % uiHashKey; //(USHORT) floor( fmod( (double) ( val * 0.618 ), (double) 1 ) * uiHashKey ) ;
}

/* ================================================ */

/*
 * hb_clsDictRealloc( PCLASS )
 *
 * Realloc (widen) class
 */

static void hb_clsDictRealloc( PCLASS pClass )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_clsDictRealloc(%p)", pClass));

   if( pClass )
   {
      PMETHOD pNewMethods;
      USHORT  uiNewHashKey = pClass->uiHashKey;
      USHORT  ui;
      USHORT  uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

      do
      {
         uiNewHashKey += ( USHORT ) HASH_KEY ;

         #ifdef DEBUG_HASH
            printf( "ReAllocating: '%s' Hashkey: %i Bucket: %i NewHashkey: %i\n", pClass->szName, pClass->uiHashKey, BUCKET, uiNewHashKey );
         #endif

         HB_TRACE( HB_TR_DEBUG, ( "ReAllocating: '%s' Hashkey: %i Bucket: %i NewHashkey: %i\n", pClass->szName, pClass->uiHashKey, BUCKET, uiNewHashKey ) );

         pNewMethods = ( PMETHOD ) hb_xgrab( uiNewHashKey * BUCKET * sizeof( METHOD ) );
         memset( pNewMethods, 0, uiNewHashKey * BUCKET * sizeof( METHOD ) );


         for( ui = 0; ui < uiLimit; ui++ )
         {
            PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ ui ].pMessage;

            if( pMessage )
            {
               USHORT uiBucket;
               USHORT uiAt = ( USHORT ) ( MsgToNum( pMessage->pSymbol->szName, uiNewHashKey ) * BUCKET );

               for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
               {
                   if( pNewMethods[ uiAt + uiBucket ].pMessage == 0 ) /* this message position is empty */
                   {
                      hb_xmemcpy( pNewMethods + (uiAt+uiBucket), pClass->pMethods + ui, sizeof( METHOD ) );
                      break;
                   }
               }

               /* Not enough go back to the beginning */
               if( uiBucket >= BUCKET ) /*&& nOccurs++ < 5)*/
               {
                 #ifdef DEBUG_HASH
                    printf( "3 Bucket: %i Hashkey: %i At: %i\n", BUCKET, pClass->uiHashKey, uiAt );
                 #endif

                  hb_xfree( pNewMethods );
                  break;
               }
            }
         }

      } while( ui < uiLimit );


      pClass->uiHashKey = uiNewHashKey;
      hb_xfree( pClass->pMethods );
      pClass->pMethods = pNewMethods;

   }
}

/*
 * hb_clsRelease( <pClass> )
 *
 * Release a class from memory
 */
static void hb_clsRelease( PCLASS pClass )
{
   USHORT uiAt;
   USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
   PMETHOD pMeth = pClass->pMethods;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsRelease(%p)", pClass));

   for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
   {
     if( pMeth->pInitValue )
     {
        hb_itemRelease( pMeth->pInitValue );
     }
   }

   hb_xfree( pClass->szName );
   hb_xfree( pClass->pMethods );

   hb_itemRelease( pClass->pClassDatas );
   hb_itemRelease( pClass->pInlines );
}


/*
 * hb_clsReleaseAll()
 *
 * Release all classes
 */
void hb_clsReleaseAll( void )
{
   SHORT uiClass;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsReleaseAll()"));

   for( uiClass = 0 ; uiClass < s_uiClasses ; uiClass++ )
   {
      /*-----------------12/21/2001 7:51PM-----------------
       * Class itself is an Array within the list of the GC,
       * it does not require explicit release, when using
       * hb_gcReleaseAll(). The class methods pInitValue,
       * are items also within the GC control.
       * UNCOMMENT if reverting to hb_gcCollectAll()
       * --------------------------------------------------*/
      hb_clsRelease( s_pClasses + uiClass  );

     /*-----------------1/2/2002 10:17PM-----------------
      * These 2 lines have to be commented if uncommenting
      * the hb_clsRelease() call above.
      * --------------------------------------------------*/
      //hb_xfree( ( s_pClasses + uiClass )->szName );
      //hb_xfree( ( s_pClasses + uiClass )->pMethods );

      /*-----------------12/21/2001 7:53PM-----------------
       * The pClassDatas and pInlines are Arrays within the
       * list of the GC, they do not require explicit
       * release when using hb_gcReleaseAll().
       * UNCOMMENT if reverting to hb_gcCollectAll()
       * --------------------------------------------------*/
      //hb_itemRelease( ( s_pClasses + uiClass )->pClassDatas );
      //hb_itemRelease( ( s_pClasses + uiClass )->pInlines );
   }

   if( s_pClasses )
   {
      hb_xfree( s_pClasses );
   }

   s_uiClasses = 0;
   s_pClasses  = NULL;
}

/* Mark all internal data as used so it will not be released by the
 * garbage collector
 */

void hb_clsIsClassRef( void )
{
   USHORT uiClass = s_uiClasses;
   PCLASS pClass = s_pClasses;
   USHORT uiAt;
   USHORT uiLimit;
   PMETHOD pMeth;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsIsClassRef()"));

   while( uiClass-- )
   {
      if( pClass->pInlines )
      {
         hb_gcItemRef( pClass->pInlines );
      }

      if( pClass->pClassDatas )
      {
         hb_gcItemRef( pClass->pClassDatas );
      }

      uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {
         if( pMeth->pInitValue )
         {
            hb_gcItemRef( pMeth->pInitValue );
         }
      }

      ++pClass;
   }
}

static BOOL hb_clsValidScope( PHB_ITEM pObject, PMETHOD pMethod, int iOptimizedSend )
{
   USHORT uiScope;

   // Checking scope. s_bClsScope is defined FALSE to retrieve values without scope violation
   // i.e. during debug or in designer mode
   if ( !s_bClsScope ) return TRUE;

   uiScope = pMethod->uiScope;

   //#define DEBUG_SCOPE

   if( uiScope & HB_OO_CLSTP_READONLY && iOptimizedSend != 2 )
   {
      // Allow anyway if all we do is read a property.
      if( pMethod->pMessage->pSymbol->szName[0] != '_' )
      {
         return TRUE;
      }
   }

  if( ( uiScope & HB_OO_CLSTP_PROTECTED ) || ( uiScope & HB_OO_CLSTP_HIDDEN ) || ( uiScope & HB_OO_CLSTP_READONLY ) )
   {
      HB_THREAD_STUB

      PHB_ITEM *pBase = HB_VM_STACK.pBase;
      PHB_ITEM pCaller;
      PCLASS pClass = s_pClasses + ( pObject->item.asArray.value->uiClass - 1 ), pRealClass;

      // ----------------- Get the Caller Symbol -----------------
      if( iOptimizedSend == 0 )
      {
         // Outer function level.
         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
      }

      if( strcmp( ( *pBase )->item.asSymbol.value->szName, "__OBJSENDMSG" ) == 0 ||
          strcmp( ( *pBase )->item.asSymbol.value->szName, "ASCAN" ) == 0 )
      {
         // Backtrack 1 level.
         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
      }

      // ----------------- Get the Caller Self -----------------

      if( strcmp( ( *pBase )->item.asSymbol.value->szName, "AEVAL" ) == 0 )
      {
        pCaller = *( pBase + 1 + 2 );
      }
      else
      {
        pCaller = *( pBase + 1 );
      }

      // ----------------- Get the Real Module -----------------

      /*
        If Method is Inerited then the Module Name may be SAME because compared to THIS INHERTED Object
        but the PARENT Class may be defined in a SEPERATE Module
       */
      if( uiScope & HB_OO_CLSTP_SUPER )
      {
         USHORT uiSuperClass = hb_objGetRealCls( pObject, pMethod->pMessage->pSymbol->szName );

         if( uiSuperClass )
         {
            pRealClass = s_pClasses + ( uiSuperClass - 1 );
         }
      }
      else
      {
         pRealClass = pClass;
      }


      // ----------------- Compare Modules -----------------

      if( HB_IS_OBJECT( pCaller ) )
      {
         PCLASS pCallerClass = s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 );

         if( pRealClass )
         {
            if( pRealClass->pModuleSymbols == NULL || pCallerClass->pModuleSymbols == NULL )
            {
               // TraceLog( NULL, "Oops! Method: '%s' Class: '%s' Caller: '%s'\n", pMethod->pMessage->pSymbol->szName, pRealClass->szName, pCallerClass->szName );
            }
            else if( pRealClass->pModuleSymbols == pCallerClass->pModuleSymbols )
            {
               // TraceLog( NULL, "Same as Parent Module: %s\n", hb_vmFindModule( pRealClass->pModuleSymbols )->szModuleName );
               return TRUE;
            }
            #ifdef DEBUG_SCOPE
            else
            {
               printf( "SuperModule: '%s' CallerModule: '%s'\n", pRealClass->pModuleSymbols->szModuleName, pCallerClass->pModuleSymbols->szModuleName );
            }
            #endif
         }
      }
      else if( HB_IS_BLOCK( pCaller ) )
      {
         PSYMBOLS pBlockModuleSymbols = hb_vmFindModule( pCaller->item.asBlock.value->pSymbols );

         if( pRealClass->pModuleSymbols == NULL || pBlockModuleSymbols == NULL )
         {
            // TraceLog( NULL, "Oops! Method: '%s' Class: '%s' Caller: '%s'\n", pMethod->pMessage->pSymbol->szName, pRealClass->szName, pCaller->item.asBlock.value->procname );
         }
         else
         {
            #ifdef DEBUG_SCOPE
               printf( "SuperModule: '%s' CallerModule: '%s'\n", pRealClass->pModuleSymbols->szModuleName, pBlockModuleSymbols->szModuleName );
            #endif

            // Same module as the module where the Super Method is defined.
            if( pRealClass->pModuleSymbols && pRealClass->pModuleSymbols == pBlockModuleSymbols )
            {
               // TraceLog( NULL, "Same as Parent Module: %s\n", hb_vmFindModule( pRealClass->pModuleSymbols )->szModuleName );
               return TRUE;
            }
         }
      }
      else
      {
         if( pRealClass->pModuleSymbols == NULL || (*pBase)->item.asSymbol.value->pDynSym->pModuleSymbols == NULL )
         {
            // TraceLog( NULL, "Oops! Method: '%s' Class: '%s' Caller: '%s'\n", pMethod->pMessage->pSymbol->szName, pRealClass->szName, (*pBase)->item.asSymbol.value->szName );
         }
         else
         {
            #ifdef DEBUG_SCOPE
               printf( "SuperModule: '%s' CallerModule: '%s'\n", pRealClass->pModuleSymbols->szModuleName, (*pBase)->item.asSymbol.value->pDynSym->pModuleSymbols->szModuleName );
            #endif

            // Same module as the module where the Super Method is defined.
            if( pRealClass->pModuleSymbols && pRealClass->pModuleSymbols == (*pBase)->item.asSymbol.value->pDynSym->pModuleSymbols )
            {
               // TraceLog( NULL, "Same as Parent Module: %s\n", hb_vmFindModule( pRealClass->pModuleSymbols )->szModuleName );
               return TRUE;
            }
         }
      }

      // ----------------- Validate Scope -----------------

      #ifdef DEBUG_SCOPE
         printf( "Method: '%s' Scope: %i\n\r", pMethod->pMessage->pSymbol->szName, uiScope );
      #endif

      #ifdef CLASSY_SCOPE
         // Outer while in Inline, Eval() or aEval().
         while( ( HB_IS_BLOCK( *( pBase + 1 ) ) || strcmp( ( *pBase )->item.asSymbol.value->szName, "AEVAL" ) == 0 ) )
         {
            pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
         }
      #else
         if( HB_IS_BLOCK( pCaller ) )
         {
            char *szCaller;
            char *pAt;

            if( pCaller->item.asBlock.value->pSelfBase == NULL )
            {
               szCaller = pCaller->item.asBlock.value->procname;

               pAt = strchr( szCaller, ':' );

               if( pAt )
               {
                  // Same class.
                  if( strncmp( szCaller, pClass->szName, pAt - szCaller ) == 0 )
                  {
                     return TRUE;
                  }
               }
               else
               {
                  // Block is a Data of Object initialized at Class Creation Function.
                  if( strcmp( szCaller, pClass->szName ) == 0 )
                  {
                     return TRUE;
                  }
               }
            }
            else
            {
               // pObject and the HB_QSelf() of block are same class.
               if( strcmp( pClass->szName, ( s_pClasses + ( pCaller->item.asBlock.value->pSelfBase->uiClass - 1 ) )->szName ) == 0 )
               {
                  return TRUE;
               }
            }

            // Either NOT same Class, or Block was created from NON Method, or INLINE Method!
            if( pBase != HB_VM_STACK.pItems )
            {
               do
               {
                  // Backtrack 1 level incase it's INLINE Method.
                  pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
               }
               while( ( HB_IS_BLOCK( *( pBase + 1 ) ) && pBase != HB_VM_STACK.pItems ) );

               pCaller = *( pBase + 1 );
            }
         }
      #endif

      if( HB_IS_OBJECT( pCaller ) )
      {
         #ifdef DEBUG_SCOPE
            printf( "Object: %s, Caller: %s\n", ( pClass->szName, ( s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 ) )->szName );
         #endif

         // This is an Inherited Method
         if( uiScope & HB_OO_CLSTP_SUPER )
         {
            char *szCallerMessage = (*pBase)->item.asSymbol.value->szName;

            #ifdef DEBUG_SCOPE
               printf( "Object: %s, Message: %s, RealClass: %s, Caller: %s, CallerMessage: %s, CallerMessageClass: %s\n",
                       pClass->szName,
                       pMethod->pMessage->pSymbol->szName,
                       hb_objGetRealClsName( pObject, pMethod->pMessage->pSymbol->szName ),
                       ( s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 ) )->szName,
                       szCallerMessage,
                       hb_objGetRealClsName( pCaller, szCallerMessage ) );
            #endif

            // It's possible that the Caller Message is a Super Messge, and Super is also where this Derived  Method is defined.
            if( hb_objGetRealCls( pObject, pMethod->pMessage->pSymbol->szName ) == hb_objGetRealCls( pCaller, szCallerMessage ) )
            {
               return TRUE;
            }

            // It's possible that caller Method is Derived from a Class which also implements the Message we now validate.
            // This means the validated Message IS avialable within the scope of the Methods that calls this Message.
            if( hb_clsHasMsg( hb_objGetRealCls( pCaller, szCallerMessage ), pMethod->pMessage->pSymbol->szName ) )
            {
               return TRUE;
            }

            // HIDDEN Method can't be called from subclass.
            if( uiScope & HB_OO_CLSTP_HIDDEN )
            {
               goto ScopeError;
            }

            // PROTECTED + READONLY can NOT be written from subclass.
            if( ( uiScope & HB_OO_CLSTP_PROTECTED ) && ( uiScope & HB_OO_CLSTP_READONLY ) )
            {
               goto ScopeError;
            }
            else
            {
               if( uiScope & HB_OO_CLSTP_READONLY )
               {
                  // Derived READONLY Message ( NOT PROTCTED ) allowed.
                  return TRUE;
               }
               else
               {
                  // If we got here, this MUST be a Derived NON HIDDEN, NON READONLY Message, thus a Derived PROTECTED Method.
                  PCLASS pCallerClass = s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 );
                  USHORT uiAt, uiLimit = ( USHORT ) ( pCallerClass->uiHashKey * BUCKET );
                  char *szClassOfMessage = hb_objGetRealClsName( pObject, pMethod->pMessage->pSymbol->szName );

                  #ifdef DEBUG_SCOPE
                     printf( "Defined in: %s\n", szClassOfMessage );
                  #endif

                  // Is the Caller derived from the Object?
                  for( uiAt = 0; uiAt < uiLimit; uiAt++ )
                  {
                     if( ( pCallerClass->pMethods[ uiAt ].uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS )
                     {
                        if( strcmp( pCallerClass->pMethods[ uiAt ].pMessage->pSymbol->szName, szClassOfMessage ) == 0 )
                        {
                           // Derived class - allow access to PROTECTED.
                           return TRUE;
                        }
                     }
                  }
               }
            }
         }
         else
         {
            // NON Inherited Message, and Caller is same class as the object, so all scopes allowed.
            if( pCaller->item.asArray.value->uiClass == pObject->item.asArray.value->uiClass )
            {
               return TRUE;
            }
            else
            {
               PCLASS pCallerClass = s_pClasses + ( pCaller->item.asArray.value->uiClass - 1 );
               USHORT uiAt, uiLimit = ( USHORT ) ( pCallerClass->uiHashKey * BUCKET );
               char *szObjectClass = ( s_pClasses + ( pObject->item.asArray.value->uiClass - 1 ) )->szName;

               // Is the Caller derived from the Object?
               for( uiAt = 0; uiAt < uiLimit; uiAt++ )
               {
                  if( ( pCallerClass->pMethods[ uiAt ].uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS )
                  {
                     if( strcmp( pCallerClass->pMethods[ uiAt ].pMessage->pSymbol->szName, szObjectClass ) == 0 )
                     {
                        if( uiScope & HB_OO_CLSTP_PROTECTED )
                        {
                           if( uiScope & HB_OO_CLSTP_READONLY )
                           {
                              // PROTECTED + READONLY can NOT be written from subclass.
                              goto ScopeError;
                           }
                           else
                           {
                              // PROTECTED (NON READONLY) can be written from subclass.
                              return TRUE;
                           }
                        }

                        // All other scopes are NOT allowed even in derived class.
                        return TRUE;
                     }
                  }
               }
            }
            // This is NOT an Inherted Method, and Caller is Different Class - No restricted scope could be valid (excpet READONLY addressed above)!
         }
      }

      // All else is not allowed.
      ScopeError:
      {
         char szScope[ 64 ];

         szScope[0] = '\0';

         strcat( szScope, "Scope Violation <" );

         if( uiScope & HB_OO_CLSTP_HIDDEN )
         {
            strcat( szScope, "HIDDEN+" );
         }

         if( uiScope & HB_OO_CLSTP_PROTECTED )
         {
            strcat( szScope, "PROTECTED+" );
         }

         if( uiScope & HB_OO_CLSTP_READONLY )
         {
            strcat( szScope, "READONLY+" );
         }

         szScope[ strlen( szScope ) - 1 ] = '>';

         hb_errRT_BASE( EG_NOMETHOD, 1004, szScope, pMethod->pMessage->pSymbol->szName, 1, pObject );

         return FALSE;
      }
   }

   return TRUE;
}

BOOL hb_clsIsParent(  USHORT uiClass, char * szParentName )
{
   USHORT uiAt, uiLimit;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );

      uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

      if( strcmp( pClass->szName, szParentName ) == 0 )
      {
         return TRUE;
      }

      for( uiAt = 0; uiAt < uiLimit; uiAt++)
      {
         if( ( pClass->pMethods[ uiAt ].uiScope & HB_OO_CLSTP_CLASS ) == HB_OO_CLSTP_CLASS )
         {
            if( strcmp( pClass->pMethods[ uiAt ].pMessage->pSymbol->szName, szParentName ) == 0 )
            {
               return TRUE;
            }
         }
      }
   }

   return FALSE;
}

/* ================================================ */

/*
 * <szName> = ( pObject )
 *
 * Get the class name of an object
 *
 */
HB_EXPORT char * hb_objGetClsName( PHB_ITEM pObject )
{
   char * szClassName;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetClsName(%p)", pObject));

   if( HB_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass )
      {
         szClassName = ( s_pClasses + pObject->item.asArray.value->uiClass - 1 )->szName;
      }
      else
      {
         szClassName = "ARRAY";
      }
   }
   else                                         /* built in types */
   {
      switch( pObject->type )
      {
         case HB_IT_NIL:
            szClassName = "NIL";
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

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            szClassName = "NUMERIC";
            break;

         case HB_IT_LOGICAL:
            szClassName = "LOGICAL";
            break;

         default:
            szClassName = "UNKNOWN";
            break;
      }
   }

   return szClassName;
}

/*
 * <szName> = ( pObject )
 *
 * Get the real class name of an object message
 * Will return the class name from wich the message is inherited in case
 * of inheritance.
 *
 */
HB_EXPORT USHORT hb_objGetRealCls( PHB_ITEM pObject, char * szName )
{
   PHB_DYNS pMsg = hb_dynsymFindName( szName );
   USHORT uiClass;
   USHORT uiCurCls;
   USHORT uiClsTree;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetRealCls(%p, %s)", pObject, szName));

   if ( !pMsg )
   {
      return 0;
   }

   uiClass = pObject->item.asArray.value->uiClass;

   /* default value to current class object */
   if( pObject->item.asArray.value->puiClsTree && pObject->item.asArray.value->puiClsTree[0] )
   {
      uiClsTree = pObject->item.asArray.value->puiClsTree[0] ;
      uiCurCls  = pObject->item.asArray.value->puiClsTree[uiClsTree] ;
   }
   else
   {
      uiClsTree = 1;          /* Flag value */
      uiCurCls = uiClass;
   }

   while (uiClsTree)
   {
      if( uiCurCls && uiCurCls <= s_uiClasses )
      {
         PCLASS pClass  = s_pClasses + ( uiCurCls - 1 );
         USHORT uiAt    = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
         USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( uiAt != uiLimit )
         {
            if( pClass->pMethods[ uiAt ].pMessage == pMsg )
            {
               uiClass = (pClass->pMethods + uiAt)->uiSprClass;
               uiClsTree=1; /* Flag Value */
               break;
            }

            uiAt++;

            if( uiAt == uiMask )
            {
               uiAt = 0;
            }
         }
      }

      if( --uiClsTree )
      {
         uiCurCls = pObject->item.asArray.value->puiClsTree[uiClsTree] ;
      }

   }

   return uiClass;
}

HB_EXPORT char * hb_objGetRealClsName( PHB_ITEM pObject, char * szName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_objGetrealClsName(%p, %s)", pObject, szName));

   if( HB_IS_ARRAY( pObject ) )
   {
      if( ! pObject->item.asArray.value->uiClass )
      {
         return "ARRAY";
      }
      else
      {
         USHORT uiClass = hb_objGetRealCls( pObject, szName );

         if( uiClass && uiClass <= s_uiClasses )
         {
            return ( s_pClasses + uiClass - 1 )->szName;
         }
         else
         {
            return "";
         }
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

/*
 * <pFunc> = hb_objGetMethod( <pObject>, <pMessage> )
 *
 * Internal function to the function pointer of a message of an object
 */
HB_EXPORT PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
  return hb_objGetMthd( (PHB_ITEM) pObject, (PHB_SYMB) pMessage, TRUE, NULL, FALSE ) ;
}

HB_EXPORT PHB_FUNC hb_objGetMthd( PHB_ITEM pObject, PHB_SYMB pMessage, BOOL lAllowErrFunc, BOOL *bConstructor, int iOptimizedSend )
{
   USHORT uiClass;
   PHB_DYNS pMsg = pMessage->pDynSym;
   PHB_FUNC pFunction;
   PMETHOD pMethod;

   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetMthd(%p, '%s', %i)", pObject, pMsg->pSymbol->szName, lAllowErrFunc, bConstructor));

   if( HB_IS_ARRAY( pObject ) )
   {
      uiClass = pObject->item.asArray.value->uiClass;
   }
   else
   {
      uiClass = 0;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );
      USHORT uiAt    = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
      USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

      while( uiAt != uiLimit )
      {
         if( pClass->pMethods[ uiAt ].pMessage == pMsg )
         {
            pMethod = pClass->pMethods + uiAt;
            pFunction = pMethod->pFunction;

            if( ! hb_clsValidScope( pObject, pMethod, iOptimizedSend ) )
            {
               // Force NO execution incase error was bypassed.
               pFunction = hb___msgVirtual;
            }

            (HB_VM_STACK.pMethod) = pMethod ;

            #ifndef HB_NO_PROFILER
               if( hb_bProfiler )
               {
                  pMethod->ulCalls++; /* Profiler */
               }
            #endif

            if( bConstructor )
            {
               *bConstructor = ( pMethod->uiScope & HB_OO_CLSTP_CTOR );
            }

            return pFunction;
         }

         uiAt++;

         if( uiAt == uiMask )
         {
            uiAt = 0;
         }
      }
   }

   (HB_VM_STACK.pMethod) = NULL;

   if( bConstructor )
   {
      *bConstructor = FALSE;
   }

   /* Default message here */

   if( s_msgClassName == NULL )
   {
      s_msgClassName = hb_dynsymGet( "CLASSNAME" );  /* Standard messages        */
      s_msgClassH    = hb_dynsymGet( "CLASSH" );     /* Not present in classdef. */
      s_msgClassSel  = hb_dynsymGet( "CLASSSEL" );
      s_msgClassFullSel = hb_dynsymGet( "CLASSFULLSEL" );
      s_msgEval      = hb_dynsymGet( "EVAL" );
      /*s_msgClsParent = hb_dynsymGet( "ISDERIVEDFROM" );*/
      /*s_msgClass     = hb_dynsymGet( "CLASS" );*/
   }

   if( pMsg == s_msgClassName )
      return hb___msgClsName;

   else if( pMsg == s_msgClassH )
      return hb___msgClsH;

   else if( pMsg == s_msgClassSel )
      return hb___msgClsSel;

   else if( pMsg == s_msgClassFullSel )
      return hb___msgClsFullSel;

   else if( pMsg == s_msgEval )
      return hb___msgEval;

   else if( pMsg == s_msgClsParent )
      return hb___msgClsParent;

/* else if( pMsg == s_msgClass )
      return hb___msgClass;       */

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );

      if( lAllowErrFunc && pClass->pFunError )
         return pClass->pFunError;
   }

   return NULL;
}

BOOL hb_clsHasMsg( USHORT uiClass, char *szMsg )
{
   PHB_DYNS pMsg = hb_dynsymFindName( szMsg );

   HB_TRACE(HB_TR_DEBUG, ("hb_clsHasMsg(%i, %s)", uiClass, szMsg));

   if( pMsg && uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );
      USHORT uiAt    = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
      USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

      while( uiAt != uiLimit )
      {
         if( pClass->pMethods[ uiAt ].pMessage == pMsg )
         {
            if( (pClass->pMethods + uiAt)->uiScope & HB_OO_CLSTP_SUPER )
            {
               if( (pClass->pMethods + uiAt)->uiScope & HB_OO_CLSTP_HIDDEN )
               {
                  return FALSE;
               }

               if( (pClass->pMethods + uiAt)->uiScope & HB_OO_CLSTP_READONLY )
               {
                  if( (pClass->pMethods + uiAt)->uiScope && HB_OO_CLSTP_PROTECTED )
                  {
                     return FALSE;
                  }
               }
            }

            //printf( "EXISTs Message: %s in Class %s\n", szMsg, pClass->szName );

            return TRUE;
         }

         uiAt++;

         if( uiAt == uiMask )
         {
            uiAt = 0;
         }
      }
   }

   return FALSE;
}

HB_EXPORT PMETHOD hb_objGetpMethod( PHB_ITEM pObject, PHB_SYMB pMessage )
{
   USHORT uiClass;
   PHB_DYNS pMsg = pMessage->pDynSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetpMethod(%p, %p)", pObject, pMessage));

   if( HB_IS_ARRAY( pObject ) )
   {
      uiClass = pObject->item.asArray.value->uiClass;
   }
   else
   {
      uiClass = 0;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );
      USHORT uiAt    = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
      USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

      while( uiAt != uiLimit )
      {
         if( pClass->pMethods[ uiAt ].pMessage == pMsg )
           return (pClass->pMethods + uiAt) ;

         uiAt++;
         if( uiAt == uiMask )
            uiAt = 0;
      }
   }

   return NULL;
}


/*
 * <uPtr> = hb_objHasMsg( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
HB_EXPORT ULONG hb_objHasMsg( PHB_ITEM pObject, char *szString )
{
   PHB_DYNS pDynSym = hb_dynsymFindName( szString );

   HB_TRACE(HB_TR_DEBUG, ("hb_objHasMsg(%p, %s)", pObject, szString));

   if( pDynSym )
   {
      return ( ULONG ) hb_objGetMthd( pObject, pDynSym->pSymbol, FALSE, NULL, FALSE );
   }
   else
   {
      return 0;
   }
}

// Worker function for HB_FUNC( __CLSADDMSG ).
void hb_clsAddMsg( USHORT uiClass, char *szMessage, long lID_or_FuncPointer_or_BlockPointer, USHORT wType, USHORT uiSprClass, USHORT uiScope, BOOL bPersistent, PHB_ITEM pInit, BOOL bCheckPrefix, BOOL bCase )
{
   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
      PHB_DYNS pMessage ;
      USHORT   uiBucket, uiAt;
      PMETHOD  pNewMeth;

      if( strcmp( "+", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpPlus" ) ;
      }
      else if (strcmp( "-", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpMinus") ;
      }
      else if (strcmp( "*", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpMult" ) ;
      }
      else if (strcmp( "/", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpDivide") ;
      }
      else if (strcmp( "%", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpMod"  ) ;
      }
      else if (strcmp( "^", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpPower") ;
      }
      else if (strcmp( "**", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpPower"  ) ;
      }
      else if (strcmp( "++", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpInc"  ) ;
      }
      else if (strcmp( "--", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpDec"  ) ;
      }
      else if (strcmp( "==", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpEqual") ;
      }
      else if (strcmp( "=", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpEqual") ;
      }
      else if (strcmp( "!=", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpNotEqual") ;
      }
      else if (strcmp( "<>", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpNotEqual") ;
      }
      else if (strcmp( "#", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpNotEqual") ;
      }
      else if (strcmp( "<", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpLess" ) ;
      }
      else if (strcmp( "<=", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpLessEqual") ;
      }
      else if (strcmp( ">", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpGreater") ;
      }
      else if (strcmp( ">=", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpGreaterEqual") ;
      }
      else if (strcmp( ":=", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpAssign") ;
      }
      else if (strcmp( "$", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpInstring") ;
      }
      else if (strcmp( "!", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpNot"    ) ;
      }
      else if (strcmp( ".NOT.", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpNot"    ) ;
      }
      else if (strcmp( ".AND.", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpAnd"    ) ;
      }
      else if (strcmp( ".OR.", szMessage) == 0 )
      {
         pMessage = hb_dynsymGet( "__OpOr"     ) ;
      }
      else
      {
         if( bCase )
         {
            pMessage = hb_dynsymGetCase( szMessage );
         }
         else
         {
            pMessage = hb_dynsymGet( szMessage );
         }
      }

      if( wType == (USHORT) HB_OO_MSG_INLINE && lID_or_FuncPointer_or_BlockPointer == 0l )
      {
         hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSADDMSG", 0 );
      }

      if( pClass->uiMethods > ( pClass->uiHashKey * BUCKET /* * 2 / 3 */ ) )
      {
         #ifdef DEBUG_HASH
            printf( "1 Methods: %i Hashkey: %i Bucket: %i\n", pClass->uiMethods, pClass->uiHashKey, BUCKET );
         #endif

         HB_TRACE( HB_TR_DEBUG, ( "1 Methods: %i Hashkey: %i Bucket: %i\n", pClass->uiMethods, pClass->uiHashKey, BUCKET ) );
         hb_clsDictRealloc( pClass );
      }

      do
      {
         uiAt = ( USHORT ) ( MsgToNum( pMessage->pSymbol->szName, pClass->uiHashKey ) * BUCKET );

         #ifdef DEBUG_HASH
            printf( "Message '%s' Num: %i\n", pMessage->pSymbol->szName, uiAt );
         #endif

         /* Find either the existing message or an open spot for a new message */
         for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
         {
            if( pClass->pMethods[ uiAt + uiBucket ].pMessage == NULL || pClass->pMethods[ uiAt+uiBucket ].pMessage == pMessage )
            {
               break;
            }
         }

         if( uiBucket >= BUCKET )
         {
            #ifdef DEBUG_HASH
               printf( "2 Message: '%s' Class: '%s' Bucket: %i\n", pMessage->pSymbol->szName, pClass->szName, BUCKET );
            #endif

            HB_TRACE( HB_TR_DEBUG, ( "2 Message '%s' At: %i First '%s' Last '%s' Class: '%s' Bucket: %i\n", pMessage->pSymbol->szName, uiAt, pClass->pMethods[ uiAt ].pMessage->pSymbol->szName,pClass->pMethods[ uiAt + 4 ].pMessage->pSymbol->szName, pClass->szName, BUCKET ) );
            hb_clsDictRealloc( pClass );
         }

      } while( uiBucket >= BUCKET );

      pNewMeth = pClass->pMethods + ( uiAt + uiBucket );

      if( ! pNewMeth->pMessage )
      {
         pNewMeth->pMessage = pMessage;
         pClass->uiMethods++;           /* One more message */
      }

      pNewMeth->uiSprClass = uiClass  ; /* now used !! */
      pNewMeth->bClsDataInitiated = 0 ; /* reset state */
      pNewMeth->ulCalls = 0;
      pNewMeth->ulTime = 0;
      pNewMeth->ulRecurse = 0;
      pNewMeth->bIsPersistent = bPersistent;
      pNewMeth->uiType = wType;

      /* in case of re-used message */
      if ( pNewMeth->pInitValue )
      {
         hb_itemRelease(pNewMeth->pInitValue) ;
         pNewMeth->pInitValue = 0 ;
      }

      switch( wType )
      {
         case HB_OO_MSG_METHOD:
            pNewMeth->pFunction = ( PHB_FUNC ) lID_or_FuncPointer_or_BlockPointer;
            pNewMeth->uiScope = uiScope;
            pNewMeth->uiData = 0;
            break;

         case HB_OO_MSG_DATA:
            pNewMeth->uiData = ( USHORT ) lID_or_FuncPointer_or_BlockPointer;
            pNewMeth->uiScope = uiScope;

            if( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' )
            {
               pNewMeth->pFunction  = hb___msgSetData;
            }
            else
            {
               pNewMeth->pFunction  = hb___msgGetData;

               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  if( HB_IS_ARRAY( pInit ) )
                  {
                     pNewMeth->pInitValue = hb_arrayClone( pInit, NULL );
                  }
                  else
                  {
                     pNewMeth->pInitValue = hb_itemNew( NULL );
                     hb_itemCopy( pNewMeth->pInitValue, pInit );
                  }
               }
            }

            break;

         case HB_OO_MSG_CLASSDATA:
            pNewMeth->uiData = ( USHORT ) lID_or_FuncPointer_or_BlockPointer;
            pNewMeth->uiDataShared = pNewMeth->uiData ;

            pNewMeth->uiScope = uiScope;

            if( ( USHORT ) hb_arrayLen( pClass->pClassDatas ) < pNewMeth->uiData )
            {
               hb_arraySize( pClass->pClassDatas, pNewMeth->uiData );
            }

            if( ! ( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' ) )
            {
               if( pInit && ! HB_IS_NIL( pInit ) ) /* Initializer found */
               {
                  if( HB_IS_ARRAY( pInit ) )
                  {
                     pNewMeth->pInitValue = hb_arrayClone( pInit, NULL );
                  }
                  else
                  {
                     pNewMeth->pInitValue = hb_itemNew( NULL );
                     hb_itemCopy( pNewMeth->pInitValue, pInit );
                  }
               }
            }

            if( ( pNewMeth->uiScope & HB_OO_CLSTP_SHARED ) != HB_OO_CLSTP_SHARED )
            {
               if( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' )
               {
                  pNewMeth->pFunction = hb___msgSetClsData;
               }
               else
               {
                  pNewMeth->pFunction = hb___msgGetClsData;
               }
            }
            else
            {
               if( bCheckPrefix && pMessage->pSymbol->szName[ 0 ] == '_' )
               {
                  pNewMeth->pFunction = hb___msgSetShrData;
                  pClass->uiDatasShared++;
               }
               else
               {
                  pNewMeth->pFunction = hb___msgGetShrData;
               }
            }

            break;

         case HB_OO_MSG_INLINE:
            pNewMeth->uiData = ( USHORT ) ( hb_arrayLen( pClass->pInlines ) + 1 );
            pNewMeth->uiScope = uiScope;
            hb_arraySize( pClass->pInlines, pNewMeth->uiData );
            hb_arraySet( pClass->pInlines, pNewMeth->uiData, (PHB_ITEM) lID_or_FuncPointer_or_BlockPointer );
            pNewMeth->pFunction = hb___msgEvalInline;
            break;

         case HB_OO_MSG_VIRTUAL:
            pNewMeth->pFunction = hb___msgVirtual;
            break;

         case HB_OO_MSG_SUPER:
            pNewMeth->uiData = ( USHORT ) lID_or_FuncPointer_or_BlockPointer;
            pNewMeth->uiSprClass = ( USHORT ) uiSprClass; /* store the super handel */
            pNewMeth->uiScope = uiScope;
            pNewMeth->pFunction = hb___msgSuper;
            break;

         case HB_OO_MSG_ONERROR:
            pClass->pFunError = ( PHB_FUNC ) lID_or_FuncPointer_or_BlockPointer;
            break;

         default:
            hb_errInternal( HB_EI_CLSINVMETHOD, NULL, "__clsAddMsg", NULL );
            break;
      }
   }
}

/*
 * __clsAddMsg( <hClass/pObject>, <cMessage>, <pFunction>, <nType>, [xInit], <uiScope>, <lPersistent> )
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
 *
 * <nType>     see HB_OO_MSG_*
 *
 * <xInit>     HB_OO_MSG_DATA      : Optional initializer for DATA
 *             HB_OO_MSG_CLASSDATA : Optional initializer for DATA
 *             HB_OO_MSG_SUPER     : Index number in array (for instance SuperObject)
 *
 * <uiScope>   HB_OO_CLSTP_EXPORTED        1 : default for data and method
 *             HB_OO_CLSTP_PROTECTED       2 : method or data protected
 *             HB_OO_CLSTP_HIDDEN          4 : method or data hidden
 *             HB_OO_CLSTP_CTOR            8 : method constructor
 *             HB_OO_CLSTP_READONLY       16 : data read only
 *             HB_OO_CLSTP_SHARED         32 : (method or) data shared
 *             HB_OO_CLSTP_CLASS          64 : message is the name of a superclass
 *             HB_OO_CLSTP_SUPER         128 : message is herited
 *             HB_OO_CLSTP_CLASSCTOR     256 : Class method constructor
 *             HB_OO_CLSTP_CLASSMETH     512 : Class method
 */
HB_FUNC( __CLSADDMSG )
{
   USHORT   uiClass, uiScope, wType, uiSprClass = 0;
   char     *szMessage, szAssign[ HB_SYMBOL_NAME_LEN ];
   long     lID_or_FuncPointer_or_BlockPointer;
   PHB_ITEM pInit = NULL;
   BOOL     bPersistent, bCase;

   // 1
   uiClass = ( USHORT ) hb_parni( 1 );
   if( uiClass == 0 )
   {
      PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );

      if( pObject )
      {
         uiClass = pObject->item.asArray.value->uiClass;
      }
   }

   // 2
   szMessage = hb_parc( 2 );

   // 3
   lID_or_FuncPointer_or_BlockPointer = (long) hb_param( 3, HB_IT_BLOCK );
   if( lID_or_FuncPointer_or_BlockPointer == 0l )
   {
      lID_or_FuncPointer_or_BlockPointer = hb_parnl( 3 );
   }

   // 4
   wType = ( USHORT ) hb_parni( 4 );

   // 5 Follows below becuase might be using 6, 7 and 8.

   // 6
   uiScope = ( USHORT ) ( ISNUM( 6 ) ? hb_parni( 6 ) : HB_OO_CLSTP_EXPORTED );

   // 7
   bPersistent = hb_parl( 7 );

   // 8
   bCase = hb_parl( 8 );

   // 5
   switch ( wType )
   {
      case HB_OO_MSG_DATA:
      case HB_OO_MSG_CLASSDATA:
         pInit = hb_param( 5, HB_IT_ANY );
         break;

      case HB_OO_MSG_PROPERTY:
      case HB_OO_MSG_CLASSPROPERTY:
         wType -= HB_OO_PROPERTY;

         hb_clsAddMsg( uiClass, szMessage, lID_or_FuncPointer_or_BlockPointer, wType, 0, uiScope, bPersistent, hb_param( 5, HB_IT_ANY ), FALSE, bCase );

         szAssign[0] = '_';
         szAssign[1] = '\0';
         strcat( (char*) szAssign, szMessage );
         szMessage = (char *) szAssign;
         bPersistent = FALSE;
         break;

      default:
         uiSprClass = ( USHORT ) hb_parnl( 5 ); /* store the super handel */
   }

   // Call worker function.
   hb_clsAddMsg( uiClass, szMessage, lID_or_FuncPointer_or_BlockPointer, wType, uiSprClass, uiScope, bPersistent, pInit, TRUE, bCase );
}

/*
 * <hClass> := __clsNew( <cClassName>, <nDatas>, <nMethods>, [ahSuper,aoSuper] )
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <nMethods>   Number of additional Methods in the class
 * <ahSuper>    Optional handle(s) of superclass(es)
 * <ahSuper>    Optional superclass(es) Object instance
 */
HB_FUNC( __CLSNEW )
{
   PCLASS pNewCls;
   ULONG ulSize;  /* USHORT is small. Maximum 409 methods. In some
                           cases it is enough. This eliminate random GPFs
                           in this function for big classes */

   PHB_ITEM pahSuper;
   USHORT i, uiSuper;
   /*USHORT nLenShrDatas = 0;*/
   USHORT nLenClsDatas = 0;
   USHORT nLenInlines = 0;
   USHORT nLenDatas = 0;
   USHORT uiKnownMethods = ( hb_parni(2) * 2 ) + hb_parni(3);

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "__ClsNew( %s, %i, %i, %i )\n", hb_parc(1), hb_parni(2), hb_parni(3), hb_itemSize( hb_itemParam(4) ) ) );

   pahSuper = hb_itemParam( 4 );      /* Replace the initial uiSuper   */
   uiSuper  = ( USHORT ) hb_itemSize( pahSuper ); /* Number of Super class present */

   if( s_pClasses )
   {
      s_pClasses = ( PCLASS ) hb_xrealloc( s_pClasses, sizeof( CLASS ) * ( s_uiClasses + 1 ) );
   }
   else
   {
      s_pClasses = ( PCLASS ) hb_xgrab( sizeof( CLASS ) );
   }

   pNewCls = s_pClasses + s_uiClasses;
   pNewCls->szName = ( char * ) hb_xgrab( hb_parclen( 1 ) + 1 );

   memset( pNewCls->szName, 0, hb_parclen( 1 ) + 1);

   strcpy( pNewCls->szName, hb_parc( 1 ) );
   pNewCls->uiDataFirst = 0;
   pNewCls->uiDatas = 0;
   pNewCls->uiMethods = 0;
   pNewCls->uiDatasShared = 0;
   pNewCls->pModuleSymbols = NULL;

   if( uiSuper )
   {
      for( i = 1; i <= uiSuper; i++ )
      {
         PHB_DYNS pMsg;
         PHB_ITEM pSuper;
         PHB_ITEM pClsAnyTmp;
         USHORT nSuper;
         USHORT ui, uiAt, uiLimit, uiCurrent ;
         PCLASS pSprCls;
         USHORT nLen;
         BOOL bResize ;

         pSuper  =  hb_itemNew( NULL );
         hb_arrayGet( pahSuper, i, pSuper);
         nSuper  = ( USHORT ) hb_itemGetNL( pSuper );
         pSprCls = s_pClasses + ( nSuper - 1 );
         uiLimit = ( USHORT ) ( pSprCls->uiHashKey * BUCKET );

         hb_itemRelease( pSuper );

         pNewCls->uiDataFirst += pSprCls->uiDatas;
         pNewCls->uiDatas      = ( USHORT ) ( pNewCls->uiDataFirst + hb_parni( 2 ) );

         if( i == 1 ) /* This is the first superclass */
         {
            HB_TRACE( HB_TR_DEBUG, ( "Class: %s Super: %s Known: %i + Super %i\n", pNewCls->szName, pSprCls->szName, uiKnownMethods, pSprCls->uiMethods ) );

            pNewCls->uiHashKey = pSprCls->uiHashKey + ( ( ( BASE_METHODS > uiKnownMethods ? BASE_METHODS : uiKnownMethods ) / BUCKET ) * 2 ) ;

            ulSize = pNewCls->uiHashKey * BUCKET * sizeof( METHOD );
            pNewCls->pMethods = ( PMETHOD ) hb_xgrab( ulSize );
            memset( pNewCls->pMethods, 0, ulSize );
            pNewCls->pFunError = pSprCls->pFunError;

            /* CLASS DATA Not Shared ( new array, new value ) */
            pNewCls->pClassDatas  = hb_arrayClone( pSprCls->pClassDatas, NULL );

            pNewCls->pInlines = hb_arrayClone( pSprCls->pInlines, NULL );

            pNewCls->uiDatasShared = pSprCls->uiDatasShared;
         }
         else
         {
            /* Ok add now the previous len to the offset */
            nLenClsDatas  = ( USHORT ) hb_itemSize( pNewCls->pClassDatas );
            nLenInlines   = ( USHORT ) hb_itemSize( pNewCls->pInlines );
            nLenDatas     = ( USHORT ) pNewCls->uiDatas;

            /* ClassDatas */
            pClsAnyTmp = hb_arrayClone( pSprCls->pClassDatas, NULL );
            nLen = ( USHORT ) hb_itemSize( pClsAnyTmp );
            for( ui = 1; ui <= nLen; ui++ )
            {
                PHB_ITEM pTmp = hb_itemNew( NULL );
                hb_arrayGet( pClsAnyTmp, ui, pTmp );
                hb_arrayAdd( pNewCls->pClassDatas, pTmp );
                hb_itemRelease( pTmp );
            }

            hb_itemRelease( pClsAnyTmp );

            /* SharedDatas */
            pNewCls->uiDatasShared += pSprCls->uiDatasShared;

            /* Inlines */
            pClsAnyTmp = hb_arrayClone( pSprCls->pInlines, NULL );
            nLen = ( USHORT ) hb_itemSize( pClsAnyTmp );

            for( ui = 1; ui <= nLen; ui++ )
            {
                PHB_ITEM pTmp = hb_itemNew( NULL );
                hb_arrayGet( pClsAnyTmp, ui, pTmp );
                hb_arrayAdd( pNewCls->pInlines, pTmp );
                hb_itemRelease( pTmp );
            }

            hb_itemRelease( pClsAnyTmp );
         }

         bResize = ( ( pNewCls->uiMethods + pSprCls->uiMethods ) > ( pNewCls->uiHashKey * BUCKET /* * 2 / 3 */ ) ) ;
         uiCurrent = 0 ;

         do
         {
            if( bResize )
            {
               #ifdef DEBUG_HASH
                  printf( "Resize\n" );
               #endif

               HB_TRACE( HB_TR_DEBUG, ( "Resize\n" ) );
               hb_clsDictRealloc( pNewCls );
               bResize=FALSE;
            }

            /* When doing the eventual second pass after call to hb_clsDictRealloc */
            /* We review only messages not already treated */
            for( ui = uiCurrent ; ui < uiLimit; ui++ )
            {
               USHORT uiBucket;

               pMsg = ( PHB_DYNS ) pSprCls->pMethods[ ui ].pMessage;

               if( pMsg )
               {
                  uiAt = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pNewCls->uiHashKey ) * BUCKET );

                  for( uiBucket = 0; uiBucket < BUCKET; uiBucket++ )
                  {
                     /* Ok, this bucket is empty */
                     if( pNewCls->pMethods[ uiAt + uiBucket ].pMessage == 0 )
                     {
                        /* Now, we can increment the msg count */
                        pNewCls->uiMethods++;

                        hb_xmemcpy( pNewCls->pMethods + ( uiAt+uiBucket ), pSprCls->pMethods + ui, sizeof( METHOD ) );

                        if(
                            pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgSetClsData
                            ||
                            pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgGetClsData
                          )
                        {
                           pNewCls->pMethods[ uiAt+uiBucket ].uiData += nLenClsDatas;
                        }

                        if(
                            pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgSetData
                            ||
                            pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgGetData
                            ||
                            pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgSuper
                          )
                        {
                           pNewCls->pMethods[ uiAt+uiBucket ].uiData     += nLenDatas;
                        }

                        if( pNewCls->pMethods[ uiAt+uiBucket ].pFunction ==  hb___msgEvalInline )
                        {
                           pNewCls->pMethods[ uiAt+uiBucket ].uiData += nLenInlines;
                        }

                        if( ( pSprCls->pMethods[ ui ].uiScope & HB_OO_CLSTP_SUPER ) != HB_OO_CLSTP_SUPER )
                        {
                           pNewCls->pMethods[ uiAt+uiBucket ].uiScope = ( USHORT ) ( pSprCls->pMethods[ ui ].uiScope + HB_OO_CLSTP_SUPER );
                        }
                        else
                        {
                           pNewCls->pMethods[ uiAt+uiBucket ].uiScope = pSprCls->pMethods[ ui ].uiScope;
                        }

                        if( pSprCls->pMethods[ ui ].pInitValue )
                        {
                           PHB_ITEM pInitValue;

                           if( HB_IS_ARRAY( pSprCls->pMethods[ ui ].pInitValue ) )
                           {
                              pNewCls->pMethods[ uiAt + uiBucket ].pInitValue = hb_arrayClone( pSprCls->pMethods[ ui ].pInitValue, NULL );
                           }
                           else
                           {
                              pInitValue = hb_itemNew( NULL );

                              hb_itemCopy( pInitValue, pSprCls->pMethods[ ui ].pInitValue );
                              pNewCls->pMethods[ uiAt + uiBucket ].pInitValue = pInitValue;
                           }
                        }

                        break;
                     }
                     else if( pNewCls->pMethods[ uiAt + uiBucket ].pMessage == pMsg )
                     {
                        break;
                     }
                  }

                  /* No space found for this message, call hb_dicrealloc() */
                  if( uiBucket == BUCKET )
                  {
                     bResize = TRUE;
                     uiCurrent = ui ;
                     break;
                  }
               }
            }

         } while ( ui < uiLimit );
      }
   }
   else
   {
      pNewCls->uiDatas      = ( USHORT ) hb_parni( 2 );
      pNewCls->uiDataFirst  = 0;
      pNewCls->uiDatasShared= 0;

      pNewCls->uiMethods    = 0;
      pNewCls->uiHashKey    = ( ( uiKnownMethods > BASE_METHODS ? uiKnownMethods : BASE_METHODS ) / BUCKET ) * 2;

      #ifdef DEBUG_HASH
         printf( "Class '%s' Known: %i Datas: %i Extras %i HashKey: %i Bucket %i\n", pNewCls->szName, uiKnownMethods, pNewCls->uiDatas, hb_parni(3), pNewCls->uiHashKey, BUCKET );
      #endif
      HB_TRACE(HB_TR_DEBUG, ( "Class '%s' Known: %i Datas: %i Extras %i HashKey: %i Bucket %i\n", pNewCls->szName, uiKnownMethods, pNewCls->uiDatas, hb_parni(3), pNewCls->uiHashKey, BUCKET ) );

      pNewCls->pMethods = ( PMETHOD ) hb_xgrab( pNewCls->uiHashKey * BUCKET * sizeof( METHOD ) );
      memset( pNewCls->pMethods, 0, pNewCls->uiHashKey * BUCKET * sizeof( METHOD ) );

      pNewCls->pClassDatas    = hb_itemArrayNew( 0 );
      pNewCls->pInlines       = hb_itemArrayNew( 0 );
      pNewCls->pFunError      = NULL;
   }

   hb_itemRelease( pahSuper );

   HB_TRACE( HB_TR_DEBUG, ( "Finalized: '%s' Known: %i Key: %i\n", pNewCls->szName, uiKnownMethods, pNewCls->uiHashKey ) );

   hb_retni( ++s_uiClasses );
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
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS pClass  = s_pClasses + ( uiClass - 1 );
         USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT uiAt    = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
         USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( ( uiAt != uiLimit ) &&
                ( pClass->pMethods[ uiAt ].pMessage &&
                ( pClass->pMethods[ uiAt ].pMessage != pMsg ) ) )
         {
            uiAt++;
            if( uiAt == uiMask )
               uiAt = 0;
         }
         if( uiAt != uiLimit )
         {                                         /* Requested method found   */
            PHB_FUNC pFunc = pClass->pMethods[ uiAt ].pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method deleted    */
            {
               // Can NOT be deleted or else refernce by number to other Inline blocks will break.
               //hb_arrayDel( pClass->pInlines, pClass->pMethods[ uiAt ].uiData );
               hb_itemClear( pClass->pInlines->item.asArray.value->pItems + pClass->pMethods[ uiAt ].uiData - 1  );
            }
                                                /* Move messages            */
            while( pClass->pMethods[ uiAt ].pMessage && uiAt != uiLimit )
            {
               hb_xmemcpy( pClass->pMethods + uiAt, pClass->pMethods + ( uiAt == uiMask ? 0 : uiAt + 1 ), sizeof( METHOD ) );
               uiAt++;

               if( uiAt == uiMask )
               {
                  uiAt = 0;
               }
            }
            memset( pClass->pMethods + uiAt, 0, sizeof( METHOD ) );
            pClass->uiMethods--;                    /* Decrease number messages */
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
   PHB_ITEM pSelf ;

   pSelf = hb_clsInst( ( USHORT ) hb_parni( 1 ));

   if( pSelf )
   {
      hb_itemRelease( hb_itemReturn( pSelf ) );
   }
}

/*
 * [<o(Super)Object>] := hb_clsInst( <hClass> )
 *
 * Create a (super)object from class definition <hClass>
 */
static PHB_ITEM hb_clsInst( USHORT uiClass )
{
   PHB_ITEM pSelf = NULL;

   if( uiClass <= s_uiClasses )
   {
      PCLASS   pClass = s_pClasses + ( uiClass - 1 );

      USHORT   uiAt;
      USHORT   uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      PMETHOD  pMeth ;

      pSelf = hb_itemNew( NULL );
      hb_arrayNew( pSelf, pClass->uiDatas );

      pSelf->item.asArray.value->uiClass    = uiClass;
      pSelf->item.asArray.value->uiPrevCls  = 0;
      /* pSelf->item.asArray.value->puiClsTree   = ( USHORT * ) hb_xgrab( sizeof( USHORT ) ); */
      /* pSelf->item.asArray.value->puiClsTree[0]=0; */
      pSelf->item.asArray.value->puiClsTree = NULL;

      /* Initialise value if initialisation was requested */
      pMeth = pClass->pMethods;
      for( uiAt = 0; uiAt < uiLimit; uiAt++, pMeth++ )
      {
         /* Init Classdata (inherited and not) if needed */
         if( pMeth->pInitValue )
         {

            if( pMeth->pFunction == hb___msgGetClsData && !( pMeth->bClsDataInitiated ) )
            {
               HB_ITEM init;
               PHB_ITEM pInit;

               ( &init )->type = HB_IT_NIL;

               hb_arrayGet( pClass->pClassDatas, pMeth->uiData, &init );

               if( init.type == HB_IT_NIL )
               {
                  if( HB_IS_ARRAY( pMeth->pInitValue ) )
                  {
                     pInit = hb_arrayClone( pMeth->pInitValue, NULL );
                  }
                  else
                  {
                     pInit = hb_itemNew( NULL );
                     hb_itemCopy( pInit, pMeth->pInitValue );
                  }

                  hb_arraySet( pClass->pClassDatas, pMeth->uiData, pInit );
                  hb_itemRelease( pInit );
                  pMeth->bClsDataInitiated = 1;
               }

               if( HB_IS_COMPLEX( &init ) )
               {
                  hb_itemClear( &init );
               }
            }
            else if( pMeth->pFunction == hb___msgGetData ) /* is a DATA but not herited */
            {
               PHB_ITEM pInitValue ;

               if( HB_IS_ARRAY( pMeth->pInitValue ) )
               {
                  pInitValue = hb_arrayClone( pMeth->pInitValue, NULL );
               }
               else
               {
                  pInitValue = hb_itemNew( NULL );
                  hb_itemCopy(pInitValue,  pMeth->pInitValue );
               }

               hb_arraySet( pSelf, pMeth->uiData, pInitValue );
               hb_itemRelease( pInitValue );
            }
            else if( pMeth->pFunction == hb___msgGetShrData && !( pMeth->bClsDataInitiated ) )
            {
               /* Init Shared Classdata as needed, we only need to init the first */
               /* not inherited classdata array where all shared will point to    */
               HB_ITEM init;
               PHB_ITEM pInit;

               ( &init )->type = HB_IT_NIL;

               hb_arrayGet( pClass->pClassDatas, pMeth->uiData, &init );
               if( init.type == HB_IT_NIL )
               {

                  if( HB_IS_ARRAY( pMeth->pInitValue ) )
                   pInit = hb_arrayClone( pMeth->pInitValue, NULL );
                  else
                  {
                   pInit = hb_itemNew( NULL );
                   hb_itemCopy( pInit, pMeth->pInitValue );
                  }


                  hb_arraySet( pClass->pClassDatas, pMeth->uiData, pInit );
                  hb_itemRelease( pInit );
                  pMeth->bClsDataInitiated = 1;
               }

               if( HB_IS_COMPLEX( &init ) )
               {
                  hb_itemClear( &init );
               }
            }
         }
      }
   }

   return pSelf;
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
   USHORT uiClass;

   if( pObject )
   {
      uiClass = pObject->item.asArray.value->uiClass;
   }
   else
   {
      uiClass = 0;
   }

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
         USHORT   uiAt     = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
         USHORT   uiMask   = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT   uiLimit  = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( ( uiAt != uiLimit ) &&
                ( pClass->pMethods[ uiAt ].pMessage &&
                ( pClass->pMethods[ uiAt ].pMessage != pMsg ) ) )
         {
            uiAt++;
            if( uiAt == uiMask )
               uiAt = 0;
         }

         if( uiAt != uiLimit )
         {                                         /* Requested method found   */
            PHB_FUNC pFunc = pClass->pMethods[ uiAt ].pFunction;

            if( pFunc == hb___msgEvalInline )      /* INLINE method changed    */
            {
               PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );

               if( pBlock == NULL )
               {
                  PHB_FUNC pFunc = (PHB_FUNC) hb_parnl( 3 );

                  if( pFunc ) // Convert to Method.
                  {
                     pClass->pMethods[ uiAt ].pFunction = pFunc;

                     // Clear the inline - can NOT be deleted or else refrence by number to other Inline methods will break.
                     hb_itemClear( pClass->pInlines->item.asArray.value->pItems + pClass->pMethods[ uiAt ].uiData - 1  );
                  }
                  else
                  {
                     hb_errRT_BASE( EG_ARG, 3000, NULL, "__CLSMODMSG", 0 );
                  }
               }
               else
               {
                  hb_arraySet( pClass->pInlines, pClass->pMethods[ uiAt ].uiData, pBlock );
               }
            }
            else if( ( pFunc == hb___msgSetData ) || ( pFunc == hb___msgGetData ) )
            {                                      /* Not allowed for DATA     */
               hb_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", "__CLSMODMSG", 0 );
            }
            else  /* Modify METHOD            */
            {
               PHB_FUNC pFunc = (PHB_FUNC) hb_parnl( 3 );

               if( pFunc )
               {
                  pClass->pMethods[ uiAt ].pFunction = pFunc;
               }
               else /* Convert to INLINE. */
               {
                  PHB_ITEM pBlock = hb_param( 3, HB_IT_BLOCK );

                  pClass->pMethods[ uiAt ].pFunction = hb___msgEvalInline;
                  hb_arrayAdd( pClass->pInlines, pBlock );
                  pClass->pMethods[ uiAt ].uiData = (unsigned short) pClass->pInlines->item.asArray.value->ulLen;
               }
            }
         }
      }
   }
}

HB_FUNC( __CLSMSGASSIGNED )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );
   USHORT uiClass;

   if( pObject )
   {
      uiClass = pObject->item.asArray.value->uiClass;
   }
   else
   {
      uiClass = 0;
   }


   HB_VM_STACK.Return.type = HB_IT_LOGICAL;
   HB_VM_STACK.Return.item.asLogical.value = FALSE;

   if( uiClass && uiClass <= s_uiClasses && pString )
   {
      PHB_DYNS pMsg = hb_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS   pClass   = s_pClasses + ( uiClass - 1 );
         USHORT   uiAt     = ( USHORT ) ( MsgToNum( pMsg->pSymbol->szName, pClass->uiHashKey ) * BUCKET );
         USHORT   uiMask   = ( USHORT ) ( pClass->uiHashKey * BUCKET );
         USHORT   uiLimit  = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );

         while( ( uiAt != uiLimit ) &&
                ( pClass->pMethods[ uiAt ].pMessage &&
                ( pClass->pMethods[ uiAt ].pMessage != pMsg ) ) )
         {
            uiAt++;

            if( uiAt == uiMask )
            {
               uiAt = 0;
            }
         }

         if( uiAt != uiLimit )
         {                                         /* Requested method found   */
            PHB_FUNC pFunc = pClass->pMethods[ uiAt ].pFunction;

            if( pFunc != hb___msgVirtual )         /* NON Virtual method */
            {
               HB_VM_STACK.Return.item.asLogical.value = TRUE;
            }
         }
      }
   }
}

/*
 * <cClassName> := ClassName( <hClass> )
 *
 * Returns class name of <hClass>
 */
HB_FUNC( __OBJGETCLSNAME )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   USHORT uiClass;

   if( pObject && pObject->item.asArray.value->uiClass )
   {
      uiClass = pObject->item.asArray.value->uiClass;

      hb_retc( s_pClasses[ uiClass - 1 ].szName );
   }
   else
   {
      uiClass = ( USHORT ) hb_parni( 1 );

      if( uiClass <= s_uiClasses )
         hb_retc( s_pClasses[ uiClass - 1 ].szName );
      else
         hb_retc( "" );
   }
}


/*
 * <lRet> := __objHasMsg( <oObj>, <cSymbol> )
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
HB_FUNC( __OBJHASMSG )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( pObject && pString )
   {
      hb_retl( hb_objHasMsg( pObject, pString->item.asString.value ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "__ObjHasMsg", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/*
 * <oNew> := __objClone( <oOld> )
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
HB_FUNC( __OBJCLONE )
{
   PHB_ITEM pSrcObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pDstObject ;

   if( pSrcObject )
   {
      pDstObject = hb_arrayClone( pSrcObject, NULL ) ;

      /* pDstObject->item.asArray.value->puiClsTree = NULL; */
      /* pDstObject->item.asArray.value->puiClsTree = ( USHORT * ) hb_xgrab( sizeof( USHORT ) ); */
      /* pDstObject->item.asArray.value->puiClsTree[0]=0; */

      hb_itemRelease( hb_itemReturn( pDstObject ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3001, NULL, "__OBJCLONE", 0 );
   }
}

void hb_objSendMsg( PHB_ITEM pObj, char *sMsg, ULONG ulArg, ... )
{
   PHB_DYNS pMsgSym = hb_dynsymFindName( sMsg );

   //printf( "%s %p\n", sMsg, pMsgSym );

   if( pMsgSym )
   {
      hb_vmPushSymbol( pMsgSym->pSymbol );
      hb_vmPush( pObj );

      if( ulArg )
      {
         unsigned long i;

         va_list ap;

         va_start( ap, ulArg );

         for( i = 0; i < ulArg; i++ )
         {
            hb_vmPush( va_arg( ap, PHB_ITEM ) );
         }

         va_end( ap );
      }

      hb_vmSend( (USHORT) ulArg );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__ObjSendMsg()", 0 );
   }
}

/**
  Get an object property value.
  Returns a pointer to HB_VM_STACK.Return on success.
  Callers should NEVER dispose it.
*/

PHB_ITEM hb_objGetPropValue( PHB_ITEM pObj, char *szProp, PHB_ITEM pDest )
{
   PHB_DYNS pMsgSym;

   HB_TRACE(HB_TR_DEBUG, ("hb_objGetPropValue(%p, %s)", pObject, szProp));

   pMsgSym = hb_dynsymFindName( szProp );

   if( pMsgSym && hb_objGetMthd( pObj, pMsgSym->pSymbol, FALSE, NULL, FALSE ) )
   {
      HB_THREAD_STUB
      hb_vmPushSymbol( pMsgSym->pSymbol );
      hb_vmPush( pObj );
      hb_vmSend( 0 );

      if (pDest != 0)
      {
         hb_itemCopy( pDest, &(HB_VM_STACK.Return) );
      }

      return &(HB_VM_STACK.Return);
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "hb_objGetPropValue()", 0 );
   }
   return NULL;
}

/**
  Set an object property value.
  The caller should place a leading "_" in szProp.
*/

void hb_objSetPropValue( PHB_ITEM pObj, char *szProp, PHB_ITEM pValue )
{
   PHB_DYNS pMsgSym = hb_dynsymFindName( szProp );

   if( pMsgSym && hb_objGetMthd( pObj, pMsgSym->pSymbol, FALSE, NULL, FALSE ) )
   {
      hb_vmPushSymbol( pMsgSym->pSymbol );
      hb_vmPush( pObj );
      hb_vmPush( pValue );
      hb_vmSend( 1 );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "hb_objSetPropValue()", 0 );
   }
}


/*
 * <xRet> = __objSendMsg( <oObj>, <cSymbol>, <xArg,..>
 *
 * Send a message to an object
 */
HB_FUNC( __OBJSENDMSG )
{
   HB_THREAD_STUB
   PHB_ITEM pObject  = hb_param( 1, HB_IT_OBJECT );
   USHORT uiPCount = hb_pcount();

   if( uiPCount >= 2 && pObject )    /* Object & message passed */
   {
                    /*hb_dynsymFindName( hb_parc(2) );*/
      PHB_DYNS pMsg = hb_dynsymGet( hb_parc(2) );

      if( pMsg )
      {
         USHORT uiParam;

         hb_vmPushSymbol( pMsg->pSymbol );      /* Push char symbol as message  */

         hb_vmPush( pObject );               /* Push object */

         for( uiParam = 3; uiParam <= uiPCount; uiParam++ )   /* Push arguments on stack */
         {
            // NOTE: hb_param() cannot be used here, because it dereferences the parameters
            hb_vmPush( hb_stackItemFromBase( uiParam ) );
         }

         hb_vmSend( ( USHORT ) ( uiPCount - 2 ) );             /* Execute message */
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJSENDMSG", 2, hb_param( 1, HB_IT_ANY ), hb_param( 2, HB_IT_ANY ) );
   }
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
   PHB_ITEM pObject  = hb_param( 1, HB_IT_OBJECT );
   USHORT uiPCount = hb_pcount();

   if( uiPCount >= 2 && pObject )    /* Object & message passed */
   {
                    /*hb_dynsymFindName( hb_parc(2) );*/
      PHB_DYNS pMsg = hb_dynsymGetCase( hb_parc(2) );

      if( pMsg )
      {
         USHORT uiParam;

         hb_vmPushSymbol( pMsg->pSymbol );      /* Push char symbol as message  */

         hb_vmPush( pObject );               /* Push object */

         for( uiParam = 3; uiParam <= uiPCount; uiParam++ )   /* Push arguments on stack */
         {
            // NOTE: hb_param() cannot be used here, because it dereferences the parameters
            hb_vmPush( hb_stackItemFromBase( uiParam ) );
         }

         hb_vmSend( ( USHORT ) ( uiPCount - 2 ) );             /* Execute message */
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3000, NULL, "__OBJSENDMSGCASE", 2, hb_param( 1, HB_IT_ANY ), hb_param( 2, HB_IT_ANY ) );
   }
}

/*
 * <hClass> := __clsInstSuper( <cName> )
 *
 * Instance super class and return class handle
 */
HB_FUNC( __CLSINSTSUPER )
{
   BOOL bFound = FALSE;
   HB_THREAD_STUB

   if( hb_pcount() >= 1 )
   {

      char *cString = hb_parc( 1 );
      PHB_DYNS pDynSym = hb_dynsymFind( cString );

      if( pDynSym )                             /* Find function            */
      {
         USHORT uiClass;

         hb_vmPushSymbol( pDynSym->pSymbol );        /* Push function name       */
         hb_vmPushNil();
         hb_vmFunction( 0 );                         /* Execute super class      */

         if( HB_IS_OBJECT( hb_stackItemFromTop( -1 ) ) )
         {
            for( uiClass = 0; ! bFound && uiClass < s_uiClasses; uiClass++ )
            {                                      /* Locate the entry         */
               if( hb_stricmp( cString , s_pClasses[ uiClass ].szName ) == 0 )
               {
                  hb_retni( uiClass + 1 );               /* Entry + 1 = hb___msgClsH    */
                  bFound = TRUE;
               }
            }
         }
         else
         {
            hb_errRT_BASE( EG_ARG, 3002, "Super class does not return an object", "__CLSINSTSUPER", 0 );
         }
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 3003, "Cannot find super class", "__CLSINSTSUPER", 0 );
      }
   }

   if( ! bFound )
   {
      hb_retni( 0 );
   }
}

/*
 * <nSeq> = __cls_CntClsData( <hClass> )
 *
 * Return number of class datas
 */
HB_FUNC( __CLS_CNTCLSDATA )
{
   HB_THREAD_STUB
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      hb_retni( hb_arrayLen( pClass->pClassDatas ) );
   }
   else hb_retni( 0 );
}


/*
 * <nSeq> = __cls_CntData( <hClass> )
 *
 * Return number of datas
 */
HB_FUNC( __CLS_CNTDATA )
{
   HB_THREAD_STUB
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
   HB_THREAD_STUB
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
   HB_THREAD_STUB
   USHORT uiClass = ( USHORT ) hb_parni( 1 );

   if( uiClass )
      hb_retni( uiClass != 0 ? ++s_pClasses[ uiClass - 1 ].uiDatas : 0 );
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
   HB_FUNCNAME( __OBJGETCLSNAME )();
}

/* NOTE: Undocumented Clipper function */
/* NOTE: Based on hb___msgClsSel() */

HB_FUNC( __CLASSSEL )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pReturn = hb_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiPos = 0;
      USHORT uiAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->uiMethods );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;
         if( pMessage )                         /* Hash Entry used ?        */
         {
            PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array             */
            hb_arraySet( pReturn, ++uiPos, pItem );
            hb_itemRelease( pItem );
         }
      }
   }

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/* to be used from Classes ERROR HANDLER method */
HB_FUNC( __GETMESSAGE )
{
   HB_THREAD_STUB
   PHB_ITEM * pBase = HB_VM_STACK.pBase;

   pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;

   hb_retc( ( *pBase )->item.asSymbol.value->szName );
}

HB_FUNC( __CLSPARENT )
{
   HB_THREAD_STUB
   hb_retl( hb_clsIsParent( hb_parni( 1 ) , hb_parc( 2 ) ) );
}

HB_FUNC( __SENDER )
{
   HB_THREAD_STUB
   PHB_ITEM * pBase = HB_VM_STACK.pBase;
   PHB_ITEM oSender = NULL;
   USHORT iLevel = 3;

   while( iLevel > 0 && pBase != HB_VM_STACK.pItems )
   {
      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
      oSender = *( pBase + 1 );

      if( ( iLevel-- == 2 && oSender->type != HB_IT_BLOCK ) || oSender->type == HB_IT_NIL )
         break;
   }

   if( iLevel == 0 && oSender != NULL && oSender->type == HB_IT_OBJECT )
   {
      hb_itemCopy( &(HB_VM_STACK.Return), oSender );
   }
}

/*
 * Added by RâC&JfL
 *
 * based on hb___msgClsH( void )
 */
HB_FUNC( __CLASSH )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_itemParam( 1 );

   hb_retni( HB_IS_OBJECT( pObject ) ? pObject->item.asArray.value->uiClass : 0 );

   hb_itemRelease( pObject );
}

/*
 * based on hb___msgEval( void )
 */
HB_FUNC( __EVAL )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_itemParam( 1 );
   USHORT uiPCount = hb_pcount();

   if( HB_IS_BLOCK( pObject ) )
   {
      USHORT uiParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pObject );                     /* Push block               */
      for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );

      hb_vmSend( ( USHORT ) uiPCount );     /* Self is also an argument */
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL", 0 );
   }

   hb_itemRelease( pObject );

}

/* ================================================ */

/*
 * <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
static HARBOUR hb___msgClsH( void )
{
   HB_THREAD_STUB
   if( HB_IS_ARRAY( hb_stackSelfItem() ) )
      hb_retni( ( hb_stackSelfItem() )->item.asArray.value->uiClass );
   else
      hb_retni( 0 );
}


/* Added by JfL&RaC
 * <logical> <= <obj>:IsDerivedFrom( xParam )
 *
 * Return true if <obj> is derived from xParam.
 * xParam can be either an obj or a classname
 */
static HARBOUR hb___msgClsParent( void )
{
   HB_THREAD_STUB
   PHB_ITEM pItemRef;
   PHB_ITEM pItemParam;
   char * szParentName = 0;
   USHORT uiClass, i;
   BOOL lClass=FALSE;

   if( HB_IS_BYREF( hb_stackSelfItem() ) ) // Is it possible?
   {
      pItemRef = hb_itemUnRef( hb_stackSelfItem() );
   }
   else
   {
      pItemRef = hb_stackSelfItem();
   }

   uiClass = pItemRef->item.asArray.value->uiClass;

   pItemParam = hb_stackItemFromBase( 1 );

   if( HB_IS_OBJECT( pItemParam ) )
   {
      szParentName = hb_objGetClsName( pItemParam );
   }
   else if( HB_IS_STRING( pItemParam ) )
   {
      szParentName = hb_itemGetC( pItemParam );
      lClass = TRUE;
   }

   for( i = 0; szParentName[ i ] != '\0'; i++ )
   {
      szParentName[ i ] = ( char ) toupper( szParentName[ i ] );
   }

   hb_retl( hb_clsIsParent( uiClass , szParentName ) );

   if( lClass )
   {
      hb_itemFreeC( szParentName );
   }
}


/*
 * <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
static HARBOUR hb___msgClsName( void )
{
   HB_THREAD_STUB
   PHB_ITEM pItemRef = hb_stackSelfItem();

   if( HB_IS_BYREF( pItemRef ) ) // Is it possible?
   {
      pItemRef = hb_itemUnRef( pItemRef );
   }

   hb_retc( hb_objGetClsName( pItemRef ) );
}


/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * <aMessages> := <obj>:ClassFullSel()
 *
 * Returns all the messages in <obj> as ClassSel plus scope
 */
static HARBOUR hb___msgClsFullSel( void )
{
   HB_THREAD_STUB
   HB_ITEM_PTR pSelf = hb_stackSelfItem();
   USHORT uiClass = ( USHORT ) ( HB_IS_ARRAY( pSelf ) ? pSelf->item.asArray.value->uiClass : 0 );
   PHB_ITEM pReturn = hb_itemNew( NULL );
   USHORT nParam = hb_parni( 1 ), uiScope = hb_parni( 2 );
   BOOL bSuper = ( ISLOG( 3 ) ? hb_parl( 3 ) : TRUE );

   if( ( ! uiClass ) && HB_IS_BYREF( pSelf ) )
   {
      PHB_ITEM pItemRef = hb_itemUnRef( pSelf ); // Is it possible?

      if( HB_IS_ARRAY( pItemRef ) )
      {
         uiClass = pItemRef->item.asArray.value->uiClass;
      }
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiPos = 0;
      USHORT uiAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->uiMethods );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

         (HB_VM_STACK.pMethod) = NULL;            /* Current method pointer   */

         if( pMessage )                         /* Hash Entry used ?        */
         {
            (HB_VM_STACK.pMethod) = pClass->pMethods + uiAt;

            if( ( nParam == HB_MSGLISTALL ) ||
                ( ( nParam == HB_MSGLISTCLASS ) &&
                  ( ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetClsData ) ||
                    ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetClsData ) ||
                    ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetShrData ) ||
                    ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetShrData ) )
                ) ||
                ( ( nParam == HB_MSGLISTPURE ) &&
                  ( ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetClsData ) ) &&
                    ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetClsData ) ) &&
                    ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetShrData ) ) &&
                    ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetShrData ) ) )
                )
              )
            {
               if( uiScope == 0 || (HB_VM_STACK.pMethod)->uiScope & uiScope )
               {
                  PHB_ITEM pSubArray = hb_itemArrayNew( 4 );

                     PHB_ITEM pMsg   = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                     PHB_ITEM pType  = hb_itemPutNI( NULL, (HB_VM_STACK.pMethod)->uiType );
                     PHB_ITEM pScope = hb_itemPutNI( NULL, (HB_VM_STACK.pMethod)->uiScope );

                     hb_arraySet( pSubArray, HB_OO_DATA_SYMBOL, pMsg );
                     // value 2 is VALUE or PFUNCTION
                     hb_arraySet( pSubArray, HB_OO_DATA_TYPE, pType );
                     hb_arraySet( pSubArray, HB_OO_DATA_SCOPE, pScope );

                     hb_itemRelease( pMsg );
                     hb_itemRelease( pType );
                     hb_itemRelease( pScope );

                  hb_arraySet( pReturn, ++uiPos, pSubArray );
                  hb_itemRelease( pSubArray );
               }
            }
         }
      }

      hb_arraySize( pReturn, uiPos );
   }

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/*
 * <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
static HARBOUR hb___msgClsSel( void )
{
   HB_THREAD_STUB
   HB_ITEM_PTR pSelf = hb_stackSelfItem();
   USHORT uiClass = ( USHORT ) ( HB_IS_ARRAY( pSelf ) ? pSelf->item.asArray.value->uiClass : 0 );
   PHB_ITEM pReturn = hb_itemNew( NULL );
   USHORT nParam = hb_parni( 1 ), uiScope = hb_parni( 2 );

   if( ( ! uiClass ) && HB_IS_BYREF( pSelf ) )
   {
      PHB_ITEM pItemRef = hb_itemUnRef( pSelf ); // Is it possible?

      if( HB_IS_ARRAY( pItemRef ) )
      {
         uiClass = pItemRef->item.asArray.value->uiClass;
      }
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiPos = 0;
      USHORT uiAt;

      hb_itemRelease( pReturn );
      pReturn = hb_itemArrayNew( pClass->uiMethods );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

         (HB_VM_STACK.pMethod) = NULL;            /* Current method pointer   */

         if( pMessage )                         /* Hash Entry used ?        */
         {
            (HB_VM_STACK.pMethod) = pClass->pMethods + uiAt;

            if( ( nParam == HB_MSGLISTALL ) ||
                ( ( nParam == HB_MSGLISTCLASS ) &&
                  ( ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetClsData ) ||
                    ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetClsData ) ||
                    ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetShrData ) ||
                    ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetShrData ) )
                ) ||
                ( ( nParam == HB_MSGLISTPURE ) &&
                  ( ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetClsData ) ) &&
                    ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetClsData ) ) &&
                    ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgSetShrData ) ) &&
                    ( ! ( (HB_VM_STACK.pMethod)->pFunction == hb___msgGetShrData ) ) )
                )
              )
            {
               if( uiScope == 0 || (HB_VM_STACK.pMethod)->uiScope & uiScope )
               {
                  PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );

                  hb_arraySet( pReturn, ++uiPos, pItem );
                  hb_itemRelease( pItem );
               }
            }
         }
      }

      hb_arraySize( pReturn, uiPos );
   }

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

/*
 * __msgEvalInline()
 *
 * Internal function executed for inline methods
 */
static HARBOUR hb___msgEvalInline( void )
{
   HB_THREAD_STUB

   HB_ITEM block;
   USHORT uiClass = ( hb_stackSelfItem() )->item.asArray.value->uiClass;
   USHORT uiParam;
   USHORT uiPCount=hb_pcount();

   ( &block )->type = HB_IT_NIL;

   hb_arrayGet( s_pClasses[ uiClass - 1 ].pInlines, (HB_VM_STACK.pMethod)->uiData, &block );

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( &block );
   hb_vmPush( hb_stackSelfItem() );            /* Push self                */

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_vmPush( hb_stackItemFromBase( uiParam ) );
   }

   hb_vmSend( ( USHORT ) (uiPCount + 1 ) );     /* Self is also an argument */

   hb_codeblockDelete( &block );              /* Release block            */
}

/*
 * __msgEval()
 *
 * Internal function for the internal EVAL method.
 */
static HARBOUR hb___msgEval( void )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pSelf = hb_stackSelfItem();
   if( HB_IS_BLOCK( pSelf ) )
   {
      USHORT uiParam;
      USHORT uiPCount=hb_pcount();

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pSelf );                     /* Push block               */
      for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );

      hb_vmSend( ( USHORT ) uiPCount );                       /* Self is also an argument */
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL", 0 );
   }
}

/*
 * __msgSuper()
 *
 * Internal function to return a superobject
 */
//static HARBOUR hb___msgSuper( void )
//{
//   PHB_ITEM pObject = hb_stackSelfItem();
//
//   pObject->item.asArray.value->uiPrevCls  = pObject->item.asArray.value->uiClass; /* backup of actual handel */
//   pObject->item.asArray.value->uiClass    = (HB_VM_STACK.pMethod)->uiSprClass;                /* superclass handel casting */
//
//   hb_itemCopy( &(HB_VM_STACK.Return), pObject );
//}

static HARBOUR hb___msgSuper( void )
{
   HB_THREAD_STUB

   PHB_ITEM pObject = hb_stackSelfItem();
   //ULONG ulLen = pObject->item.asArray.value->ulLen;
   PHB_ITEM pCopy = hb_itemArrayNew(1);

   /* Now save the Self object as the 1st elem. */
   hb_arraySet( pCopy, 1 , pObject );

   /* Or Store original object as 1st elem */
   /* hb_itemCopy( pCopy->item.asArray.value->pItems , pObject) ; */

   /* And transform it into a fake object */
   pCopy->item.asArray.value->uiPrevCls  = pObject->item.asArray.value->uiClass; /* backup of actual handel */
   pCopy->item.asArray.value->uiClass    = (HB_VM_STACK.pMethod)->uiSprClass;                /* superclass handel casting */
   pCopy->item.asArray.value->puiClsTree = NULL;

   hb_itemRelease( hb_itemReturn( pCopy ) );
}

/*
 * __msgClass()
 *
 * Internal function to return Self at Self:Class call (classy compatibility)
 */
/*
static HARBOUR hb___msgClass( void )
{
   hb_itemCopy( &(HB_VM_STACK.Return), hb_stackSelfItem() );
}
*/

/*
 * __msgGetClsData()
 *
 * Internal function to return a CLASSDATA
 */
HARBOUR hb___msgGetClsData( void )
{
   HB_THREAD_STUB
   USHORT uiClass = ( hb_stackSelfItem() )->item.asArray.value->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
   {
      hb_arrayGet( s_pClasses[ uiClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiData, &(HB_VM_STACK.Return) );
   }
}


/*
 * __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
HARBOUR hb___msgSetClsData( void )
{
   HB_THREAD_STUB
   USHORT uiClass = ( hb_stackSelfItem() )->item.asArray.value->uiClass;

   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      hb_arraySet( s_pClasses[ uiClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiData, pReturn );
   }

   hb_itemCopy( &(HB_VM_STACK.Return), pReturn );
}

/*
 * __msgGetShrData()
 *
 * Internal function to return a SHAREDDATA
 */
HARBOUR hb___msgGetShrData( void )
{
   HB_THREAD_STUB
   USHORT uiSprCls = (HB_VM_STACK.pMethod)->uiSprClass;

   if( uiSprCls && uiSprCls <= s_uiClasses )
      hb_arrayGet( s_pClasses[ uiSprCls - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiDataShared, &(HB_VM_STACK.Return) );
}

/*
 * __msgSetShrData()
 *
 * Internal function to set a SHAREDDATA
 */
HARBOUR hb___msgSetShrData( void )
{
   HB_THREAD_STUB
   USHORT uiSprCls = (HB_VM_STACK.pMethod)->uiSprClass;

   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );

   if( uiSprCls && uiSprCls <= s_uiClasses )
   {
      hb_arraySet( s_pClasses[ uiSprCls - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiDataShared, pReturn );
   }

   hb_itemCopy( &(HB_VM_STACK.Return), pReturn );
}

/*
 * __msgGetData()
 *
 * Internal function to return a DATA
 */
HARBOUR hb___msgGetData( void )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_stackSelfItem();
   USHORT uiIndex = (HB_VM_STACK.pMethod)->uiData;

   /* will arise only if the class has been modified after first instance */
   if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) ) /* Resize needed */
   {
      hb_arraySize( pObject, uiIndex ); /* Make large enough */
   }

   hb_arrayGet( pObject, uiIndex, &(HB_VM_STACK.Return) );

   if( HB_VM_STACK.Return.type == HB_IT_BLOCK && HB_VM_STACK.Return.item.asBlock.value->pSelfBase == NULL )
   {
      HB_VM_STACK.Return.item.asBlock.value->pSelfBase = pObject->item.asArray.value;
   }
}

/*
 * __msgSetData()
 *
 * Internal function to set a DATA
 */
HARBOUR hb___msgSetData( void )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_stackSelfItem();
   PHB_ITEM pReturn = hb_stackItemFromBase( 1 );
   USHORT uiIndex = (HB_VM_STACK.pMethod)->uiData;

   /* will arise only if the class has been modified after first instance */
   if( uiIndex > ( USHORT ) hb_arrayLen( pObject ) ) /* Resize needed ? */
   {
      hb_arraySize( pObject, uiIndex ); /* Make large enough */
   }

   hb_arraySet( pObject, uiIndex, pReturn );

   hb_itemReturn( pReturn );
}

/* No comment :-) */
static HARBOUR hb___msgVirtual( void )
{
   /* hb_ret(); */ /* NOTE: It's safe to comment this out */
   ;
}

PCLASS hb_clsClassesArray( void )
{
   return s_pClasses;
}

USHORT hb_clsMaxClasses( void )
{
   return s_uiClasses;
}

/* NOTE: Used by the preprocessor to implement Classy compatibility to Harbour
         Receive an variable number of param and return an array of it.
         No param will return a NULL array */


HB_FUNC( __CLS_PARAM )
{
   HB_THREAD_STUB
   PHB_ITEM array;
   USHORT uiParam = ( USHORT ) hb_pcount();
   USHORT n;

   if( uiParam >= 1 )
   {
      array = hb_itemArrayNew( uiParam );
      for( n = 1; n <= uiParam; n++ )
      {
         PHB_ITEM iTmp = hb_itemParam( n );
         hb_arraySet( array, n, iTmp );
         hb_itemRelease( iTmp );
      }
   }
   else
   {
      PHB_ITEM iTmp = hb_itemPutC( NULL, (char *) "HBObject" );
      array = hb_itemArrayNew( 1 );
      hb_arraySet( array, 1, iTmp );
      hb_itemRelease( iTmp );
   }

   hb_itemRelease( hb_itemReturn( array ) );
}

/* This one is used when HB_NOTOBJECT is defined before HBCLASS.CH */
/* it will avoid any default object to be inherited */
HB_FUNC( __CLS_PAR00 )
{
   HB_THREAD_STUB
   PHB_ITEM array;
   USHORT uiParam = ( USHORT ) hb_pcount();
   USHORT n;

   array = hb_itemArrayNew( uiParam );
   for( n = 1; n <= uiParam; n++ )
    {
         PHB_ITEM iTmp = hb_itemParam( n );
         hb_arraySet( array, n, iTmp );
         hb_itemRelease( iTmp );
    }

   hb_itemRelease( hb_itemReturn( array ) );
}

HB_FUNC( __GETMSGPRF ) /* profiler: returns a method called and consumed times */
                       /* ( nClass, cMsg ) --> aMethodInfo { nTimes, nTime } */
{
   #ifndef HB_NO_PROFILER
      HB_THREAD_STUB

      PCLASS pClass  = s_pClasses + ( hb_parnl( 1 ) - 1 );
      char * cMsg    = hb_parc( 2 );
      USHORT uiAt    = ( USHORT ) ( MsgToNum( cMsg, pClass->uiHashKey ) * BUCKET );
      USHORT uiMask  = ( USHORT ) ( pClass->uiHashKey * BUCKET );
      USHORT uiLimit = ( USHORT ) ( uiAt ? ( uiAt - 1 ) : ( uiMask - 1 ) );
      PMETHOD pMethod;

      hb_reta( 2 );
      hb_stornl( 0, -1, 1 );
      hb_stornl( 0, -1, 2 );

      while( uiAt != uiLimit )
      {
         if( ! strcmp( pClass->pMethods[ uiAt ].pMessage->pSymbol->szName, cMsg ) )
         {
            pMethod = pClass->pMethods + uiAt;
            hb_stornl( pMethod->ulCalls, -1, 1 );
            hb_stornl( pMethod->ulTime, -1, 2 );
            return;
         }
         uiAt++;

         if( uiAt == uiMask )
         {
            uiAt = 0;
         }
      }
   #endif
}

void hb_mthAddTime( PMETHOD pMethod, ULONG ulClockTicks )
{
   #ifndef HB_NO_PROFILER
      if( pMethod != NULL )
      {
         pMethod->ulTime += ulClockTicks;
      }
   #else
      HB_SYMBOL_UNUSED( pMethod );
      HB_SYMBOL_UNUSED( ulClockTicks );
   #endif
}

/* __ClsGetProperties( nClassHandle ) --> aPropertiesNames
 * Notice that this function works quite similar to __CLASSSEL()
 * except that just returns the name of the datas and methods
 * that have been declared as PROPERTY (or PERSISTENT) */

HB_FUNC( __CLSGETPROPERTIES )
{
   USHORT uiClass = ( USHORT ) hb_parni( 1 );
   PHB_ITEM pReturn;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses + ( uiClass - 1 );
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
      USHORT uiAt;

      pReturn = hb_itemArrayNew( 0 );
                                                /* Create a transfer array  */
      for( uiAt = 0; uiAt < uiLimit; uiAt++ )
      {
         PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

         if( ( pMessage != NULL ) && pClass->pMethods[ uiAt ].bIsPersistent )
         {
            PHB_ITEM pItem = hb_itemPutC( NULL, pMessage->pSymbol->szName );
                                                /* Add to array */
            hb_arrayAdd( pReturn, pItem );
            hb_itemRelease( pItem );
         }
      }
   }
   else
   {
      pReturn = hb_itemNew( NULL );
   }

   hb_itemRelease( hb_itemReturn( pReturn ) );
}

HB_FUNC( __CLSGETPROPERTIESANDVALUES )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );

   if( pObject )
   {
      USHORT uiClass = pObject->item.asArray.value->uiClass;

      if( uiClass && uiClass <= s_uiClasses )
      {
         PCLASS pClass = s_pClasses + ( uiClass - 1 );
         USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
         USHORT uiAt;

         HB_ITEM Return;
         HB_ITEM Property;
         HB_ITEM Value;
         HB_ITEM SubArray;


         Return.type   = HB_IT_NIL;
         Property.type = HB_IT_NIL;
         Value.type    = HB_IT_NIL;
         SubArray.type = HB_IT_NIL;

         hb_arrayNew( &Return, 0 );

         for( uiAt = 0; uiAt < uiLimit; uiAt++ )
         {
            PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

            if( pMessage && pClass->pMethods[ uiAt ].bIsPersistent && pClass->pMethods[ uiAt ].uiData )
            {
               hb_itemPutC( &Property, pMessage->pSymbol->szName );
               hb_arrayGet( pObject, pClass->pMethods[ uiAt ].uiData, &Value );

               hb_arrayNew( &SubArray, 2 );
               hb_arraySetForward( &SubArray, 1, &Property );
               hb_arraySetForward( &SubArray, 2, &Value );

               hb_arrayAddForward( &Return, &SubArray );
            }
         }

         hb_itemReturn( &Return );
      }
   }
}

HB_FUNC( __CLSGETIVARNAMESANDVALUES )
{
   PHB_ITEM pObject = hb_param( 1, HB_IT_ARRAY );
   USHORT uiScope = hb_parni( 2 );

   if( pObject )
   {
      USHORT uiClass = pObject->item.asArray.value->uiClass;

      if( uiClass && uiClass <= s_uiClasses )
      {
         PCLASS pClass = s_pClasses + ( uiClass - 1 );
         USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET ); /* Number of Hash keys      */
         USHORT uiAt;

         HB_ITEM Return;
         HB_ITEM Property;
         HB_ITEM Value;
         HB_ITEM SubArray;

         Return.type   = HB_IT_NIL;
         Property.type = HB_IT_NIL;
         Value.type    = HB_IT_NIL;
         SubArray.type = HB_IT_NIL;

         hb_arrayNew( &Return, 0 );

         for( uiAt = 0; uiAt < uiLimit; uiAt++ )
         {
            PHB_DYNS pMessage = ( PHB_DYNS ) pClass->pMethods[ uiAt ].pMessage;

            if( pMessage && pClass->pMethods[ uiAt ].uiData && ( uiScope == 0 || pClass->pMethods[ uiAt ].uiScope & uiScope ) )
            {
               if( pClass->pMethods[ uiAt ].pFunction == hb___msgGetData ||
                   pClass->pMethods[ uiAt ].pFunction == hb___msgGetClsData ||
                   pClass->pMethods[ uiAt ].pFunction == hb___msgGetShrData )
               {
                  hb_itemPutC( &Property, pMessage->pSymbol->szName );
                  hb_arrayGet( pObject, pClass->pMethods[ uiAt ].uiData, &Value );

                  hb_arrayNew( &SubArray, 2 );
                  hb_arraySetForward( &SubArray, 1, &Property );
                  hb_arraySetForward( &SubArray, 2, &Value );

                  hb_arrayAddForward( &Return, &SubArray );
               }
            }
         }

         hb_itemReturn( &Return );
      }
   }
}

PHB_DYNS hb_clsSymbolFromFunction( PHB_ITEM pObject, PHB_FUNC pFunction )
{
   USHORT uiClass;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsSymbolFromFunction(%p, %p)", pObject, pFunction));

   if( HB_IS_ARRAY( pObject ) )
   {
      uiClass = pObject->item.asArray.value->uiClass;
   }
   else
   {
      uiClass = 0;
   }

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass  = s_pClasses + ( uiClass - 1 );
      USHORT ui;
      USHORT uiLimit = ( USHORT ) ( pClass->uiHashKey * BUCKET );

      for( ui = 0; ui < uiLimit; ui++ )
      {
         if( pClass->pMethods[ ui ].pFunction == pFunction )
         {
            //printf( "Function %i Name: %s\n", pFunction, pClass->pMethods[ ui ].pMessage->pSymbol->szName );
            return pClass->pMethods[ ui ].pMessage;
         }
      }
   }

   return NULL;
}

/*
 * <nPtr> := HB_ObjMsgPtr( <oObj>, <cMessage> )
 */
HB_FUNC( HB_OBJMSGPTR )
{
   HB_THREAD_STUB
   PHB_ITEM pObject = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

   if( pObject && pString )
   {
      hb_retnl( hb_objHasMsg( pObject, pString->item.asString.value ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_ObjMsgPtr", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}


/* ================================================ */

/* JC1: get class handle from name
 *
 * Param: the name of the class
 * output: the position of the class in the array of classes, or nothing.
 */
UINT hb_clsGetHandleFromName( char *szClassName )
{
   PCLASS start = s_pClasses;
   USHORT uPos = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_clsGetHandleFromName(%s)", szClassName));

   while ( s_uiClasses > uPos && strcmp( start->szName, szClassName ) != 0 )
   {
      uPos ++;
      start++;
   }

   if ( uPos == s_uiClasses )
   {
      return 0;
   }

   return uPos + 1;
}

HB_FUNC( __CLSGETHANDLEFROMNAME )
{
   HB_THREAD_STUB
   hb_retni( hb_clsGetHandleFromName( hb_parc(1) ) );
}

void hb_clsSetModule( USHORT uiClass )
{
   HB_THREAD_STUB

   if( uiClass && (* HB_VM_STACK.pBase)->item.asSymbol.value->pDynSym )
   {
      ( s_pClasses + ( uiClass - 1 ) )->pModuleSymbols = (* HB_VM_STACK.pBase)->item.asSymbol.value->pDynSym->pModuleSymbols;

      #if 0
         printf( "Class: '%s' Caller: '%s' Module: '%s'\n", ( s_pClasses + ( uiClass - 1 ) )->szName,
                                                            (* HB_VM_STACK.pBase)->item.asSymbol.value->szName,
                                                            ( s_pClasses + ( uiClass - 1 ) )->pModuleSymbols->szModuleName );
      #endif
   }
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
   HB_THREAD_STUB
   BOOL bOldClsScope = s_bClsScope;

   if ( ISLOG( 1 ) )
   {
      s_bClsScope = hb_parl( 1 );
   }

   hb_retl( bOldClsScope );
}

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * Auto Initialize Class Flag
 *
 * <bOldClsAutoInit> := __SetClassAutoInit( <bNewClsAutoInit> )
 *
 * <bNewClsAutoInit> =
 *       FALSE - No auto initialization (Default)
 *       TRUE  - Auto initialize class with its default constructor
 *               If true class will be initialized calling its first constructor method
 *               i.e.: oWin := TWindow() is equivalent to oWin := TWindow():New()
 *
 */
HB_FUNC( __SETCLASSAUTOINIT )
{
   HB_THREAD_STUB
   BOOL bOldClsAutoInit = s_bClsAutoInit;

   if ( ISLOG( 1 ) )
   {
      s_bClsAutoInit = hb_parl( 1 );
   }

   hb_retl( bOldClsAutoInit );
}


