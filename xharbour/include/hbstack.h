/*
 * $Id: hbstack.h,v 1.38 2006/06/08 09:57:13 druzus Exp $
 */

/*
 * Harbour Project source code:
 * The eval stack
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

/* TOFIX: There are several things in this file which are not part of the
          standard Harbour API, in other words these things are not
          guaranteed to remain unchanged. To avoid confusion these should be
          moved to somewhere else (like HBRTL.H). [vszakats] */

#ifndef HB_STACK_H_
#define HB_STACK_H_

#include "hbvmpub.h"
#include "hbapi.h"

#if !defined( STACK_INITHB_ITEMS )
   #define STACK_INITHB_ITEMS      200
#endif
#ifdef HB_THREAD_SUPPORT
   #if !defined( STACK_THREADHB_ITEMS )
      #define STACK_THREADHB_ITEMS    100
   #endif
#endif
#if !defined( STACK_EXPANDHB_ITEMS )
   #define STACK_EXPANDHB_ITEMS    20
#endif

/* JC1: test for macro accessing the stack */
#include "thread.h"

HB_EXTERN_BEGIN

struct hb_class_method;

/* stack managed by the virtual machine */
#ifndef HB_THREAD_SUPPORT
typedef struct
{
   PHB_ITEM * pItems;       /* pointer to the stack items */
   PHB_ITEM * pPos;         /* pointer to the latest used item */
   LONG     wItems;       /* total items that may be holded on the stack */
   HB_ITEM  Return;       /* latest returned value */
   PHB_ITEM * pBase;        /* stack frame position for the current function call */
   PHB_ITEM * pEvalBase;    /* stack frame position for the evaluated codeblock */
   int      iStatics;     /* statics base for the current function call */
   char     szDate[ 9 ];  /* last returned date from _pards() yyyymmdd format */

   /* JC1: thread safe classes messaging */
   struct hb_class_method * pMethod;        /* Selcted method to send message to */

   USHORT uiActionRequest;

   HB_ITEM aWithObject[ HB_MAX_WITH_OBJECTS ];
   UINT    wWithObjectCounter;
   BOOL    bWithObject;

   HB_ITEM  aEnumCollection[ HB_MAX_ENUMERATIONS ];
   PHB_ITEM apEnumVar[ HB_MAX_ENUMERATIONS ];
   ULONG    awEnumIndex[ HB_MAX_ENUMERATIONS ];
   UINT     wEnumCollectionCounter;

   int aiExtraParams[HB_MAX_MACRO_ARGS];
   int iExtraParamsIndex;
   PHB_SYMB apExtraParamsSymbol[HB_MAX_MACRO_ARGS];
   int aiExtraElements[HB_MAX_MACRO_ARGS];
   int iExtraElementsIndex;
   int iExtraElements;
   int iExtraIndex;

   /* BEGIN SEQUENCE [RECOVER] END*/
   struct _HB_SEQUENCE *pSequence;
   
} HB_STACK;

extern HB_STACK hb_stackST;
extern HB_STACK * hb_stack_ptr;
#else
extern HB_STACK hb_stackMT;
#endif

extern HB_EXPORT BOOL hb_stack_ready;

typedef struct
{
   LONG lBaseItem;
   LONG iStatics;
} HB_STACK_STATE;    /* used to save/restore stack state in hb_vmDo)_ */

#ifdef HB_STACK_MACROS
   #define hb_stackItemFromTop( n )    ( * ( HB_VM_STACK.pPos + (int)(n) ) )
   #define hb_stackItemFromBase( n )   ( ( *HB_VM_STACK.pBase )->item.asSymbol.paramcnt < 255 ? *( HB_VM_STACK.pBase + (int)(n) + 1 ) : *( HB_VM_STACK.pBase + (int)(n) + 1 + ( *HB_VM_STACK.pBase )->item.asSymbol.paramcnt - 256 ) )
   #define hb_stackTopOffset( )        ( HB_VM_STACK.pPos - HB_VM_STACK.pItems )
   #define hb_stackBaseOffset( )       ( HB_VM_STACK.pBase - HB_VM_STACK.pItems + 1 )
   #define hb_stackTopItem( )          ( * HB_VM_STACK.pPos )
   #define hb_stackBaseItem( )         ( * HB_VM_STACK.pBase )
   #define hb_stackSelfItem( )         ( * ( HB_VM_STACK.pBase + 1 ) )
   #define hb_stackItem( iItemPos )    ( * ( HB_VM_STACK.pItems + (LONG) ( iItemPos ) ) )
   #define hb_stackReturnItem( )       ( &(HB_VM_STACK.Return) )
   #define hb_stackDateBuffer()        ( HB_VM_STACK.szDate )

   #define hb_stackDec( )              do \
                                       { \
                                          if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems ) \
                                          { \
                                             hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                          } \
                                       } \
                                       while( 0 )

   #define hb_stackPop( )              do \
                                       { \
                                          if( HB_IS_COMPLEX( *( HB_VM_STACK.pPos - 1 ) ) ) \
                                          { \
                                             hb_itemClear( *( HB_VM_STACK.pPos - 1 ) ); \
                                          } \
                                          else \
                                          { \
                                             ( *( HB_VM_STACK.pPos - 1 ) )->type = HB_IT_NIL; \
                                          } \
                                          \
                                          if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems ) \
                                          { \
                                             hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                          } \
                                       } \
                                       while( 0 )

   #define hb_stackPush( )             do \
                                       { \
                                          if( HB_VM_STACK.wItems - 1 <= HB_VM_STACK.pPos - HB_VM_STACK.pItems ) \
                                          { \
                                             hb_stackIncrease(); \
                                          } \
                                          \
                                          ( *(++HB_VM_STACK.pPos) )->type = HB_IT_NIL; \
                                       } \
                                       while( 0 )

#else
   extern HB_EXPORT HB_ITEM_PTR hb_stackItemFromTop( int nFromTop );
   extern HB_EXPORT HB_ITEM_PTR hb_stackItemFromBase( int nFromBase );
   extern HB_EXPORT LONG        hb_stackTopOffset( void );
   extern HB_EXPORT LONG        hb_stackBaseOffset( void );
   extern HB_EXPORT HB_ITEM_PTR hb_stackTopItem( void );
   extern HB_EXPORT HB_ITEM_PTR hb_stackBaseItem( void );
   extern HB_EXPORT HB_ITEM_PTR hb_stackSelfItem( void );
   extern HB_EXPORT HB_ITEM_PTR hb_stackItem( LONG iItemPos );
   extern HB_EXPORT HB_ITEM_PTR hb_stackReturnItem( void );
   extern HB_EXPORT char *      hb_stackDateBuffer( void );

   extern HB_EXPORT void        hb_stackDec( void );        /* pops an item from the stack without clearing it's contents */
   extern HB_EXPORT void        hb_stackPop( void );        /* pops an item from the stack */
   extern HB_EXPORT void        hb_stackPush( void );       /* pushes an item on to the stack */
#endif

/* stack management functions */
extern void    hb_stackDispLocal( void );  /* show the types of the items on the stack for debugging purposes */
extern void    hb_stackDispCall( void );
extern void    hb_stackFree( void );       /* releases all memory used by the stack */
extern void    hb_stackInit( void );       /* initializes the stack */
extern void    hb_stackIncrease( void );   /* increase the stack size */

extern void hb_stackRemove( LONG lUntilPos );
HB_EXPORT extern HB_ITEM_PTR hb_stackNewFrame( HB_STACK_STATE * pStack, USHORT uiParams );
HB_EXPORT extern void hb_stackOldFrame( HB_STACK_STATE * pStack );

HB_EXTERN_END

#endif /* HB_STACK_H_ */
