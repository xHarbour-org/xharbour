/*
 * $Id$
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
          moved to somewhere else (like hbrtl.h). [vszakats] */

#ifndef HB_STACK_H_
#define HB_STACK_H_

#include "hbvmpub.h"
#include "hbapi.h"

#define _HB_SET_INTERNAL_
#include "hbset.h"

/* JC1: test for macro accessing the stack */
#include "thread.h"

#if defined( HB_OS_WIN )
// Don't move, must precede any #include of Windows to fix CINTERFACE support in guiddef.h and propkeydef.h!!!
#include "cinterface.h"
#include <windows.h>    /* for CRITICAL_SECTION */
#endif

HB_EXTERN_BEGIN

#if defined( _HB_API_INTERNAL_ )

typedef struct _HB_STACKRDD
{
   const char *   szDefaultRDD;     /* default RDD */
   void **        waList;           /* Allocated WorkAreas */
   USHORT *       waNums;           /* Allocated WorkAreas */

   USHORT         uiWaMax;          /* Number of allocated WA */
   USHORT         uiWaSpace;        /* Number of allocated WA */
   USHORT         uiWaNumMax;       /* Number of allocated WA */
/*
  AJ: hard-wired HB_THREAD_SUPPORT is eliminated here in order to make this
  struct common. At this stage, it seems that HB_CRITICAL_T was not defined
  on ST compilation, therefore a direct approach using CRITICAL_SECTION is
  adapted here. This may fails on other system. Non-windows developers are
  expected to make necessary modification if this proves fail on his system.
*/
#if 0
   #ifdef HB_THREAD_SUPPORT
      HB_CRITICAL_T  mtxWorkArea;      /* Mutex */
      BOOL           fMtLockInit;      /* Lock initialized */
      HB_COUNTER     ulCounter;
   #endif
#else
   #if defined( HB_OS_WIN )
      CRITICAL_SECTION  mtxWorkArea;   /* Mutex */
   #else
      HB_CRITICAL_T     mtxWorkArea;   /* Mutex */
   #endif
   BOOL           fMtLockInit;         /* Lock initialized */
   HB_COUNTER     ulCounter;
#endif
}
HB_STACKRDD, * PHB_STACKRDD;


struct hb_class_method;

/* stack managed by the virtual machine */
#ifndef HB_THREAD_SUPPORT
typedef struct
{
   PHB_ITEM * pItems;         /* pointer to the stack items */
   PHB_ITEM * pPos;           /* pointer to the latest used item */
   PHB_ITEM * pEnd;           /* pointer to the end of stack items */
   long       wItems;         /* total items that may be holded on the stack */
   HB_ITEM    Return;         /* latest returned value */
   PHB_ITEM * pBase;          /* stack frame position for the current function call */
   PHB_ITEM * pEvalBase;      /* stack frame position for the evaluated codeblock */
   long       lStatics;       /* statics base for the current function call */
   long       lWithObject;    /* stack offset to base current WITH OBJECT item */
   long       lRecoverBase;   /* current SEQUENCE envelope offset or 0 if no SEQUENCE is active */
   //USHORT   uiActionRequest;/* Request for some action - stop processing of opcodes */
   char       szDate[ 26 ];   /* last returned date from _pards() yyyymmdd format */
   PHB_STACKRDD rdd;          /* RDD related data */
   HB_STACKRDD_TLS rddTls;    /* RDD related data which is always thread-local */

   HB_SET_STRUCT set;
   /* JC1: thread safe classes messaging */
   struct hb_class_method * pMethod;        /* Selcted method to send message to */

   HB_ITEM aWithObject[ HB_MAX_WITH_OBJECTS ];
   UINT    wWithObjectCounter;

   HB_ITEM  aEnumCollection[ HB_MAX_ENUMERATIONS ];
   PHB_ITEM apEnumVar[ HB_MAX_ENUMERATIONS ];
   HB_SIZE  awEnumIndex[ HB_MAX_ENUMERATIONS ];
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

   unsigned int uiVMFlags;
} HB_STACK, * PHB_STACK;

#if !defined( _HB_STACK_LOCAL_MACROS_ )
#ifndef __IMPORT__
extern HB_EXPORT HB_STACK hb_stackST;
#else
extern HB_IMPORT HB_STACK hb_stackST;
#endif
#endif

extern HB_STACK * hb_stack_ptr;
#else
#if !defined( _HB_STACK_LOCAL_MACROS_ )
#ifndef __IMPORT__
extern HB_EXPORT HB_STACK hb_stackMT;
#else
extern HB_IMPORT HB_STACK hb_stackMT;
#endif
#endif
#endif

#if !defined( _HB_STACK_LOCAL_MACROS_ )
extern BOOL hb_stack_ready;
#endif

typedef struct
{
   long lStatics;
} HB_STACK_STATE;    /* used to save/restore stack state in hb_vmDo)_ */

#else

#ifndef HB_THREAD_SUPPORT
typedef void * PHB_STACK;
#endif
typedef void * PHB_STACKRDD;

#endif /* _HB_API_INTERNAL_ */

extern HB_EXPORT PHB_ITEM hb_stackItemFromTop( int nFromTop );
extern HB_EXPORT PHB_ITEM hb_stackItemFromBase( int nFromBase );
extern HB_EXPORT long        hb_stackTopOffset( void );
extern HB_EXPORT long        hb_stackBaseOffset( void );
extern HB_EXPORT long        hb_stackTotalItems( void );
extern HB_EXPORT PHB_ITEM hb_stackTopItem( void );
extern HB_EXPORT PHB_ITEM hb_stackBaseItem( void );
extern HB_EXPORT PHB_ITEM hb_stackSelfItem( void );
extern HB_EXPORT PHB_ITEM hb_stackItem( long iItemPos );
extern HB_EXPORT PHB_ITEM hb_stackReturnItem( void );
extern HB_EXPORT char *      hb_stackDateBuffer( void );

extern HB_EXPORT void        hb_stackPop( void );        /* pops an item from the stack */
extern HB_EXPORT void        hb_stackDec( void );        /* pops an item from the stack without clearing it's contents */
extern HB_EXPORT void        hb_stackPush( void );       /* pushes an item on to the stack */
extern HB_EXPORT PHB_ITEM hb_stackAllocItem( void );  /* allocates new item on the top of stack, returns pointer to it */
extern HB_EXPORT void        hb_stackPushReturn( void );
extern HB_EXPORT void        hb_stackPopReturn( void );
extern void                  hb_stackRemove( long lUntilPos );

/* stack management functions */
extern long    hb_stackBaseProcOffset( int iLevel );
extern void    hb_stackDispLocal( void );  /* show the types of the items on the stack for debugging purposes */
extern void    hb_stackDispCall( void );
extern void    hb_stackFree( void );       /* releases all memory used by the stack */
extern void    hb_stackInit( void );       /* initializes the stack */
extern void    hb_stackIncrease( void );   /* increase the stack size */

extern PHB_IOERRORS hb_stackIOErrors( void );
extern PHB_STACKRDD hb_stackRDD( void );
extern PHB_STACKRDD_TLS hb_stackRDDTLS( void );

extern HB_EXPORT USHORT      hb_stackGetActionRequest( void );
extern HB_EXPORT void        hb_stackSetActionRequest( USHORT uiAction );

extern HB_EXPORT PHB_ITEM hb_stackLocalVariable( int *piFromBase );
extern HB_EXPORT PHB_ITEM ** hb_stackItemBasePtr( void );

#if defined( _HB_API_INTERNAL_ )

   extern void     hb_stackSetStaticsBase( long lBase );
   extern long     hb_stackGetStaticsBase( void );
   void            hb_stack_init( PHB_STACK pStack );

#endif /* _HB_API_INTERNAL_ */

#if defined( _HB_API_INTERNAL_ ) || defined( _HB_SET_INTERNAL_ )
   extern PHB_SET_STRUCT hb_stackSetStruct( void );
#endif

#if defined( HB_STACK_MACROS )

#define hb_stackItemFromTop( n )    ( * ( HB_VM_STACK.pPos + (int)(n) ) )
#define hb_stackItemFromBase( n )   ( *( HB_VM_STACK.pBase + (int)(n) + 1 ) )
#define hb_stackTopOffset( )        ( HB_VM_STACK.pPos - HB_VM_STACK.pItems )
#define hb_stackBaseOffset( )       ( HB_VM_STACK.pBase - HB_VM_STACK.pItems + 1 )
#define hb_stackTotalItems( )       ( HB_VM_STACK.wItems )
#define hb_stackTopItem( )          ( * HB_VM_STACK.pPos )
#define hb_stackBaseItem( )         ( * HB_VM_STACK.pBase )
#define hb_stackSelfItem( )         ( * ( HB_VM_STACK.pBase + 1 ) )
#define hb_stackItem( iItemPos )    ( * ( HB_VM_STACK.pItems + (LONG) ( iItemPos ) ) )
#define hb_stackReturnItem( )       ( &(HB_VM_STACK.Return) )
#define hb_stackDateBuffer()        ( HB_VM_STACK.szDate )

#define hb_stackRDD( )              ( HB_VM_STACK.rdd )
#define hb_stackRDDTLS( )           ( &HB_VM_STACK.rddTls )

#define hb_stackItemBasePtr( )      ( &HB_VM_STACK.pItems )
#define hb_stackGetStaticsBase( )   ( HB_VM_STACK.lStatics )
#define hb_stackSetStaticsBase( n ) do { HB_VM_STACK.lStatics = ( n ); } while ( 0 )
#define hb_stackGetActionRequest( ) ( HB_VM_STACK.uiVMFlags & HB_REQUEST_MASK )
#define hb_stackSetActionRequest( n )   do { \
                                           HB_VM_STACK.uiVMFlags &= ~HB_REQUEST_MASK; HB_VM_STACK.uiVMFlags |= (n); \
                                        } while ( 0 )

#define hb_stackDec( )              do { \
                                       if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                    } while ( 0 )

#define hb_stackPop( )              do { \
                                       if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                       if( HB_IS_COMPLEX( *( HB_VM_STACK.pPos ) ) ) \
                                          hb_itemClear( *( HB_VM_STACK.pPos ) ); \
                                       else \
                                          ( *( HB_VM_STACK.pPos ) )->type = HB_IT_NIL; \
                                    } while ( 0 )

#define hb_stackPush( )             do { \
                                       if( ++HB_VM_STACK.pPos == HB_VM_STACK.pEnd ) \
                                          hb_stackIncrease(); \
                                    } while ( 0 )

#define hb_stackPopReturn( )        do { \
                                       if( HB_IS_COMPLEX( &HB_VM_STACK.Return ) ) \
                                          hb_itemClear( &HB_VM_STACK.Return ); \
                                       if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems ) \
                                          hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                       hb_itemMove( &HB_VM_STACK.Return, * HB_VM_STACK.pPos ); \
                                    } while ( 0 )

#define hb_stackPushReturn( )       do { \
                                       hb_itemMove( * HB_VM_STACK.pPos, &HB_VM_STACK.Return ); \
                                       if( ++HB_VM_STACK.pPos == HB_VM_STACK.pEnd ) \
                                          hb_stackIncrease(); \
                                    } while ( 0 )


#define hb_stackAllocItem( )        ( ( ++HB_VM_STACK.pPos == HB_VM_STACK.pEnd ? \
                                        hb_stackIncrease() : (void) 0 ), \
                                      * ( HB_VM_STACK.pPos - 1 ) )
/*
   #define hb_stackLocalVariable( p )  ( ( ( ( *HB_VM_STACK.pBase )->item.asSymbol.paramcnt > \
                                             ( * HB_VM_STACK.pBase )->item.asSymbol.paramdeclcnt ) && \
                                           ( * (p) ) > ( * HB_VM_STACK.pBase )->item.asSymbol.paramdeclcnt ) ? \
                                         ( * ( HB_VM_STACK.pBase + ( int ) ( * (p) += \
                                             ( * HB_VM_STACK.pBase )->item.asSymbol.paramcnt - \
                                             ( * HB_VM_STACK.pBase )->item.asSymbol.paramdeclcnt ) + 1 ) ) : \
                                         ( * ( HB_VM_STACK.pBase + ( int ) ( * (p) ) + 1 ) ) )
*/
#define hb_stackLocalVariable( p )  ( * ( HB_VM_STACK.pBase + ( int ) ( * (p) ) + 1 ) )

#define hb_stackSetStruct( )        ( &HB_VM_STACK.set )

#endif

#ifdef _HB_API_INTERNAL_
HB_EXPORT PHB_ITEM hb_stackNewFrame( HB_STACK_STATE * pStack, USHORT uiParams );
HB_EXPORT void hb_stackOldFrame( HB_STACK_STATE * pStack );
#endif

HB_EXPORT PHB_ITEM * hb_stackGetBase( int iLevel );
HB_EXPORT void hb_stackClearPrivateBases( void );
HB_EXTERN_END

#endif /* HB_STACK_H_ */
