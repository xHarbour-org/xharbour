/*
 * $Id: hbapi.h,v 1.165 2005/07/10 05:07:05 walito Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the Extend API, Array API, misc API and base declarations
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

#ifndef HB_APIEXT_H_
#define HB_APIEXT_H_

#include "hbvmpub.h"

HB_EXTERN_BEGIN

#define HB_MAX_MACRO_ARGS   16
#define HB_MAX_WITH_OBJECTS 16
#define HB_MAX_ENUMERATIONS 16

/* items types and type checking macros */
#define HB_IT_NIL       ( ( USHORT ) 0x0000 )
#define HB_IT_POINTER   ( ( USHORT ) 0x0001 )
#define HB_IT_INTEGER   ( ( USHORT ) 0x0002 )
#define HB_IT_HASH      ( ( USHORT ) 0x0004 )
#define HB_IT_LONG      ( ( USHORT ) 0x0008 )
#define HB_IT_DOUBLE    ( ( USHORT ) 0x0010 )
#define HB_IT_DATE      ( ( USHORT ) 0x0020 )
#define HB_IT_LOGICAL   ( ( USHORT ) 0x0080 )
#define HB_IT_SYMBOL    ( ( USHORT ) 0x0100 )
#define HB_IT_ALIAS     ( ( USHORT ) 0x0200 )
#define HB_IT_STRING    ( ( USHORT ) 0x0400 )
#define HB_IT_MEMOFLAG  ( ( USHORT ) 0x0800 )
#define HB_IT_MEMO      ( HB_IT_MEMOFLAG | HB_IT_STRING )
#define HB_IT_BLOCK     ( ( USHORT ) 0x1000 )
#define HB_IT_BYREF     ( ( USHORT ) 0x2000 )
#define HB_IT_MEMVAR    ( ( USHORT ) 0x4000 )
#define HB_IT_ARRAY     ( ( USHORT ) 0x8000 )
#define HB_IT_OBJECT    HB_IT_ARRAY
#define HB_IT_NUMERIC   ( ( USHORT ) ( HB_IT_INTEGER | HB_IT_LONG | HB_IT_DOUBLE ) )
#define HB_IT_NUMINT    ( ( USHORT ) ( HB_IT_INTEGER | HB_IT_LONG ) )
#define HB_IT_ANY       ( ( USHORT ) 0xFFFF )
#define HB_IT_COMPLEX   ( ( USHORT ) ( HB_IT_STRING | HB_IT_BLOCK | HB_IT_ARRAY | HB_IT_MEMVAR | HB_IT_HASH | HB_IT_BYREF ) )

#define HB_IS_OF_TYPE( p, t ) ( ( ( p )->type & ~HB_IT_BYREF ) == t )

#define HB_IS_BYREF( p )      ( ( p )->type & HB_IT_BYREF )
#define HB_IS_ARRAY( p )      ( ( p )->type == HB_IT_ARRAY )
#define HB_IS_NIL( p )        ( ( p )->type == HB_IT_NIL )
#define HB_IS_BLOCK( p )      ( ( p )->type == HB_IT_BLOCK )
#define HB_IS_DATE( p )       ( ( p )->type == HB_IT_DATE )
#define HB_IS_DOUBLE( p )     ( ( p )->type == HB_IT_DOUBLE )
#define HB_IS_INTEGER( p )    ( ( p )->type == HB_IT_INTEGER )
#define HB_IS_LOGICAL( p )    ( ( p )->type == HB_IT_LOGICAL )
#define HB_IS_LONG( p )       ( ( p )->type == HB_IT_LONG )
#define HB_IS_NUMERIC( p )    ( ( p )->type & HB_IT_NUMERIC  || HB_IS_DATE(p) || ( HB_IS_STRING(p) && (p)->item.asString.length == 1 ) )
#define HB_IS_NUMBER( p )     ( ( p )->type & HB_IT_NUMERIC )
#define HB_IS_NUMINT( p )     ( ( p )->type & HB_IT_NUMINT )
#define HB_IS_OBJECT( p )     ( HB_IS_ARRAY( p ) && ( p )->item.asArray.value->uiClass )
#define HB_IS_STRING( p )     ( ( p )->type & HB_IT_STRING )
#define HB_IS_MEMO( p )       ( ( p )->type & HB_IT_MEMO )
#define HB_IS_SYMBOL( p )     ( ( p )->type == HB_IT_SYMBOL )
#define HB_IS_MEMVAR( p )     ( ( p )->type & HB_IT_MEMVAR )
#define HB_IS_POINTER( p )    ( ( p )->type & HB_IT_POINTER )
#define HB_IS_HASH( p )       ( ( p )->type == HB_IT_HASH )
#define HB_IS_ORDERABLE( p )  ( ( p )->type & ( HB_IT_STRING | HB_IT_NUMERIC | HB_IT_DATE) )
#define HB_IS_COMPLEX( p )    ( ( p )->type & HB_IT_COMPLEX )
#define HB_IS_SIMPLE( p )     ( ( p )->type & ( HB_IT_NIL | HB_IT_NUMERIC | HB_IT_DATE | HB_IT_LOGICAL ) )
#define HB_IS_BADITEM( p )    ( ( p )->type & HB_IT_COMPLEX && ( p )->type & ~( HB_IT_COMPLEX | HB_IT_MEMOFLAG ) )

#define HB_IS_NUMBER_INT( p ) HB_IS_NUMINT( p )
#define HB_IT_NUMERINT        HB_IT_NUMINT

#if defined(__GNUC__)
#  define HB_ITEM_NIL      { HB_IT_NIL, {} }
#else
#  define HB_ITEM_NIL      { HB_IT_NIL, NULL }
#endif

#define ISNIL( n )         ( hb_param( n, HB_IT_ANY ) == NULL || HB_IS_NIL( hb_param( n, HB_IT_ANY ) ) ) /* NOTE: Intentionally using a different method */
#define ISCHAR( n )        ( hb_param( n, HB_IT_STRING ) != NULL )
#define ISNUM( n )         ( hb_param( n, HB_IT_NUMERIC ) != NULL )
#define ISLOG( n )         ( hb_param( n, HB_IT_LOGICAL ) != NULL )
#define ISDATE( n )        ( hb_param( n, HB_IT_DATE ) != NULL )
#define ISMEMO( n )        ( hb_param( n, HB_IT_MEMO ) != NULL )
#define ISBYREF( n )       ( hb_parinfo( n ) & HB_IT_BYREF ) /* NOTE: Intentionally using a different method */
#define ISARRAY( n )       ( hb_param( n, HB_IT_ARRAY ) != NULL )
#define ISOBJECT( n )      ( ISARRAY( n ) && hb_param( n, HB_IT_ARRAY )->item.asArray.value->uiClass != 0 )
#define ISBLOCK( n )       ( hb_param( n, HB_IT_BLOCK ) != NULL ) /* Not available in CA-Cl*pper. */
#define ISPOINTER( n )     ( hb_param( n, HB_IT_POINTER ) != NULL ) /* Not available in CA-Cl*pper. */
#define ISHASH( n )        ( hb_param( n, HB_IT_HASH ) != NULL ) /* Not available in CA-Cl*pper. */

#define HB_ITEM_LOCK( pItem )   ( (pItem)->type == HB_IT_ARRAY ? hb_gcLock( (pItem)->item.asArray.value ) : \
                                  (pItem)->type == HB_IT_BLOCK ? hb_gcLock( (pItem)->item.asBlock.value ) : NULL )

#define HB_ITEM_UNLOCK( pItem ) ( (pItem)->type == HB_IT_ARRAY ? hb_gcUnlock( (pItem)->item.asArray.value ) : \
                                  (pItem)->type == HB_IT_BLOCK ? hb_gcUnlock( (pItem)->item.asBlock.value ) : NULL )

#define HB_ITEM_GET_NUMINTRAW( p )  ( HB_IS_INTEGER( p ) ? \
                                      ( HB_LONG ) p->item.asInteger.value : \
                                      ( HB_LONG ) p->item.asLong.value )

#define HB_ITEM_PUT_NUMINTRAW( p, v )  \
               do { \
                  if( HB_LIM_INT( v ) ) \
                  { \
                     (p)->type = HB_IT_INTEGER; \
                     (p)->item.asInteger.length = HB_INT_LENGTH( v ); \
                     (p)->item.asInteger.value = ( int ) (v); \
                  } \
                  else \
                  { \
                     (p)->type = HB_IT_LONG; \
                     (p)->item.asLong.value = (v); \
                     (p)->item.asLong.length = HB_LONG_LENGTH( v ); \
                  } \
               } while ( 0 )

#define HB_ITEM_NEW(hb)  HB_ITEM hb = HB_ITEM_NIL

typedef struct _HB_VALUE
{
   PHB_ITEM    pVarItem;
   HB_COUNTER  counter;
   HB_HANDLE   hPrevMemvar;
} HB_VALUE, * PHB_VALUE, * HB_VALUE_PTR;

typedef struct _HB_NESTED_CLONED
{
   PHB_BASEARRAY            pSrcBaseArray;
   PHB_ITEM                 pDest;
   struct _HB_NESTED_CLONED * pNext;
} HB_NESTED_CLONED, * PHB_NESTED_CLONED;

typedef struct
{
   PHB_DYNS pDynSym;             /* Pointer to dynamic symbol */
} DYNHB_ITEM, *PDYNHB_ITEM, *DYNHB_ITEM_PTR;

/* RDD method return codes */
typedef USHORT ERRCODE;
#define SUCCESS            0
#define FAILURE            1

extern HB_SYMB  hb_symEval;

/* garbage collector */
/* holder of memory block information */
/* NOTE: USHORT is used intentionally to fill up the structure to
 * full 16 bytes (on 16/32 bit environment)
 */
#define HB_GARBAGE_FUNC( hbfunc )   void hbfunc( void * Cargo ) /* callback function for cleaning garbage memory pointer */
typedef HB_GARBAGE_FUNC( HB_GARBAGE_FUNC_ );
typedef HB_GARBAGE_FUNC_ * HB_GARBAGE_FUNC_PTR;

typedef struct HB_GARBAGE_
{
   struct HB_GARBAGE_ *pNext;  /* next memory block */
   struct HB_GARBAGE_ *pPrev;  /* previous memory block */
   HB_GARBAGE_FUNC_PTR pFunc;  /* cleanup function called before memory releasing */
   USHORT locked;              /* locking counter */
   USHORT used;                /* used/unused block */
} HB_GARBAGE, *HB_GARBAGE_PTR;

extern HB_EXPORT HB_ITEM_PTR hb_gcGripGet( HB_ITEM_PTR pItem );
extern HB_EXPORT void   hb_gcGripDrop( HB_ITEM_PTR pItem );

extern HB_EXPORT void * hb_gcAlloc( ULONG ulSize, HB_GARBAGE_FUNC_PTR pFunc ); /* allocates a memory controlled by the garbage collector */
extern HB_EXPORT void   hb_gcFree( void *pAlloc ); /* deallocates a memory allocated by the garbage collector */
extern HB_EXPORT void * hb_gcLock( void *pAlloc ); /* do not release passed memory block */
extern HB_EXPORT void * hb_gcUnlock( void *pAlloc ); /* passed block is allowed to be released */
extern HB_EXPORT void   hb_gcCollect( void ); /* checks if a single memory block can be released */
extern HB_EXPORT void   hb_gcCollectAll( BOOL bForce ); /* checks if all memory blocks can be released */

extern void   hb_gcReleaseAll( void ); /* release all memory blocks unconditionally */
extern void   hb_gcItemRef( HB_ITEM_PTR pItem ); /* checks if passed item refers passed memory block pointer */
extern void   hb_gcInit( void );
extern BOOL   hb_gcSetCollecting( BOOL bCollecting );

extern HB_EXPORT void   hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals );  /* invokes the virtual machine */
extern           void   hb_vmIsLocalRef( void ); /* hvm.c - mark all local variables as used */

#ifdef HB_THREAD_SUPPORT
   extern           void   hb_threadIsLocalRef( void ); /* thread.c - mark all local variables as used */
#endif

extern           void   hb_vmIsStaticRef( void ); /* hvm.c - mark all static variables as used */
extern           void   hb_vmIsGlobalRef( void ); /* hvm.c - mark all global variables as used */
extern HB_EXPORT void   hb_vmRegisterGlobals( PHB_ITEM **pGlobals, short iGlobals ); /* hvm.c - Register module globals into s_aGlobals */

extern void   hb_vmGlobalUnlock( PHB_ITEM pGlobal ); /* hvm.c - Calls hb_gcUnlock(...) when needed. */

#ifndef HB_THREAD_SUPPORT
   extern void   hb_memvarsIsMemvarRef( void ); /* memvars.c - mark all memvar variables as used */
#else
   extern void   hb_memvarsIsMemvarRef( void * ); /* memvars.c - mark all memvar variables as used */
#endif

extern void   hb_clsIsClassRef( void ); /* classes.c - mark all class internals as used */
extern HB_GARBAGE_FUNC( hb_codeblockDeleteGarbage ); /* clear a codeblock before releasing by the GC */
extern HB_GARBAGE_FUNC( hb_arrayReleaseGarbage ); /* clear an array before releasing by the GC */

/* Extend API */
extern HB_EXPORT char *   hb_parc( int iParam, ... );  /* retrieve a string parameter */
extern HB_EXPORT char *   hb_parcx( int iParam, ... );  /* retrieve a string parameter */
extern HB_EXPORT ULONG    hb_parclen( int iParam, ... ); /* retrieve a string parameter length */
extern HB_EXPORT ULONG    hb_parcsiz( int iParam, ... ); /* retrieve a by-reference string parameter length, including terminator */
extern HB_EXPORT char *   hb_pards( int iParam, ... ); /* retrieve a date as a string yyyymmdd */
extern HB_EXPORT char *   hb_pardsbuff( char * szDate, int iParam, ... ); /* retrieve a date as a string yyyymmdd */
extern HB_EXPORT LONG     hb_pardl( int iParam, ... ); /* retrieve a date as long integer - number of days from Julian's day */
extern HB_EXPORT ULONG    hb_parinfa( int iParamNum, ULONG uiArrayIndex ); /* retrieve length or element type of an array parameter */
extern HB_EXPORT int      hb_parinfo( int iParam ); /* Determine the param count or data type */
extern HB_EXPORT int      hb_parl( int iParam, ... ); /* retrieve a logical parameter as an int */
extern HB_EXPORT double   hb_parnd( int iParam, ... ); /* retrieve a numeric parameter as a double */
extern HB_EXPORT int      hb_parni( int iParam, ... ); /* retrieve a numeric parameter as a integer */
extern HB_EXPORT LONG     hb_parnl( int iParam, ... ); /* retrieve a numeric parameter as a LONG */
extern HB_EXPORT HB_LONG  hb_parnint( int iParam, ... ); /* retrieve a numeric parameter as a HB_LONG */
extern HB_EXPORT void *   hb_parptr( int iParam, ... ); /* retrieve a parameter as a pointer */
extern HB_EXPORT void *   hb_parpointer( int iParam ); /* retrieve ONLY a pointer from ONLY HB_IT_POINTER, or retunrs NULL */
extern HB_EXPORT PHB_ITEM hb_param( int iParam, int iMask ); /* retrieve a generic parameter */
extern HB_EXPORT PHB_ITEM hb_paramError( int iParam ); /* Returns either the generic parameter or a NIL item if param not provided */
extern HB_EXPORT BOOL     hb_extIsArray( int iParam );

#ifndef HB_LONG_LONG_OFF
   extern LONGLONG HB_EXPORT hb_parnll( int iParam, ... ); /* retrieve a numeric parameter as a double */
#endif

#define hb_retc_buffer( szText )                   hb_retcAdopt( (szText) )
#define hb_retclen_buffer( szText, ulLen )         hb_retclenAdopt( (szText), (ulLen) )
#define hb_retc_const( szText )                    hb_retcStatic( (szText) )

#ifndef HB_NO_DEFAULT_API_MACROS
   #ifndef HB_API_MACROS
      #define HB_API_MACROS
   #endif
#endif

#ifndef HB_ARRAY_COUNTER_DEFAULT_HOLDERS
   #define HB_ARRAY_COUNTER_DEFAULT_HOLDERS 1
#endif

#ifdef HB_API_MACROS
   #include "hbapiitm.h"
   #ifndef HB_COMP_H_
      #include "hbstack.h"
   #endif

    #define hb_pcount()                          ( ( int ) ( ( ( * HB_VM_STACK.pBase )->item.asSymbol.paramcnt < 255 ) ? ( * HB_VM_STACK.pBase )->item.asSymbol.paramcnt : ( * HB_VM_STACK.pBase )->item.asSymbol.paramcnt - 256 ) )

    #define hb_ret()                             hb_itemClear( &HB_VM_STACK.Return )
    #define hb_reta( ulLen )                     hb_arrayNew( &HB_VM_STACK.Return, (ulLen) )
    #define hb_retc( szText )                    hb_itemPutC( &HB_VM_STACK.Return, (szText) )
    #define hb_retclen( szText, ulLen )          hb_itemPutCL( &HB_VM_STACK.Return, (szText), (ulLen) )

    #define hb_retcAdopt( szText )                    hb_itemPutCPtr( &HB_VM_STACK.Return, (szText), strlen( szText ) )
    #define hb_retclenAdopt( szText, ulLen )          hb_itemPutCPtr( &HB_VM_STACK.Return, (szText), (ulLen) )
    #define hb_retcAdoptStatic( szText )              hb_itemPutCStatic( &HB_VM_STACK.Return, (szText) )
    #define hb_retclenAdoptStatic( szText, ulLen )    hb_itemPutCLStatic( &HB_VM_STACK.Return, (szText), (ulLen) )

    #define hb_retclenAdoptRaw( szText, ulLen )       hb_itemPutCRaw( &HB_VM_STACK.Return, (szText), (ulLen) )
    #define hb_retclenAdoptRawStatic( szText, ulLen ) hb_itemPutCRawStatic( &HB_VM_STACK.Return, (szText), (ulLen) )

    #define hb_retds( szDate )                   hb_itemPutDS( &HB_VM_STACK.Return, (szDate) )
    #define hb_retd( lYear, lMonth, lDay )       hb_itemPutD( &HB_VM_STACK.Return, (lYear), (lMonth), (lDay) )
    #define hb_retdl( lJulian )                  hb_itemPutDL( &HB_VM_STACK.Return, (lJulian) )
    #define hb_retl( iLogical )                  hb_itemPutL( &HB_VM_STACK.Return, (iLogical) ? TRUE : FALSE )
    #define hb_retnd( dNumber )                  hb_itemPutND( &HB_VM_STACK.Return, (dNumber) )
    #define hb_retni( iNumber )                  hb_itemPutNI( &HB_VM_STACK.Return, (iNumber) )
    #define hb_retnl( lNumber )                  hb_itemPutNL( &HB_VM_STACK.Return, (lNumber) )
    #define hb_retnlen( dNumber, iWidth, iDec )  hb_itemPutNLen( &HB_VM_STACK.Return, (dNumber), (iWidth), (iDec) )
    #define hb_retndlen( dNumber, iWidth, iDec ) hb_itemPutNDLen( &HB_VM_STACK.Return, (dNumber), (iWidth), (iDec) )
    #define hb_retnilen( iNumber, iWidth )       hb_itemPutNILen( &HB_VM_STACK.Return, (iNumber), (iWidth) )
    #define hb_retnllen( lNumber, iWidth )       hb_itemPutNLLen( &HB_VM_STACK.Return, (lNumber), (iWidth) )
    #define hb_retptr( voidPtr )                 hb_itemPutPtr( &HB_VM_STACK.Return, (voidPtr) )
    #define hb_retptrGC( voidPtr )               hb_itemPutPtrGC( &HB_VM_STACK.Return, (voidPtr) )
    #define hb_retnint( llNumber )               hb_itemPutNInt( &HB_VM_STACK.Return, (llNumber) )
    #define hb_retnintlen( llNumber, iWidth )    hb_itemPutNIntLen( &HB_VM_STACK.Return, (llNumber), (iWidth) )
    #define hb_retnll( llNumber )                hb_itemPutNLL( &HB_VM_STACK.Return, (llNumber) )
    #define hb_retnlllen( llNumber, iWidth )     hb_itemPutNLLLen( &HB_VM_STACK.Return, (llNumber), (iWidth) )

#else
   /* JC1: including thread anyways, because it defines some void macros when not in MT */
   #include "thread.h"
    extern int   HB_EXPORT  hb_pcount( void );          /* returns the number of suplied parameters */

    extern void  HB_EXPORT  hb_ret( void );             /* post a NIL return value */
    extern void  HB_EXPORT  hb_retc( const char * szText );   /* returns a string */
    extern void  HB_EXPORT  hb_retclen( const char * szText, ULONG ulLen ); /* returns a string with a specific length */

    extern void  HB_EXPORT  hb_retcAdopt( char * szText );
    extern void  HB_EXPORT  hb_retclenAdopt( char * szText, ULONG ulLen );
    extern void  HB_EXPORT  hb_retcAdoptStatic( char * szText );
    extern void  HB_EXPORT  hb_retclenAdoptStatic( char * szText, ULONG ulLen );

    extern void  HB_EXPORT  hb_retclenAdoptRaw( char * szText, ULONG ulLen );
    extern void  HB_EXPORT  hb_retclenAdoptRawStatic( char * szText, ULONG ulLen );

    extern void  HB_EXPORT  hb_retds( const char * szDate );  /* returns a date, must use yyyymmdd format */
    extern void  HB_EXPORT  hb_retd( int iYear, int iMonth, int iDay ); /* returns a date */
    extern void  HB_EXPORT  hb_retdl( LONG lJulian );   /* returns a LONG value as a julian date */
    extern void  HB_EXPORT  hb_retl( int iTrueFalse );  /* returns a logical integer */
    extern void  HB_EXPORT  hb_retnd( double dNumber ); /* returns a double */
    extern void  HB_EXPORT  hb_retni( int iNumber );    /* returns a integer number */
    extern void  HB_EXPORT  hb_retnl( LONG lNumber );   /* returns a LONG number */
    extern void  HB_EXPORT  hb_retnlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
    extern void  HB_EXPORT  hb_retndlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
    extern void  HB_EXPORT  hb_retnilen( int iNumber, int iWidth ); /* returns a integer number, with specific width */
    extern void  HB_EXPORT  hb_retnllen( LONG lNumber, int iWidth ); /* returns a LONG number, with specific width */
    extern void  HB_EXPORT  hb_reta( ULONG ulLen );  /* returns an array with a specific length */
    extern void  HB_EXPORT  hb_retptr( void * voidPtr );  /* returns a pointer */
    extern void  HB_EXPORT  hb_retptrGC( void * voidPtr ); /* returns a pointer to an allocated memory, collected by GC */
    extern void  HB_EXPORT  hb_retnint( HB_LONG llNumber );
    extern void  HB_EXPORT  hb_retnintlen( HB_LONG llNumber, int iWidth );
#ifndef HB_LONG_LONG_OFF
    extern void  HB_EXPORT  hb_retnll( LONGLONG llNumber ); /* returns a long long int */
    extern void  HB_EXPORT  hb_retnlllen( LONGLONG llNumber, int iWidth ); /* returns a long long int, with specific width */
#endif

#endif

extern void  HB_EXPORT  hb_storc( char * szText, int iParam, ... ); /* stores a szString on a variable by reference */
extern void  HB_EXPORT  hb_storclen( char * szText, ULONG ulLength, int iParam, ... ); /* stores a fixed length string on a variable by reference */
extern void  HB_EXPORT  hb_storclenAdopt( char * szText, ULONG ulLength, int iParam, ... ); /* stores a fixed length string on a variable by reference */
extern void  HB_EXPORT  hb_stords( char * szDate, int iParam, ... );   /* szDate must have yyyymmdd format */
extern void  HB_EXPORT  hb_stordl( LONG lJulian, int iParam, ... ); /* stores a Julian's date value on a variable by reference */
extern void  HB_EXPORT  hb_storl( int iLogical, int iParam, ... ); /* stores a logical integer on a variable by reference */
extern void  HB_EXPORT  hb_storni( int iValue, int iParam, ... ); /* stores an integer on a variable by reference */
extern void  HB_EXPORT  hb_stornl( LONG lValue, int iParam, ... ); /* stores a LONG on a variable by reference */
extern void  HB_EXPORT  hb_stornint( HB_LONG lValue, int iParam, ... ); /* stores a HB_LONG on a variable by reference */
extern void  HB_EXPORT  hb_stornd( double dValue, int iParam, ... ); /* stores a double on a variable by reference */
extern void  HB_EXPORT  hb_storptr( void * pointer, int iParam, ... ); /* stores a pointer on a variable by reference */

#ifndef HB_LONG_LONG_OFF
   extern void  HB_EXPORT  hb_stornll( LONGLONG llValue, int iParam, ... ); /* stores a long long int on a variable by reference */
#endif

extern HB_EXPORT void    hb_xinit( void );                         /* Initialize fixed memory subsystem */
extern HB_EXPORT void    hb_xexit( void );                         /* Deinitialize fixed memory subsystem */
extern HB_EXPORT void *  hb_xalloc( ULONG ulSize );                /* allocates memory, returns NULL on failure */
extern HB_EXPORT void *  hb_xgrab( ULONG ulSize );                 /* allocates memory, exits on failure */
extern HB_EXPORT void    hb_xfree( void * pMem );                  /* frees memory */
extern HB_EXPORT void *  hb_xrealloc( void * pMem, ULONG ulSize ); /* reallocates memory */
extern HB_EXPORT ULONG   hb_xsize( void * pMem );                  /* returns the size of an allocated memory block */
extern HB_EXPORT ULONG   hb_xquery( USHORT uiMode );               /* Query different types of memory information */

/* #if UINT_MAX == ULONG_MAX */
/* it fails on 64bit platforms where int has 32 bit and long has 64 bit.
   we need these functions only when max(size_t) < max(long)
   and only on 16bit platforms, so the below condition seems to be
   more reasonable. */
#if UINT_MAX > USHRT_MAX
   /* NOTE: memcpy/memset can work with ULONG data blocks */
   #define  hb_xmemcpy  memcpy
   #define  hb_xmemset  memset
#else
   /* NOTE: otherwise, the hb_xmemcpy and hb_xmemset functions
            will be used to copy and/or set ULONG data blocks */
extern void *   hb_xmemcpy( void * pDestArg, void * pSourceArg, ULONG ulLen ); /* copy more than memcpy() can */
extern void *   hb_xmemset( void * pDestArg, int iFill, ULONG ulLen ); /* set more than memset() can */
#endif

/* array management */
extern HB_EXPORT BOOL      hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ); /* creates a new array */
extern HB_EXPORT ULONG     hb_arrayLen( PHB_ITEM pArray ); /* retrives the array len */
extern HB_EXPORT BOOL      hb_arrayIsObject( PHB_ITEM pArray ); /* retrives if the array is an object */
extern HB_EXPORT BOOL      hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue ); /* add a new item to the end of an array item */
extern HB_EXPORT BOOL      hb_arrayAddForward( PHB_ITEM pArray, PHB_ITEM pItemValue ); /* add a new item to the end of an array item */
extern HB_EXPORT BOOL      hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex ); /* insert a nil item into an array, without changing the length */
extern HB_EXPORT BOOL      hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex ); /* delete an array item, without changing length */
extern HB_EXPORT BOOL      hb_arraySize( PHB_ITEM pArray, ULONG ulLen ); /* sets the array total length */
extern HB_EXPORT BOOL      hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult ); /* retrieve last item in an array */
extern HB_EXPORT BOOL      hb_arrayRelease( PHB_ITEM pArray ); /* releases an array - don't call it - use ItemRelease() !!! */
extern HB_EXPORT BOOL      hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* sets an array element */
extern HB_EXPORT BOOL      hb_arraySetForward( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* sets an array element by forwarding it's value */
extern HB_EXPORT BOOL      hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* retrieves an item */
extern HB_EXPORT BOOL      hb_arrayGetByRef( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* retrieves an item by ref */
extern HB_EXPORT PHB_ITEM  hb_arrayGetItemPtr( PHB_ITEM pArray, ULONG ulIndex ); /* returns pointer to specified element of the array */
extern HB_EXPORT ULONG     hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen ); /* copy a string into an array item */
extern HB_EXPORT char *    hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string contained on an array element */
extern HB_EXPORT char *    hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string pointer on an array element */
extern HB_EXPORT ULONG     hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string length contained on an array element */
extern HB_EXPORT void *    hb_arrayGetPtr( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the pointer contained on an array element */
extern HB_EXPORT BOOL      hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the logical value contained on an array element */
extern HB_EXPORT int       hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the int value contained on an array element */
extern HB_EXPORT LONG      hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the LONG numeric value contained on an array element */
extern HB_EXPORT HB_LONG   hb_arrayGetNInt( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the max size integer value contained on an array element */
extern HB_EXPORT double    hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the double value contained on an array element */
extern HB_EXPORT double    hb_itemGetNDDec( PHB_ITEM pItem, int * piDec );
extern HB_EXPORT char *    hb_arrayGetDS( PHB_ITEM pArray, ULONG ulIndex, char * szDate ); /* retrieves the date value contained in an array element */
extern HB_EXPORT LONG      hb_arrayGetDL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the date value contained in an array element, as a LONG integer */
extern HB_EXPORT USHORT    hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the type of an array item */
extern HB_EXPORT void      hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG ulStart, ULONG ulCount ); /* fill an array with a given item */
extern HB_EXPORT ULONG     hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount, BOOL bExact, BOOL bAllowChar ); /* scan an array for a given item, or until code-block item returns TRUE */
extern HB_EXPORT BOOL      hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG * pulStart, ULONG * pulCount ); /* execute a code-block for every element of an array item */
extern HB_EXPORT BOOL      hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG * pulStart, ULONG * pulCount, ULONG * pulTarget ); /* copy items from one array to another */
extern HB_EXPORT PHB_ITEM  hb_arrayClone( PHB_ITEM pArray, PHB_NESTED_CLONED pClonedList ); /* returns a duplicate of an existing array, including all nested items */
extern HB_EXPORT PHB_ITEM  hb_arrayClone2( PHB_ITEM pArray, PHB_NESTED_CLONED pClonedList ); /* returns a duplicate of an existing array, including all nested items */
extern HB_EXPORT PHB_ITEM  hb_arrayCloneEx( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, PHB_NESTED_CLONED pClonedList ); /* returns a duplicate of an existing array, including all nested items */
extern HB_EXPORT BOOL      hb_arraySort( PHB_ITEM pArray, ULONG * pulStart, ULONG * pulCount, PHB_ITEM pBlock ); /* sorts an array item */
extern HB_EXPORT PHB_ITEM  hb_arrayFromStack( USHORT uiLen ); /* Creates and returns an Array of n Elements from the Eval Stack - Does NOT pop the items. */
extern HB_EXPORT PHB_ITEM  hb_arrayFromParams( PHB_ITEM *pBase ); /* Creates and returns an Array of Generic Parameters for specified base symbol. */
extern HB_EXPORT PHB_ITEM  hb_arrayFromParamsLocked( PHB_ITEM *pBase ); /* Creates and returns GC-LOCKED an Array of Generic Parameters for specified base symbol. */

#ifndef HB_LONG_LONG_OFF
   extern HB_EXPORT LONGLONG hb_arrayGetNLL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the long long int value contained on an array element */
#endif

extern HB_EXPORT PHB_ITEM hb_objClone( PHB_ITEM pObject ); /* returns a duplicate of an existing array, including all nested items */

#ifndef HB_ARRAY_USE_COUNTER
   extern void hb_arrayReleaseHolder( PHB_BASEARRAY pBaseArray, void *pOwner );
   extern void hb_arrayReleaseHolderGarbage( PHB_BASEARRAY pBaseArray, void *pOwner );
   extern void hb_arrayRegisterHolder( PHB_BASEARRAY pBaseArray, void *pHolder );
   extern void hb_arrayResetHolder( PHB_BASEARRAY pBaseArray, void *pOldHolder, void *pNewHolder );
#endif

/* string management */

#define HB_ISSPACE( c ) ( ( c ) == ' ' || \
                          ( c ) == HB_CHAR_HT || \
                          ( c ) == HB_CHAR_LF || \
                          ( c ) == HB_CHAR_CR )

extern HB_EXPORT int      hb_stricmp( const char * s1, const char * s2 ); /* compare two strings without regards to case */
extern HB_EXPORT int      hb_strnicmp( const char * s1, const char * s2, ULONG ulLen ); /* compare two string without regards to case, limited by length */
extern HB_EXPORT char *   hb_strupr( char * pszText ); /* convert a string in-place to upper-case */
extern HB_EXPORT char *   hb_strdup( const char * pszText ); /* returns a pointer to a newly allocated copy of the source string */
extern HB_EXPORT char *   hb_strndup( const char * pszText, ULONG ulLen ); /* returns a pointer to a newly allocated copy of the source string not longer then ulLen */
extern HB_EXPORT ULONG    hb_strnlen( const char * pszText, ULONG ulLen ); /* like strlen() but result is limited to ulLen */
extern HB_EXPORT char *   hb_xstrcat( char *dest, const char *src, ... ); /* Concatenates multiple strings into a single result */
extern HB_EXPORT char *   hb_xstrcpy( char *szDest, const char *szSrc, ...); /* Concatenates multiple strings into a single result */
extern HB_EXPORT BOOL     hb_compStrToNum( const char* szNum, HB_LONG * plVal, double * pdVal, int * piDec, int * piWidth );  /* converts string to number, sets iDec, iWidth and returns TRUE if results is double, used by compiler */
extern HB_EXPORT BOOL     hb_valStrnToNum( const char* szNum, ULONG ulLen, HB_LONG * plVal, double * pdVal, int * piDec, int * piWidth );  /* converts string to number, sets iDec, iWidth and returns TRUE if results is double, used by VAL() */
extern HB_EXPORT BOOL     hb_strToNum( const char* szNum, HB_LONG * plVal, double * pdVal ); /* converts string to number, returns TRUE if results is double */
extern HB_EXPORT BOOL     hb_strnToNum( const char* szNum, ULONG ulLen, HB_LONG * plVal, double * pdVal ); /* converts string to number, returns TRUE if results is double */

extern HB_EXPORT BOOL     hb_strMatchRegExp( const char * szString, const char * szMask ); /* compare two strings using a regular expression pattern */
extern HB_EXPORT BOOL     hb_strMatchWild(const char *szString, const char *szPattern ); /* compare two strings using pattern with wildcard (?*) */
extern HB_EXPORT BOOL     hb_strEmpty( const char * szText, ULONG ulLen ); /* returns whether a string contains only white space */
extern HB_EXPORT void     hb_strDescend( char * szStringTo, const char * szStringFrom, ULONG ulLen ); /* copy a string to a buffer, inverting each character */
extern HB_EXPORT ULONG    hb_strAt( const char * szSub, ULONG ulSubLen, const char * szText, ULONG ulLen ); /* returns an index to a sub-string within another string */
extern HB_EXPORT char *   hb_stripOutComments( char* buffer ); /* extract uncommented part of read buffer */
extern HB_EXPORT char *   hb_strUpper( char * szText, ULONG ulLen ); /* convert an existing string buffer to upper case */
extern HB_EXPORT char *   hb_strUpperCopy( char * szText, ULONG ulLen );
extern HB_EXPORT char *   hb_strLower( char * szText, ULONG ulLen ); /* convert an existing string buffer to lower case */
extern HB_EXPORT char *   hb_strncpy( char * pDest, const char * pSource, ULONG ulLen ); /* copy at most ulLen bytes from string buffer to another buffer and _always_ set 0 in destin buffer */
extern HB_EXPORT char *   hb_strncat( char * pDest, const char * pSource, ULONG ulLen ); /* copy at most ulLen-strlen(pDest) bytes from string buffer to another buffer and _always_ set 0 in destin buffer */
extern HB_EXPORT char *   hb_strncpyTrim( char * pDest, const char * pSource, ULONG ulLen );
extern HB_EXPORT char *   hb_strncpyUpper( char * pDest, const char * pSource, ULONG ulLen ); /* copy an existing string buffer to another buffer, as upper case */
extern HB_EXPORT char *   hb_strncpyUpperTrim( char * pDest, const char * pSource, ULONG ulLen );
extern HB_EXPORT double   hb_strVal( const char * szText, ULONG ulLen ); /* return the numeric value of a character string representation of a number */
extern HB_EXPORT char  *  hb_strLTrim( const char * szText, ULONG * ulLen ); /* return a pointer to the first non-white space character */
extern HB_EXPORT ULONG    hb_strRTrimLen( const char * szText, ULONG ulLen, BOOL bAnySpace ); /* return length of a string, ignoring trailing white space (or true spaces) */

extern HB_EXPORT double   hb_numRound( double dResult, int iDec ); /* round a number to a specific number of digits */
extern HB_EXPORT double   hb_numInt( double dNum ); /* take the integer part of the number */

/* architecture dependent number conversions */
extern HB_EXPORT void     hb_put_ieee754( BYTE * ptr, double d );
extern HB_EXPORT double   hb_get_ieee754( BYTE * ptr );
extern HB_EXPORT void     hb_put_ord_ieee754( BYTE * ptr, double d );
extern HB_EXPORT double   hb_get_ord_ieee754( BYTE * ptr );
extern HB_EXPORT double   hb_get_rev_double( BYTE * ptr );
extern HB_EXPORT double   hb_get_std_double( BYTE * ptr );

#if defined( HB_LONG_LONG_OFF )
   extern HB_EXPORT double   hb_get_le_int64( BYTE * ptr );
   extern HB_EXPORT double   hb_get_le_uint64( BYTE * ptr );
   extern HB_EXPORT void     hb_put_le_uint64( BYTE * ptr, double d );
#endif

/* class management */
extern HB_EXPORT BOOL     hb_clsIsParent( USHORT uiClass, char * szParentName ); /* is a class handle inherited from szParentName Class ? */
extern HB_EXPORT BOOL     hb_clsHasMsg( USHORT uiClass, char *szMsg );

/* object management */
extern HB_EXPORT char *   hb_objGetClsName( PHB_ITEM pObject ); /* retrieves an object class name */
extern HB_EXPORT char *   hb_objGetRealClsName( PHB_ITEM pObject, char * szString  ); /* retrieves an object class name for a specific message */
extern HB_EXPORT PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pSymMsg ); /* returns the method pointer of a object class */
extern HB_EXPORT PHB_FUNC hb_objGetMthd( PHB_ITEM pObject, PHB_SYMB pMessage, BOOL lAllowErrFunc, BOOL *bConstructor, int iOptimizedSend );
extern HB_EXPORT PHB_FUNC hb_objHasMsg( PHB_ITEM pObject, char * szString ); /* returns TRUE/FALSE whether szString is an existing message for object */
extern HB_EXPORT PHB_ITEM hb_objSendMsg( PHB_ITEM pObj, char *cMsg, ULONG ulArg, ... );
/*#define hb_objGetPropValue( pObj, szProp, pDestNullable ) \
   if ( pDestNullable == NULL ) \
   {\
*/
extern HB_EXPORT USHORT   hb_objGetRealCls( PHB_ITEM pObject, char * szName );
extern HB_EXPORT USHORT   hb_objGetClass( PHB_ITEM pItem );

/* dynamic symbol table management */

extern HB_EXPORT PHB_DYNS    hb_dynsymGet( char * szName );    /* finds and creates a dynamic symbol if not found */
extern HB_EXPORT PHB_DYNS    hb_dynsymGetCase( char * szName );    /* finds and creates a dynamic symbol if not found CASE SENSTIVE! */
extern HB_EXPORT PHB_DYNS    hb_dynsymNew( PHB_SYMB pSymbol, PSYMBOLS pModuleSymbols ); /* creates a new dynamic symbol based on a local one */
extern HB_EXPORT PHB_DYNS    hb_dynsymFind( char * szName );   /* finds a dynamic symbol */
extern HB_EXPORT PHB_DYNS    hb_dynsymFindName( char * szName ); /* converts to uppercase and finds a dynamic symbol */
extern HB_EXPORT void        hb_dynsymLog( void );             /* displays all dynamic symbols */
extern HB_EXPORT void        hb_dynsymRelease( void );         /* releases the memory of the dynamic symbol table */
extern HB_EXPORT USHORT      hb_dynsymEval( PHB_DYNS_FUNC pFunction, void * Cargo ); /* enumerates all dynamic symbols */
extern HB_EXPORT PHB_DYNS    hb_dynsymFindFromFunction( PHB_FUNC pFunc ); /* returns a dynamic symbol for a given function pointer. */
extern HB_EXPORT PHB_DYNS    hb_dynsymPos( USHORT uiPos ); /* returns a dynamic symbol from a position index. */
extern HB_EXPORT PDYNHB_ITEM hb_dynsymItems( void );
extern HB_EXPORT USHORT    * hb_dynsymCount( void );

/* JC1: reentrant function support for dynsym where locking is unapplicable. */
#ifdef HB_THREAD_SUPPORT
   extern HB_EXPORT PHB_DYNS hb_dynsymNew_r( PHB_SYMB pSymbol, PSYMBOLS pModuleSymbols, PHB_DYNS pDest );
   extern HB_EXPORT PHB_DYNS hb_dynsymGet_r( char * szName, PHB_DYNS pDest );
   extern HB_EXPORT PHB_DYNS hb_dynsymGetCase_r( char * szName, PHB_DYNS pDest );
   extern HB_EXPORT PHB_DYNS hb_dynsymFind_r( char * szName, PHB_DYNS pDest );
   extern HB_EXPORT PHB_DYNS hb_dynsymFindName_r( char * szName, PHB_DYNS pDest );
   extern HB_EXPORT PHB_DYNS hb_dynsymFindFromFunction_r( PHB_FUNC pFunc, PHB_DYNS pDest );
#else
   #define hb_dynsymNew_r( a, b )      hb_dynsymNew( a )
   #define hb_dynsymGet_r( a, b )      hb_dynsymGet( a )
   #define hb_dynsymGetCase_r( a, b )  hb_dynsymGetCase( a )
   #define hb_dynsymFind_r( a, b )     hb_dynsymFind( a )
   #define hb_dynsymFindName_r( a, b ) hb_dynsymFindName( a )
   #define hb_dynsymFindFromFunction_r( a, b ) hb_dynsymFindFromFunction( a )
#endif

/* Command line and environment argument management */
extern void HB_EXPORT hb_cmdargInit( int argc, char * argv[] ); /* initialize command line argument API's */
extern int      hb_cmdargARGC( void ); /* retrieve command line argument count */
extern char **  hb_cmdargARGV( void ); /* retrieve command line argument buffer pointer */
extern BOOL     hb_cmdargIsInternal( const char * szArg ); /* determine if a string is an internal setting */
extern BOOL     hb_cmdargCheck( const char * pszName ); /* Check if a given internal switch (like //INFO) was set */
extern char *   hb_cmdargString( const char * pszName ); /* Returns the string value of an internal switch (like //TEMPPATH:"C:\") */
extern int      hb_cmdargNum( const char * pszName ); /* Returns the numeric value of an internal switch (like //F:90) */
extern void     hb_cmdargProcessVM( void ); /* Check for command line internal arguments */

/* Symbol management */
extern PHB_SYMB HB_EXPORT hb_symbolNew( char * szName ); /* create a new symbol */

/* Codeblock management */
extern HB_CODEBLOCK_PTR hb_codeblockNew( BYTE * pBuffer, USHORT uiLocals, USHORT * pLocalPosTable, PHB_SYMB pSymbols, PHB_ITEM** pGlobals ); /* create a code-block */
extern HB_CODEBLOCK_PTR hb_codeblockMacroNew( BYTE * pBuffer, USHORT usLen );
extern void     hb_codeblockDelete( HB_ITEM_PTR pItem ); /* delete a codeblock */
extern PHB_ITEM hb_codeblockGetVar( PHB_ITEM pItem, LONG iItemPos ); /* get local variable referenced in a codeblock */
extern PHB_ITEM hb_codeblockGetRef( HB_CODEBLOCK_PTR pCBlock, PHB_ITEM pRefer ); /* get local variable passed by reference */

/* memvars subsystem */
extern HB_HANDLE hb_memvarValueNew( HB_ITEM_PTR pSource, BOOL bTrueMemvar ); /* create a new global value */
extern HB_VALUE_PTR * hb_memvarValueBaseAddress( void ); /* retrieve the base address of the values table */

/* JC1: thread version is a little different */
#ifndef HB_THREAD_SUPPORT
extern void     hb_memvarsInit( void ); /* initialize the memvar API system */
extern void     hb_memvarsRelease( void ); /* clear all PUBLIC and PRIVATE variables */
#endif

extern           void     hb_memvarValueIncRef( HB_HANDLE hValue ); /* increase the reference count of a global value */
extern           void     hb_memvarValueDecRef( HB_HANDLE hValue ); /* decrease the reference count of a global value */
extern           void     hb_memvarValueDecGarbageRef( HB_HANDLE hValue ); /* decrease the reference count of a detached local variable */
extern           void     hb_memvarSetValue( PHB_SYMB pMemvarSymb, HB_ITEM_PTR pItem ); /* copy an item into a symbol */
extern           ERRCODE  hb_memvarGet( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb ); /* copy an symbol value into an item */
extern           void     hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb ); /* copy an symbol value into an item, with error trapping */
extern           void     hb_memvarGetRefer( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb ); /* copy a reference to a symbol value into an item, with error trapping */
extern           ULONG    hb_memvarGetPrivatesBase( void ); /* retrieve current PRIVATE variables stack base */
extern           void     hb_memvarSetPrivatesBase( ULONG ulBase ); /* release PRIVATE variables created after specified base */
extern           void     hb_memvarNewParameter( PHB_SYMB pSymbol, PHB_ITEM pValue );
extern           char   * hb_memvarGetStrValuePtr( char * szVarName, ULONG *pulLen );
extern           void     hb_memvarCreateFromItem( PHB_ITEM pMemvar, BYTE bScope, PHB_ITEM pValue );
extern           int      hb_memvarScope( char * szVarName ); /* retrieve scope of a dynamic variable symbol */
extern HB_EXPORT PHB_ITEM hb_memvarGetValueByHandle( HB_HANDLE hMemvar );
extern           PHB_ITEM hb_memvarDetachLocal( HB_ITEM_PTR pLocal ); /* Detach a local variable from the eval stack */

/* console I/O subsystem */
extern           void     hb_conInit( void ); /* initialize the console API system */
extern           void     hb_conRelease( void ); /* release the console API system */
extern           char     HB_EXPORT *hb_conNewLine( void ); /* retrieve a pointer to a static buffer containing new-line characters */
extern           void     hb_conOutAlt( const char * pStr, ULONG ulLen ); /* output an string to console and/or printer/alternative device/file */
extern           void     hb_conOutStd( const char * pStr, ULONG ulLen ); /* output an string to STDOUT */
extern HB_EXPORT void     hb_conOutErr( const char * pStr, ULONG ulLen ); /* output an string to STDERR */
extern           USHORT   hb_conSetCursor( BOOL bSetCursor, USHORT usNewCursor ); /* retrieve and optionally set cursor shape */
extern           char *   hb_conSetColor( char * szColor ); /* retrieve and optionally set console color */
extern           void     hb_conXSaveRestRelease( void ); /* release the save/restore API */

/* compiler and macro compiler */
extern char *   hb_compReservedName( char * szName ); /* determines if a string contains a reserve word */

/* misc */
extern char *   hb_procinfo( int iLevel, char * szName, USHORT *uLine, char *szModuleName ); /* retrieve a procedure source location info */

/* macro compiler */

typedef struct HB_CBVAR_  /* This structure holds local variables declared in a codeblock */
{
   char * szName;
   BYTE bType;
   struct HB_CBVAR_ * pNext;
} HB_CBVAR, * HB_CBVAR_PTR;

typedef struct HB_PCODE_INFO_   /* compiled pcode container */
{
   BYTE * pCode;           /* pointer to a memory block where pcode is stored */
   ULONG  lPCodeSize;      /* total memory size for pcode */
   ULONG  lPCodePos;       /* actual pcode offset */
   struct HB_PCODE_INFO_ * pPrev;
   HB_CBVAR_PTR pLocals;
} HB_PCODE_INFO, * HB_PCODE_INFO_PTR;

typedef struct HB_MACRO_    /* a macro compiled pcode container */
{
   char * string;          /* compiled string */
   ULONG  length;          /* length of the string */
   ULONG  pos;             /* current position inside of compiled string */
   int    Flags;           /* some flags we may need */
   int    status;          /* status of compilation */
   HB_ITEM_PTR pError;     /* error object returned from the parser */
   ULONG  supported;       /* various flags for supported capabilities */
   int    FlexState;       /* internal flex state during parsing */
   HB_PCODE_INFO_PTR pCodeInfo;  /* pointer to pcode buffer and info */
   void * pParseInfo;      /* data needed by the parser - it should be 'void *' to allow different implementation of macr compiler */
   USHORT uiNameLen;       /* the maximum symbol name length */
   BOOL   bShortCuts;      /* are we using logical shorcuts (in OR/AND)  */
   int exprType;           /* type of successfully compiled expression */
   int iListElements;
} HB_MACRO, * HB_MACRO_PTR;

extern HB_EXPORT void         hb_macroGetValue( HB_ITEM_PTR pItem, BYTE iContext, BYTE flags ); /* retrieve results of a macro expansion */
extern HB_EXPORT void         hb_macroSetValue( HB_ITEM_PTR pItem, BYTE flags ); /* assign a value to a macro-expression item */
extern HB_EXPORT void         hb_macroTextValue( HB_ITEM_PTR pItem ); /* macro text substitution */
extern HB_EXPORT void         hb_macroPushSymbol( HB_ITEM_PTR pItem ); /* handle a macro function calls, e.g. var := &macro() */
extern HB_EXPORT void         hb_macroRun( HB_MACRO_PTR pMacro ); /* executes pcode compiled by macro compiler */
extern HB_EXPORT HB_MACRO_PTR hb_macroCompile( char * szString ); /* compile a string and return a pcode buffer */
extern HB_EXPORT void         hb_macroDelete( HB_MACRO_PTR pMacro ); /* release all memory allocated for macro evaluation */
extern HB_EXPORT char *       hb_macroTextSubst( char * szString, ULONG *pulStringLen ); /* substitute macro variables occurences within a given string */
extern HB_EXPORT BOOL         hb_macroIsIdent( char * szString ); /* determine if a string is a valid function or variable name */
extern HB_EXPORT void         hb_macroPopAliasedValue( HB_ITEM_PTR pAlias, HB_ITEM_PTR pVar, BYTE flags ); /* compiles and evaluates an aliased macro expression */
extern HB_EXPORT void         hb_macroPushAliasedValue( HB_ITEM_PTR pAlias, HB_ITEM_PTR pVar, BYTE flags ); /* compiles and evaluates an aliased macro expression */
extern HB_EXPORT char *       hb_macroGetType( PHB_ITEM pItem, BYTE Flags ); /* determine the type of an expression */
extern HB_EXPORT char *       hb_macroExpandString( char *szString, ULONG ulLength, BOOL *pbNewString ); /* expands valid '&' operator */

/* idle states */
extern void     hb_idleState( BOOL bIndefinite ); /* services a single idle state */
extern void     hb_idleReset( void ); /* reset idle state routine count*/
extern void     hb_idleSleep( double dSeconds ); /* sleep for a given time serving idle task */

extern void     hb_idleShutDown( void ); /* closes all idle state tasks */
extern void *   hb_idleAddFunc( PHB_ITEM pBlock ); /* Adds a codeblock or an executable array */
extern PHB_ITEM hb_idleDelFunc( void * pID ); /* Deletes a prevuiously added codeblock */

/* Background functions */

typedef struct HB_BACKGROUNDTASK_
{
   ULONG    ulTaskID;     /* task identifier */
   PHB_ITEM pTask;        /* pointer to the task item */
   double   dSeconds;     /* internal - last time this task has gone */
   int      millisec;     /* milliseconds after this task must run */
   BOOL     bActive;      /* task is active ? */
} HB_BACKGROUNDTASK, * PHB_BACKGROUNDTASK, * HB_BACKGROUNDTASK_PTR;

extern void     hb_backgroundRunSingle( ULONG ulID ); /* run a single background routine */
extern void     hb_backgroundRun( void ); /* run all background routines */
extern void     hb_backgroundReset( void ); /* reset internal counter */
extern void     hb_backgroundShutDown( void ); /* closes all background tasks */
extern ULONG    hb_backgroundAddFunc( PHB_ITEM pBlock, int nMillisec, BOOL bActive ); /* Adds a codeblock or an executable array */
extern PHB_ITEM hb_backgroundDelFunc( ULONG ulID ); /* Deletes a prevuiously added task */
extern PHB_BACKGROUNDTASK hb_backgroundFind( ULONG ulID );
extern BOOL     hb_backgroundActive( ULONG ulID, BOOL bActive );
extern int      hb_backgroundTime( ULONG ulID, int nMillisec );

/* seconds functions */
extern HB_EXPORT double hb_secondsCPU(int n);
extern HB_EXPORT double hb_dateSeconds( void );

#define hb_seconds()    hb_dateSeconds()

/* misc */
extern char * hb_verPlatform( void ); /* retrieves a newly allocated buffer containing platform version */
extern char * hb_verCompiler( void ); /* retrieves a newly allocated buffer containing compiler version */
extern char * hb_verHarbour( void ); /* retrieves a newly allocated buffer containing harbour version */
extern char * hb_verBuildInfo( BOOL ); /* display harbour, compiler, and platform versions to standard console */
extern BOOL hb_iswinnt(void) ; /* return .T. if OS == WinNt, 2000, XP */
extern char * hb_builddate( void ); /* return date and time of harbour.exe build */

/* Executable array execution */
extern BOOL hb_execFromArray( PHB_ITEM pCallableArray );

/* environment variables access */
/* WARNING: This returned pointer must be freed if not NULL using hb_xfree( ( void * ) ptr ); */
extern HB_EXPORT char * hb_getenv( const char * name );

/* JC1: Indirect execution */
BOOL hb_execFromArray( PHB_ITEM pFirst );

/* Version tracking related things */

#ifdef HB_FILE_VER_STATIC
   #define HB_FILE_VER( id ) static char s_hb_file_ver[] = id;
#else
   #define HB_FILE_VER( id )
#endif

/* Translation related things */

/* Dummy define for start */
#ifndef HB_I_
   #define HB_I_( x ) x
#endif

/* PP Functions */
extern ULONG HB_EXPORT hb_AtSkipStrings( const char * szSub, ULONG ulSubLen, const char * szText, ULONG ulLen );

/* Misc */
extern HB_EXPORT char *  hb_strLowerCopy( char * szText, ULONG ulLen );
extern HB_EXPORT HB_LONG hb_strValInt( const char * szText, int * iOverflow );
extern HB_EXPORT int     Wild2RegEx( char *sWild, char* sRegEx, BOOL bMatchCase );
extern HB_EXPORT BOOL    hb_regex( char cRequest, PHB_ITEM pRegEx, PHB_ITEM pString );

#include "local.h"

HB_EXTERN_END

#endif /* HB_APIEXT_H_ */
