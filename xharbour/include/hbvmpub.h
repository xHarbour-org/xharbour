/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the generated C language source code
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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
 * Hash support added by Giancarlo Niccolai
 */

#ifndef HB_VMPUB_H_
#define HB_VMPUB_H_

#if ! ( defined( DEBUG ) || defined( _DEBUG ) )
   #ifndef NDEBUG
      #define NDEBUG
   #endif
#endif
#include <assert.h>

#include "hbdefs.h"

   HB_EXTERN_BEGIN

   struct _HB_DYNS;
   struct _SYMBOLS;
   struct _HB_PCODEFUNC;
/*
   // Now it should not be longer necessary
   #if defined(_MSC_VER) && _MSC_VER < 1000
      #pragma pack(8)
   #endif
*/

   /* symbol support structure */
   typedef struct
   {
      const char *         szName;     /* the name of the symbol */
      union
      {
         HB_SYMBOLSCOPE    value;      /* the scope of the symbol */
         void *            pointer;    /* filler to force alignment */
      } scope;
      union
      {
         PHB_FUNC               pFunPtr;       /* function address for function symbol table entries */
         PHB_FUNC             * pIndirectFunPtr;
         struct _HB_PCODEFUNC * pCodeFunc;
         int                    iStaticsBase;  /* static array index for module */
      } value;
      struct _HB_DYNS * pDynSym;         /* pointer to its dynamic symbol if defined */
   } HB_SYMB, * PHB_SYMB;

   #define HB_SYM_GETDYNSYM(pSym)      ( assert( (pSym)->pDynSym ), (pSym)->pDynSym )
   #define HB_SYM_GETMODULESYM(pSym)   ( HB_SYM_GETDYNSYM(pSym)->pModuleSymbols )

   #define HB_SYM_GETGLOBALS(pSym)     ( HB_SYM_GETMODULESYM(pSym)->pGlobals )
   #define HB_SYM_GETGLOBALSPTR(pSym)  &( HB_SYM_GETMODULESYM(pSym)->pGlobals)
   #define HB_SYM_GETNAMESPACES(pSym)  ( HB_SYM_GETMODULESYM(pSym) ? HB_SYM_GETMODULESYM(pSym)->pNamespaces : NULL )

   #define HB_GETSYM()                 ( ( *HB_VM_STACK.pBase )->item.asSymbol.value )

   #define HB_GETMODULESYM()           ( HB_GETSYM()->pDynSym ? HB_SYM_GETMODULESYM( HB_GETSYM() ) : NULL )

   #define HB_GETDYNSYM()              HB_SYM_GETDYNSYM( HB_GETSYM() )
   #define HB_GETGLOBALS()             HB_SYM_GETGLOBALS( HB_GETSYM() )
   #define HB_GETGLOBALSPTR()          HB_SYM_GETGLOBALSPTR( HB_GETSYM() )
   #define HB_GETNAMESPACES()          HB_SYM_GETNAMESPACES( HB_GETSYM() )

   #define HB_BASE_GETSYM(pBase)       ( (*pBase)->item.asSymbol.value )
   #define HB_BASE_GETMODULESYM(pBase) HB_SYM_GETMODULESYM( HB_BASE_GETSYM( pBase ) )

   #define HB_STACK_OR_BLOCK_LOCAL( p, n ) do{ \
                                                if( (n) >= 0 ) \
                                                { \
                                                   p = hb_stackItemFromBase(n); \
                                                   \
                                                   if( HB_IS_BYREF( p ) ) \
                                                   { \
                                                      p = hb_itemUnRef( p ); \
                                                   } \
                                                } \
                                                else \
                                                { \
                                                   p = hb_codeblockGetVar( hb_stackSelfItem(),  ( n ) ); \
                                                } \
                                             }while(0);

/*
   // Now it should not be longer necessary
   #if defined(_MSC_VER) && _MSC_VER < 1000
      #pragma pack()
   #endif
*/

   typedef struct _SYMBOLS
   {
      PHB_SYMB pSymbolTable;     /* pointer to the module's symbol table */
      UINT     uiModuleSymbols;  /* number of symbols on that table */
      struct _SYMBOLS * pNext;   /* pointer to the next SYMBOLS structure */
      HB_SYMBOLSCOPE hScope;     /* scope collected from all symbols in module used to speed initialization code */
      void *   hDynLib;          /* handler to dynamic library */
      BOOL     fAllocated;       /* the symbol table is dynamically allocated and should be freed on HVM exit */
      BOOL     fActive;          /* the symbol table is currently active */
      BOOL     fInitStatics;     /* static initialization should be executed */
      char *   szModuleName;     /* module name */
      struct _HB_ITEM **pGlobals;/* pointer to the module &pConstantGlobals[0] */
      char *   pNamespaces;
      ULONG    ulID;                /* module unique identifier */
   } SYMBOLS, * PSYMBOLS;        /* structure to keep track of all modules symbol tables */

   extern PSYMBOLS hb_vmFindModule( PHB_SYMB pModuleSymbols );
   extern PSYMBOLS hb_vmFindModuleByName( char *szModuleName );
   extern void     hb_vmFreeSymbols( PSYMBOLS pSymbols );
   extern PSYMBOLS hb_vmRegisterSymbols( PHB_SYMB pModuleSymbols, UINT uiSymbols, const char * szModuleName,  BOOL fDynLib, BOOL fClone, struct _HB_ITEM **pGlobals );
   extern void     hb_vmBeginSymbolGroup( void * hDynLib, BOOL fClone );
   extern void     hb_vmInitSymbolGroup( void * hNewDynLib, int argc, char * argv[] );
   extern void     hb_vmExitSymbolGroup( void * hDynLib );


   /* forward declarations */
   struct _HB_CODEBLOCK;
   struct _HB_BASEARRAY;
   struct _HB_ITEM;
   struct _HB_VALUE;
   struct _HB_EXTREF;

   /* Internal structures that holds data */
   struct hb_struArray
   {
      struct _HB_BASEARRAY * value;
   };

   /* Internal structures that holds data */
   struct hb_struHash
   {
      struct _HB_BASEHASH *value;
   };

   struct hb_struBlock
   {
      long    statics;
      USHORT  paramcnt;
      struct  _HB_CODEBLOCK * value;
   };

   struct hb_struDate
   {
      long value;
      long time;
   };

   /* this definition signals that number of decimal places for double value
    * was not specified at compile time (the value is a result of optimization
    * performed by the compiler)
    */
   #define HB_DEFAULT_WIDTH     255
   #define HB_DEFAULT_DECIMALS  255

   struct hb_struDouble
   {
      double value;
      UINT   length;
      UINT   decimal;
   };

   struct hb_struInteger
   {
      UINT  length;
      int   value;
   };

   struct hb_struLogical
   {
      BOOL value;
   };

   struct hb_struLong
   {
      HB_LONG value;
      UINT    length;
   };

   struct hb_struMemvar
   {
      struct _HB_VALUE ** itemsbase;
      long offset;
      long value;
   };

   struct hb_struPointer
   {
      void * value;
      BOOL collect;
   };

   struct hb_struRefer
   {
      union
      {
         struct _HB_CODEBLOCK * block;         /* codeblock */
         struct _HB_BASEARRAY * pBaseArray;    /* Array Members */
         struct _HB_ITEM ** *itemsbasePtr;     /* local variables */
      } BasePtr;
      long offset;    /* 0 for static variables */
      long value;
   };

   struct hb_struExtRef
   {
      void * value;                          /* value item pointer */
      const struct _HB_EXTREF * func;        /* extended reference functions */
   };

   struct hb_struString
   {
      HB_SIZE         length;
      HB_SIZE         allocated;
      char          * value;
      HB_COUNTER    * pulHolders; /* number of holders of this string */
   };

   typedef struct _HB_SYMBCARGO
   {
      long        stackbase;
      HB_SIZE     privatesbase;
      USHORT      lineno;
      USHORT      arguments;
      USHORT      locals;
      USHORT      params;
      UINT        uiSuperClass;
   } HB_SYMBCARGO, * PHB_SYMBCARGO;

   struct hb_struSymbol
   {
      PHB_SYMB      value;
      PHB_SYMBCARGO pCargo;
   };

   /* items hold at the virtual machine stack */
   typedef struct _HB_ITEM
   {
      HB_TYPE type;
      union
      {
         struct hb_struArray   asArray;
         struct hb_struBlock   asBlock;
         struct hb_struDate    asDate;
         struct hb_struDouble  asDouble;
         struct hb_struInteger asInteger;
         struct hb_struLogical asLogical;
         struct hb_struLong    asLong;
         struct hb_struMemvar  asMemvar;
         struct hb_struPointer asPointer;
         struct hb_struRefer   asRefer;
         struct hb_struExtRef  asExtRef;
         struct hb_struString  asString;
         struct hb_struSymbol  asSymbol;
         struct hb_struHash    asHash;
      } item;
   } HB_ITEM, * PHB_ITEM;

#ifndef HB_ARRAY_USE_COUNTER
   typedef struct _HB_ARRAY_HOLDER
   {
      void *pOwner;
      struct _HB_ARRAY_HOLDER *pNext;
   } HB_ARRAY_HOLDER, *PHB_ARRAY_HOLDER;
#endif

   typedef struct _HB_BASEARRAY
   {
      PHB_ITEM   pItems;       /* pointer to the array items */
      HB_SIZE    ulLen;        /* number of items in the array */
      HB_COUNTER ulHolders;    /* number of holders of this array */
      USHORT     uiClass;      /* offset to the classes base if it is an object */
      USHORT     uiPrevCls;    /* for fixing after access super */
      USHORT *   puiClsTree;   /* without use  */
      HB_SIZE    ulAllocated;
      HB_SIZE    ulBlock;
      UINT       uiFlags;
#ifndef HB_ARRAY_USE_COUNTER
      PHB_ARRAY_HOLDER pOwners;
#endif
   } HB_BASEARRAY, * PHB_BASEARRAY;

   /* Hash utility functions */
   #define HB_HASH_ORDER_FUNC_( hbfunc )\
                    int hbfunc( PHB_ITEM, PHB_ITEM, BOOL )
   typedef HB_HASH_ORDER_FUNC_( HB_HASH_ORDER_FUNC );
   typedef HB_HASH_ORDER_FUNC *PHB_HASH_ORDER_FUNC;

   typedef struct _HB_BASEHASH
   {
      PHB_ITEM    pKeys;
      PHB_ITEM    pValues;        /* pointer to the array items */
      HB_SIZE     ulLen;          /* number of items in the array */
      HB_SIZE     ulAllocated;    /* items allocated in keys and values */
      PHB_HASH_ORDER_FUNC fOrder; /* returns -1, 0 or 1 */
      BOOL        bCase;          /* Case sensitivity */
      BOOL        bAutoAdd;       /* Signal error if key is not found on assign */
      USHORT      uiLevel;        /* Pagination depth level */
      HB_SIZE     ulTotalLen;     /* Total lenght in paged hashes */
      HB_SIZE     ulPageSize;     /* Maximum size allowed per page */
      HB_SIZE*    pAccessAA;      /* Associative Array pointer */
      HB_COUNTER  ulHolders;      /* number of holders of this hash */
   } HB_BASEHASH, * PHB_BASEHASH;

   #define CBF_DYNAMIC        0x0001
   #define CBF_DYNAMIC_BUFFER 0x0002
   #define CBF_PRIVATE_VARS   0x0004

   /* internal structure for codeblocks */
   typedef struct _HB_CODEBLOCK
   {
      PHB_SYMB   symbol;         /* pointer to symbol where block was defined */
      USHORT     lineno;         /* line number where block was defined */
      USHORT     uiLocals;       /* number of referenced local variables */
      PHB_ITEM   pLocals;        /* table with referenced local variables */
      BYTE       *pCode;         /* codeblock pcode */
      HB_COUNTER ulCounter;      /* numer of references to this codeblock */
      USHORT     uLen;
      USHORT     uiClass;
      USHORT     uiFlags;
   } HB_CODEBLOCK, * PHB_CODEBLOCK;

#if defined( HB_LEGACY_LEVEL )
   /* dynamic symbol structure */
   typedef struct _HB_DYNS
   {
      HB_HANDLE hArea;        /* Workarea number */
      HB_HANDLE hMemvar;      /* Index number into memvars ( publics & privates ) array */
      PHB_SYMB  pSymbol;      /* pointer to its relative local symbol */
      PSYMBOLS  pModuleSymbols;
      HB_SIZE   ulCalls;      /* profiler support */
      HB_SIZE   ulTime;       /* profiler support */
      HB_SIZE   ulRecurse;    /* profiler support */
   } HB_DYNS, * PHB_DYNS;
#else
   /* dynamic symbol structure */
   typedef struct _HB_DYNS
   {
      HB_HANDLE hArea;        /* Workarea number */
      HB_HANDLE hMemvar;      /* Index number into memvars ( publics & privates ) array */
      PHB_SYMB  pSymbol;      /* pointer to its relative local symbol */
      PSYMBOLS  pModuleSymbols;
#if ! defined( HB_NO_PROFILER )
      HB_SIZE   ulCalls;      /* profiler support */
      HB_SIZE   ulTime;       /* profiler support */
      HB_SIZE   ulRecurse;    /* profiler support */
#endif
   } HB_DYNS, * PHB_DYNS;
#endif
#if defined( HB_LEGACY_LEVEL ) || !defined( HB_NO_PROFILER )
   #define HB_DYNS_INIT { 0, 0, NULL, NULL, 0, 0, 0 }
#else
   #define HB_DYNS_INIT { 0, 0, NULL, NULL }
#endif

   #define HB_DYNS_FUNC( hbfunc )   BOOL hbfunc( PHB_DYNS pDynSymbol, void * Cargo )
   typedef HB_DYNS_FUNC( PHB_DYNS_FUNC );

   /* pCode dynamic function - HRB */
   typedef struct _HB_PCODEFUNC
   {
      BYTE *      pCode;      /* function body - PCODE */
      PHB_SYMB    pSymbols;   /* module symbol table */
   } HB_PCODEFUNC, * PHB_PCODEFUNC;

   typedef void (*HB_INIT_FUNC)(void *);
   /* List of functions used by hb_vmAtInit()/hb_vmAtExit() */
   typedef struct _HB_FUNC_LIST
   {
      HB_INIT_FUNC   pFunc;
      void *         cargo;
      struct _HB_FUNC_LIST * pNext;
   } HB_FUNC_LIST, * PHB_FUNC_LIST;

   typedef void     ( * HB_EXTREF_FUNC0 )( void * );
   typedef PHB_ITEM ( * HB_EXTREF_FUNC1 )( PHB_ITEM );
   typedef PHB_ITEM ( * HB_EXTREF_FUNC2 )( PHB_ITEM, PHB_ITEM );
   typedef void     ( * HB_EXTREF_FUNC3 )( PHB_ITEM );

   typedef struct _HB_EXTREF
   {
      HB_EXTREF_FUNC1 read;
      HB_EXTREF_FUNC2 write;
      HB_EXTREF_FUNC3 copy;
      HB_EXTREF_FUNC0 clear;
      HB_EXTREF_FUNC0 mark;
   } HB_EXTREF, * PHB_EXTREF;

   /* Harbour Functions scope ( HB_SYMBOLSCOPE ) */
   #define HB_FS_PUBLIC    ( ( HB_SYMBOLSCOPE ) 0x0001 )
   #define HB_FS_STATIC    ( ( HB_SYMBOLSCOPE ) 0x0002 )
   #define HB_FS_FIRST     ( ( HB_SYMBOLSCOPE ) 0x0004 )
   #define HB_FS_INIT      ( ( HB_SYMBOLSCOPE ) 0x0008 | HB_FS_STATIC )
   #define HB_FS_EXIT      ( ( HB_SYMBOLSCOPE ) 0x0010 | HB_FS_STATIC )
   #define HB_FS_CRITICAL  ( ( HB_SYMBOLSCOPE ) 0x0020 )
   //                      ( ( HB_SYMBOLSCOPE ) 0x0040 )
   //                      ( ( HB_SYMBOLSCOPE ) 0x0080 )

   #define HB_FS_PCODEFUNC ( ( HB_SYMBOLSCOPE ) 0x0100 )
   #define HB_FS_LOCAL     ( ( HB_SYMBOLSCOPE ) 0x0200 )
   #define HB_FS_DYNCODE   ( ( HB_SYMBOLSCOPE ) 0x0400 )

   #define HB_FS_DEFERRED  ( ( HB_SYMBOLSCOPE ) 0x0800 )

   #define HB_FS_UTILITY   ( ( HB_SYMBOLSCOPE ) 0x1000 )
   #define HB_FS_CLSERROR  ( ( HB_SYMBOLSCOPE ) 0x2000 )

   #define HB_FS_INITEXIT  ( HB_FS_INIT | HB_FS_EXIT )

   #define HB_FS_INDIRECT  ( ( HB_SYMBOLSCOPE ) 0x4000 )

   /*
    * be careful !!! the symbols: HB_FS_INITEXIT is bitfield.
    * Any comparisons should always look like:
    * ( ( cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
    */

   /*
      ( ( scope ) | HB_FS_STATIC )

      For compatability with older PCODE 9 when HB_FS_INITEXIT did NOT include HB_FS_STATIC bit flag!
    */
   #define HB_ISINITEXIT( scope ) ( ( ( ( scope ) | HB_FS_STATIC ) & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
   #define HB_ISINIT( scope )     ( ( ( ( scope ) | HB_FS_STATIC ) & HB_FS_INITEXIT ) == HB_FS_INIT )
   #define HB_ISEXIT( scope )     ( ( ( ( scope ) | HB_FS_STATIC ) & HB_FS_INITEXIT ) == HB_FS_EXIT )

   extern HB_EXPORT void hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols );  /* invokes the virtual machine */

   HB_EXTERN_END

#endif /* HB_VMPUB_H_ */
