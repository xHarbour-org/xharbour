/*
 * $Id: hbvmpub.h,v 1.3 2002/09/19 01:57:42 ronpinkas Exp $
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

#ifndef HB_VMPUB_H_
#define HB_VMPUB_H_

#include "hbdefs.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

struct _HB_DYNS;

#if defined(_MSC_VER) && _MSC_VER < 1000
   #pragma pack(8)
#endif

/* symbol support structure */
typedef struct
{
   char *            szName;  /* the name of the symbol */
   HB_SYMBOLSCOPE    cScope;  /* the scope of the symbol */
   PHB_FUNC          pFunPtr; /* function address for function symbol table entries */
   struct _HB_DYNS * pDynSym; /* pointer to its dynamic symbol if defined */
} HB_SYMB, * PHB_SYMB;
#if defined(_MSC_VER) && _MSC_VER < 1000
   #pragma pack()
#endif

/* dynamic symbol structure */
typedef struct _HB_DYNS
{
   HB_HANDLE hArea;        /* Workarea number */
   HB_HANDLE hMemvar;      /* Index number into memvars ( publics & privates ) array */
   PHB_SYMB  pSymbol;      /* pointer to its relative local symbol */
   PHB_FUNC  pFunPtr;      /* Pointer to the function address */
   ULONG     ulCalls;      /* profiler support */
   ULONG     ulTime;       /* profiler support */
   ULONG     ulRecurse;    /* profiler support */
} HB_DYNS, * PHB_DYNS, * HB_DYNS_PTR;

/* forward declarations */
struct _HB_CODEBLOCK;
struct _HB_BASEARRAY;
struct _HB_ITEM;
struct _HB_VALUE;

/* Internal structures that holds data */
struct hb_struArray
{
   struct _HB_BASEARRAY * value;
};

struct hb_struBlock
{
   LONG statics;
   USHORT lineno;
   USHORT paramcnt;
   struct _HB_CODEBLOCK * value;
};

struct hb_struDate
{
   long value;
};

/* this definition signals that number of decimal places for double value
 * was not specified at compile time (the value is a result of optimization
 * performed by the compiler)
 */
#define HB_DEFAULT_WIDTH     255
#define HB_DEFAULT_DECIMALS  255

struct hb_struDouble
{
   USHORT length;
   USHORT decimal;
   double value;
};

struct hb_struInteger
{
   USHORT length;
   int value;
};

struct hb_struLogical
{
   BOOL value;
};

struct hb_struLong
{
   USHORT length;
   long value;
};

struct hb_struMemvar
{
   struct _HB_VALUE ** itemsbase;
   LONG offset;
   LONG value;
};

struct hb_struPointer
{
   void * value;
   BOOL collect;
};

struct hb_struRefer
{
   union {
      struct _HB_CODEBLOCK * block;    /* codeblock */
      struct _HB_ITEM ** itemsbase;    /* static variables */
      struct _HB_ITEM ** *itemsbasePtr; /* local variables */
   } BasePtr;
   LONG offset;    /* 0 for static variables */
   LONG value;
   BOOL bNeedLock;
};

struct hb_struString
{
   ULONG           length;
   char            *value;
   BOOL            bStatic;
   USHORT          *puiHolders; /* number of holders of this string */
};

struct hb_struSymbol
{
   LONG stackbase;
   USHORT lineno;
   USHORT paramcnt;
   PHB_SYMB value;
};

/* items hold at the virtual machine stack */
typedef struct _HB_ITEM
{
   USHORT type;
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
      struct hb_struString  asString;
      struct hb_struSymbol  asSymbol;
   } item;
} HB_ITEM, * PHB_ITEM, * HB_ITEM_PTR;

typedef struct _HB_BASEARRAY
{
   PHB_ITEM pItems;       /* pointer to the array items */
   ULONG    ulLen;        /* number of items in the array */
   USHORT   uiHolders;    /* number of holders of this array */
   USHORT   uiClass;      /* offset to the classes base if it is an object */
   USHORT   uiPrevCls;    /* for fixing after access super */
   USHORT * puiClsTree;   /* remember array of super called ID Tree  */
} HB_BASEARRAY, * PHB_BASEARRAY, * HB_BASEARRAY_PTR;

/* internal structure for codeblocks */
typedef struct _HB_CODEBLOCK
{
   BYTE     *pCode;       /* codeblock pcode */
   PHB_ITEM pLocals;      /* table with referenced local variables */
   USHORT   uiLocals;     /* number of referenced local variables */
   PHB_SYMB pSymbols;     /* codeblocks symbols */
   ULONG    ulCounter;    /* numer of references to this codeblock */
   BOOL     dynBuffer;    /* is pcode buffer allocated dynamically */
   PHB_ITEM **pGlobals;
} HB_CODEBLOCK, * PHB_CODEBLOCK, * HB_CODEBLOCK_PTR;


#define HB_DYNS_FUNC( hbfunc )   BOOL hbfunc( PHB_DYNS pDynSymbol, void * Cargo )
typedef HB_DYNS_FUNC( PHB_DYNS_FUNC );

/* Harbour Functions scope ( HB_SYMBOLSCOPE ) */
#define HB_FS_PUBLIC   ( ( HB_SYMBOLSCOPE ) 0x00 )
#define HB_FS_STATIC   ( ( HB_SYMBOLSCOPE ) 0x02 )
#define HB_FS_FIRST    ( ( HB_SYMBOLSCOPE ) 0x04 )
#define HB_FS_INIT     ( ( HB_SYMBOLSCOPE ) 0x08 )
#define HB_FS_EXIT     ( ( HB_SYMBOLSCOPE ) 0x10 )
#define HB_FS_INITEXIT ( HB_FS_INIT | HB_FS_EXIT )
#define HB_FS_MESSAGE  ( ( HB_SYMBOLSCOPE ) 0x20 )
#define HB_FS_MEMVAR   ( ( HB_SYMBOLSCOPE ) 0x80 )

extern void HB_EXPORT hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals );  /* invokes the virtual machine */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_VMPUB_H_ */
