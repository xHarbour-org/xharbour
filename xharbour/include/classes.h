/*
 * $Id: classes.h,v 1.15 2004/08/14 07:57:53 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * Header file for classes.c
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

#ifndef HB_CLASSES_H_
#define HB_CLASSES_H_

HB_EXTERN_BEGIN

#define HB_CLASS_OP_PLUS            ( ( ULONG ) 0x00000001 )
#define HB_CLASS_OP_MINUS           ( ( ULONG ) 0x00000002 )
#define HB_CLASS_OP_MULT            ( ( ULONG ) 0x00000004 )
#define HB_CLASS_OP_DIVIDE          ( ( ULONG ) 0x00000008 )
#define HB_CLASS_OP_MOD             ( ( ULONG ) 0x00000010 )
#define HB_CLASS_OP_POWER           ( ( ULONG ) 0x00000020 )
#define HB_CLASS_OP_INC             ( ( ULONG ) 0x00000040 )
#define HB_CLASS_OP_DEC             ( ( ULONG ) 0x00000080 )
#define HB_CLASS_OP_EQUAL           ( ( ULONG ) 0x00000100 )
#define HB_CLASS_OP_NOTEQUAL        ( ( ULONG ) 0x00000200 )
#define HB_CLASS_OP_LESS            ( ( ULONG ) 0x00000400 )
#define HB_CLASS_OP_LESSEQUAL       ( ( ULONG ) 0x00000800 )
#define HB_CLASS_OP_GREATER         ( ( ULONG ) 0x00001000 )
#define HB_CLASS_OP_GREATEREQUAL    ( ( ULONG ) 0x00002000 )
#define HB_CLASS_OP_ASSIGN          ( ( ULONG ) 0x00004000 )
#define HB_CLASS_OP_INSTRING        ( ( ULONG ) 0x00008000 )
#define HB_CLASS_OP_NOT             ( ( ULONG ) 0x00010000 )
#define HB_CLASS_OP_AND             ( ( ULONG ) 0x00020000 )
#define HB_CLASS_OP_OR              ( ( ULONG ) 0x00040000 )
#define HB_CLASS_OP_ARRAYINDEX      ( ( ULONG ) 0x00080000 )
#define HB_CLASS_OP_BITAND          ( ( ULONG ) 0x00100000 )
#define HB_CLASS_OP_BITOR           ( ( ULONG ) 0x00200000 )
#define HB_CLASS_OP_BITXOR          ( ( ULONG ) 0x00400000 )
#define HB_CLASS_OP_BITSHIFTR       ( ( ULONG ) 0x00800000 )
#define HB_CLASS_OP_BITSHIFTL       ( ( ULONG ) 0x01000000 )


typedef struct hb_class_method
{
   PHB_DYNS pMessage;            /* Method Symbolic name */
   PHB_FUNC pFunction;           /* Function 'pointer' */
   USHORT   uiData;              /* Item position for data (Harbour like, begin from 1) */
   USHORT   uiDataShared;        /* Item position for datashared (original pos within Shared Class) */
   USHORT   uiSprClass;          /* Originalclass'handel (super or current class'handel if not herited). */ /*Added by RAC&JF*/
   USHORT   uiScope;             /* Scoping value */
   PHB_ITEM pInitValue;          /* Init Value for data */
   BYTE     bClsDataInitiated;   /* There is one value assigned at init time */
   ULONG    ulCalls;             /* profiler support */
   ULONG    ulTime;              /* profiler support */
   ULONG    ulRecurse;           /* profiler support */
   BOOL     bIsPersistent;       /* persistence support */
   USHORT   uiType;              /* Type value */
} METHOD, * PMETHOD;

typedef struct
{
   USHORT   uiData;
   USHORT   uiAt;
   PHB_ITEM pInitValue;
} CLSDINIT, * PCLSDINIT;

typedef struct
{
   PHB_DYNS pMessage;
   USHORT   uiAt;
} METHDYN, * PMETHDYN;

typedef struct
{
   char *   szName;         /* Class name */
   USHORT   uiDatas;        /* Total Data Counter */
   USHORT   uiDataFirst;    /* First uiData from this class */
   PMETHOD  pMethods;
   USHORT   uiMethods;      /* Total Method initialised Counter */
   USHORT   uiReserved;
   USHORT   uiDatasShared;  /* Total shared Class data within Class data */
   USHORT   uiScope;
   PHB_ITEM pClassDatas;    /* Harbour Array for ClassDatas and shared */
   PHB_ITEM pInlines;       /* Array for inline codeblocks */
   PHB_FUNC pFunError;      /* error handler for not defined messages */
   PHB_FUNC pDestructor;    /* Destructor */
   PSYMBOLS pModuleSymbols;
   ULONG    fOpOver;        /* Flags for Operators overload */
   USHORT   uiDataInitiated;
   USHORT   uiFriends;
   PCLSDINIT pInitValues;
   PMETHDYN pMethDyn;
   USHORT * pFriends;
   
} CLASS, * PCLASS;

extern HB_SYMB  hb_symDestructor;

extern USHORT hb_cls_uiArrayClass, hb_cls_uiBlockClass, hb_cls_uiCharacterClass, hb_cls_uiDateClass,
       hb_cls_uiLogicalClass, hb_cls_uiNilClass, hb_cls_uiNumericClass, hb_cls_uiPointerClass;

extern void     hb_clsReleaseAll( void );    /* releases all defined classes */
extern HB_EXPORT PHB_DYNS hb_clsSymbolFromFunction( PHB_ITEM pObject, PHB_FUNC pFunction );

extern HB_EXPORT PCLASS hb_clsClassesArray( void );
extern HB_EXPORT USHORT hb_clsMaxClasses( void );

extern HB_EXPORT PMETHOD hb_objGetpMethod( PHB_ITEM, PHB_SYMB );
extern HB_EXPORT ULONG   hb_objGetOpOver( PHB_ITEM pObject );

void * hb_mthRequested( void );

void hb_clsFinalize( PHB_ITEM pObject );

HB_EXTERN_END
#endif
