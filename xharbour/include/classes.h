/*
 * $Id: classes.h,v 1.0 2003/03/14 22:23:29 ronpinkas Exp $
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

#if defined(HB_EXTERN_C)
extern "C" {
#endif

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
} METHOD, * PMETHOD;

typedef struct
{
   char *   szName;         /* Class name */
   USHORT   uiDatas;        /* Total Data Counter */
   USHORT   uiDataFirst;    /* First uiData from this class */
   PMETHOD  pMethods;
   USHORT   uiMethods;      /* Total Method initialised Counter */
   USHORT   uiHashKey;
   USHORT   uiDatasShared;  /* Total shared Class data within Class data */
   PHB_ITEM pClassDatas;    /* Harbour Array for ClassDatas and shared */
   PHB_ITEM pInlines;       /* Array for inline codeblocks */
   PHB_FUNC pFunError;      /* error handler for not defined messages */
} CLASS, * PCLASS;

#if defined(HB_EXTERN_C)
}
#endif
#endif
