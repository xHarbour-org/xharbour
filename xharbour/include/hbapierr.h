/*
 * $Id: hbapierr.h,v 1.7 2004/03/31 09:19:53 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the Error API
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

#ifndef HB_APIERR_H_
#define HB_APIERR_H_

#include "hbapi.h"
#include "error.ch"

HB_EXTERN_BEGIN

/* Error codes (returned from hb_errLaunch()) */

#define E_BREAK                         0xFFFF
#define E_RETRY                         1
#define E_DEFAULT                       0

/* Error flags */

#define EF_NONE                         0
#define EF_CANRETRY                     1
#define EF_CANSUBSTITUTE                2
#define EF_CANDEFAULT                   4

/* oError:Severity */

/* ... defined in error.ch */

/* oError:SubSystem (commonly used) */

#define HB_ERR_SS_BASE                  "BASE"
#define HB_ERR_SS_TERMINAL              "TERM"
#define HB_ERR_SS_DBCMD                 "DBCMD"
#define HB_ERR_SS_TOOLS                 "TOOLS"

/* oError:GenCode */

/* ... defined in extend.ch */

/* Internal error numbers */

#define HB_ERR_IE_NOT_ENOUGH_MEM        1024
#define HB_ERR_IE_ERR_RECOV_FAIL        1025
#define HB_ERR_IE_UNREC_ERROR           1026
#define HB_ERR_IE_GENERIC               1027

/* Standard API */

extern char     HB_EXPORT * hb_errGetDescription    ( PHB_ITEM pError );
extern char     HB_EXPORT * hb_errGetFileName       ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetFlags          ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetGenCode        ( PHB_ITEM pError );
extern char     HB_EXPORT * hb_errGetOperation    ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetOsCode         ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetSeverity       ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetSubCode        ( PHB_ITEM pError );
extern char     HB_EXPORT * hb_errGetSubSystem    ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetTries          ( PHB_ITEM pError );
extern char     HB_EXPORT * hb_errGetProcName     ( PHB_ITEM pError );
extern USHORT   HB_EXPORT hb_errGetProcLine       ( PHB_ITEM pError );

#ifdef HB_THREAD_SUPPORT
extern UINT     HB_EXPORT hb_errGetRunningThreads ( PHB_ITEM pError );
extern HB_THREAD_T HB_EXPORT hb_errGetThreadId    ( PHB_ITEM pError );
extern UINT     HB_EXPORT hb_errGetVmThreadId     ( PHB_ITEM pError );
#endif

extern USHORT   HB_EXPORT hb_errLaunch            ( PHB_ITEM pError );
extern PHB_ITEM HB_EXPORT hb_errNew               ( void );
extern PHB_ITEM HB_EXPORT hb_errPutArgs           ( PHB_ITEM pError, ULONG ulArgCount, ... );
extern PHB_ITEM HB_EXPORT hb_errPutDescription    ( PHB_ITEM pError, char * szDescription );
extern PHB_ITEM HB_EXPORT hb_errPutFileName       ( PHB_ITEM pError, char * szFileName );
extern PHB_ITEM HB_EXPORT hb_errPutFlags          ( PHB_ITEM pError, USHORT uiFlags );
extern PHB_ITEM HB_EXPORT hb_errPutGenCode        ( PHB_ITEM pError, USHORT uiGenCode );
extern PHB_ITEM HB_EXPORT hb_errPutModuleName     ( PHB_ITEM pError, char * szModuleName );
extern PHB_ITEM HB_EXPORT hb_errPutOperation      ( PHB_ITEM pError, char * szOperation );
extern PHB_ITEM HB_EXPORT hb_errPutOsCode         ( PHB_ITEM pError, USHORT uiOsCode );
extern PHB_ITEM HB_EXPORT hb_errPutSeverity       ( PHB_ITEM pError, USHORT uiSeverity );
extern PHB_ITEM HB_EXPORT hb_errPutSubCode        ( PHB_ITEM pError, USHORT uiSubCode );
extern PHB_ITEM HB_EXPORT hb_errPutSubSystem      ( PHB_ITEM pError, char * szSubSystem );
extern PHB_ITEM HB_EXPORT hb_errPutTries          ( PHB_ITEM pError, USHORT uiTries );
extern PHB_ITEM HB_EXPORT hb_errPutProcName       ( PHB_ITEM pError, char * szProcname );
extern PHB_ITEM HB_EXPORT hb_errPutProcLine       ( PHB_ITEM pError, USHORT uiProcline );
extern void     HB_EXPORT hb_errRelease           ( PHB_ITEM pError );

#ifdef HB_THREAD_SUPPORT
extern PHB_ITEM HB_EXPORT hb_errPutRunningThreads ( PHB_ITEM pError, UINT uiCount );
extern PHB_ITEM HB_EXPORT hb_errPutThreadId       ( PHB_ITEM pError, HB_THREAD_T thId );
extern PHB_ITEM HB_EXPORT hb_errPutVmThreadId     ( PHB_ITEM pError, UINT uiVmId );
#endif

extern void     HB_EXPORT hb_errRelease           ( PHB_ITEM pError );

/* Harbour additions */

extern void     HB_EXPORT hb_errInit              ( void );
extern void     HB_EXPORT hb_errExit              ( void );

extern PHB_ITEM HB_EXPORT hb_errLaunchSubst       ( PHB_ITEM pError );

extern PHB_ITEM HB_EXPORT hb_errRT_New( USHORT uiSeverity, char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags );

extern PHB_ITEM HB_EXPORT hb_errRT_New_Subst( USHORT uiSeverity, char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags );

extern USHORT   HB_EXPORT hb_errRT_BASE           ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, ULONG ulArgCount, ... );
extern USHORT   HB_EXPORT hb_errRT_BASE_Ext1      ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOsCode, USHORT uiFlags, ULONG ulArgCount, ... );
extern PHB_ITEM HB_EXPORT hb_errRT_BASE_Subst     ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, ULONG ulArgCount, ... );
extern void     HB_EXPORT hb_errRT_BASE_SubstR    ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, ULONG ulArgCount, ... );
extern USHORT   HB_EXPORT hb_errRT_TERM           ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOSCode, USHORT uiFlags );
extern USHORT   HB_EXPORT hb_errRT_DBCMD          ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation );
extern USHORT   HB_EXPORT hb_errRT_TOOLS          ( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation );

extern void     HB_EXPORT hb_errInternal          ( ULONG ulIntCode, char * szText, char * szPar1, char * szPar2 );

/* Low-level error handling */
struct HB_ERROR_INFO_;   /* forward declaration */
#define HB_ERROR_HANDLE( hbfunc )   HB_ITEM_PTR hbfunc( struct HB_ERROR_INFO_ * ErrorInfo )
typedef HB_ERROR_HANDLE( HB_ERROR_HANDLER );
typedef HB_ERROR_HANDLER * HB_ERROR_HANDLER_PTR;

typedef struct HB_ERROR_INFO_
{
   HB_ERROR_HANDLER_PTR Func;
   HB_ITEM_PTR Error;
   void * Cargo;
   struct HB_ERROR_INFO_ * Previous;
   HB_ITEM_PTR ErrorBlock;
} HB_ERROR_INFO, * HB_ERROR_INFO_PTR;

/*  set/get current error handler */
extern HB_ERROR_INFO_PTR HB_EXPORT hb_errorHandler( HB_ERROR_INFO_PTR pNewHandler );

HB_EXTERN_END

#endif /* HB_APIERR_H_ */
