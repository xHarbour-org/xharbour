/*
 * $Id: isprint.c,v 1.25 2003/12/12 10:16:06 druzus Exp $
 */

/*
 * Harbour Project source code:
 * ISPRINTER() function
 *
 * Copyright 1999-2002 Viktor Szakats <viktor.szakats@syenar.hu>
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
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    ISPRINTER() support for win32
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbapiitm.h"

#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))

   #include <stdio.h>
   #include <winspool.h>

   static DWORD IsPrinterError(HANDLE hPrinter);

   static BOOL GetJobs(HANDLE hPrinter,JOB_INFO_2 **ppJobInfo,int *pcJobs);
          DWORD hb_printerIsReadyn( char * pszPrinterName );
   static DWORD IsPrinterErrorn(HANDLE hPrinter);
   extern BOOL hb_GetDefaultPrinter(LPTSTR pPrinterName, LPDWORD pdwBufferSize);
#endif
#define MAXBUFFERSIZE 255    // 7/10/2003 12:51p.m.

BOOL hb_printerIsReady( char * pszPrinterName )
{
   BOOL bIsPrinter;

#if defined(HB_OS_DOS)

   /* NOTE: DOS specific solution, using BIOS interrupt */

   {
      USHORT uiPort;

      if( hb_strnicmp( pszPrinterName, "PRN", 3 ) == 0 )
      {
         union REGS regs;

         regs.h.ah = 2;
         regs.HB_XREGS.dx = 0; /* LPT1 */

         HB_DOS_INT86( 0x17, &regs, &regs );

         bIsPrinter = ( regs.h.ah == 0x90 );
      }
      else if( strlen( pszPrinterName ) >= 4 &&
               hb_strnicmp( pszPrinterName, "LPT", 3 ) == 0 &&
               ( uiPort = atoi( pszPrinterName + 3 ) ) > 0 )
      {
         union REGS regs;

         regs.h.ah = 2;
         regs.HB_XREGS.dx = uiPort - 1;

         HB_DOS_INT86( 0x17, &regs, &regs );

         bIsPrinter = ( regs.h.ah == 0x90 );
      }
      else
         bIsPrinter = FALSE;
   }

#elif defined(HB_OS_WIN_32) && !defined(__RSXNT__)

   {
      HANDLE hPrinter;

      if (*pszPrinterName) {
        bIsPrinter= OpenPrinter( pszPrinterName, &hPrinter, NULL ) ;
        if (bIsPrinter ) {
          bIsPrinter = (BOOL) !IsPrinterError( hPrinter );
          CloseHandle(hPrinter) ;
        }
      }
      else
        bIsPrinter = FALSE ;
   }

#else

   /* NOTE: Platform independent method, at least it will compile and run
            on any platform, but the result may not be the expected one,
            since Unix/Linux doesn't support LPT/COM by nature, other OSs
            may not reflect the actual physical presence of the printer when
            trying to open it, since we are talking to the spooler.
            [vszakats] */

   {
      FHANDLE fhnd = hb_fsOpen( ( BYTE * ) pszPrinterName, FO_WRITE | FO_SHARED | FO_PRIVATE );
      bIsPrinter = ( fhnd != FS_ERROR );
      hb_fsClose( fhnd );
   }

#endif

   return bIsPrinter;
}


/* NOTE: The parameter is an extension over CA-Cl*pper, it's also supported
         by Xbase++. [vszakats] */

HB_FUNC( ISPRINTER )
{
   #if defined(HB_OS_WIN_32) && !defined(__RSXNT__)
   {
      char DefaultPrinter[MAXBUFFERSIZE];
      DWORD pdwBufferSize = MAXBUFFERSIZE;
      hb_GetDefaultPrinter( ( LPTSTR ) &DefaultPrinter, &pdwBufferSize);
      hb_retl( hb_printerIsReady( ISCHAR( 1 ) ? hb_parcx( 1 ) : (char*)DefaultPrinter ) );
   }
   #else
      hb_retl( hb_printerIsReady( ISCHAR( 1 ) ? hb_parcx( 1 ) : "LPT1" ) );
   #endif
}

/* The code below does the check for the printer under Win32 */

#if defined(HB_OS_WIN_32) && !defined(__RSXNT__) && !defined(__CYGWIN__)

static DWORD IsPrinterError( HANDLE hPrinter ) {
  BOOL Result = -1 ;
  PRINTER_INFO_2 * pPrinterInfo;
  DWORD cByteNeeded;
  HB_TRACE(HB_TR_DEBUG, ("isprint.c IsPrinterError()"));
  GetPrinter(hPrinter, 2, NULL, 0, &cByteNeeded);
  if (cByteNeeded>0) {
    pPrinterInfo = (PRINTER_INFO_2 *) hb_xgrab(cByteNeeded);
    if (pPrinterInfo) {
      if (GetPrinter(hPrinter,2,(LPBYTE)pPrinterInfo,cByteNeeded,&cByteNeeded))
        Result = pPrinterInfo->Status;
      hb_xfree(pPrinterInfo) ;
    }
  }
  return Result;
}


static DWORD IsPrinterErrorn( HANDLE hPrinter ) {
  JOB_INFO_2  *pJobs;
  int         cJobs,i;
  DWORD       dwError;
  dwError = IsPrinterError(hPrinter) ;  // Just return the PrinterStatus
/*

// This original logic is flawed - it works if there is only one error on the printer
//   but the WinApi allows a combination of flags.
//   The caller to XISPRINTER() should take the return value and determine the error(s).
//   8/10/2003 12:46p.m. Peter Rees

  if (dwError) {
    switch (dwPrinterStatus &
        (PRINTER_STATUS_ERROR |
         PRINTER_STATUS_PAPER_JAM |
         PRINTER_STATUS_PAPER_OUT |
         PRINTER_STATUS_PAPER_PROBLEM |
         PRINTER_STATUS_OUTPUT_BIN_FULL |
         PRINTER_STATUS_NOT_AVAILABLE |
         PRINTER_STATUS_NO_TONER |
         PRINTER_STATUS_OUT_OF_MEMORY |
         PRINTER_STATUS_OFFLINE |
         PRINTER_STATUS_DOOR_OPEN) ) {

    case PRINTER_STATUS_ERROR :
      dwError = 10;
      break;
    case PRINTER_STATUS_PAPER_JAM :
      dwError = 11;
      break;
    case PRINTER_STATUS_PAPER_OUT :
      dwError = 12;
      break;
    case PRINTER_STATUS_PAPER_PROBLEM :
      dwError = 13;
      break;
    case PRINTER_STATUS_OUTPUT_BIN_FULL :
      dwError = 14;
      break;
    case PRINTER_STATUS_NOT_AVAILABLE :
      dwError = 15;
      break;
    case PRINTER_STATUS_NO_TONER :
      dwError = 16;
      break;
    case PRINTER_STATUS_OUT_OF_MEMORY :
      dwError = 17;
      break;
    case PRINTER_STATUS_OFFLINE :
      dwError = 18;
      break;
    case PRINTER_STATUS_DOOR_OPEN:
      dwError = 19;
      break;
    }
*/
  if (!dwError) {
    if (GetJobs(hPrinter, &pJobs, &cJobs)) {
      for (i=0; !dwError && i < cJobs; i++) {
        if (pJobs[i].Status & JOB_STATUS_ERROR)
          dwError = -20 ;
        else if (pJobs[i].Status & JOB_STATUS_OFFLINE)
          dwError = -21 ;
        else if (pJobs[i].Status & JOB_STATUS_PAPEROUT)
          dwError = -22 ;
        else if (pJobs[i].Status & JOB_STATUS_BLOCKED_DEVQ)
          dwError = -23 ;
      }
      hb_xfree(pJobs) ;
    }
  }
  return dwError;
}

static BOOL GetJobs(HANDLE hPrinter,JOB_INFO_2 **ppJobInfo,int *pcJobs) {
  DWORD Result = FALSE ;
  DWORD cByteNeeded;
  DWORD nReturned;
  DWORD cByteUsed;
  JOB_INFO_2 * pJobStorage;
  PRINTER_INFO_2 * pPrinterInfo;
  HB_TRACE(HB_TR_DEBUG, ("isprint.c GetJobs()"));
  GetPrinter(hPrinter, 2, NULL, 0, &cByteNeeded);
  if (cByteNeeded>0) {
    pPrinterInfo = (PRINTER_INFO_2 *) hb_xgrab(cByteNeeded);
    if (pPrinterInfo) {
      if (GetPrinter(hPrinter,2,(LPBYTE)pPrinterInfo,cByteNeeded,&cByteUsed)) {
        EnumJobs(hPrinter,0,pPrinterInfo->cJobs,2,NULL,0,(LPDWORD)&cByteNeeded,(LPDWORD)&nReturned) ;
        if (cByteNeeded>0) {
          pJobStorage = (JOB_INFO_2 *) hb_xgrab(cByteNeeded);
          if (pJobStorage) {
            if (EnumJobs(hPrinter,0,nReturned,2,(LPBYTE)pJobStorage,cByteNeeded,(LPDWORD)&cByteUsed,(LPDWORD)&nReturned)) {
              *pcJobs = nReturned;
              *ppJobInfo = pJobStorage;
              Result = TRUE ;
            }
            else {
              hb_xfree(pJobStorage) ;
            }
          }
        }
      }
      hb_xfree(pPrinterInfo);
    }
  }
  return Result;
}

DWORD hb_printerIsReadyn( char * pszPrinterName )
{
  DWORD dwPrinter= -1;
  HANDLE hPrinter;

  if (*pszPrinterName && OpenPrinter( pszPrinterName, &hPrinter, NULL )) {
    dwPrinter =  IsPrinterErrorn( hPrinter );
    CloseHandle(hPrinter) ;
  }
  return dwPrinter;
}

HB_FUNC( XISPRINTER )
{
  char DefaultPrinter[MAXBUFFERSIZE];
  DWORD pdwBufferSize = MAXBUFFERSIZE;
  hb_GetDefaultPrinter( ( LPTSTR ) &DefaultPrinter, &pdwBufferSize);
  hb_retnl( hb_printerIsReadyn( ISCHAR( 1 ) ? hb_parcx( 1 ) : (char*)DefaultPrinter ) );
}

#endif

