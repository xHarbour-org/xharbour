/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level print functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

HB_FUNC( HWG_OPENPRINTER )
{
   hb_retnl( (LONG) CreateDC( NULL, hb_parc(1), NULL, NULL ) );
}

HB_FUNC( HWG_OPENDEFAULTPRINTER )
{
  DWORD            dwNeeded, dwReturned ;
  HDC              hDC;
  PRINTER_INFO_4 * pinfo4;
  PRINTER_INFO_5 * pinfo5;

  if (GetVersion () & 0x80000000)         // Windows 98
  {
     EnumPrinters (PRINTER_ENUM_DEFAULT, NULL, 5, NULL,
           0, &dwNeeded, &dwReturned) ;

     pinfo5 = (PRINTER_INFO_5*)malloc (dwNeeded) ;

     EnumPrinters (PRINTER_ENUM_DEFAULT, NULL, 5, (PBYTE) pinfo5,
           dwNeeded, &dwNeeded, &dwReturned) ;
     hDC = CreateDC (NULL, pinfo5->pPrinterName, NULL, NULL) ;

     free (pinfo5) ;
  }
  else                                    // Windows NT
  {
     EnumPrinters (PRINTER_ENUM_LOCAL, NULL, 4, NULL,
           0, &dwNeeded, &dwReturned) ;

     pinfo4 = (PRINTER_INFO_4*)malloc (dwNeeded) ;

     EnumPrinters (PRINTER_ENUM_LOCAL, NULL, 4, (PBYTE) pinfo4,
           dwNeeded, &dwNeeded, &dwReturned) ;
     hDC = CreateDC (NULL, pinfo4->pPrinterName, NULL, NULL) ;

     free (pinfo4) ;
  }
  hb_retnl( (LONG) hDC );   
}

HB_FUNC( HWG_STARTDOC )
{
   DOCINFO di;
   di.cbSize = sizeof(DOCINFO); 
   di.lpszDocName = hb_parc( 2 );
   di.lpszOutput = (LPTSTR) NULL; 
   di.lpszDatatype = (LPTSTR) NULL; 
   di.fwType = 0; 

   hb_retnl( (LONG) StartDoc( (HDC) hb_parnl( 1 ), &di ) );
}

HB_FUNC( HWG_ENDDOC )
{
   EndDoc( (HDC) hb_parnl( 1 ) );
}

HB_FUNC( HWG_STARTPAGE )
{
   hb_retnl( (LONG) StartPage( (HDC) hb_parnl( 1 ) ) );
}

HB_FUNC( HWG_ENDPAGE )
{
   hb_retnl( (LONG) EndPage( (HDC) hb_parnl( 1 ) ) );
}

HB_FUNC ( GETDEVICEAREA )
{
   HDC hDC = (HDC) hb_parnl( 1 );
   PHB_ITEM aMetr = hb_itemArrayNew( 7 );
   PHB_ITEM temp;

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,HORZRES ) );
   hb_itemArrayPut( aMetr, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,VERTRES ) );
   hb_itemArrayPut( aMetr, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,HORZSIZE ) );
   hb_itemArrayPut( aMetr, 3, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,VERTSIZE ) );
   hb_itemArrayPut( aMetr, 4, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,LOGPIXELSX ) );
   hb_itemArrayPut( aMetr, 5, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,LOGPIXELSY ) );
   hb_itemArrayPut( aMetr, 6, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, GetDeviceCaps( hDC,RASTERCAPS ) );
   hb_itemArrayPut( aMetr, 7, temp );
   hb_itemRelease( temp );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}

HB_FUNC( CREATEENHMETAFILE )
{
   HWND hWnd = (HWND) hb_parnl( 1 );
   HDC hDCref = GetDC( hWnd ), hDCmeta;
   LPCTSTR lpFilename = (hb_pcount()>1)? hb_parc(2):NULL;
   int iWidthMM, iHeightMM, iWidthPels, iHeightPels;
   RECT rc;
   // char cres[80];

   /* Determine the picture frame dimensions. 
    * iWidthMM is the display width in millimeters. 
    * iHeightMM is the display height in millimeters. 
    * iWidthPels is the display width in pixels. 
    * iHeightPels is the display height in pixels 
    */ 
    
   iWidthMM = GetDeviceCaps(hDCref, HORZSIZE); 
   iHeightMM = GetDeviceCaps(hDCref, VERTSIZE); 
   iWidthPels = GetDeviceCaps(hDCref, HORZRES); 
   iHeightPels = GetDeviceCaps(hDCref, VERTRES); 

    
   /* 
    * Retrieve the coordinates of the client 
    * rectangle, in pixels. 
    */ 
    
   GetClientRect( hWnd, &rc );
   // sprintf( cres,"%d %d %d %d %d %d %d %d",iWidthMM, iHeightMM, iWidthPels, iHeightPels,rc.left,rc.top,rc.right,rc.bottom );
   // MessageBox( GetActiveWindow(), cres, "", MB_OK | MB_ICONINFORMATION );
    
   /* 
    * Convert client coordinates to .01-mm units. 
    * Use iWidthMM, iWidthPels, iHeightMM, and 
    * iHeightPels to determine the number of 
    * .01-millimeter units per pixel in the x- 
    *  and y-directions. 
    */ 
    
   rc.left   = (rc.left * iWidthMM * 100)/iWidthPels; 
   rc.top    = (rc.top * iHeightMM * 100)/iHeightPels; 
   rc.right  = (rc.right * iWidthMM * 100)/iWidthPels; 
   rc.bottom = (rc.bottom * iHeightMM * 100)/iHeightPels; 

   hDCmeta = CreateEnhMetaFile( hDCref, lpFilename, &rc, NULL );
   ReleaseDC( hWnd, hDCref );
   hb_retnl( (LONG) hDCmeta );

}

HB_FUNC( CREATEMETAFILE )
{
   HDC hDCref = (HDC) hb_parnl( 1 ), hDCmeta;
   LPCTSTR lpFilename = (hb_pcount()>1 && !ISNIL(2))? hb_parc(2):NULL;
   int iWidthMM, iHeightMM;
   RECT rc;

   /* Determine the picture frame dimensions. 
    * iWidthMM is the display width in millimeters. 
    * iHeightMM is the display height in millimeters. 
    * iWidthPels is the display width in pixels. 
    * iHeightPels is the display height in pixels 
    */ 
    
   iWidthMM = GetDeviceCaps(hDCref, HORZSIZE); 
   iHeightMM = GetDeviceCaps(hDCref, VERTSIZE); 

   /* 
    * Convert client coordinates to .01-mm units. 
    * Use iWidthMM, iWidthPels, iHeightMM, and 
    * iHeightPels to determine the number of 
    * .01-millimeter units per pixel in the x- 
    *  and y-directions. 
    */ 
    
   rc.left   = 0;
   rc.top    = 0;
   rc.right  = iWidthMM * 100;
   rc.bottom = iHeightMM * 100;

   hDCmeta = CreateEnhMetaFile( hDCref, lpFilename, &rc, NULL );
   hb_retnl( (LONG) hDCmeta );

}

HB_FUNC( CLOSEENHMETAFILE )
{
   hb_retnl( (LONG) CloseEnhMetaFile( (HDC) hb_parnl(1) ) );
}

HB_FUNC( DELETEENHMETAFILE )
{
   hb_retnl( (LONG) DeleteEnhMetaFile( (HENHMETAFILE) hb_parnl(1) ) );
}

HB_FUNC( PLAYENHMETAFILE )
{
   HDC hDC = (HDC) hb_parnl(1);
   RECT rc;

   if( hb_pcount() > 2 )
   {
      rc.left   = hb_parni(3);
      rc.top    = hb_parni(4);
      rc.right  = hb_parni(5);
      rc.bottom = hb_parni(6);
   }
   else
      GetClientRect( WindowFromDC( hDC ), &rc );
   hb_retnl( (LONG) PlayEnhMetaFile( hDC, (HENHMETAFILE)  hb_parnl(2), &rc ) );
}

HB_FUNC( PRINTENHMETAFILE )
{
   HDC hDC = (HDC) hb_parnl(1);
   // DOCINFO di;
   RECT rc;

   /*
   di.cbSize = sizeof(DOCINFO); 
   di.lpszDocName = hb_parc( 3 );
   di.lpszOutput = (LPTSTR) NULL; 
   di.lpszDatatype = (LPTSTR) NULL; 
   di.fwType = 0; 
   */

   SetRect( &rc, 0, 0, GetDeviceCaps( hDC, HORZRES ), GetDeviceCaps( hDC, VERTRES ) );

   // StartDoc( hDC, &di );
   StartPage( hDC );
   hb_retnl( (LONG) PlayEnhMetaFile( hDC, (HENHMETAFILE)  hb_parnl(2), &rc ) );
   EndPage( hDC );
   // EndDoc( hDC );
}
