/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level painting functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "item.api"

HB_FUNC ( INVALIDATERECT )
{
   RECT rc;

   if( hb_pcount() > 2 )
   {
      rc.left = hb_parni( 3 );
      rc.top = hb_parni( 4 );
      rc.right = hb_parni( 5 );
      rc.bottom = hb_parni( 6 );
   }

   InvalidateRect(
    (HWND) hb_parnl( 1 ),	// handle of window with changed update region  
    ( hb_pcount() > 2 )? &rc:NULL,	// address of rectangle coordinates 
    hb_parni( 2 )	// erase-background flag 
   );
}

HB_FUNC ( RECTANGLE )
{
   HDC hDC = (HDC) hb_parnl( 1 );
   int x1 = hb_parni( 2 ), y1 = hb_parni( 3 ), x2 = hb_parni( 4 ), y2 = hb_parni( 5 );
   MoveToEx( hDC, x1, y1, NULL );
   LineTo( hDC, x2, y1 );
   LineTo( hDC, x2, y2 );
   LineTo( hDC, x1, y2 );
   LineTo( hDC, x1, y1 );
/*
   Rectangle(
    (HDC) hb_parnl( 1 ),	// handle of device context 
    hb_parni( 2 ),	// x-coord. of bounding rectangle's upper-left corner 
    hb_parni( 3 ),	// y-coord. of bounding rectangle's upper-left corner 
    hb_parni( 4 ),	// x-coord. of bounding rectangle's lower-right corner  
    hb_parni( 5 ) 	// y-coord. of bounding rectangle's lower-right corner  
   );
*/
}

HB_FUNC ( DRAWLINE )
{
   MoveToEx( (HDC) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL );
   LineTo( (HDC) hb_parnl( 1 ), hb_parni( 4 ), hb_parni( 5 ) );
}

HB_FUNC( PIE )
{
   int res = Pie(
    (HDC) hb_parnl(1),	// handle to device context 
    hb_parni(2),	// x-coord. of bounding rectangle's upper-left corner 
    hb_parni(3),	// y-coord. of bounding rectangle's upper-left corner  
    hb_parni(4),	// x-coord. of bounding rectangle's lower-right corner  
    hb_parni(5), 	// y-coord. bounding rectangle's f lower-right corner  
    hb_parni(6),	// x-coord. of first radial's endpoint 
    hb_parni(7),	// y-coord. of first radial's endpoint 
    hb_parni(8),	// x-coord. of second radial's endpoint 
    hb_parni(9) 	// y-coord. of second radial's endpoint 
   );
   if( !res )
     hb_retnl( (LONG) GetLastError() );
   else
     hb_retnl( 0 );
}

HB_FUNC( ELLIPSE )
{
   int res =  Ellipse(
    (HDC) hb_parnl(1),	// handle to device context 
    hb_parni(2),	// x-coord. of bounding rectangle's upper-left corner 
    hb_parni(3),	// y-coord. of bounding rectangle's upper-left corner  
    hb_parni(4),	// x-coord. of bounding rectangle's lower-right corner  
    hb_parni(5) 	// y-coord. bounding rectangle's f lower-right corner  
   );
   if( !res )
     hb_retnl( (LONG) GetLastError() );
   else
     hb_retnl( 0 );
}

HB_FUNC ( FILLRECT )
{
   RECT rc;

   rc.left = hb_parni( 2 );
   rc.top = hb_parni( 3 );
   rc.right = hb_parni( 4 );
   rc.bottom = hb_parni( 5 );

   FillRect(
    (HDC) hb_parnl( 1 ),      // handle to device context 
    &rc,                      // pointer to structure with rectangle  
    (HBRUSH) hb_parnl( 6 )    // handle to brush 
   );
}

HB_FUNC ( ROUNDRECT )
{
   hb_parl( RoundRect(
    (HDC) hb_parnl( 1 ),   // handle of device context 
    hb_parni( 2 ),         // x-coord. of bounding rectangle's upper-left corner 
    hb_parni( 3 ),         // y-coord. of bounding rectangle's upper-left corner 
    hb_parni( 4 ),         // x-coord. of bounding rectangle's lower-right corner 
    hb_parni( 5 ),         // y-coord. of bounding rectangle's lower-right corner 
    hb_parni( 6 ),         // width of ellipse used to draw rounded corners  
    hb_parni( 7 )          // height of ellipse used to draw rounded corners  
   ) );
}

HB_FUNC ( REDRAWWINDOW )
{
   RedrawWindow(
    (HWND) hb_parnl( 1 ),  // handle of window
    NULL,                  // address of structure with update rectangle
    NULL,                  // handle of update region
    (UINT)hb_parni( 2 )    // array of redraw flags
   );
}

HB_FUNC ( DRAWBUTTON )
{
   RECT rc;
   HDC hDC = (HDC) hb_parnl( 1 );
   UINT iType = hb_parni( 6 );

   rc.left = hb_parni( 2 );
   rc.top = hb_parni( 3 );
   rc.right = hb_parni( 4 );
   rc.bottom = hb_parni( 5 );

   if( iType == 0 )
      FillRect( hDC, &rc, (HBRUSH) (COLOR_3DFACE+1) );
   else
   {
      FillRect( hDC, &rc, (HBRUSH) ( ( (iType & 2)? COLOR_3DSHADOW:COLOR_3DHILIGHT )+1) );
      rc.left ++; rc.top ++;
      FillRect( hDC, &rc, (HBRUSH) ( ( (iType & 2)? COLOR_3DHILIGHT:(iType & 4)? COLOR_3DDKSHADOW:COLOR_3DSHADOW )+1) );
      rc.right --; rc.bottom --;
      if( iType & 4 )
      {
         FillRect( hDC, &rc, (HBRUSH) ( ( (iType & 2)? COLOR_3DSHADOW:COLOR_3DLIGHT )+1) );
         rc.left ++; rc.top ++;
         FillRect( hDC, &rc, (HBRUSH) ( ( (iType & 2)? COLOR_3DLIGHT:COLOR_3DSHADOW )+1) );
         rc.right --; rc.bottom --;
      }
      FillRect( hDC, &rc, (HBRUSH) (COLOR_3DFACE+1) );
   }
}

/*
 * DrawEdge( hDC,x1,y1,x2,y2,nFlag,nBorder )
 */
HB_FUNC ( DRAWEDGE )
{
   RECT rc;
   HDC hDC = (HDC) hb_parnl( 1 );
   UINT edge = (UINT) hb_parni(6);
   UINT grfFlags = (UINT) hb_parni(7);

   rc.left = hb_parni( 2 );
   rc.top = hb_parni( 3 );
   rc.right = hb_parni( 4 );
   rc.bottom = hb_parni( 5 );

   hb_retl( DrawEdge( hDC, &rc, edge, grfFlags ) );
}

HB_FUNC ( LOADICON )
{
   if( ISNUM(1) )
      hb_retnl( (LONG) LoadIcon( NULL, (LPCTSTR) hb_parnl( 1 ) ) );
   else
      hb_retnl( (LONG) LoadIcon( GetModuleHandle( NULL ), (LPCTSTR) hb_parc( 1 ) ) );
}

HB_FUNC ( LOADIMAGE )
{
   hb_retnl( (LONG) 
          LoadImage( (HINSTANCE)hb_parnl(1),    // handle of the instance that contains the image
                  (LPCTSTR)hb_parc(2),          // name or identifier of image
                  (UINT) hb_parni(3),           // type of image
                  hb_parni(4),                  // desired width
                  hb_parni(5),                  // desired height
                  (UINT)hb_parni(6)             // load flags
   ) );
}

HB_FUNC ( LOADBITMAP )
{
   if( ISNUM(1) )
      hb_retnl( (LONG) LoadBitmap( NULL, (LPCTSTR) hb_parnl( 1 ) ) );
   else
      hb_retnl( (LONG) LoadBitmap( GetModuleHandle( NULL ), (LPCTSTR) hb_parc( 1 ) ) );
}

/*
 * Window2Bitmap( hWnd )
 */
HB_FUNC ( WINDOW2BITMAP )
{
   HWND hWnd = (HWND) hb_parnl(1);
   BOOL lFull = ( ISNIL(2) )? 0 : (BOOL)hb_parl(2);
   HDC hDC = ( lFull )? GetWindowDC( hWnd ) : GetDC( hWnd );
   HDC hDCmem = CreateCompatibleDC( hDC );
   HBITMAP hBitmap;
   RECT rc;

   if( lFull )
      GetWindowRect( hWnd, &rc );
   else
      GetClientRect( hWnd, &rc );
   hBitmap = CreateCompatibleBitmap( hDC, rc.right-rc.left, rc.bottom-rc.top );
   SelectObject( hDCmem, hBitmap );

   BitBlt( hDCmem, 0, 0, rc.right-rc.left, rc.bottom-rc.top, hDC, 0, 0, SRCCOPY );

   DeleteDC( hDCmem );
   DeleteDC( hDC );
   hb_retnl( (LONG) hBitmap );
}

/*
 * DrawBitmap( hDC, hBitmap, style, x, y, width, height )
 */
HB_FUNC ( DRAWBITMAP )
{
   HDC hDC = (HDC) hb_parnl( 1 );
   HDC hDCmem = CreateCompatibleDC( hDC );
   DWORD dwraster = (ISNIL(3))? SRCCOPY:hb_parnl(3);
   HBITMAP hBitmap = (HBITMAP) hb_parnl( 2 );
   BITMAP  bitmap;
   int nWidthDest = ( hb_pcount()>=5 && !ISNIL(6) )? hb_parni(6):0;
   int nHeightDest = ( hb_pcount()>=6 && !ISNIL(7) )? hb_parni(7):0;

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   if( nWidthDest && ( nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight ))
   {
      StretchBlt( hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem, 
                  0, 0, bitmap.bmWidth, bitmap.bmHeight, dwraster );
   }
   else
   {
      BitBlt( hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );
   }

   DeleteDC( hDCmem );
}

/*
 * DrawTransparentBitmap( hDC, hBitmap, x, y )
 */
HB_FUNC ( DRAWTRANSPARENTBITMAP )
{
   HDC hDC = (HDC) hb_parnl( 1 );
   HBITMAP hBitmap = (HBITMAP) hb_parnl( 2 );
   COLORREF crOldBack = SetBkColor( hDC, 0x00FFFFFF );
   COLORREF crOldText = SetTextColor( hDC, 0 );
   HBITMAP bitmapTrans;
   HBITMAP pOldBitmapImage, pOldBitmapTrans;
   BITMAP  bitmap;
   HDC dcImage, dcTrans;
   int x = hb_parni( 3 );
   int y = hb_parni( 4 );

   // Create two memory dcs for the image and the mask
   dcImage = CreateCompatibleDC( hDC );
   dcTrans = CreateCompatibleDC( hDC );
   // Select the image into the appropriate dc
   pOldBitmapImage = (HBITMAP) SelectObject( dcImage, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   // Create the mask bitmap
   bitmapTrans = CreateBitmap( bitmap.bmWidth, bitmap.bmHeight, 1, 1, NULL);
   // Select the mask bitmap into the appropriate dc
   pOldBitmapTrans = (HBITMAP) SelectObject( dcTrans, bitmapTrans );
   // Build mask based on transparent colour
   SetBkColor( dcImage, 0x00FFFFFF );
   BitBlt( dcTrans, 0, 0, bitmap.bmWidth, bitmap.bmHeight, dcImage, 0, 0, SRCCOPY );
   // Do the work - True Mask method - cool if not actual display
   BitBlt( hDC, x, y, bitmap.bmWidth, bitmap.bmHeight, dcImage, 0, 0, SRCINVERT );
   BitBlt( hDC, x, y, bitmap.bmWidth, bitmap.bmHeight, dcTrans, 0, 0, SRCAND );
   BitBlt( hDC, x, y, bitmap.bmWidth, bitmap.bmHeight, dcImage, 0, 0, SRCINVERT );
   // Restore settings
   SelectObject( dcImage, pOldBitmapImage);
   SelectObject( dcTrans, pOldBitmapTrans );
   SetBkColor( hDC,crOldBack );
   SetTextColor( hDC,crOldText );

   DeleteObject( bitmapTrans );
   DeleteDC( dcImage );
   DeleteDC( dcTrans );
}

/*  SpreadBitmap( hDC, hWnd, hBitmap, style )
*/
HB_FUNC ( SPREADBITMAP )
{
   HDC hDC = (HDC) hb_parnl( 1 );
   HDC hDCmem = CreateCompatibleDC( hDC );
   DWORD dwraster = (ISNIL(4))? SRCCOPY:hb_parnl(4);
   HBITMAP hBitmap = (HBITMAP) hb_parnl( 3 );
   BITMAP  bitmap;
   RECT rc;

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   GetClientRect( (HWND) hb_parnl( 2 ),	&rc );

   while( rc.top < rc.bottom )
   {
      while( rc.left < rc.right )
      {
         BitBlt( hDC, rc.left, rc.top, bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );
         rc.left += bitmap.bmWidth;
      }
      rc.left = 0;
      rc.top += bitmap.bmHeight;
   }

   DeleteDC( hDCmem );
}

HB_FUNC ( GETBITMAPSIZE )
{
   BITMAP  bitmap;
   PHB_ITEM aMetr = _itemArrayNew( 2 );
   PHB_ITEM temp;

   GetObject( (HBITMAP) hb_parnl( 1 ), sizeof( BITMAP ), ( LPVOID ) &bitmap );

   temp = _itemPutNL( NULL, bitmap.bmWidth );
   _itemArrayPut( aMetr, 1, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, bitmap.bmHeight );
   _itemArrayPut( aMetr, 2, temp );
   _itemRelease( temp );

   _itemReturn( aMetr );
   _itemRelease( aMetr );
}

HB_FUNC ( OPENBITMAP )
{
   BITMAPFILEHEADER bmfh;
   BITMAPINFOHEADER bmih;
   LPBITMAPINFO lpbmi;
   DWORD dwRead;
   LPVOID lpvBits;
   HGLOBAL hmem1, hmem2;
   HBITMAP hbm;
   HDC hDC = (hb_pcount()>1 && !ISNIL(2))? (HDC)hb_parnl(2):NULL;
   HANDLE hfbm = CreateFile( hb_parc( 1 ), GENERIC_READ, FILE_SHARE_READ, 
                   (LPSECURITY_ATTRIBUTES) NULL, OPEN_EXISTING, 
                   FILE_ATTRIBUTE_READONLY, (HANDLE) NULL );

   if( ( (long int)hfbm ) <= 0 )
   {
      hb_retnl( -1 );
      return;
   }
   /* Retrieve the BITMAPFILEHEADER structure. */ 
   ReadFile( hfbm, &bmfh, sizeof(BITMAPFILEHEADER), &dwRead, NULL );
 
   /* Retrieve the BITMAPFILEHEADER structure. */ 
   ReadFile( hfbm, &bmih, sizeof(BITMAPINFOHEADER), &dwRead, NULL );
 
   /* Allocate memory for the BITMAPINFO structure. */ 
 
   hmem1 = GlobalAlloc( GHND, sizeof(BITMAPINFOHEADER) + 
             ((1<<bmih.biBitCount) * sizeof(RGBQUAD))); 
   lpbmi = (LPBITMAPINFO)GlobalLock( hmem1 );
 
   /*  Load BITMAPINFOHEADER into the BITMAPINFO  structure. */ 
   lpbmi->bmiHeader.biSize = bmih.biSize; 
   lpbmi->bmiHeader.biWidth = bmih.biWidth; 
   lpbmi->bmiHeader.biHeight = bmih.biHeight; 
   lpbmi->bmiHeader.biPlanes = bmih.biPlanes; 

   lpbmi->bmiHeader.biBitCount = bmih.biBitCount; 
   lpbmi->bmiHeader.biCompression = bmih.biCompression; 
   lpbmi->bmiHeader.biSizeImage = bmih.biSizeImage; 
   lpbmi->bmiHeader.biXPelsPerMeter = bmih.biXPelsPerMeter; 
   lpbmi->bmiHeader.biYPelsPerMeter = bmih.biYPelsPerMeter; 
   lpbmi->bmiHeader.biClrUsed = bmih.biClrUsed; 
   lpbmi->bmiHeader.biClrImportant = bmih.biClrImportant; 
 
   /*  Retrieve the color table. 
    * 1 << bmih.biBitCount == 2 ^ bmih.biBitCount 
   */ 
   ReadFile(hfbm, lpbmi->bmiColors, 
      ((1<<bmih.biBitCount) * sizeof(RGBQUAD)), 
      &dwRead, (LPOVERLAPPED) NULL); 
 
   /* Allocate memory for the required number of  bytes. */ 
   hmem2 = GlobalAlloc( GHND, (bmfh.bfSize - bmfh.bfOffBits) );
   lpvBits = GlobalLock(hmem2); 
 
   /* Retrieve the bitmap data. */ 
 
   ReadFile(hfbm, lpvBits, (bmfh.bfSize - bmfh.bfOffBits), &dwRead, NULL );

   if( !hDC )
      hDC = GetDC( 0 );
  /* Create a bitmap from the data stored in the .BMP file.  */ 
   hbm = CreateDIBitmap( hDC, &bmih, CBM_INIT, lpvBits, lpbmi, DIB_RGB_COLORS );
   if( hb_pcount() < 2 || ISNIL(2) )
      ReleaseDC( 0, hDC );
 
  /* Unlock the global memory objects and close the .BMP file. */ 
 
   GlobalUnlock(hmem1); 
   GlobalUnlock(hmem2); 
   CloseHandle(hfbm); 
   hb_retnl( (LONG) hbm );
}

HB_FUNC ( DRAWICON )
{
   DrawIcon( (HDC)hb_parnl( 1 ), hb_parni( 3 ), hb_parni( 4 ), (HICON)hb_parnl( 2 ) );
}

HB_FUNC ( GETSYSCOLOR )
{
   hb_retnl( (LONG) GetSysColor( hb_parni( 1 ) ) );
}

HB_FUNC ( CREATEPEN )
{
   hb_retnl( (LONG) CreatePen(
               hb_parni( 1 ),	// pen style 
               hb_parni( 2 ),	// pen width  
               (COLORREF) hb_parnl( 3 ) 	// pen color 
             ) );
}

HB_FUNC ( CREATESOLIDBRUSH )
{
   hb_retnl( (LONG) CreateSolidBrush(
               (COLORREF) hb_parnl( 1 ) 	// brush color
             ) );
}

HB_FUNC ( CREATEHATCHBRUSH )
{
   hb_retnl( (LONG) CreateHatchBrush(
               hb_parni(1), (COLORREF) hb_parnl(2) ) );
}

HB_FUNC ( SELECTOBJECT )
{
   hb_retnl( (LONG) SelectObject(
              (HDC) hb_parnl( 1 ),	// handle of device context 
              (HGDIOBJ) hb_parnl( 2 ) 	// handle of object  
             ) );
}

HB_FUNC ( DELETEOBJECT )
{
   DeleteObject(
      (HGDIOBJ) hb_parnl( 1 ) 	// handle of object  
   );
}

HB_FUNC ( GETDC )
{
   hb_retnl( (LONG) GetDC( (HWND) hb_parnl( 1 ) ) );
}

HB_FUNC ( RELEASEDC )
{
   hb_retnl( (LONG) ReleaseDC( (HWND) hb_parnl( 1 ), (HDC) hb_parnl( 2 ) ) );
}

HB_FUNC( GETDRAWITEMINFO )
{
   DRAWITEMSTRUCT * lpdis = (DRAWITEMSTRUCT*)hb_parnl(1);
   PHB_ITEM aMetr = _itemArrayNew( 7 );
   PHB_ITEM temp;

   temp = _itemPutNL( NULL, lpdis->itemID );
   _itemArrayPut( aMetr, 1, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, lpdis->itemAction );
   _itemArrayPut( aMetr, 2, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, (LONG)lpdis->hDC );
   _itemArrayPut( aMetr, 3, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, lpdis->rcItem.left );
   _itemArrayPut( aMetr, 4, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, lpdis->rcItem.top );
   _itemArrayPut( aMetr, 5, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, lpdis->rcItem.right );
   _itemArrayPut( aMetr, 6, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, lpdis->rcItem.bottom );
   _itemArrayPut( aMetr, 7, temp );
   _itemRelease( temp );

   _itemReturn( aMetr );
   _itemRelease( aMetr );
}
