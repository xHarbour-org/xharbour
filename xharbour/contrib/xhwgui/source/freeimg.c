/*
 * FreeImage wrappers for Harbour/HwGUI
 *
 * Copyright 2003 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

/*
// How do I convert a HBITMAP to a FreeImage image ?

   HBITMAP hbmp;
   FIBITMAP *dib;
   if(hbmp) { 
      BITMAP bm;
      GetObject(hbmp, sizeof(BITMAP), (LPSTR) &bm);
      dib = FreeImage_Allocate(bm.bmWidth, bm.bmHeight, bm.bmBitsPixel);
      HDC dc = GetDC(NULL);
      int Success = GetDIBits(dc, hbmp, 0, FreeImage_GetHeight(dib), 
         FreeImage_GetBits(dib), FreeImage_GetInfo(dib), DIB_RGB_COLORS);
      ReleaseDC(NULL, dc);
   }
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "FreeImage.h"

typedef char * ( WINAPI *FREEIMAGE_GETVERSION )( void );
typedef FIBITMAP* ( WINAPI *FREEIMAGE_LOAD)( FREE_IMAGE_FORMAT fif, char *filename, int flags FI_DEFAULT(0) );
typedef void ( WINAPI *FREEIMAGE_UNLOAD )( FIBITMAP *dib );
typedef FIBITMAP* ( WINAPI *FREEIMAGE_ALLOCATE)( int width, int height, int bpp );
typedef BOOL ( WINAPI *FREEIMAGE_SAVE)( FREE_IMAGE_FORMAT fif, FIBITMAP* dib, char *filename, int flags FI_DEFAULT(0) );
typedef FREE_IMAGE_FORMAT ( WINAPI *FREEIMAGE_GETFIFFROMFILENAME)( char *filename);
typedef ULONG ( WINAPI *FREEIMAGE_GETWIDTH )( FIBITMAP *dib );
typedef ULONG ( WINAPI *FREEIMAGE_GETHEIGHT )( FIBITMAP *dib );
typedef BYTE * ( WINAPI *FREEIMAGE_GETBITS )( FIBITMAP *dib );
typedef BITMAPINFO * ( WINAPI *FREEIMAGE_GETINFO )( FIBITMAP *dib );
typedef BITMAPINFOHEADER * ( WINAPI *FREEIMAGE_GETINFOHEADER )( FIBITMAP *dib );

static HINSTANCE hFreeImageDll = NULL;
static FREEIMAGE_LOAD pLoad = NULL;
static FREEIMAGE_UNLOAD pUnload = NULL;
static FREEIMAGE_ALLOCATE pAllocate = NULL;
static FREEIMAGE_SAVE pSave = NULL;
static FREEIMAGE_GETFIFFROMFILENAME pGetfiffromfile = NULL;
static FREEIMAGE_GETWIDTH pGetwidth = NULL;
static FREEIMAGE_GETHEIGHT pGetheight = NULL;
static FREEIMAGE_GETBITS pGetbits = NULL;
static FREEIMAGE_GETINFO pGetinfo = NULL;
static FREEIMAGE_GETINFOHEADER pGetinfoHead = NULL;

BOOL FreeImgInit( void )
{
   if( !hFreeImageDll )
   {
      hFreeImageDll = LoadLibrary( (LPCTSTR)"FreeImage.dll" );
      if( !hFreeImageDll )
      {
         MessageBox( GetActiveWindow(), "Library not loaded", "FreeImage.dll", MB_OK | MB_ICONSTOP );
         return 0;
      }
   }
   return 1;
}

FARPROC GetFunction( FARPROC h, LPCSTR funcname )
{
   if( !h )
   {
      if( !hFreeImageDll && !FreeImgInit() )
      {
         return (FARPROC)NULL;
      }
      else
         return GetProcAddress( hFreeImageDll, funcname );
   }
   else
      return h;
}

HB_FUNC( FI_INIT )
{
   hb_retl( FreeImgInit() );
}

HB_FUNC( FI_END )
{
   if( hFreeImageDll )
   {
      FreeLibrary( hFreeImageDll );
      hFreeImageDll = NULL;
      pLoad = NULL;
      pUnload = NULL;
      pAllocate = NULL;
      pSave = NULL;
      pGetfiffromfile = NULL;
      pGetwidth = NULL;
      pGetheight = NULL;
      pGetbits = NULL;
      pGetinfo = NULL;
      pGetinfoHead = NULL;
   }
}

HB_FUNC( FI_VERSION )
{
   FREEIMAGE_GETVERSION pFunc =
     (FREEIMAGE_GETVERSION) GetFunction( NULL,"_FreeImage_GetVersion@0" );

   hb_retc( (pFunc)? pFunc() : "" );
}

HB_FUNC( FI_UNLOAD )
{
   pUnload = (FREEIMAGE_UNLOAD) GetFunction( (FARPROC)pUnload,"_FreeImage_Unload@4" );

   if( pUnload )
      pUnload( (FIBITMAP*)hb_parnl(1) );
}

HB_FUNC( FI_LOAD )
{
   pLoad = (FREEIMAGE_LOAD) GetFunction( (FARPROC)pLoad,"_FreeImage_Load@12" );
   pGetfiffromfile = (FREEIMAGE_GETFIFFROMFILENAME) GetFunction( (FARPROC)pGetfiffromfile,"_FreeImage_GetFIFFromFilename@4" );

   if( pGetfiffromfile && pLoad )
   {
      char *name = hb_parc( 1 );
      hb_retnl( (LONG) pLoad( pGetfiffromfile(name), name, (hb_pcount()>1)? hb_parni(2) : 0 ) );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( FI_SAVE )
{
   pSave = (FREEIMAGE_SAVE) GetFunction( (FARPROC)pSave,"_FreeImage_Save@16" );
   pGetfiffromfile = (FREEIMAGE_GETFIFFROMFILENAME) GetFunction( (FARPROC)pGetfiffromfile,"_FreeImage_GetFIFFromFilename@4" );

   if( pGetfiffromfile && pSave )
   {
      char *name = hb_parc( 2 );
      hb_retl( (BOOL) pSave( pGetfiffromfile(name), (FIBITMAP*)hb_parnl(1), name, (hb_pcount()>2)? hb_parni(3) : 0 ) );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( FI_GETWIDTH )
{
   pGetwidth = (FREEIMAGE_GETWIDTH) GetFunction( (FARPROC)pGetwidth,"_FreeImage_GetWidth@4" );

   hb_retnl( ( pGetwidth )? pGetwidth( (FIBITMAP*)hb_parnl(1) ) : 0 );
}

HB_FUNC( FI_GETHEIGHT )
{
   pGetheight = (FREEIMAGE_GETHEIGHT) GetFunction( (FARPROC)pGetheight,"_FreeImage_GetHeight@4" );

   hb_retnl( ( pGetheight )? pGetheight( (FIBITMAP*)hb_parnl(1) ) : 0 );
}

HB_FUNC( FI_2BITMAP )
{
   FIBITMAP* dib = (FIBITMAP*) hb_parnl( 1 );
   HDC hDC = GetDC( 0 );

   pGetbits = (FREEIMAGE_GETBITS) GetFunction( (FARPROC)pGetbits,"_FreeImage_GetBits@4" );
   pGetinfo = (FREEIMAGE_GETINFO) GetFunction( (FARPROC)pGetinfo,"_FreeImage_GetInfo@4" );
   pGetinfoHead = (FREEIMAGE_GETINFOHEADER) GetFunction( (FARPROC)pGetinfoHead,"_FreeImage_GetInfoHeader@4" );

   hb_retnl( (LONG) CreateDIBitmap( hDC, pGetinfoHead( dib ),
      CBM_INIT, pGetbits( dib ), pGetinfo( dib ), DIB_RGB_COLORS) );

   ReleaseDC( 0,hDC );
}

HB_FUNC( FI_DRAW )
{
   FIBITMAP* dib = (FIBITMAP*) hb_parnl( 1 );
   HDC hDC = (HDC) hb_parnl( 2 );
   int nWidth = (int) hb_parnl( 3 ), nHeight = (int) hb_parnl( 4 );
   int nDestWidth, nDestHeight;
   POINT pp[2];
   // char cres[40];
   // BOOL l;

   if( hb_pcount() > 6 && !ISNIL( 7 ) )
   {
      nDestWidth  = hb_parni( 7 );
      nDestHeight = hb_parni( 8 );
   }
   else
   {
      nDestWidth  = nWidth;
      nDestHeight = nHeight;
   }

   pp[0].x = hb_parni( 5 );
   pp[0].y = hb_parni( 6 );
   pp[1].x = pp[0].x + nDestWidth;
   pp[1].y = pp[0].y + nDestHeight;
   // sprintf( cres,"\n %d %d %d %d",pp[0].x,pp[0].y,pp[1].x,pp[1].y );
   // writelog(cres);
   // l = DPtoLP( hDC, pp, 2 );
   // sprintf( cres,"\n %d %d %d %d %d",pp[0].x,pp[0].y,pp[1].x,pp[1].y,l );
   // writelog(cres);

   pGetbits = (FREEIMAGE_GETBITS) GetFunction( (FARPROC)pGetbits,"_FreeImage_GetBits@4" );
   pGetinfo = (FREEIMAGE_GETINFO) GetFunction( (FARPROC)pGetinfo,"_FreeImage_GetInfo@4" );

   if( pGetbits && pGetinfo )
   {
      SetStretchBltMode( hDC, COLORONCOLOR );
      StretchDIBits( hDC, pp[0].x, pp[0].y, pp[1].x-pp[0].x, pp[1].y-pp[0].y,
         0, 0, nWidth, nHeight,
         pGetbits( dib ), pGetinfo( dib ), DIB_RGB_COLORS, SRCCOPY );
   }
}

HB_FUNC( FI_BMP2FI )
{
   HBITMAP hbmp = (HBITMAP) hb_parnl(1);

   if( hbmp )
   { 
      FIBITMAP *dib;
      BITMAP bm;

      pAllocate = (FREEIMAGE_ALLOCATE) GetFunction( (FARPROC)pAllocate,"_FreeImage_Allocate@24" );
      pGetbits = (FREEIMAGE_GETBITS) GetFunction( (FARPROC)pGetbits,"_FreeImage_GetBits@4" );
      pGetinfo = (FREEIMAGE_GETINFO) GetFunction( (FARPROC)pGetinfo,"_FreeImage_GetInfo@4" );
      pGetheight = (FREEIMAGE_GETHEIGHT) GetFunction( (FARPROC)pGetheight,"_FreeImage_GetHeight@4" );

      if( pAllocate && pGetbits && pGetinfo && pGetheight )
      {
         HDC hDC = GetDC( NULL );

         GetObject( hbmp, sizeof(BITMAP), (LPSTR) &bm );
         dib = pAllocate( bm.bmWidth, bm.bmHeight, bm.bmBitsPixel );
         GetDIBits( hDC, hbmp, 0, pGetheight( dib ), 
            pGetbits( dib ), pGetinfo( dib ), DIB_RGB_COLORS );
         ReleaseDC( NULL, hDC );
         hb_retnl( (LONG) dib );
         return;
      }
   }
   hb_retnl( 0 );
}
