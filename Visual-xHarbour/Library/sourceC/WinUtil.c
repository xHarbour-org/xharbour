/*
 * $Id$
 */

//#ifndef NONAMELESSUNION
   //#define NONAMELESSUNION
//#endif

#define _WIN32_DCOM

#define SEE_MASK_NOASYNC           0x00000100

#include "item.api"
#include "hbdefs.h"
#include "hbvmpub.h"
#include "hbinit.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbvm.h"
#include "hbapierr.h"

#ifndef _WIN32_DCOM
#define _WIN32_DCOM
#endif

// Don't move, must precede any #include of Windows to fix CINTERFACE support in guiddef.h and propkeydef.h!!!
#include "cinterface.h"
#include <windows.h>
#include <process.h>
#include <objbase.h>
#include <winnt.h>

#include <wbemidl.h>
#include <commdlg.h>
#include <commctrl.h>
#include <shlobj.h>
#include <tchar.h>
#include <comcat.h>
#include <wininet.h>
#include <winsock.h>

#include <stdio.h>
#include <olectl.h>
#include <pshpack8.h>
#include <psapi.h>
#include <tchar.h>

#include <Tlhelp32.h>
#include <winbase.h>
#include <string.h>
//#include <Awesomium\awesomium_capi.h>

#define __strcpy            _tcscpy
#define __strcat            _tcscat
#define __strupr            _tcsupr
#define __strlwr            _strlwr
#define __strlen            _tcslen
#define __strstr            _tcsstr
#define __strchr            _tcschr
#define __strrchr           _tcsrchr
#define __sprintf           _stprintf
#define __strncmp           _tcsncmp
#define __strcmp            _tcscmp

#define stricmp           _stricmp

#define _bset  memset
#define _bcopy memcpy
#define MAX_PATH_LEN    254

#define SCI_SETPROPERTY 4004

typedef struct
{
   BOOL bRoot;
   LPITEMIDLIST lpi;
   LPSHELLFOLDER lpsfParent;
   LPITEMIDLIST lpifq;
}LPTVITEMDATA;

typedef struct tagLVID
{
   BOOL bRoot;
   LPSHELLFOLDER lpsfParent;
   LPITEMIDLIST  lpi;
   LPITEMIDLIST  lpifq;
   ULONG         ulAttribs;
} LVITEMDATA, *LPLVITEMDATA;

BOOL GetName(LPSHELLFOLDER lpsf, LPITEMIDLIST lpi, DWORD dwFlags, LPSTR lpFriendlyName);

#define OLESTR(str)     L##str
#define COLORREF2RGB(Color) (Color & 0xff00) | ((Color >> 16) & 0xff) | ((Color << 16) & 0xff0000)

#define ECM_FIRST  0x1500
//#define EM_SETCUEBANNER    (ECM_FIRST + 1)

HB_EXTERN_BEGIN
extern BSTR hb_oleAnsiToSysString( const char *cString );
extern LPWSTR hb_oleAnsiToWide( const char *cString );
extern char * hb_oleWideToAnsi( LPWSTR wString );
HB_EXTERN_END

static IDispatch *pDispPtr;
static HMODULE hLib = NULL;
static PHB_DYNS pDynSym;
static PHB_DYNS pFontDynSym;
static PHB_DYNS pColorDynSym;

static HHOOK s_hMsgHook = NULL;

typedef BOOL (WINAPI *ATLAXWININIT)(void);
typedef BOOL (WINAPI *ATLAXWINTERM)(void);
typedef HRESULT (WINAPI *ATLAXGETCONTROL)(HWND, IUnknown**);
typedef HRESULT (WINAPI *ATLAXCREATECONTROL)(LPCOLESTR, HWND, IStream*, IUnknown**);
typedef HRESULT (WINAPI *ATLAXCREATECONTROLLIC)(LPCOLESTR, HWND, IStream*, IUnknown**, BSTR);
typedef HRESULT (WINAPI *ATLAXGETHOST)(HWND, IUnknown**);
typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);
typedef BOOL (WINAPI *NAMEDPIPESERVERPROCESSID) (HANDLE, PULONG );


void LoadAtlAx( void )
{
   if( hLib == NULL )
   {
      ATLAXWININIT pAtlAxWinInit;

      hLib = LoadLibrary( "atl.dll" );
      pAtlAxWinInit = (ATLAXWININIT) GetProcAddress( hLib, "AtlAxWinInit" );

      if ( pAtlAxWinInit )
      {
         pAtlAxWinInit();
      }
   }
}

BOOL WINAPI GetNamedPipeServerProcessId( HANDLE Pipe, PULONG ServerProcessId )
{
   NAMEDPIPESERVERPROCESSID fnGetNamedPipeServerProcessId;
   fnGetNamedPipeServerProcessId = (NAMEDPIPESERVERPROCESSID) GetProcAddress( GetModuleHandle( TEXT("kernel32") ), "GetNamedPipeServerProcessId");
   if (NULL != fnGetNamedPipeServerProcessId)
   {
      return fnGetNamedPipeServerProcessId( &Pipe, ServerProcessId );
   }
   return FALSE;
}

HB_FUNC( BITMAPTOREGION )
{
   HBITMAP hBmp = (HBITMAP) hb_parnl(1);
   COLORREF cTransparentColor = (COLORREF) hb_parnl(2);
   COLORREF cTolerance = (COLORREF) hb_parnl(3);
   HRGN hRgn = NULL;
   INT y;
   INT x;
   if (hBmp)
   {
      // Create a memory DC inside which we will scan the bitmap content
      HDC hMemDC = CreateCompatibleDC(NULL);
      if (hMemDC)
      {
         HRGN h;
         RECT *pr;
         RGNDATA *pData;
         HANDLE hData;
         DWORD maxRects;
         HDC hDC;
         HBITMAP holdBmp;
         HBITMAP hbm32;
         VOID * pbits32;
         BITMAPINFOHEADER bmi;

         BYTE *p32;
         BITMAP bm;

         BYTE lr, hr;
         BYTE lg, hg;
         BYTE lb, hb;

         if( cTransparentColor )
         {
            lr = GetBValue(cTransparentColor);
            lg = GetGValue(cTransparentColor);
            lb = GetRValue(cTransparentColor);
         }

         if( cTolerance )
         {
            hr = GetRValue(cTolerance);
            hg = GetGValue(cTolerance);
            hb = GetBValue(cTolerance);
         }

         GetObject(hBmp, sizeof(bm), &bm);

         // Create a 32 bits depth bitmap and select it into the memory DC
         bmi.biSize          = sizeof(BITMAPINFOHEADER);
         bmi.biWidth         = bm.bmWidth;
         bmi.biHeight        = bm.bmHeight;
         bmi.biPlanes        = 1;
         bmi.biBitCount      = 32;
         bmi.biCompression   = BI_RGB;
         bmi.biSizeImage     = 0;
         bmi.biXPelsPerMeter = 0;
         bmi.biYPelsPerMeter = 0;
         bmi.biClrUsed       = 0;
         bmi.biClrImportant  = 0;
         hbm32 = CreateDIBSection(hMemDC, (BITMAPINFO *)&bmi, DIB_RGB_COLORS, &pbits32, NULL, 0);
         if (hbm32)
         {
            holdBmp = (HBITMAP)SelectObject(hMemDC, hbm32);

            // Create a DC just to copy the bitmap into the memory DC
            hDC = CreateCompatibleDC(hMemDC);
            if (hDC)
            {
               // Get how many bytes per row we have for the bitmap bits (rounded up to 32 bits)
               BITMAP bm32;
               GetObject(hbm32, sizeof(bm32), &bm32);
               while (bm32.bmWidthBytes % 4)
                  bm32.bmWidthBytes++;

               // Copy the bitmap into the memory DC
               holdBmp = (HBITMAP)SelectObject(hDC, hBmp);
               BitBlt(hMemDC, 0, 0, bm.bmWidth, bm.bmHeight, hDC, 0, 0, SRCCOPY);

               #define ALLOC_UNIT   100
               maxRects = ALLOC_UNIT;
               hData = GlobalAlloc(GMEM_MOVEABLE, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects));
               pData = (RGNDATA *)GlobalLock(hData);
               pData->rdh.dwSize = sizeof(RGNDATAHEADER);
               pData->rdh.iType = RDH_RECTANGLES;
               pData->rdh.nCount = pData->rdh.nRgnSize = 0;
               SetRect(&pData->rdh.rcBound, MAXLONG, MAXLONG, 0, 0);

               // Scan each bitmap row from bottom to top (the bitmap is inverted vertically)
               p32 = (BYTE *)bm32.bmBits + (bm32.bmHeight - 1) * bm32.bmWidthBytes;
               for ( y = 0; y < bm.bmHeight; y++) {
                  // Scan each bitmap pixel from left to right
                  for ( x = 0; x < bm.bmWidth; x++) {
                     // Search for a continuous range of "non transparent pixels"
                     int x0 = x;
                     if( cTransparentColor )
                     {
                        LONG *p = (LONG *)p32 + x;

                        while (x < bm.bmWidth)
                        {
                           BYTE b = GetRValue(*p);
                           if (b == lr)
                           {
                              b = GetGValue(*p);
                              if (b == lg)
                              {
                                 b = GetBValue(*p);
                                 if (b == lb)
                                 {
                                    // This pixel is "transparent"
                                    break;
                                 }
                              }
                           }
                           p++;
                           x++;
                        }
                     }
                     if( (x > x0) || ( ! cTransparentColor ) )
                     {
                        // Add the pixels (x0, y) to (x, y+1) as a new rectangle in the region
                        if (pData->rdh.nCount >= maxRects)
                        {
                           GlobalUnlock(hData);
                           maxRects += ALLOC_UNIT;
                           hData = GlobalReAlloc(hData, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects), GMEM_MOVEABLE);
                           pData = (RGNDATA *)GlobalLock(hData);
                        }
                        pr = (RECT *)&pData->Buffer;
                        SetRect(&pr[pData->rdh.nCount], x0, y, x, y+1);
                        if (x0 < pData->rdh.rcBound.left)
                           pData->rdh.rcBound.left = x0;
                        if (y < pData->rdh.rcBound.top)
                           pData->rdh.rcBound.top = y;
                        if (x > pData->rdh.rcBound.right)
                           pData->rdh.rcBound.right = x;
                        if (y+1 > pData->rdh.rcBound.bottom)
                           pData->rdh.rcBound.bottom = y+1;
                        pData->rdh.nCount++;
                     }
                  }

                  // Go to next row (remember, the bitmap is inverted vertically)
                  p32 -= bm32.bmWidthBytes;
               }

               // Create or extend the region with the remaining rectangles
               h = ExtCreateRegion(NULL, sizeof(RGNDATAHEADER) + (sizeof(RECT) * maxRects), pData);
               if (hRgn)
               {
                  CombineRgn(hRgn, hRgn, h, RGN_OR);
                  DeleteObject(h);
               }
               else
                  hRgn = h;

               // Clean up
               SelectObject(hDC, holdBmp);
               DeleteDC(hDC);
            }

            DeleteObject(SelectObject(hMemDC, holdBmp));
         }

         DeleteDC(hMemDC);
      }
   }

   hb_retnl((LONG)hRgn);
}

HB_FUNC( GETBMPSIZE )
{
   PHB_ITEM aArray = _itemArrayNew(2) ;
   PHB_ITEM tmp ;
   BITMAP bm;
   HBITMAP hBmp = (HBITMAP) hb_parnl(1);
   GetObject(hBmp, sizeof(bm), &bm);
   tmp = _itemPutNL( NULL, bm.bmWidth );
   hb_arraySet( aArray, 1, tmp );
   _itemRelease( tmp );
   tmp = _itemPutNL( NULL, bm.bmHeight );
   hb_arraySet( aArray, 2, tmp );
   _itemRelease( tmp );
  _itemReturn( aArray );
  _itemRelease( aArray );
}

//----------------------------------------------------------------------------------------
HB_FUNC( MOVETO )
{
   hb_retl( MoveToEx( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), NULL ) );
}

//----------------------------------------------------------------------------------------
HB_FUNC( NOT )
{
   hb_retnl( ~( hb_parnl(1) ) ) ;
}

//----------------------------------------------------------------------------------------
HB_FUNC( WINSETCURSOR )
{
   hb_retnl( (LONG) SetCursor( (HCURSOR) hb_parnl( 1 ) ) ) ;
}



//----------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTBEGINDRAG )
{
   hb_retl( ImageList_BeginDrag( (HIMAGELIST) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4) ) );
}

//----------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTDRAGMOVE )
{
   hb_retl( ImageList_DragMove( hb_parni(1), hb_parni(2) ) );
}

//----------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTENDDRAG )
{
   ImageList_EndDrag();
}

//----------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTDRAGENTER )
{
   hb_retl( ImageList_DragEnter( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3) ) );
}

//----------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTDRAGSHOWNOLOCK )
{
   ImageList_DragShowNolock( hb_parl(1) );
}





//----------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTGETIMAGECOUNT )
{
   hb_retni( ImageList_GetImageCount((HIMAGELIST) hb_parnl(1) ) );
}
/*
HB_FUNC( IMAGELISTSETCOLORTABLE )
{
   RGBQUAD *prgb;
   prgb->rgbRed   = 0;
   prgb->rgbGreen = 0;
   prgb->rgbBlue  = 0;
   prgb->rgbReserved = 0;

   hb_retni( ImageList_SetColorTable((HIMAGELIST) hb_parnl(1), hb_parni(2), hb_parni(3), prgb ) );
}
*/
HB_FUNC( IMAGELISTGETBITMAPS )
{
   IMAGEINFO ImageInfo;
   ImageList_GetImageInfo( (HIMAGELIST) hb_parnl(1), hb_parni(2), &ImageInfo );
   hb_reta( 2 );
   hb_stornl( (long) ImageInfo.hbmImage, -1, 1 );
   hb_stornl( (long) ImageInfo.hbmMask,  -1, 2 );
}

//HB_FUNC( IMAGELISTGETIMAGEINFO )
//{
//   LPIMAGEINFO pImageInfo = (LPIMAGEINFO) hb_param( 3, HB_IT_STRING )->item.asString.value;
//   hb_retl( ImageList_GetImageInfo((HIMAGELIST) hb_parnl(1), hb_parni(2), (struct _IMAGEINFO*) pImageInfo ) );
//}

HB_FUNC( IMAGELISTDESTROY )
{
   hb_retl( ImageList_Destroy( (HIMAGELIST) hb_parnl(1) ) );
}

HB_FUNC( IMAGELISTLOADIMAGE )
{
   hb_retnl( (LONG) ImageList_LoadImage( (HINSTANCE) hb_parnl(1), (LPCTSTR) hb_parc(2), (INT) hb_parni(3), (INT) hb_parni(4), (COLORREF) hb_parnl(5), (UINT) hb_parnl(6), (UINT) hb_parnl(7) ) );
}

HB_FUNC( IMAGELISTREMOVEALL )
{
   hb_retl( ImageList_RemoveAll( (HIMAGELIST) hb_parnl(1) ) );
}

HB_FUNC( IMAGELISTREMOVE )
{
   hb_retl( ImageList_Remove( (HIMAGELIST) hb_parnl(1), hb_parni(2) ) );
}

HB_FUNC( IMAGELISTCOPY )
{
   hb_retnl( (LONG) ImageList_Copy((HIMAGELIST) hb_parnl(1), (int) hb_parnl(2), (HIMAGELIST) hb_parni(3), (INT) hb_parni(4), (UINT) hb_parni(5) ) );
}

HB_FUNC( IMAGELISTGETICON )
{
   hb_retnl( (LONG) ImageList_GetIcon((HIMAGELIST) hb_parnl(1), hb_parni(2), (UINT) hb_parni(3) ) );
}

HB_FUNC ( IMAGELISTCREATE )
{
   hb_retnl( (LONG) ImageList_Create( hb_parni(1), hb_parni(2), hb_parnl(3), hb_parni(4), hb_parni(5) ) );
}

HB_FUNC( IMAGELISTADDMASKED )
{
   hb_retni( ImageList_AddMasked((HIMAGELIST) hb_parnl(1), (HBITMAP) hb_parnl(2), (COLORREF) hb_parnl(3) ) );
}

HB_FUNC( IMAGELISTADD )
{
   hb_retni( ImageList_Add((HIMAGELIST) hb_parnl(1), (HBITMAP) hb_parnl(2), (HBITMAP) hb_parnl(3) ) );
}

HB_FUNC ( IMAGELISTREPLACEICON )
{
   hb_retni( ImageList_ReplaceIcon( (HIMAGELIST) hb_parnl(1), (int) hb_parni(2), (HICON) hb_parnl(3) ) );
}

HB_FUNC ( IMAGELISTADDICON )
{
   hb_retni( ImageList_AddIcon( (HIMAGELIST) hb_parnl(1), (HICON) hb_parnl(2) ) );
}

HB_FUNC ( IMAGELISTDUPLICATE )
{
   hb_retnl( (LONG) ImageList_Duplicate( (HIMAGELIST) hb_parnl(1) ) );
}

HB_FUNC( IMAGELISTGETICONSIZE )
{
   int cx ;
   int cy ;
   if ( ImageList_GetIconSize( (HIMAGELIST) hb_parnl( 1 ), &cx, &cy) )
   {
      hb_storni( cx, 2 );
      hb_storni( cy, 3 );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( IMAGELISTSETICONSIZE )
{
   hb_retl( ImageList_SetIconSize( (HIMAGELIST) hb_parnl(1), hb_parni(2), hb_parni(3) ) );
}

HB_FUNC( IMAGELISTMERGE )
{
   hb_retl( (BOOL) ImageList_Merge((HIMAGELIST) hb_parnl(1), hb_parni(2), (HIMAGELIST) hb_parnl(3), hb_parni(4), hb_parni(5), hb_parni(6) ) );
}

HB_FUNC( IMAGELISTDRAW )
{
   hb_retl( ImageList_Draw((HIMAGELIST) hb_parnl(1), hb_parni(2), (HDC) hb_parnl(3), hb_parni(4), hb_parni(5), (UINT) hb_parni(6) ) );
}

HB_FUNC( IMAGELISTDRAWEX )
{
   hb_retl( ImageList_DrawEx((HIMAGELIST) hb_parnl(1), hb_parni(2), (HDC) hb_parnl(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7),(COLORREF) hb_parnl(8), (COLORREF) hb_parnl(9), (UINT) hb_parni(10) ) );
}

HB_FUNC( HEADERSETIMAGELIST )
{
   hb_retnl( (ULONG) Header_SetImageList((HWND) hb_parnl(1), (LPARAM) hb_parnl(2) ) ) ;
}

HB_FUNC( HEADERINSERTITEM )
{
   HDITEM *hdi = ( HDITEM *) hb_param( 3, HB_IT_STRING )->item.asString.value ;
   hb_retni( Header_InsertItem( (HWND) hb_parnl(1), hb_parni(2), hdi ) ) ;
}

HB_FUNC( PEEKW )
{
   hb_retni( * ( LPWORD ) hb_parnl( 1 ) );
}

HB_FUNC( PEEKL )
{
   hb_retnl( * (LPDWORD) hb_parnl( 1 ) ) ;
}

HB_FUNC( PEEK )
{
 if ( hb_pcount()==2 )
    hb_retclen( (char *) (ULONG) hb_parnl( 1 ), hb_parnl( 2 ) );
 else
    hb_retc( (char *) (ULONG) hb_parnl( 1 ) );
}

HB_FUNC( POKE )
{
   if( hb_pcount() ==3 )
      hb_xmemcpy( (char *) hb_parnl(1), hb_parcx( 2 ), hb_parnl( 3 ) );
   else
      hb_xmemcpy( (char *) hb_parnl(1), hb_parcx( 2 ), hb_parclen( 2 ) );
}
/*
HB_FUNC( MEMFREE )
{
   hb_xfree( ( char * ) hb_parnl( 1 ) ) ;
}

HB_FUNC( MEMALLOC )
{
   hb_retnl( ( LONG) hb_xgrab( hb_parnl( 1 ) ) ) ;
}
*/
HB_FUNC( BIN2F )
{
   hb_retnd( (double) *( (float *) hb_parcx( 1 ) ) );
}

HB_FUNC( BIN2D )
{
  hb_retnd( *( (double *) hb_parcx( 1 ) ) );
}

HB_FUNC ( LISTVIEWSETITEMSTATE )
{
   ListView_SetItemState( (HWND) hb_parnl(1), hb_parni(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4) );
}

HB_FUNC ( LISTVIEWDELETEALLITEMS )
{
   ListView_DeleteAllItems( (HWND) hb_parnl(1) );
}

HB_FUNC ( LISTVIEWDELETECOLUMN )
{
   ListView_DeleteColumn( (HWND) hb_parnl(1), (INT) hb_parni(2) );
}

HB_FUNC ( LISTVIEWSETITEMCOUNT )
{
   ListView_SetItemCount( (HWND) hb_parnl(1), hb_parnl(2) );
}

HB_FUNC ( LISTVIEWGETNEXTITEM )
{
   hb_retnl(ListView_GetNextItem( (HWND) hb_parnl(1), hb_parni(2), hb_parnl(3) ));
}

HB_FUNC ( LISTVIEWINSERTCOLUMN )
{
   LV_COLUMN *lvColumn = ( LV_COLUMN *) hb_param( 3, HB_IT_STRING )->item.asString.value ;
   ListView_InsertColumn( (HWND)hb_parnl(1), hb_parni(2), lvColumn );
}

HB_FUNC( TVGETSELTEXT )
{
   HWND hWnd = ( HWND ) hb_parnl( 1 );
   HTREEITEM hItem = TreeView_GetSelection( hWnd );
   TV_ITEM tvi;
   BYTE buffer[ 100 ];
   if( hItem )
   {
      tvi.mask       = TVIF_TEXT;
      tvi.hItem      = hItem;
      tvi.pszText    = ( char *)buffer;
      tvi.cchTextMax = 100;
      TreeView_GetItem( hWnd, &tvi );
      hb_retc( tvi.pszText );
   }
   else
      hb_retc( "" );
}

HB_FUNC( TVGETSELECTED )
{
   hb_retnl( (LONG) TreeView_GetSelection( (HWND) hb_parnl(1) ) );
}

HB_FUNC( TVSETIMAGELIST )
{
   hb_retnl( (LONG) TreeView_SetImageList( (HWND) hb_parnl(1), (HIMAGELIST) hb_parnl(2), hb_parnl(3) ) );
}

HB_FUNC( TVDELETEITEM )
{
   TreeView_DeleteItem( (HWND) hb_parnl(1), (HTREEITEM) hb_parnl(2) );
}

HB_FUNC( TVITEMSETCHECK )
{
   TreeView_SetCheckState( (HWND) hb_parnl(1), (HTREEITEM) hb_parnl(2), hb_parl(3) );
}

HB_FUNC( TVITEMGETCHECK )
{
   hb_retl( TreeView_GetCheckState( (HWND) hb_parnl(1), (HTREEITEM) hb_parnl(2) ) );
}



#include <sddl.h>

typedef BOOL (WINAPI *xpCSSD2SD)( LPCSTR, DWORD, PSECURITY_DESCRIPTOR *, PULONG );

HB_FUNC( CREATEDIRECTORY )
{
    SECURITY_ATTRIBUTES sa;
    char *szSD = "D:P"                      // DACL
                 "(A;OICI;GA;;;BG)"         // Deny Guests
                 "(A;OICI;GA;;;SY)"         // Allow SYSTEM Full Control
                 "(A;OICI;GA;;;BA)"         // Allow Admins Full Control
                 "(A;OICI;GRGWGX;;;IU)";    // Allow Interactive Users RWX


    HINSTANCE h = (HINSTANCE) LoadLibraryEx( "ADVAPI32.DLL", NULL, 0 );

    if( h ){

       xpCSSD2SD pfnCSSD2SD = (xpCSSD2SD) GetProcAddress( h, "ConvertStringSecurityDescriptorToSecurityDescriptorA" ) ;

       if ( pfnCSSD2SD ){
          sa.nLength = sizeof( SECURITY_ATTRIBUTES );
          sa.bInheritHandle = FALSE ;
          if ( pfnCSSD2SD(  szSD,  SDDL_REVISION_1,  &(sa.lpSecurityDescriptor),  NULL ) ) {
             hb_retl( CreateDirectory( (LPCSTR) hb_parc( 1 ), &sa ) ) ;
             LocalFree(sa.lpSecurityDescriptor);
          }
          else
            hb_retl(FALSE);
       }
       else
          hb_retl( CreateDirectory( (LPCSTR) hb_parc( 1 ), &sa ) ) ;
    }
    else
       hb_retl( CreateDirectory( (LPCSTR) hb_parc( 1 ), &sa ) ) ;
}


//-----------------------------------------------------------------------------------------------------------------------------------------

HB_FUNC (TABCTRL_INSERTITEM)
{
   TC_ITEM item;
   item.mask = TCIF_TEXT | TCIF_IMAGE;
   item.iImage = ISNIL(4) ? -1 : (LONG) hb_parnl(4);
   item.pszText = (LPSTR) hb_parcx(2);
   hb_retni( TabCtrl_InsertItem( (HWND) hb_parnl(1), (INT) hb_parni(3), &item) );
}

HB_FUNC (TABCTRL_SETCURSEL)
{
   hb_retni( TabCtrl_SetCurSel( (HWND) hb_parnl(1) , hb_parni (2) ) );
}

HB_FUNC (TABCTRL_GETCURSEL)
{
   hb_retni ( TabCtrl_GetCurSel( (HWND) hb_parnl (1) ) ) ;
}

HB_FUNC (TABCTRL_GETITEM)
{
   TC_ITEM item;
   hb_retl(TabCtrl_GetItem( (HWND) hb_parnl (1), (int) hb_parni(2) , &item ) );
}

HB_FUNC (TABCTRL_GETITEMCOUNT)
{
   hb_retni( TabCtrl_GetItemCount( (HWND) hb_parnl(1) ) ) ;
}

HB_FUNC (TABCTRL_GETITEMRECT)
{
   RECT rc;
   PHB_ITEM aRect = _itemArrayNew( 4 );
   PHB_ITEM temp;

   TabCtrl_GetItemRect((HWND) hb_parnl (1), hb_parni(2), &rc);

   temp = _itemPutNL( NULL, rc.left );
   hb_arraySet( aRect, 1, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, rc.top );
   hb_arraySet( aRect, 2, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, rc.right );
   hb_arraySet( aRect, 3, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, rc.bottom );
   hb_arraySet( aRect, 4, temp );
   _itemRelease( temp );

   _itemReturn( aRect );
   _itemRelease( aRect );
}

HB_FUNC (TABCTRL_GETROWCOUNT)
{
   hb_retni( TabCtrl_GetRowCount( (HWND) hb_parnl(1) ) ) ;
}

HB_FUNC(TABCTRL_GETIMAGELIST)
{
   hb_retnl( (LONG) TabCtrl_GetImageList( (HWND) hb_parnl(1) ) ) ;
}

HB_FUNC(TABCTRL_SETIMAGELIST)
{
   hb_retnl( (LONG) TabCtrl_SetImageList( (HWND) hb_parnl( 1 ),
                    (LPARAM)(HIMAGELIST) hb_parnl( 2 ) ) ) ;
}

HB_FUNC( TABCTRL_SETITEM )
{
   TC_ITEM item;
   item.mask = TCIF_TEXT | TCIF_IMAGE;
   item.iImage = -1;
   item.pszText = (LPSTR) hb_parcx( 3 );
   hb_retl( TabCtrl_SetItem( (HWND) hb_parnl( 1 ), hb_parni( 2 ), &item) ) ;
}

HB_FUNC(TABCTRL_DELETEALLITEMS)
{
   hb_retl(TabCtrl_DeleteAllItems((HWND) hb_parnl(1)));
}

HB_FUNC(TABCTRL_DELETEITEM)
{
   hb_retl(TabCtrl_DeleteItem((HWND) hb_parnl(1), (WPARAM) hb_parni(2)));
}

HB_FUNC(TABCTRL_HITTEST)
{
   TCHITTESTINFO tcht ;
   hb_parni( TabCtrl_HitTest( (HWND) hb_parnl(1), &tcht ) ) ;
}

HB_FUNC(TABCTRL_SETITEMEXTRA)
{
   hb_retl( TabCtrl_SetItemExtra( (HWND) hb_parnl(1), (int) hb_parni(2) ) ) ;
}

HB_FUNC(TABCTRL_ADJUSTRECT)
{
  RECT rc;

  if ( ISARRAY(3) )
  {
     rc.left   = hb_parnl(3,1);
     rc.top    = hb_parnl(3,2);
     rc.right  = hb_parnl(3,3);
     rc.bottom = hb_parnl(3,4);

     TabCtrl_AdjustRect( (HWND) hb_parnl(1), (BOOL) hb_parl(2), &rc );

     hb_stornl( rc.left  , 3 ,1 ) ;
     hb_stornl( rc.top   , 3, 2 ) ;
     hb_stornl( rc.right , 3 ,3 ) ;
     hb_stornl( rc.bottom, 3, 4 ) ;
  }
}

HB_FUNC(TABCTRL_SETITEMSIZE)
{
   hb_retnl( TabCtrl_SetItemSize( (HWND) hb_parnl(1), (int) hb_parni(2), (int) hb_parni(3) ) );
}

HB_FUNC(TABCTRL_REMOVEIMAGE)
{
  TabCtrl_RemoveImage( (HWND) hb_parnl(1), (int) hb_parni(2) ) ;
}

HB_FUNC(TABCTRL_SETPADDING)
{
   TabCtrl_SetPadding( (HWND) hb_parnl(1), (int) hb_parni(2), (int) hb_parni(3) ) ;
}

HB_FUNC(TABCTRL_GETTOOLTIPS)
{
   hb_retnl( (LONG) TabCtrl_GetToolTips( (HWND) hb_parnl( 1 ) ) );
}

HB_FUNC(TABCTRL_SETTOOLTIPS)
{
   TabCtrl_SetToolTips( (HWND) hb_parnl(1), (HWND) hb_parnl(2) ) ;
}

HB_FUNC(TABCTRL_GETCURFOCUS)
{
   hb_retni( TabCtrl_GetCurFocus( (HWND) hb_parnl(1) ) );
}

HB_FUNC(TABCTRL_SETCURFOCUS)
{
  hb_retni( TabCtrl_SetCurFocus( (HWND) hb_parnl(1), (int) hb_parni(2) ) );
}

HB_FUNC(TABCTRL_SETMINTABWIDTH)
{
   hb_retni( TabCtrl_SetMinTabWidth( (HWND) hb_parnl(1), (int) hb_parni(2) ) );
}

HB_FUNC(TABCTRL_DESELECTALL)
{
   TabCtrl_DeselectAll( (HWND) hb_parnl(1), (UINT) hb_parni( 2 ) ) ;
}

HB_FUNC(TABCTRL_HIGHLIGHTITEM)
{
   hb_retl( TabCtrl_HighlightItem( (HWND) hb_parnl(1), (int) hb_parni(2), (WORD) hb_parni(3) ) );
}

HB_FUNC(TABCTRL_SETEXTENDEDSTYLE)
{
   hb_retnl( TabCtrl_SetExtendedStyle( (HWND) hb_parnl(1), (DWORD) hb_parnl(2) ) ) ;
}

HB_FUNC(TABCTRL_GETEXTENDEDSTYLE)
{
   hb_retnl( TabCtrl_GetExtendedStyle( (HWND) hb_parnl(1) ) ) ;
}

HB_FUNC(TABCTRL_SETUNICODEFORMAT)
{
   hb_retl( TabCtrl_SetUnicodeFormat( (HWND) hb_parnl(1), hb_parl(2) ) );
}

HB_FUNC(TABCTRL_GETUNICODEFORMAT)
{
   hb_retl( TabCtrl_GetUnicodeFormat( (HWND) hb_parnl(1) ) ) ;
}

HB_FUNC( OR )
{
  hb_retnl( hb_parnl(1) | hb_parnl(2) ) ;
}


HB_FUNC( SHGETSPECIALFOLDERLOCATION )
{
   LPITEMIDLIST pidlBrowse;
   SHGetSpecialFolderLocation(GetActiveWindow(), ISNIL(1) ? CSIDL_DRIVES : hb_parni(1), &pidlBrowse) ;
   hb_retnl( (long) pidlBrowse );
}

LPITEMIDLIST ConvertPathToLpItemIdList(const char *pszPath)
{
    LPITEMIDLIST  pidl = NULL;
    LPSHELLFOLDER pDesktopFolder = NULL;
    OLECHAR       olePath[MAX_PATH];
    ULONG         chEaten;
    ULONG         dwAttributes;

    if( SUCCEEDED( SHGetDesktopFolder( &pDesktopFolder ) ) )
    {
        MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, pszPath, -1, olePath, MAX_PATH );
        pDesktopFolder->lpVtbl->ParseDisplayName(pDesktopFolder,NULL,NULL,olePath,&chEaten,&pidl,&dwAttributes);
        pDesktopFolder->lpVtbl->Release( pDesktopFolder );
    }

    return pidl;
}

HB_FUNC( CONVERTPATHTOLPITEMIDLIST )
{
   hb_retnl( (LONG) ConvertPathToLpItemIdList( hb_parc(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( _SHGETFILEINFO )
{
   SHFILEINFO psfi;
   LPCTSTR cPath = hb_parcx(1);
   DWORD  dAtt   = hb_parnl(2);
   UINT   uFlags = hb_parnl(4);
   hb_retnl( SHGetFileInfo( cPath, dAtt, &psfi, (UINT) sizeof(SHFILEINFO), uFlags ) );
   hb_storclen( (char*) &psfi, sizeof( SHFILEINFO), 3 ) ;
}

HB_FUNC( WAITEXECUTE )
{
   SHELLEXECUTEINFO sei = {0};
   sei.cbSize       = sizeof( SHELLEXECUTEINFO );
   sei.fMask        = SEE_MASK_NOASYNC|SEE_MASK_NOCLOSEPROCESS;
   sei.hwnd         = GetActiveWindow();
   sei.lpVerb       = NULL;
   sei.lpFile       = (LPCTSTR) hb_parc(1);
   sei.lpParameters = (LPCTSTR) hb_parc(2);
   sei.nShow        = hb_parni(3);
   ShellExecuteEx( &sei );
   WaitForSingleObject( sei.hProcess, INFINITE );
   hb_retnl( (LONG) sei.hProcess );
}

HB_FUNC( SHGETDESKTOPFOLDER )
{
   char *lpBuffer = (char*) hb_xgrab( MAX_PATH + 1 );
   LPITEMIDLIST  pidl;
   LPSHELLFOLDER pDesktopFolder;
   OLECHAR       olePath[MAX_PATH];
   ULONG         chEaten;
   ULONG         dwAttributes;
   SHGetDesktopFolder( &pDesktopFolder );
   pDesktopFolder->lpVtbl->ParseDisplayName( pDesktopFolder, NULL, NULL, olePath, &chEaten, &pidl, &dwAttributes );
   pDesktopFolder->lpVtbl->Release( pDesktopFolder );

   SHGetPathFromIDList(pidl,lpBuffer);
   hb_retc( lpBuffer );
   hb_xfree( lpBuffer);
}

HB_FUNC( SHBROWSEFORFOLDER )
{
   HWND hwnd = ISNIL(1) ? GetActiveWindow() : (HWND) hb_parnl(1);
   BROWSEINFO BrowseInfo;
   char *lpBuffer = (char*) hb_xgrab( MAX_PATH + 1 );
   LPITEMIDLIST pidlBrowse;

   SHGetSpecialFolderLocation(hwnd, ISNIL(4) ? CSIDL_DRIVES : hb_parni(4), &pidlBrowse) ;
   BrowseInfo.hwndOwner = hwnd;

   if( ISCHAR(4) )
   {
      pidlBrowse = ConvertPathToLpItemIdList( hb_parc(4) );
   }

   BrowseInfo.pidlRoot = pidlBrowse;
   BrowseInfo.pszDisplayName = lpBuffer;
   BrowseInfo.lpszTitle = ISNIL (2) ?  "Select a Folder" : hb_parcx(2);
   BrowseInfo.ulFlags = hb_parni(3);
   BrowseInfo.lpfn = (BFFCALLBACK) hb_parnl(5);
   BrowseInfo.lParam = 1;
   BrowseInfo.iImage = 0;
   pidlBrowse = SHBrowseForFolder(&BrowseInfo);

   if ( pidlBrowse )
   {
     SHGetPathFromIDList(pidlBrowse,lpBuffer);
     hb_retc( lpBuffer );
   }
   else
   {
     hb_retc( "" );
   }

   hb_xfree( lpBuffer);
}

HB_FUNC( SHGETPATHFROMIDLIST )
{
   LPITEMIDLIST pidlBrowse = (LPITEMIDLIST) hb_parnl(1);
   char *lpBuffer = (char*) hb_xgrab( MAX_PATH + 1 );
   SHGetPathFromIDList( pidlBrowse, lpBuffer );
   hb_retc( lpBuffer );
   hb_xfree( lpBuffer);
}

#define SM_CXSHADOW 4
BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
/*
BOOL Array2Rect(PHB_ITEM aRect, RECT *rc )
{
   if (HB_IS_ARRAY(aRect) && hb_arrayLen(aRect) == 4) {
      rc->left   = hb_arrayGetNL(aRect,1);
      rc->top    = hb_arrayGetNL(aRect,2);
      rc->right  = hb_arrayGetNL(aRect,3);
      rc->bottom = hb_arrayGetNL(aRect,4);
      return TRUE ;
   }
   return FALSE;
}
*/
void DrawShadow (HDC hDCIn, HDC hDCOut, RECT rc)
{
   int x, y, nEnd, nMakeSpec;
   COLORREF cr, cr2;
    for ( x = rc.left; x < rc.right-1; x++ )
    {
        nEnd = ( x > rc.right-SM_CXSHADOW*2 ) ? rc.right-SM_CXSHADOW-x : SM_CXSHADOW;

        for ( y = ( x < 2 ) ? 2-x : x > rc.right-SM_CXSHADOW-3 ? x-rc.right+SM_CXSHADOW+3 : 0; y < nEnd; y++ )
        {
            int nMakeSpec = 78+(3-(x==0?0:(x==1?(y<2?0:1):(x==2?(y<2?y:2):y))))*5;
            cr = GetPixel (hDCIn, x+SM_CXSHADOW, rc.bottom-y-1);
            cr2 = RGB(((nMakeSpec * (INT) GetRValue(cr)) / 100),
                            ((nMakeSpec * (INT) GetGValue(cr)) / 100),
                            ((nMakeSpec * (INT) GetBValue(cr)) / 100));
         SetPixel (hDCOut, x+SM_CXSHADOW, rc.bottom-y-1, cr2);
        }
    }
    for ( x = 0; x < SM_CXSHADOW; x++ )
    {
        for ( y = ( x < rc.top ) ? rc.top-x : rc.top; y < rc.bottom-x-SM_CXSHADOW-((x>0)?1:2); y++ )
        {
            nMakeSpec = 78+(3-(y==0?0:(y==1?(x<2?0:1):(y==2?(x<2?x:2):x))))*5;
            cr = GetPixel (hDCIn, rc.right-x-1, y+SM_CXSHADOW);
            cr2 = RGB(((nMakeSpec * (INT) GetRValue(cr)) / 100),
                            ((nMakeSpec * (INT) GetGValue(cr)) / 100),
                            ((nMakeSpec * (INT) GetBValue(cr)) / 100));
         SetPixel (hDCOut, rc.right-x-1, y+SM_CXSHADOW, cr2);
        }
    }
}

HB_FUNC( _DRAWSHADOW )
{
   RECT rc;
   Array2Rect( hb_param( 3, HB_IT_ARRAY ), &rc );
   DrawShadow( (HDC) hb_parnl(1), (HDC) hb_parnl(2), rc );
}

HB_FUNC( GETWHEELDELTA )
{
  hb_retni( (short int) HIWORD(hb_parnl(1)) / 120 );
}

HB_FUNC( GETSCREENBITMAP )
{
    RECT pRect;
    HDC     hDC;
    HDC     hMemDC;
    HBITMAP hOldBitmap;
    HBITMAP hNewBitmap = NULL;
    Array2Rect( hb_param(1,HB_IT_ARRAY), &pRect );

    if ( (hDC = GetDC( ISNIL(2) ? NULL : (HWND) hb_parnl(2) )) != NULL )
    {
        if ( (hMemDC = CreateCompatibleDC(hDC)) != NULL )
        {
            if ( (hNewBitmap = CreateCompatibleBitmap(hDC, pRect.right - pRect.left, pRect.bottom - pRect.top)) != NULL )
            {
                hOldBitmap = (HBITMAP) SelectObject(hMemDC, hNewBitmap);
                BitBlt(hMemDC, 0, 0, pRect.right - pRect.left, pRect.bottom - pRect.top,
                          hDC, pRect.left, pRect.top, SRCCOPY);
                SelectObject(hMemDC, (HGDIOBJ) hOldBitmap);
            }
            DeleteDC(hMemDC);
        }
        ReleaseDC(ISNIL(2) ? NULL : (HWND) hb_parnl(2), hDC);
    }
    hb_retnl( (LONG) hNewBitmap );
}

HB_FUNC ( DRAWBITMAP )
{
   HDC hDC = (HDC) hb_parnl( 1 );
   HDC hDCmem = CreateCompatibleDC( hDC );
   DWORD dwraster = (ISNIL(3))? SRCCOPY : hb_parnl(3);
   HBITMAP hBitmap = (HBITMAP) hb_parnl( 2 );
   BITMAP  bitmap;
   BOOL lStretch = (ISLOG( 8 ))?  hb_parl( 8 ) : FALSE  ;
   int nWidthDest = ( hb_pcount() >=5 && !ISNIL(6) )? hb_parni(6):0;
   int nHeightDest = ( hb_pcount()>=6 && !ISNIL(7) )? hb_parni(7):0;

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   if( lStretch && nWidthDest && ( nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight ))
   {
      StretchBlt( hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem,
                  0, 0, bitmap.bmWidth, bitmap.bmHeight, dwraster );
   }
   else
   {
      if( nWidthDest )
      {
         bitmap.bmWidth = nWidthDest;
      }
      if( nHeightDest )
      {
         bitmap.bmHeight = nHeightDest;
      }
      BitBlt( hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );
   }

   DeleteDC( hDCmem );
}

HB_FUNC ( XDRAWBITMAP )
{
   HDC hDC          = (HDC) hb_parnl(1);
   HDC hDCmem       = CreateCompatibleDC( hDC );
   DWORD dwraster   = hb_parnl(3);
   HBITMAP hBitmap  = (HBITMAP) hb_parnl(2);
   int nWidthDest   = hb_parni(6);
   int nHeightDest  = hb_parni(7);
   int xGrid        = hb_parni(8);
   int yGrid        = hb_parni(9);
   HBITMAP hBmpGrid = (HBITMAP) hb_parnl(10);
   int j;
   int i;
   BITMAP  bitmap;
   HDC hDCGrid;

   if( hBmpGrid )
   {
      hDCGrid = CreateCompatibleDC( hDCmem );
   }

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );

   if( nWidthDest )
   {
      bitmap.bmWidth = nWidthDest;
   }
   if( nHeightDest )
   {
      bitmap.bmHeight = nHeightDest;
   }

   if( hBmpGrid )
   {
      SelectObject( hDCGrid, hBmpGrid );
      j=0;
      while (j < nHeightDest)
      {
         i=0;
         while (i < nWidthDest)
         {
            BitBlt( hDCmem, i, j, xGrid, yGrid, hDCGrid, 0, 0, SRCAND );
            i += xGrid;
         }
         j += yGrid;
      }
   }
   BitBlt( hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );

   DeleteDC( hDCmem );
   if( hDCGrid )
   {
      DeleteDC( hDCGrid );
   }
}

HB_FUNC( _GETCLASSNAME )
{
   char *cText= (char*) hb_xgrab( MAX_PATH+1 );

   GetClassName( (HWND) hb_parnl( 1 ),
                 (LPSTR) cText ,
                 MAX_PATH
                ) ;

   hb_retc( cText);
   hb_xfree( cText ) ;
}


HB_FUNC ( DRAWGRID )
{
   HDC hDC          = (HDC) hb_parnl(1);
   int xGrid        = hb_parni(2);
   int yGrid        = hb_parni(3);
   int nWidth       = hb_parni(4);
   int nHeight      = hb_parni(5);
   COLORREF cColor  = (COLORREF) hb_parnl(6);
   int j=0;
   while (j < nHeight)
   {
      int i=0;
      while (i < nWidth)
      {
         SetPixel( hDC, i, j, cColor );
         i += xGrid;
      }
      j += yGrid;
   }
}

HB_FUNC ( DRAWGRIDOLD )
{
   HDC hDC          = (HDC) hb_parnl(1);
   HDC hMemDC       = CreateCompatibleDC( hDC );
   HBITMAP hBitmap  = (HBITMAP) hb_parnl(2);
   int xGrid        = hb_parni(3);
   int yGrid        = hb_parni(4);
   int nWidth       = hb_parni(5);
   int nHeight      = hb_parni(6);
   int j;
   DWORD dwraster     = hb_parnl(7);
   HBITMAP hOldBmp;

   hOldBmp = (HBITMAP) SelectObject( hMemDC, hBitmap );

   j=0;
   while (j < nHeight)
   {
      int i=0;
      while (i < nWidth)
      {
         BitBlt( hDC, i, j, xGrid, yGrid, hMemDC, 0, 0, dwraster );
         i += xGrid;
      }
      j += yGrid;
   }
   SelectObject( hMemDC, hOldBmp );
   DeleteDC( hMemDC );
}


HB_FUNC ( GETXPARAM )
{
   short x = (short) LOWORD( hb_parnl(1) );
   hb_retni(x);
}

HB_FUNC ( GETYPARAM )
{
   short y = (short) HIWORD( hb_parnl(1) );
   hb_retni(y);
}

HB_FUNC( _REGISTERCLASSEX )
{
   hb_retnl( (LONG) RegisterClassEx( (CONST WNDCLASSEX*) hb_parc(1) ) );
}

HB_FUNC( _GETCLASSINFO )
{
   WNDCLASS WndClass  ;
   if ( GetClassInfo( ISNIL(1) ? NULL : (HINSTANCE) hb_parnl( 1 ), (LPCSTR) hb_parc( 2 ), &WndClass ) )
   {
      hb_retclen( (char*) &WndClass, sizeof(WNDCLASS) ) ;
   }
}

HB_FUNC( ISREGISTERED )
{
   WNDCLASS WndClass  ;
   hb_retl( (BOOL) GetClassInfo( ISNIL(1) ? NULL : (HINSTANCE) hb_parnl( 1 ), (LPCSTR) hb_parc( 2 ), &WndClass ) );
}

HB_FUNC( ISBADCODEPTR )
{
   hb_retl( IsBadCodePtr( (FARPROC) hb_parnl(1) ) );
}

/*
HB_FUNC( TOHSL )
{
   int r = hb_parni(1);
   int g = hb_parni(2);
   int b = hb_parni(3);
   unsigned char minval = min(r, min(g, b));
   unsigned char maxval = max(r, max(g, b));
   long mdiff  = (long) maxval - (long) minval;
   long msum   = (long) maxval + (long) minval;
   long m_hue;
   long m_luminance = msum / 510.0f;
   long m_saturation;
   if (maxval == minval)
   {
      m_saturation = 0.0f;
      m_hue = 0.0f;
   }
   else
   {
     long rnorm = (maxval - r  ) / mdiff;
     long gnorm = (maxval - g) / mdiff;
     long bnorm = (maxval - b ) / mdiff;

     m_saturation = (m_luminance <= 0.5f) ? (mdiff / msum) : (mdiff / (510.0f - msum));

     if (r == maxval) m_hue = 60.0f * (6.0f + bnorm - gnorm);
     if (g == maxval) m_hue = 60.0f * (2.0f + rnorm - bnorm);
     if (b == maxval) m_hue = 60.0f * (4.0f + gnorm - rnorm);
     if (m_hue > 360.0f) m_hue = m_hue - 360.0f;
   }

   hb_stornl( m_hue, 4 );
   hb_stornl( m_saturation, 5 );
   hb_stornl( m_luminance, 6 );
}
*/
//---------------------------------------
COLORREF __DarkenColorXP( long lScale, COLORREF lColor)
{
  long red   = MulDiv(GetRValue(lColor),(255-lScale),255);
  long green = MulDiv(GetGValue(lColor),(255-lScale),255);
  long blue  = MulDiv(GetBValue(lColor),(255-lScale),255);

  return RGB(red, green, blue);
}

HB_FUNC( DRAWSHADOWXP )
{
   HDC hdc = (HDC) hb_parnl(1);
   int ix = hb_parni(2);
   int iy = hb_parni(3);
   int iHeight = hb_parni(4);
   int x, y;
   COLORREF pixel;

   for (x=1; x<=4 ;x++)
   {
      for (y=4; y<8 ;y++)
      {
         pixel = GetPixel(hdc, ix-x,y+iy);
         SetPixel(hdc, ix-x,y+iy, __DarkenColorXP(2* 3 * x * (y - 3), pixel)) ;
      }
      for (y=8; y<=(iHeight-1) ;y++)
      {
         pixel = GetPixel(hdc, ix-x,y+iy);
         SetPixel(hdc, ix-x, y+iy, __DarkenColorXP( 2*15 * x, pixel) );
      }
   }
}

HB_FUNC( DARKENCOLORXP )
{
   hb_retnl( (long) __DarkenColorXP( hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

COLORREF _MixedColor(COLORREF colorA,COLORREF colorB)
{
  int red   = MulDiv(86,GetRValue(colorA),100) + MulDiv(14,GetRValue(colorB),100);
  int green = MulDiv(86,GetGValue(colorA),100) + MulDiv(14,GetGValue(colorB),100);
  int blue  = MulDiv(86,GetBValue(colorA),100) + MulDiv(14,GetBValue(colorB),100);
  return RGB( min(red,0xff),min(green,0xff), min(blue,0xff));
}

COLORREF _MidColor(COLORREF colorA,COLORREF colorB)
{
  int red   = MulDiv(7,GetRValue(colorA),10) + MulDiv(3,GetRValue(colorB),10);
  int green = MulDiv(7,GetGValue(colorA),10) + MulDiv(3,GetGValue(colorB),10);
  int blue  = MulDiv(7,GetBValue(colorA),10) + MulDiv(3,GetBValue(colorB),10);
  return RGB( min(red,0xff),min(green,0xff), min(blue,0xff));
}

COLORREF _LightenColor( COLORREF lColor, int iScale )
{
  long R = MulDiv(255-GetRValue(lColor),iScale,255)+GetRValue(lColor);
  long G = MulDiv(255-GetGValue(lColor),iScale,255)+GetGValue(lColor);
  long B = MulDiv(255-GetBValue(lColor),iScale,255)+GetBValue(lColor);
  return RGB(R, G, B);
}

COLORREF _DarkenColor( COLORREF lColor, long lScale )
{
  long red   = MulDiv(GetRValue(lColor),(255-lScale),255);
  long green = MulDiv(GetGValue(lColor),(255-lScale),255);
  long blue  = MulDiv(GetBValue(lColor),(255-lScale),255);
  return RGB(red, green, blue);
}

COLORREF _BleachColor( COLORREF color, int Add)
{
  return RGB( min (GetRValue(color)+(GetRValue(color)%Add), 255),
              min (GetGValue(color)+(GetGValue(color)%Add), 255),
              min (GetBValue(color)+(GetBValue(color)%Add), 255));
}

COLORREF _GetAlphaBlendColor(COLORREF blendColor, COLORREF pixelColor,int weighting)
{
  DWORD refR;
  DWORD refG;
  DWORD refB;

  if( pixelColor == CLR_NONE )
  {
     return CLR_NONE;
  }

  refR = ((weighting * GetRValue(pixelColor)) + ((255-weighting) * GetRValue(blendColor))) / 256;
  refG = ((weighting * GetGValue(pixelColor)) + ((255-weighting) * GetGValue(blendColor))) / 256;
  refB = ((weighting * GetBValue(pixelColor)) + ((255-weighting) * GetBValue(blendColor))) / 256;

  return RGB(refR,refG,refB);
}


HB_FUNC( MIXEDCOLOR )
{
  hb_retnl( (long) _MixedColor( (COLORREF) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

HB_FUNC( MIDCOLOR )
{
  hb_retnl( (long) _MidColor( (COLORREF) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

HB_FUNC( LIGHTENCOLOR )
{
  hb_retnl( (long) _LightenColor( (COLORREF) hb_parnl(1), hb_parni(2) ) );
}

HB_FUNC( __COLORLIGHTEN )
{
  COLORREF cColor = (COLORREF) hb_parnl(1);
  int iPerc  = hb_parni(2);
  int iRed   = GetRValue( cColor );
  int iGreen = GetGValue( cColor );
  int iBlue  = GetBValue( cColor );

  iRed   = ( 255 - iRed   ) * iPerc + iRed;
  iGreen = ( 255 - iGreen ) * iPerc + iGreen;
  iBlue  = ( 255 - iBlue  ) * iPerc + iBlue;

  hb_retnl( (long) RGB( iRed, iGreen, iBlue ) );
}


HB_FUNC( DARKENCOLOR )
{
  hb_retnl( (long) _DarkenColor( (COLORREF) hb_parnl(1), hb_parnl(2) ) );
}

HB_FUNC( BLEACHCOLOR )
{
  hb_retnl( (long) _BleachColor( (COLORREF) hb_parnl(1), hb_parni(2) ) );
}

HB_FUNC( GETALPHABLENDCOLOR )
{
  hb_retnl( (long) _GetAlphaBlendColor( (COLORREF) hb_parnl(1), (COLORREF) hb_parnl(2), hb_parni(3) ) );
}

HB_FUNC( DRAWRIGHTSHADOW )
{
   HDC hDC   = (HDC) hb_parnl(1);
   RECT rect;
   COLORREF colorMenuBar = (COLORREF) hb_parnl(3);
   int winW   = hb_parni(4);
   int X;
   int Y;

   Array2Rect( hb_param( 2, HB_IT_ARRAY ) , &rect );

   for (Y=1; Y<=4 ;Y++)
   {
      for (X=0; X<4 ;X++)
      {
         SetPixel(hDC, rect.left+X,rect.bottom-Y,colorMenuBar);
      }
      for (X=4; X<8 ;X++)
      {
         SetPixel(hDC, rect.left+X,rect.bottom-Y, _DarkenColor(colorMenuBar, 2* 3 * Y * (X - 3) ) );
      }
      for (X=8; X<=(winW-1) ;X++)
      {
         SetPixel(hDC, rect.left+X,rect.bottom-Y, _DarkenColor(colorMenuBar, 2*15 * Y) );
      }
   }
}

/*
HB_FUNC( RIGHTSHADOW )
{
   HDC hDC    = (HDC) hb_parnl(1);
   HDC hDcDsk = (HDC) hb_parnl(2);
   int winW   = hb_parni(3);
   int winH   = hb_parni(4);
   int xOrg   = hb_parni(5);
   int yOrg   = hb_parni(6);
   int X;
   int Y;
   for (X=1; X<=4 ;X++)
   {
      for (Y=0; Y<4 ;Y++)
      {
         SetPixel(hDC,winW-X,Y, GetPixel(hDcDsk,xOrg+winW-X,yOrg+Y) );
      }
      for (Y=4; Y<8 ;Y++)
      {
         COLORREF c = GetPixel(hDcDsk, xOrg + winW - X, yOrg + Y);
         SetPixel(hDC,winW-X,Y,_DarkenColor(c, 2* 3 * X * (Y - 3)));
      }
      for (Y=8; Y<=(winH-5) ;Y++)
      {
         COLORREF c = GetPixel(hDcDsk, xOrg + winW - X, yOrg + Y);
         SetPixel( hDC, winW - X, Y, _DarkenColor(c,2* 15 * X) );
      }
      for (Y=(winH-4); Y<=(winH-1) ;Y++)
      {
         COLORREF c = GetPixel(hDcDsk, xOrg + winW - X, yOrg + Y);
         SetPixel( hDC, winW - X, Y, _DarkenColor(c,2* 3 * X * -(Y - winH)));
      }
   }
}
*/

/*
void DrawXORText(HDC hdc,POINT p,char *text)
    {
        int ltext=strlen(text);


        SIZE s;
        GetTextExtentPoint32(hdc,text,-ltext,&s);


        HDC hdcmem=CreateCompatibleDC(hdc)-;
        SetTextColor(hdcmem,RGB(255,25-5,255));    // white
        SetBkColor(  hdcmem,RGB(0,0,0));    // black


        HBITMAP hbitmap=CreateBitmap(s.cx,s.cy-,1,1,NULL);
        hbitmap=(HBITMAP)SelectObject(-hdcmem,hbitmap);


         ExtTextOut(hdcmem,0,0,0,NULL,t-ext,ltext,NULL);


         BitBlt(hdc,p.x,p.y,s.cx,s.cy,h-dcmem,0,0,SRCINVERT);


         hbitmap=(HBITMAP)SelectObject(-hdcmem,hbitmap);
         DeleteObject(hbitmap);


        DeleteDC(hdcmem);
    }


*/
#pragma comment(lib, "comctl32.lib")

typedef BOOL (WINAPI * SHGIL_PROC)  (HIMAGELIST *phLarge, HIMAGELIST *phSmall);
typedef BOOL (WINAPI * FII_PROC) (BOOL fFullInit);
HMODULE     hShell32 = 0;

//
// Use undocumented SHELL APIs to get system imagelists
//
BOOL GetSystemImageList(HIMAGELIST *phLarge, HIMAGELIST *phSmall)
{
   SHGIL_PROC  Shell_GetImageLists;
   FII_PROC FileIconInit;

   if(phLarge == 0 || phSmall == 0)
      return FALSE;

   // Don't free until we terminate - otherwise, the image-lists will be destroyed
   if(hShell32 == 0)
      hShell32 = LoadLibrary("shell32.dll");

   if(hShell32 == 0)
      return FALSE;

   // Get Undocumented APIs from Shell32.dll:
   // Shell_GetImageLists and FileIconInit
   //
   Shell_GetImageLists  = (SHGIL_PROC)  GetProcAddress(hShell32, (LPCSTR)71);
   FileIconInit       = (FII_PROC)    GetProcAddress(hShell32, (LPCSTR)660);

   // FreeIconList@8 = ord 227

   if(Shell_GetImageLists == 0)
   {
      FreeLibrary(hShell32);
      return FALSE;
   }

   // Initialize imagelist for this process - function not present on win95/98
   if(FileIconInit != 0)
      FileIconInit(TRUE);

   // Get handles to the large+small system image lists!
   Shell_GetImageLists(phLarge, phSmall);

   return TRUE;
}

HB_FUNC( SYSGETIMAGELIST )
{
   HIMAGELIST hLarge;
   HIMAGELIST hSmall;

   if( GetSystemImageList( &hLarge, &hSmall ) )
   {
      hb_stornl( (LONG) hLarge, 1 );
      hb_stornl( (LONG) hSmall, 2 );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*
BOOL CreateDragImage(LPCWSTR pszPath, SHDRAGIMAGE *psdi)
{
  psdi->hbmpDragImage = NULL;
  SHFILEINFOW sfi;
  HIMAGELIST himl = (HIMAGELIST)
    SHGetFileInfoW(pszPath, 0, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX);
  if (himl) {
    int cx, cy;
    ImageList_GetIconSize(himl, &cx, &cy);
    psdi->sizeDragImage.cx = cx;
    psdi->sizeDragImage.cy = cy;
    psdi->ptOffset.x = cx;
    psdi->ptOffset.y = cy;
    psdi->crColorKey = CLR_NONE;
    HDC hdc = CreateCompatibleDC(NULL);
    if (hdc) {
      psdi->hbmpDragImage = CreateBitmap(cx, cy, 1, 32, NULL);
      if (psdi->hbmpDragImage) {
        HBITMAP hbmPrev = SelectBitmap(hdc, psdi->hbmpDragImage);
        ImageList_Draw(himl, sfi.iIcon, hdc, 0, 0, ILD_NORMAL);
        SelectBitmap(hdc, hbmPrev);
      }
      DeleteDC(hdc);
    }
  }
  return psdi->hbmpDragImage != NULL;
}
*/

HB_FUNC( GETCSTR )
{
   hb_retni( (int) L"aaa" );
}

#include <wincon.h>
#include <stdlib.h>
#include <time.h>

#include <Nb30.h>

const long lNAME_SIZE = 1024;

BOOL ResetAdapter(UCHAR lana)
{
   NCB ncb;
   UCHAR uRet;
   ZeroMemory(&ncb, sizeof(NCB));

   ncb.ncb_command = NCBRESET;
   ncb.ncb_lsn = 0x00;
   ncb.ncb_callname[0] = 20;
   ncb.ncb_callname[2] = 100;
   ncb.ncb_lana_num = lana;

   uRet = Netbios(&ncb);
   if(uRet == NRC_GOODRET)
      return TRUE;

   SetLastError(uRet);
   return FALSE;
}


BOOL GetAdapterStatus(UCHAR lana, UCHAR uMACAddress[])
{
   UCHAR uRet;
   NCB ncb;
   BYTE buffer[1026];
   ADAPTER_STATUS *pAS;

   ZeroMemory(&ncb, sizeof(NCB));
   ZeroMemory(buffer, lNAME_SIZE+2);

   ncb.ncb_command = NCBASTAT;
   ncb.ncb_buffer = buffer;
   ncb.ncb_length = lNAME_SIZE;
   ncb.ncb_callname[0] = '*';
   ncb.ncb_lana_num = lana;

   uRet = Netbios(&ncb);
   if(uRet == NRC_GOODRET)
   {
      pAS = (ADAPTER_STATUS *)buffer;
      memcpy(uMACAddress, &(pAS->adapter_address), 6);
      return TRUE;
   }
   SetLastError(uRet);
   return FALSE;
}

LPCSTR GetMacAddress( char* szBuffer )
{
   // Ensure buffer size is big enough to hold the address

   UCHAR uRet;
   NCB ncb;
   LANA_ENUM lenum;
   UCHAR uMACAddress[6];
   int i;
   int iLEnumCnt = 1;
   UCHAR uRASAdapterAddress[6] = { 0x44, 0x45, 0x53, 0x54, 0x0, 0x0 }; // DEST

   ZeroMemory(&ncb, sizeof(NCB));
   ZeroMemory(&lenum, sizeof(lenum));

   // Helper function using GetVersionEx()

   ncb.ncb_command = NCBENUM;
   ncb.ncb_buffer = (UCHAR *)&lenum;
   ncb.ncb_length = sizeof(lenum);

   uRet = Netbios(&ncb);

   if(uRet == NRC_GOODRET)
   {
      iLEnumCnt = (int)lenum.length;
   }

   for( i = 0; i < iLEnumCnt; ++i )
   {
      if( ResetAdapter( lenum.lana[i] ) )
      {
         if( GetAdapterStatus(lenum.lana[i], uMACAddress ) )
         {
            // ignore RAS adapter address, we want MAC
            if( memcmp( uMACAddress, uRASAdapterAddress, 6 ) != 0 )
            {
               sprintf( szBuffer, "%02X:%02X:%02X:%02X:%02X:%02X", (int)uMACAddress[0], (int)uMACAddress[1], (int)uMACAddress[2], (int)uMACAddress[3], (int)uMACAddress[4], (int)uMACAddress[5]);
               return (LPCSTR) szBuffer;
            }
         }
      }
   }

   return NULL;
}

HB_FUNC( GETMACADDRESS )
{
   TCHAR szBuffer[18];
   hb_retc( GetMacAddress( szBuffer ) );
}

HB_FUNC( __ATLAXCREATECONTROL )
{
   IStream  *pStr = NULL;
   IUnknown *pUnk = NULL;
   ATLAXCREATECONTROL pAtlAxCreateControl = (ATLAXCREATECONTROL) GetProcAddress( hLib, "AtlAxCreateControl" );
   if (pAtlAxCreateControl)
   {
      pAtlAxCreateControl( (LPCOLESTR) hb_parc(2), (HWND) hb_parnl(1), pStr, &pUnk );
      hb_retnl( (long) pUnk );
   }
   else
   {
      hb_retnl(0);
   }
}

HB_FUNC( __ATLAXCREATECONTROLLIC )
{
   IStream  *pStr = NULL;
   IUnknown *pUnk = NULL;
   BSTR bstrLic =   NULL;
   ATLAXCREATECONTROLLIC pAtlAxCreateControlLic = (ATLAXCREATECONTROLLIC) GetProcAddress( hLib, "AtlAxCreateControlLic" );
   if (pAtlAxCreateControlLic)
   {
      pAtlAxCreateControlLic( (LPCOLESTR) hb_parc(2), (HWND) hb_parnl(1), pStr, &pUnk, bstrLic );
      hb_retnl( (long) pUnk );
   }
   else
   {
      hb_retnl(0);
   }
}

HB_FUNC( __ATLAXGETHOST )
{
   IUnknown *pUnk = NULL;
   ATLAXGETHOST pAtlAxGetHost = (ATLAXGETHOST) GetProcAddress( hLib, "AtlAxGetHost" );
   if (pAtlAxGetHost)
   {
      pAtlAxGetHost( (HWND) hb_parnl(1), &pUnk );
      hb_retnl( (long) pUnk );
   }
   else
   {
      hb_retnl(0);
   }
}

HB_FUNC( __AXTRANSLATEBYHANDLE )
{
   IOleObject *browserObject;
   IOleInPlaceActiveObject *pIOIPAO = NULL;
   MSG *pMsg = (MSG*) hb_param( 2, HB_IT_STRING )->item.asString.value;
   browserObject = *((IOleObject **)GetWindowLong( (HWND) hb_parnl(1), GWL_USERDATA));

   if (!browserObject->lpVtbl->QueryInterface(browserObject, &IID_IOleInPlaceActiveObject, (void**)&pIOIPAO))
   {
       pIOIPAO->lpVtbl->TranslateAccelerator(pIOIPAO,pMsg);
   }
}

IUnknown* AxGetUnknown( HWND hWnd )
{
   IUnknown *pUnk = NULL;
   ATLAXGETCONTROL pAtlAxGetControl = (ATLAXGETCONTROL) GetProcAddress( hLib, "AtlAxGetControl" );
   if (pAtlAxGetControl)
   {
      pAtlAxGetControl( hWnd, &pUnk );
   }
   return pUnk;
}

HB_FUNC( __AXGETUNKNOWN )
{
   hb_retnl( (long) AxGetUnknown( (HWND) hb_parnl(1) ) );
}


//AtlAdvise(m_ipConnectedView, this->GetUnknown(), IID_IActiveViewEvents, &m_dwViewCookie);

//------------------------------------------------------------------------------------------------------------------------------------

BOOL AxTranslateMessage( IUnknown *pUnk, MSG *pMsg )
{
   BOOL bRet = FALSE;
   IOleInPlaceActiveObject *pIOIPAO = NULL;
   if( SUCCEEDED( pUnk->lpVtbl->QueryInterface( pUnk, &IID_IOleInPlaceActiveObject, (void**) &pIOIPAO ) ) )
   {
      HRESULT hr = pIOIPAO->lpVtbl->TranslateAccelerator( pIOIPAO, pMsg );
      pIOIPAO->lpVtbl->Release(pIOIPAO);
      bRet = ( S_OK == hr );
   }
   return bRet;
}

HB_FUNC( __AXTRANSLATEMESSAGE )
{
   hb_retl( AxTranslateMessage( (IUnknown*) hb_parnl(1), (MSG*) hb_param( 2, HB_IT_STRING )->item.asString.value ) );
}

static LRESULT CALLBACK MsgFilterFunc( int nCode, WPARAM wParam, LPARAM lParam )
{
   PMSG msg;

   if( nCode < 0 )
   {
      return CallNextHookEx( s_hMsgHook, nCode, wParam, lParam );
   }

   msg = (PMSG) lParam;

   HWND hWnd = msg->hwnd;
   UINT nMsg = 0;

   if( ( msg->message == WM_KEYDOWN || msg->message == WM_KEYUP ) && ( msg->wParam == VK_BACK || msg->wParam == VK_LEFT || msg->wParam == VK_RIGHT || msg->wParam == VK_UP || msg->wParam == VK_DOWN ) )
   {
      nMsg = WM_KEYUP;
   }
   if( nMsg == 0 || msg->message == nMsg )
   {
      IUnknown *pUnk;
      while( IsWindow( hWnd ) )
      {
         if( pUnk = AxGetUnknown( hWnd ) )
         {
            if( AxTranslateMessage( pUnk, msg ) )
            {
               return 0;
            }
            else
            {
               if( msg->message == 1169 )
               {
                  SetFocus( msg->hwnd );
                  return 0;
               }
            }
         }
         hWnd = GetParent( hWnd );
      }
   }
   return CallNextHookEx( s_hMsgHook, nCode, wParam, lParam );
}

HB_FUNC( __MSGHOOKFUNC )
{
   if( s_hMsgHook == NULL )
      s_hMsgHook = SetWindowsHookEx( WH_MSGFILTER, (HOOKPROC) MsgFilterFunc, NULL, GetCurrentThreadId() );
}

HB_FUNC( __MSGUNHOOKFUNC )
{
   UnhookWindowsHookEx( s_hMsgHook );
   s_hMsgHook = NULL;
}

/*
BOOL PreTranslateAccelerator(MSG* pMsg)
{
     // Accelerators are only keyboard or mouse messages
     if ((pMsg->message < WM_KEYFIRST || pMsg->message >
          WM_KEYLAST) && (pMsg->message <
          WM_MOUSEFIRST || pMsg->message >
          WM_MOUSELAST))
         return FALSE;

     // Find a direct child of this window from the
     // window that has focus.
     // This will be AtlAxWin window for the hosted
     // control.
     HWND hWndCtl = ::GetFocus();
     if( IsChild(hWndCtl) && GetParent(hWndCtl) !=
         m_hWnd ) {
         do hWndCtl = GetParent(hWndCtl);
         while( GetParent(hWndCtl) != m_hWnd );
     }

     // Give the control (via the AtlAxWin) a chance to
     // translate this message
     if (SendMessage(hWndCtl, WM_FORWARDMSG, 0, (LPARAM)pMsg) )
        return TRUE;

     // Check for dialog-type navigation accelerators
     return IsDialogMessage(pMsg);
 }
 */
/*
HB_FUNC( __AXTRANSLATE )
{
  BOOL bRet = FALSE;
  IWebBrowser2  *webBrowser2;
  IOleInPlaceActiveObject *OleInPlaceActiveObject;

  MSG  *pMsg = (MSG*) hb_param( 2, HB_IT_STRING )->item.asString.value;
  IOleObject    *browserObject = *((IOleObject **)GetWindowLong( (HWND) hb_parnl(1), GWL_USERDATA));

  HRESULT hr = (!browserObject->lpVtbl->QueryInterface(browserObject, &IID_IOleInPlaceActiveObject, (void **)&OleInPlaceActiveObject));

  if (SUCCEEDED(hr))
  {
     hr = OleInPlaceActiveObject->lpVtbl->TranslateAccelerator( OleInPlaceActiveObject, pMsg);
     OleInPlaceActiveObject->lpVtbl->Release(OleInPlaceActiveObject);
     bRet = ( S_OK == hr );
  }
  hb_retl( bRet );
}

*/
BOOL AxTranslateMessageEx( MSG *pMsg );


HB_FUNC( __AXTRANSLATEMESSAGEEX )
{
   MSG *msg = (MSG*) hb_param( 1, HB_IT_STRING )->item.asString.value;
   hb_retl( AxTranslateMessageEx( msg ) );
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __AXGETDISPATCH )
{
   IDispatch *pDisp = NULL;
   IUnknown  *pUnk = (IUnknown*) hb_parnl(1);
   HWND hwnd = (HWND) hb_parnl(2);

   pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, (void**) &pDisp );

   if( hwnd )
   {
      IOleObject *lpOleObject = NULL;

      if( SUCCEEDED( pUnk->lpVtbl->QueryInterface( pUnk, &IID_IOleObject, (void**) &lpOleObject ) ) );
      {
         IOleClientSite* lpOleClientSite;

         if( SUCCEEDED( lpOleObject->lpVtbl->GetClientSite( lpOleObject, &lpOleClientSite ) ) )
         {
            lpOleObject->lpVtbl->DoVerb( lpOleObject, hb_parni(3), NULL, lpOleClientSite, 0, hwnd, NULL );
            lpOleClientSite->lpVtbl->Release( lpOleClientSite );
         }
      }
   }

   hb_retnl( (long) pDisp );
}

HB_FUNC( __AXGETMISCSTATUS )
{
   IDispatch *pDisp = NULL;
   IUnknown  *pUnk = (IUnknown*) hb_parnl(1);
   IOleObject *lpOleObject = NULL;

   pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, (void**) &pDisp );

   if( SUCCEEDED( pUnk->lpVtbl->QueryInterface( pUnk, &IID_IOleObject, (void**) &lpOleObject ) ) );
   {
      DWORD pdwStatus;
      if( SUCCEEDED( lpOleObject->lpVtbl->GetMiscStatus( lpOleObject, (DWORD) hb_parni(2), &pdwStatus ) ) )
      {
         hb_retnl( (long) pdwStatus );
      }
   }
}

HB_FUNC( __AXCLOSE )
{
   IUnknown  *pUnk = (IUnknown*) hb_parnl(1);
   IOleObject *lpOleObject = NULL;
   if( SUCCEEDED( pUnk->lpVtbl->QueryInterface( pUnk, &IID_IOleObject, (void**) &lpOleObject ) ) );
   {
      lpOleObject->lpVtbl->Close( lpOleObject, 1 );
   }
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __AXGETPROPERTIESDIALOG )
{
   IUnknown  *pUnk = (IUnknown*) hb_parnl(2);
   IOleObject *lpOleObject = NULL;
   HRESULT hRes;
   if( SUCCEEDED( pUnk->lpVtbl->QueryInterface( pUnk, &IID_IOleObject, (void**) &lpOleObject ) ) );
   {
      IOleClientSite* lpOleClientSite;
      if( SUCCEEDED( lpOleObject->lpVtbl->GetClientSite( lpOleObject, &lpOleClientSite ) ) )
      {
         hRes = lpOleObject->lpVtbl->DoVerb( lpOleObject, OLEIVERB_PROPERTIES, NULL, lpOleClientSite, 0, (HWND) hb_parnl(1), NULL );
         lpOleClientSite->lpVtbl->Release( lpOleClientSite );
         hb_retnl( (long) hRes );
      }
   }
}

//------------------------------------------------------------------------------------------------------------------------------------
BOOL AxAtlSetProp( IDispatch* lpDispatch, LPSTR cProp, PHB_ITEM pValue )
{
   DISPID property_ID;
   BOOL bRet = FALSE;

   BSTR bstrMessage = hb_oleAnsiToSysString( cProp );
   lpDispatch->lpVtbl->GetIDsOfNames( lpDispatch, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, 0, &property_ID );
   if( bstrMessage != NULL )
   {
      SysFreeString( bstrMessage );
   }

   if ( property_ID == -1)
   {
       hb_retl( bRet );
   }
   else
   {
      BSTR bstrValue = NULL;
      DISPID Putter = DISPID_PROPERTYPUT;
      DISPPARAMS params;
      VARIANTARG pArgs;
      VARIANT result;
      EXCEPINFO exception;
      UINT argerr;
      memset(&params, 0, sizeof params);

      if ( SUCCEEDED( lpDispatch->lpVtbl->Invoke( lpDispatch, property_ID, &IID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYPUT, &params, &result, &exception, &argerr ) ) )
      {
         memset(&pArgs, 0, sizeof pArgs);

         pArgs.vt = result.vt;

         switch( result.vt )
         {
            case VT_BSTR:
                 bstrValue = hb_oleAnsiToSysString( hb_itemGetCPtr( pValue ) );
                 pArgs.bstrVal = bstrValue;
                 break;

            case VT_I2:
                 pArgs.intVal = hb_itemGetNI( pValue );
                 break;

            case VT_I4:
                 pArgs.lVal = hb_itemGetNL( pValue );
                 break;

            case VT_BOOL:
                 pArgs.boolVal = hb_itemGetL( pValue ) ? VARIANT_TRUE : VARIANT_FALSE;
                 break;
         }

         params.cArgs = 1;
         params.rgvarg = &pArgs;
         params.cNamedArgs = 1;
         params.rgdispidNamedArgs = &Putter;

         bRet = SUCCEEDED(lpDispatch->lpVtbl->Invoke( lpDispatch, property_ID, &IID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYPUT, &params, NULL, NULL, NULL ) );

         if( bstrValue != NULL );
         {
            SysFreeString( bstrValue );
         }
      }
   }
   return bRet;
}

HB_FUNC( __AXSETPROPERTY )
{
   hb_retl( AxAtlSetProp( (IDispatch*) hb_parnl(1), (LPSTR) hb_parc(2), (PHB_ITEM) hb_param( 3, HB_IT_ANY ) ) );
}

HB_FUNC( __AXGETPROPERTY )
{
   DISPID property_ID;
   IDispatch* lpDispatch = (IDispatch*) hb_parnl(1);
   BSTR bstrMessage = hb_oleAnsiToSysString( hb_parc(2) );
   lpDispatch->lpVtbl->GetIDsOfNames( lpDispatch, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, 0, &property_ID );
   if( bstrMessage != NULL )
   {
      SysFreeString( bstrMessage );
   }

   if ( property_ID != -1)
   {
      EXCEPINFO exception;
      UINT argerr;
      VARIANT result;
      DISPPARAMS params;

      memset(&params, 0, sizeof params);

      if ( SUCCEEDED( lpDispatch->lpVtbl->Invoke( lpDispatch, property_ID, &IID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYGET, &params, &result, &exception, &argerr ) ) )
      {
         IUnknown  *pUnk   = NULL;
         IDispatch *pDisp  = NULL;
         switch( result.vt )
         {
            case VT_DISPATCH:
                 hb_retnl( (long) result.pdispVal );
                 break;

            case VT_UNKNOWN:
                 pUnk = result.punkVal;
                 if( pUnk )
                 {
                    pDispPtr = pDisp;
                    pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) &IID_IDispatch, (void **) &pDispPtr );
                    hb_retnl( (long) pDispPtr );
                 }
                 break;

            case VT_BOOL:
                 hb_retl( (BOOL) result.boolVal );
                 break;

            case VT_I4:
                 hb_retnl( (long) result.plVal );
                 break;

            case VT_I2:
                 hb_retni( (int) result.pintVal );
                 break;

            case VT_BSTR:
            {
                 int iLen = WideCharToMultiByte( CP_ACP, 0, (LPCWSTR) result.bstrVal, -1, NULL, 0, NULL, NULL );
                 if ( iLen )
                 {
                    char *cString = (char *) hb_xgrab( iLen );
                    WideCharToMultiByte( CP_ACP, 0, (LPCWSTR) result.bstrVal, -1, cString, iLen, NULL, NULL );
                    hb_retc( cString );
                    hb_xfree( cString );
                 }
            }
            break;
         }
      }
   }
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __AXCALLMETHOD )
{
   IDispatch* lpDispatch = (IDispatch*) hb_parnl(1);
   LPSTR cMethod         = (LPSTR) hb_parc(2);
   PHB_ITEM aParams      = (PHB_ITEM) hb_param( 3, HB_IT_ARRAY );

   DISPID MethodID;
   BSTR bstrMessage = hb_oleAnsiToSysString( cMethod );
   int iCount = aParams->item.asArray.value->ulLen;

   lpDispatch->lpVtbl->GetIDsOfNames( lpDispatch, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, 0, &MethodID );
   if( bstrMessage != NULL )
   {
      SysFreeString( bstrMessage );
   }

   if ( MethodID != -1)
   {
      int n, nArg;
      DISPPARAMS pParams;
      VARIANTARG *pArgs = NULL;
      VARIANT    result;
      VariantInit(&result);

      if( iCount > 0 )
      {
         PHB_ITEM pItem;
         pArgs = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * iCount );
         for( n = 0; n < iCount; n++ )
         {
            nArg = iCount - n;
            VariantInit( &( pArgs[ n ] ) );

            pItem = hb_itemArrayGet( aParams, nArg );

            switch( pItem->type )
            {
               case HB_IT_STRING:
                  pArgs[n].vt = VT_BSTR;
                  pArgs[n].bstrVal = hb_oleAnsiToSysString( hb_itemGetCPtr( pItem ) );
                  break;

               case HB_IT_LOGICAL:
                  pArgs[n].vt = VT_BOOL;
                  pArgs[n].boolVal = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
            }
         }
         pParams.cArgs = iCount;
         pParams.rgvarg = pArgs;
         pParams.rgdispidNamedArgs = 0;
         pParams.cNamedArgs = 0;
      }

      if( SUCCEEDED( lpDispatch->lpVtbl->Invoke( lpDispatch, MethodID, &IID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYGET | DISPATCH_METHOD, &pParams, &result, NULL, NULL )))//&exception, &argerr ) ) )
      {
         IUnknown  *pUnk   = NULL;
         IDispatch *pDisp  = NULL;
         switch( result.vt )
         {
            case VT_DISPATCH:
                 hb_retnl( (long) result.pdispVal );
                 break;

            case VT_UNKNOWN:
                 pUnk = result.punkVal;
                 if( pUnk )
                 {
                    pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) &IID_IDispatch, (void **) &pDisp );
                    hb_retnl( (long) pDisp );
                 }
                 break;

            case VT_BOOL:
                 hb_retl( (BOOL) result.boolVal );
                 break;

            case VT_I4:
                 hb_retnl( (long) result.plVal );
                 break;

            case VT_I2:
                 hb_retni( (int) result.pintVal );
                 break;

            case VT_BSTR:
            {
                 int iLen = WideCharToMultiByte( CP_ACP, 0, (LPCWSTR) result.bstrVal, -1, NULL, 0, NULL, NULL );
                 if ( iLen )
                 {
                    char *cString = (char *) hb_xgrab( iLen );
                    WideCharToMultiByte( CP_ACP, 0, (LPCWSTR) result.bstrVal, -1, cString, iLen, NULL, NULL );
                    hb_retc( cString );
                    hb_xfree( cString );
                 }
            }
            break;
         }
      }
   }
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __AXRELEASEUNKNOWN )
{
   IUnknown *pUnk = (IUnknown*) hb_parnl(1);
   pUnk->lpVtbl->Release( pUnk );
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __AXRELEASEDISPATCH )
{
   IDispatch *pDisp = (IDispatch*) hb_parnl(1);
   pDisp->lpVtbl->Release( pDisp );
}

//------------------------------------------------------------------------------------------------------------------------------------

HB_FUNC( __LOADATLAX )
{
   LoadAtlAx();
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __UNLOADATLAX )
{
   ATLAXWINTERM pAtlAxWinTerm = (ATLAXWINTERM) GetProcAddress( hLib, "AtlAxWinTerm" );
   if( pAtlAxWinTerm )
   {
      pAtlAxWinTerm();
      FreeLibrary(hLib);
   }
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __GETPIXELFROMBMP )
{
   HDC      hDcMem;
   HBITMAP  hBmp = (HBITMAP) hb_parnl(1);
   HBITMAP  hBmpOld;
   COLORREF cColor;
   if( hBmp )
   {
      hDcMem  = CreateCompatibleDC( NULL );
      hBmpOld = (HBITMAP) SelectObject( hDcMem, (HGDIOBJ) hBmp );

      cColor = GetPixel( hDcMem, 0, 0 );
      SelectObject( hDcMem, hBmpOld );
      DeleteDC( hDcMem );
   }
   hb_retnl( (long) cColor );
}

HB_FUNC( __ENUMSERVICES )
{
   const char *szServer = hb_parc(1);

   SC_HANDLE scm;
   BOOL success;
   LPENUM_SERVICE_STATUS status;
   DWORD numServices=0, sizeNeeded=0, resume=0;

   scm = OpenSCManager(szServer, 0, SC_MANAGER_ALL_ACCESS);
   if( scm )
   {
      success = EnumServicesStatus( scm, SERVICE_WIN32 | SERVICE_DRIVER, SERVICE_STATE_ALL, 0, 0, &sizeNeeded, &numServices, &resume );
      if( GetLastError() == ERROR_MORE_DATA )
      {
         status = (LPENUM_SERVICE_STATUS)
         LocalAlloc(LPTR, sizeNeeded);

         resume = 0;
         success = EnumServicesStatus(scm, SERVICE_WIN32 | SERVICE_DRIVER, SERVICE_STATE_ALL, status, sizeNeeded, &sizeNeeded, &numServices, &resume );
         if( success )
         {
            DWORD i;
            HB_ITEM_NEW( Array );
            HB_ITEM_NEW( Item );

            hb_arrayNew( &Array, 0 );

            for (i=0; i < numServices; i++)
            {
                hb_arrayNew( &Item, 2 );
                hb_itemPutC( hb_arrayGetItemPtr( &Item, 1 ), status[i].lpServiceName );
                hb_itemPutC( hb_arrayGetItemPtr( &Item, 2 ), status[i].lpDisplayName );
                hb_arrayAddForward( &Array, &Item );
            }
            hb_itemReturnForward( &Array );
         }
         LocalFree(status);
      }
      CloseServiceHandle(scm);
   }
}

HB_FUNC ( _REGENUMVALUE )
{
   long bErr;
   TCHAR pwzName[256];
   ULONG cchName = 256;
   DWORD fType;
   BYTE  lpData[2048];
   DWORD cbData = 2048;

   bErr = RegEnumValue( (HKEY) hb_parnl(1), hb_parnl(2), pwzName, &cchName, NULL, &fType, lpData, &cbData);
   if ( bErr == ERROR_SUCCESS )
   {
      hb_storc( (char *) pwzName, 3 );
      hb_stornl( ( long ) fType, 4 );
      hb_storc( (char *) lpData, 5 );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*
//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __ENUMSERVICES )
{
   //Open the Service Control Manager
   SC_HANDLE sc = OpenSCManager (NULL,NULL,SC_MANAGER_ENUMERATE_SERVICE);
   //Check if OpenSCManager returns NULL. Otherwise proceed
   if (sc != NULL)
   {
      OutputDebugString("Opened SCM using OpenSCManager");
      ENUM_SERVICE_STATUS service_data, *lpservice;
      BOOL retVal;
      DWORD bytesNeeded,srvCount,resumeHandle = 0,srvType, srvState;
      srvType = SERVICE_WIN32;
      srvState = SERVICE_STATE_ALL;
      //Call EnumServicesStatus using the handle returned by OpenSCManager
      retVal = EnumServicesStatus (sc,srvType,srvState,&service_data,sizeof(service_data), &bytesNeeded,&srvCount,&resumeHandle);
      DWORD err = GetLastError();
      //Check if EnumServicesStatus needs more memory space
      if ((retVal == FALSE) || err == ERROR_MORE_DATA)
      {
         DWORD dwBytes = bytesNeeded + sizeof(ENUM_SERVICE_STATUS);
         lpservice = new ENUM_SERVICE_STATUS [dwBytes];
         EnumServicesStatus (sc,srvType,srvState,lpservice,dwBytes, &bytesNeeded,&srvCount,&resumeHandle);
      }
      printf("Count of NT Services using EnumServicesStatus : %d\n\n",srvCount);
      for(int i=0;srvCount)
      {
         OutputDebugString( lpservice[i].lpDisplayName );
         //printf("%s\n",lpservice[i].lpDisplayName);
      }
   }
   //Close the SC_HANLDE returned by OpenSCManager
   CloseServiceHandle(sc);
}
*/

static HWND hList;
static BOOL bAscending;

int CALLBACK ListViewCompareProc( LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
   static LV_FINDINFO fi;
   static int  nItem1, nItem2;
   static char szBuf1[30], szBuf2[30];

   // Determine the items that we are comparing.
   fi.flags  = LVFI_PARAM;
   fi.lParam = lParam1;
   nItem1    = ListView_FindItem(hList, -1, &fi);

   fi.lParam = lParam2;
   nItem2    = ListView_FindItem(hList, -1, &fi);

   // Retrieve the item text so we can compare it.
   ListView_GetItemText(hList, nItem1, lParamSort, szBuf1, sizeof(szBuf1));
   ListView_GetItemText(hList, nItem2, lParamSort, szBuf2, sizeof(szBuf2));

   // Return the comparison results.
   if( bAscending )
   {
      return strcmp(szBuf1, szBuf2);
   }
   else
   {
      return strcmp(szBuf1, szBuf2) * -1;
   }
}

int CALLBACK ListViewCompareProc2(LPARAM lParam1,
                                 LPARAM lParam2,
                                 LPARAM lParamSort)
{
    LPLVITEMDATA lplvid1 = (LPLVITEMDATA)lParam1;
    LPLVITEMDATA lplvid2 = (LPLVITEMDATA)lParam2;
    int  iResult = 1;

    if( lplvid1 && lplvid2)
    {
       if( (lplvid1->ulAttribs & SFGAO_FOLDER) && !( lplvid2->ulAttribs & SFGAO_FOLDER) )
       {
            return -1;
       }
       if( !(lplvid1->ulAttribs & SFGAO_FOLDER) &&  (lplvid2->ulAttribs & SFGAO_FOLDER) )
            return 1;

       //GetName( lplvid1->lpsfParent, lplvid1->lpi, SHGDN_NORMAL, szTemp1 );
       //GetName( lplvid2->lpsfParent, lplvid2->lpi, SHGDN_NORMAL, szTemp2 );

       //iResult = lstrcmpi(szTemp1, szTemp2) ;
    }

    return iResult;
}

HB_FUNC( __LISTVIEWSORTCOLUMN )
{
   hList = (HWND) hb_parnl(1);
   bAscending = hb_parl(3);
   SendMessage( hList, LVM_SORTITEMS, (WPARAM)(LPARAM)hb_parnl(2), (LPARAM)(PFNLVCOMPARE)ListViewCompareProc2 );
}

HB_FUNC( __LISTVIEWGETSUBITEM )
{
   LPARAM lParam = (LPARAM) hb_parnl(1);
   hb_retni( (int)((NMLISTVIEW*)lParam)->iSubItem );
}

HB_FUNC( __LISTVIEWGETNEWSTATE )
{
   LPARAM lParam = (LPARAM) hb_parnl(1);
   hb_retni( (int)((NMLISTVIEW*)lParam)->uNewState );
}

BOOL __SystemShutdown( LPTSTR lpMsg )
{
   HANDLE hToken;              // handle to process token
   TOKEN_PRIVILEGES tkp;       // pointer to token structure
   BOOL fResult;               // system shutdown flag

   // Get the current process token handle so we can get shutdown privilege.
   if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
   {
      return FALSE;
   }

   // Get the LUID for shutdown privilege.
   LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid);

   tkp.PrivilegeCount = 1;  // one privilege to set
   tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

   // Get shutdown privilege for this process.
   AdjustTokenPrivileges(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES) NULL, 0);

   // Cannot test the return value of AdjustTokenPrivileges.
   if (GetLastError() != ERROR_SUCCESS)
   {
      return FALSE;
   }

   // Display the shutdown dialog box and start the countdown.
   fResult = InitiateSystemShutdown( NULL,    // shut down local computer
                                     lpMsg,   // message for user
                                     30,      // time-out period, in seconds
                                     FALSE,   // ask user to close apps
                                     TRUE);   // reboot after shutdown

   if (!fResult)
   {
      return FALSE;
   }

   // Disable shutdown privilege.
   tkp.Privileges[0].Attributes = 0;
   AdjustTokenPrivileges(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES) NULL, 0);
   return TRUE;
}

// SystemShutdown( cMessage )
// Invokes the "The System will Shutdown" dialog with a specific message and countdown to the shutdown procedure.
HB_FUNC( SYSTEMSHUTDOWN )
{
   hb_retl( __SystemShutdown( (LPSTR) hb_parc(1) ) );
}

//------------------------------------------------------------------------------------------------------------------------------------
PBITMAPINFO CreateBitmapInfoStruct( HBITMAP hBmp )
{
    BITMAP      bmp;
    PBITMAPINFO pbmi;
    WORD        cClrBits;

    GetObject(hBmp, sizeof(BITMAP), (LPSTR)&bmp);

    cClrBits = (WORD)(bmp.bmPlanes * bmp.bmBitsPixel);

    if (cClrBits == 1)
        cClrBits = 1;
    else if (cClrBits <=  4)
        cClrBits = 4;
    else if (cClrBits <=  8)
        cClrBits = 8;
    else if (cClrBits <= 16)
        cClrBits = 16;
    else if (cClrBits <= 24)
        cClrBits = 24;
    else cClrBits = 32;

    if (cClrBits != 24)
        pbmi = (PBITMAPINFO) LocalAlloc(LPTR, sizeof(BITMAPINFOHEADER) + sizeof(RGBQUAD) * (1<< cClrBits));
    else
        pbmi = (PBITMAPINFO) LocalAlloc(LPTR, sizeof(BITMAPINFOHEADER));

    pbmi->bmiHeader.biSize     = sizeof(BITMAPINFOHEADER);
    pbmi->bmiHeader.biWidth    = bmp.bmWidth;
    pbmi->bmiHeader.biHeight   = bmp.bmHeight;
    pbmi->bmiHeader.biPlanes   = bmp.bmPlanes;
    pbmi->bmiHeader.biBitCount = bmp.bmBitsPixel;
    if (cClrBits < 24)
    {
        pbmi->bmiHeader.biClrUsed = (1<<cClrBits);
    }
    pbmi->bmiHeader.biCompression = BI_RGB;
    pbmi->bmiHeader.biSizeImage   = ((pbmi->bmiHeader.biWidth * cClrBits +31) & ~31) /8 * pbmi->bmiHeader.biHeight;
    pbmi->bmiHeader.biClrImportant = 0;
    return pbmi;
 }

HB_FUNC( SENDBITMAP )
{
   SOCKET mySocket = (SOCKET) hb_parnl(1);
   BITMAPFILEHEADER hdr;
   PBITMAPINFOHEADER pbih;
   LPBYTE lpBits;
   PBITMAPINFO pbi;

   HBITMAP hBMP = (HBITMAP) hb_parnl(2);
   HDC hDC = (HDC) hb_parnl(3);

   pbi    = CreateBitmapInfoStruct( hBMP );
   pbih   = (PBITMAPINFOHEADER) pbi;
   lpBits = (LPBYTE) GlobalAlloc(GMEM_FIXED, pbih->biSizeImage);

   GetDIBits(hDC, hBMP, 0, (WORD) pbih->biHeight, lpBits, pbi, DIB_RGB_COLORS );

   hdr.bfType = 0x4d42;

   hdr.bfSize = (DWORD) (sizeof(BITMAPFILEHEADER) + pbih->biSize + pbih->biClrUsed * sizeof(RGBQUAD) + pbih->biSizeImage);
   hdr.bfReserved1 = 0;
   hdr.bfReserved2 = 0;

   hdr.bfOffBits = (DWORD) sizeof(BITMAPFILEHEADER) + pbih->biSize + pbih->biClrUsed * sizeof (RGBQUAD);

   send( mySocket, (char*) hb_parc(4), hb_parclen(4), 0);
   send( mySocket, (const char*)&hdr,  sizeof(BITMAPFILEHEADER), 0);
   send( mySocket, (const char*)&pbih, sizeof(BITMAPINFOHEADER), 0);
   send( mySocket, (char *)(&lpBits),  sizeof(lpBits), 0);

   GlobalFree((HGLOBAL)lpBits);
}

HB_FUNC( GETBITMAPBITS )
{
   BITMAPFILEHEADER hdr;
   PBITMAPINFOHEADER pbih;
   LPBYTE lpBits;
   PBITMAPINFO pbi;

   HBITMAP hBMP = (HBITMAP) hb_parnl(1);
   HDC hDC = GetDC(0);

   pbi    = CreateBitmapInfoStruct( hBMP );
   pbih   = (PBITMAPINFOHEADER) pbi;
   lpBits = (LPBYTE) GlobalAlloc(GMEM_FIXED, pbih->biSizeImage);

   GetDIBits(hDC, hBMP, 0, (WORD) pbih->biHeight, lpBits, pbi, DIB_RGB_COLORS );

   hdr.bfType = 0x4d42;

   hdr.bfSize = (DWORD) (sizeof(BITMAPFILEHEADER) + pbih->biSize + pbih->biClrUsed * sizeof(RGBQUAD) + pbih->biSizeImage);
   hdr.bfReserved1 = 0;
   hdr.bfReserved2 = 0;

   hdr.bfOffBits = (DWORD) sizeof(BITMAPFILEHEADER) + pbih->biSize + pbih->biClrUsed * sizeof (RGBQUAD);

   hb_retnl( (long) lpBits);
   //GlobalFree((HGLOBAL)lpBits);
   ReleaseDC(0,hDC);
}

//------------------------------------------------------------------------------------------------------------------------------------
// The following example code defines a function that initializes the remaining structures, retrieves the array of palette indices, opens the file, copies the data, and closes the file.
//
void CreateBMPFile( const char *pszFile, HBITMAP hBMP, HDC hDC )
{
   HANDLE            hf;       // file handle
   BITMAPFILEHEADER  hdr;      // bitmap file-header
   PBITMAPINFOHEADER pbih;     // bitmap info-header
   LPBYTE            lpBits;   // memory pointer
   DWORD             cb;       // incremental count of bytes
   BYTE              *hp;      // byte pointer
   DWORD             dwTmp;
   PBITMAPINFO       pbi;

   pbi = CreateBitmapInfoStruct( hBMP );
   pbih   = (PBITMAPINFOHEADER) pbi;
   lpBits = (LPBYTE) GlobalAlloc(GMEM_FIXED, pbih->biSizeImage);

   // Retrieve the color table (RGBQUAD array) and the bits
   // (array of palette indices) from the DIB.
   //
   GetDIBits(hDC, hBMP, 0, (WORD) pbih->biHeight, lpBits, pbi, DIB_RGB_COLORS );

   hdr.bfType = 0x4d42;        // 0x42 = "B" 0x4d = "M"

   // Compute the size of the entire file.
   //
   hdr.bfSize = (DWORD) (sizeof(BITMAPFILEHEADER) + pbih->biSize + pbih->biClrUsed * sizeof(RGBQUAD) + pbih->biSizeImage);
   hdr.bfReserved1 = 0;
   hdr.bfReserved2 = 0;

   // Compute the offset to the array of color indices.
   //
   hdr.bfOffBits = (DWORD) sizeof(BITMAPFILEHEADER) + pbih->biSize + pbih->biClrUsed * sizeof (RGBQUAD);

   // Create the .BMP file.
   //
   hf = CreateFile(pszFile, GENERIC_READ | GENERIC_WRITE, (DWORD) 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, (HANDLE) NULL);

   // Copy the BITMAPFILEHEADER into the .BMP file.
   //
   WriteFile(hf, (LPVOID) &hdr, sizeof(BITMAPFILEHEADER), (LPDWORD) &dwTmp,  NULL);

   // Copy the BITMAPINFOHEADER and RGBQUAD array into the file.
   //
   WriteFile(hf, (LPVOID) pbih, sizeof(BITMAPINFOHEADER) + pbih->biClrUsed * sizeof (RGBQUAD), (LPDWORD) &dwTmp, ( NULL));

   // Copy the array of color indices into the .BMP file.
   //
   //  dwTotal = cb = pbih->biSizeImage;
   cb = pbih->biSizeImage;
   hp = lpBits;
   WriteFile(hf, (LPSTR) hp, (int) cb, (LPDWORD) &dwTmp,NULL);

   // Close the .BMP file.
   //
   CloseHandle(hf);

   // Free memory.
   GlobalFree((HGLOBAL)lpBits);
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTSAVEIMAGE )
{
   HICON hIcon;
   ICONINFO iconInfo;
   HDC hDC = GetDC( NULL );
   hIcon = ImageList_GetIcon( (HIMAGELIST) hb_parnl(1), hb_parni(2), ILD_NORMAL );
   GetIconInfo( hIcon, &iconInfo);
   CreateBMPFile( hb_parc(3), (HBITMAP) iconInfo.hbmColor, hDC );
   ReleaseDC( NULL, hDC );
   hb_retni( hb_parni(2) );
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( CREATEBMPFILE )
{
   HDC hDC = GetDC( NULL );
   CreateBMPFile( hb_parc(2), (HBITMAP) hb_parnl(1), ISNIL(2) ? hDC : (HDC) hb_parnl(2) );
   ReleaseDC( NULL, hDC );
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( GETREALKEYNAME )
{
   UINT vk = (UINT) hb_parni(1);
   unsigned int sc = MapVirtualKey(vk, 0);

   //int iSize = hb_parni(3);
   char * buf = (char *) hb_xgrab( 256 );

   unsigned short int temp;
   BOOL asc = (vk <= 32) || (vk >= 112 && vk <= 135) || (vk == VK_MULTIPLY);

   if( ! asc && vk != VK_DIVIDE )
   {
      asc = ToAscii( vk, sc, (const BYTE *) buf, &temp, 1 );
   }

   // Set bits
   sc <<= 16;
   sc |= 0x1 << 25; // <- don't care

   if( ! asc )
   {
      sc |= 0x1 << 24; // <- extended bit
   }

   // Convert to ANSI string
   if( GetKeyNameText(sc, buf, 256) )
   {
      hb_storclenAdopt( buf, strlen(buf), 2 );
   }
}

HPALETTE CreateDIBPalette (LPBITMAPINFO lpbmi, LPINT lpiNumColors)
{
   LPBITMAPINFOHEADER  lpbi;
   LPLOGPALETTE lpPal;
   HANDLE hLogPal;
   HPALETTE hPal = NULL;
   int i;

   lpbi = (LPBITMAPINFOHEADER)lpbmi;
   if (lpbi->biBitCount <= 8)
   {
       *lpiNumColors = (1 << lpbi->biBitCount);
   }
   else
   {
       *lpiNumColors = 0;  // No palette needed for 24 BPP DIB
   }

   if (lpbi->biClrUsed > 0)
   {
       *lpiNumColors = lpbi->biClrUsed;  // Use biClrUsed
   }

   if (*lpiNumColors)
   {
      hLogPal = GlobalAlloc (GHND, sizeof (LOGPALETTE) +
                             sizeof (PALETTEENTRY) * (*lpiNumColors));
      lpPal = (LPLOGPALETTE) GlobalLock (hLogPal);
      lpPal->palVersion    = 0x300;
      lpPal->palNumEntries = *lpiNumColors;

      for (i = 0;  i < *lpiNumColors;  i++)
      {
         lpPal->palPalEntry[i].peRed   = lpbmi->bmiColors[i].rgbRed;
         lpPal->palPalEntry[i].peGreen = lpbmi->bmiColors[i].rgbGreen;
         lpPal->palPalEntry[i].peBlue  = lpbmi->bmiColors[i].rgbBlue;
         lpPal->palPalEntry[i].peFlags = 0;
      }
      hPal = CreatePalette (lpPal);
      GlobalUnlock (hLogPal);
      GlobalFree   (hLogPal);
   }
   return hPal;
}


void __stdcall _InternetStatusCallback( HINTERNET hInternet, DWORD dwContext, DWORD dwInternetStatus, LPVOID lpvStatusInformation, DWORD dwStatusInformationLength)
{
   if( pDynSym )
   {
       hb_vmPushSymbol( (HB_SYMB *) pDynSym );
       hb_vmPushNil();
       hb_vmPushLong( (long) hInternet );
       hb_vmPushLong( (long) dwContext );
       hb_vmPushLong( (long) dwInternetStatus );

       if( dwInternetStatus == INTERNET_STATUS_RESPONSE_RECEIVED )
       {
          hb_vmPushLong( *((LPDWORD) lpvStatusInformation) );
       }
       else
       {
          hb_vmPushLong( (LONG) lpvStatusInformation );
       }

       hb_vmPushLong( (long) dwStatusInformationLength );
       hb_vmDo( 5 );
   }
}


HB_FUNC ( _INTERNETSETSTATUSCALLBACK )
{
   pDynSym = (PHB_DYNS) hb_parptr(2);
   hb_retnl( (long) InternetSetStatusCallback( (HINTERNET) hb_parnl(1), (INTERNET_STATUS_CALLBACK) _InternetStatusCallback ) );
}

HB_FUNC( _BMPREPLACECOLOR )
{
   HBITMAP hBmp = (HBITMAP) hb_parnl(1);
   COLORREF cOldColor = (COLORREF) hb_parnl(2);
   COLORREF cNewColor = (COLORREF) hb_parnl(3);
   HDC hBmpDC = (HDC) hb_parnl(4);
   HBITMAP RetBmp=NULL;

   if (hBmp)
   {
      HDC BufferDC=CreateCompatibleDC(NULL);

      if (BufferDC)
      {
         HBITMAP hTmpBitmap = (HBITMAP) NULL;
         HGDIOBJ PreviousBufferObject;
         HDC DirectDC;

         if( hBmpDC )
         {
            if (hBmp == (HBITMAP)GetCurrentObject(hBmpDC, OBJ_BITMAP))
            {
               hTmpBitmap = CreateBitmap(1, 1, 1, 1, NULL);
               SelectObject(hBmpDC, hTmpBitmap);
            }
         }

         PreviousBufferObject = SelectObject(BufferDC,hBmp);
         DirectDC = CreateCompatibleDC(NULL);

         if (DirectDC)
         {
            BITMAP bm;
            BITMAPINFO bmi;
            HBITMAP DirectBitmap;
            UINT *ptPixels;

            GetObject(hBmp, sizeof(bm), &bm);

            ZeroMemory(&bmi,sizeof(BITMAPINFO));
            bmi.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
            bmi.bmiHeader.biWidth=bm.bmWidth;
            bmi.bmiHeader.biHeight=bm.bmHeight;
            bmi.bmiHeader.biPlanes=1;
            bmi.bmiHeader.biBitCount=32;

            DirectBitmap= CreateDIBSection(DirectDC, (BITMAPINFO *)&bmi, DIB_RGB_COLORS,(void **)&ptPixels, NULL, 0);

            if (DirectBitmap)
            {
               HGDIOBJ PreviousObject=SelectObject(DirectDC, DirectBitmap);
               int i;

               BitBlt(DirectDC,0,0,bm.bmWidth,bm.bmHeight,BufferDC,0,0,SRCCOPY);
               cOldColor=COLORREF2RGB(cOldColor);
               cNewColor=COLORREF2RGB(cNewColor);

               for( i = ( ( bm.bmWidth * bm.bmHeight ) - 1 ); i >= 0; i-- )
               {
                  if (ptPixels[i]==cOldColor)
                  {
                     ptPixels[i]=cNewColor;
                  }
               }

               SelectObject(DirectDC,PreviousObject);

               RetBmp=DirectBitmap;
            }

            DeleteDC(DirectDC);
         }

         if (hTmpBitmap)
         {
            SelectObject(hBmpDC, hBmp);
            DeleteObject(hTmpBitmap);
         }

         SelectObject(BufferDC,PreviousBufferObject);
         DeleteDC(BufferDC);
      }
   }

   hb_retnl( (long) RetBmp );
}

HB_FUNC( PICTUREDISPLAYBLACKANDWHITE )
{
   HDC hMemDC = (HDC) hb_parnl(1);
   HBITMAP m_hBitmap = (HBITMAP) hb_parnl(2);
   int m_nWidth = hb_parni(3);
   int m_nHeight = hb_parni(4);

   BITMAPINFO bi;
   BOOL bRes;
   char *buf;

   long nCount=0;

   int i;

   // Bitmap header
   bi.bmiHeader.biSize = sizeof(bi.bmiHeader);
   bi.bmiHeader.biWidth = m_nWidth;
   bi.bmiHeader.biHeight = m_nHeight;
   bi.bmiHeader.biPlanes = 1;
   bi.bmiHeader.biBitCount = 32;
   bi.bmiHeader.biCompression = BI_RGB;
   bi.bmiHeader.biSizeImage = m_nWidth * 4 * m_nHeight;
   bi.bmiHeader.biClrUsed = 0;
   bi.bmiHeader.biClrImportant = 0;

   buf = (char *) malloc(m_nWidth * 4 * m_nHeight);
   bRes = GetDIBits(hMemDC, m_hBitmap, 0, m_nHeight, buf, &bi, DIB_RGB_COLORS);

   for( i = 0; i < m_nHeight; ++i )
   {
      int j;

      for( j = 0; j < m_nWidth; ++j )
      {
         long lVal=0;
         int b;
         int g;
         int r;

         memcpy(&lVal, &buf[nCount], 4);

         // Get the color value from buffer
         b = GetRValue(lVal);
         g = GetGValue(lVal);
         r = GetBValue(lVal);

         // get the average color value
         lVal = (r+g+b)/3;

         // assign to RGB color
         lVal = RGB(lVal, lVal, lVal);
         memcpy(&buf[nCount], &lVal, 4);

         nCount+=4;
      }
   }
   SetDIBits(hMemDC, m_hBitmap, 0, bRes, buf,  &bi, DIB_RGB_COLORS);
   free(buf);
}

HB_FUNC( PICTUREINVERTCOLORS )
{
   HDC hMemDC = (HDC) hb_parnl(1);
   HBITMAP m_hBitmap = (HBITMAP) hb_parnl(2);
   int m_nWidth = hb_parni(3);
   int m_nHeight = hb_parni(4);

   BITMAPINFO bi;
   BOOL bRes;
   char *buf;

   long nCount=0;
   int i;

   // Bitmap header
   bi.bmiHeader.biSize = sizeof(bi.bmiHeader);
   bi.bmiHeader.biWidth = m_nWidth;
   bi.bmiHeader.biHeight = m_nHeight;
   bi.bmiHeader.biPlanes = 1;
   bi.bmiHeader.biBitCount = 32;
   bi.bmiHeader.biCompression = BI_RGB;
   bi.bmiHeader.biSizeImage = m_nWidth * 4 * m_nHeight;
   bi.bmiHeader.biClrUsed = 0;
   bi.bmiHeader.biClrImportant = 0;

   buf = (char *) malloc(m_nWidth * 4 * m_nHeight);
   bRes = GetDIBits(hMemDC, m_hBitmap, 0, m_nHeight, buf, &bi, DIB_RGB_COLORS);

   for( i = 0; i < m_nHeight; ++i )
   {
       int j;

       for( j = 0; j < m_nWidth; ++j )
       {
           long lVal = 0;
           int b;
           int g;
           int r;

           memcpy( &lVal, &buf[nCount], 4 );

           b = 255-GetRValue(lVal);
           g = 255-GetGValue(lVal);
           r = 255-GetBValue(lVal);

           lVal = RGB(b, g, r);

           memcpy(&buf[nCount], &lVal, 4);
           nCount+=4;
       }
   }

   SetDIBits(hMemDC, m_hBitmap, 0, bRes, buf,  &bi, DIB_RGB_COLORS);
   free(buf);
}
/*
HB_FUNC( GETPIXELPOS )
{
   HDC hMemDC = (HDC) hb_parnl(1);
   COLORREF cColor = (COLORREF) hb_parnl(2);
   int m_nWidth = hb_parni(3);
   int m_nHeight = hb_parni(4);

   for (int i=0; i<m_nHeight; ++i)
   {
      for (int j=0; j<m_nWidth; ++j)
      {

         if( GetPixel( hMemDC, i, j ) == cColor )
         {
            hb_retnl( MAKELPARAM( i, j ) );
            return;
         }
      }
   }
}
*/

HB_FUNC( GETFILESIZE )
{
    const TCHAR *fileName = (const TCHAR*) hb_parc(1);
    BOOL                        fOk;
    WIN32_FILE_ATTRIBUTE_DATA   fileInfo;

    if (NULL == fileName)
    {
       hb_retnl( (long) -1 );
       return;
    }
    fOk = GetFileAttributesEx(fileName, GetFileExInfoStandard, (void*)&fileInfo);
    if (!fOk)
    {
       hb_retnl( (long) -1 );
       return;
    }
    assert(0 == fileInfo.nFileSizeHigh);
    hb_retnl( (long)fileInfo.nFileSizeLow );
}

HB_FUNC( __GETHEADERITEM )
{
    NMHEADER *hdr = (NMHEADER*) hb_parnl(1);
    hb_retni( hdr->iItem );
}

#define BUFSIZE 256

#define PROCESSOR_ARCHITECTURE_PPC              3
#define PROCESSOR_ARCHITECTURE_SHX              4
#define PROCESSOR_ARCHITECTURE_ARM              5
#define PROCESSOR_ARCHITECTURE_AMD64            9
#define PROCESSOR_ARCHITECTURE_UNKNOWN          0xFFFF

#define SM_SERVERR2    89
#define VER_SUITE_STORAGE_SERVER 0x00002000
#define VER_SUITE_COMPUTE_SERVER 0x00004000
#define VER_SUITE_WH_SERVER      0x00008000

#define PRODUCT_BUSINESS                          0x00000006 //Business
#define PRODUCT_BUSINESS_N                        0x00000010 //Business N
#define PRODUCT_CLUSTER_SERVER                    0x00000012 //HPC Edition
#define PRODUCT_DATACENTER_SERVER                 0x00000008 //Server Datacenter (full installation)
#define PRODUCT_DATACENTER_SERVER_CORE            0x0000000C //Server Datacenter (core installation)
#define PRODUCT_DATACENTER_SERVER_CORE_V          0x00000027 //Server Datacenter without Hyper-V (core installation)
#define PRODUCT_DATACENTER_SERVER_V               0x00000025 //Server Datacenter without Hyper-V (full installation)
#define PRODUCT_ENTERPRISE                        0x00000004 //Enterprise
#define PRODUCT_ENTERPRISE_E                      0x00000046 //Enterprise E
#define PRODUCT_ENTERPRISE_N                      0x0000001B //Enterprise N
#define PRODUCT_ENTERPRISE_SERVER                 0x0000000A //Server Enterprise (full installation)
#define PRODUCT_ENTERPRISE_SERVER_CORE            0x0000000E //Server Enterprise (core installation)
#define PRODUCT_ENTERPRISE_SERVER_CORE_V          0x00000029 //Server Enterprise without Hyper-V (core installation)
#define PRODUCT_ENTERPRISE_SERVER_IA64            0x0000000F //Server Enterprise for Itanium-based Systems
#define PRODUCT_ENTERPRISE_SERVER_V               0x00000026 //Server Enterprise without Hyper-V (full installation)
#define PRODUCT_HOME_BASIC                        0x00000002 //Home Basic
#define PRODUCT_HOME_BASIC_E                      0x00000043 //Home Basic E
#define PRODUCT_HOME_BASIC_N                      0x00000005 //Home Basic N
#define PRODUCT_HOME_PREMIUM                      0x00000003 //Home Premium
#define PRODUCT_HOME_PREMIUM_E                    0x00000044 //Home Premium E
#define PRODUCT_HOME_PREMIUM_N                    0x0000001A //Home Premium N
#define PRODUCT_HYPERV                            0x0000002A //Microsoft Hyper-V Server
#define PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT  0x0000001E //Windows Essential Business Server Management Server
#define PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING   0x00000020 //Windows Essential Business Server Messaging Server
#define PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY    0x0000001F //Windows Essential Business Server Security Server
#define PRODUCT_PROFESSIONAL                      0x00000030 //Professional
#define PRODUCT_PROFESSIONAL_E                    0x00000045 //Professional E
#define PRODUCT_PROFESSIONAL_N                    0x00000031 //Professional N
#define PRODUCT_SERVER_FOR_SMALLBUSINESS          0x00000018 //Windows Server 2008 for Windows Essential Server Solutions
#define PRODUCT_SERVER_FOR_SMALLBUSINESS_V        0x00000023 //Windows Server 2008 without Hyper-V for Windows Essential Server Solutions
#define PRODUCT_SERVER_FOUNDATION                 0x00000021 //Server Foundation
#define PRODUCT_SMALLBUSINESS_SERVER              0x00000009 //Windows Small Business Server
#define PRODUCT_STANDARD_SERVER                   0x00000007 //Server Standard (full installation)
#define PRODUCT_STANDARD_SERVER_CORE              0x0000000D //Server Standard (core installation)
#define PRODUCT_STANDARD_SERVER_CORE_V            0x00000028 //Server Standard without Hyper-V (core installation)
#define PRODUCT_STANDARD_SERVER_V                 0x00000024 //Server Standard without Hyper-V (full installation)
#define PRODUCT_STARTER                           0x0000000B //Starter
#define PRODUCT_STARTER_E                         0x00000042 //Starter E
#define PRODUCT_STARTER_N                         0x0000002F //Starter N
#define PRODUCT_STORAGE_ENTERPRISE_SERVER         0x00000017 //Storage Server Enterprise
#define PRODUCT_STORAGE_EXPRESS_SERVER            0x00000014 //Storage Server Express
#define PRODUCT_STORAGE_STANDARD_SERVER           0x00000015 //Storage Server Standard
#define PRODUCT_STORAGE_WORKGROUP_SERVER          0x00000016 //Storage Server Workgroup
#define PRODUCT_UNDEFINED                         0x00000000 //An unknown product
#define PRODUCT_ULTIMATE                          0x00000001 //Ultimate
#define PRODUCT_ULTIMATE_E                        0x00000047 //Ultimate E
#define PRODUCT_ULTIMATE_N                        0x0000001C //Ultimate N
#define PRODUCT_WEB_SERVER                        0x00000011 //Web Server (full installation)
#define PRODUCT_WEB_SERVER_CORE                   0x0000001D //Web Server (core installation)


typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);
typedef BOOL (WINAPI *PGPI)(DWORD, DWORD, DWORD, DWORD, PDWORD);

HB_FUNC( GETOSDISPLAYSTRING )
{
   OSVERSIONINFOEX osvi;
   SYSTEM_INFO si;
   PGNSI pGNSI;
   PGPI pGPI;
   DWORD dwType;
   char pszOS[256];
   char cExtra[80];

   pszOS[0] = 0;
   cExtra[0] = 0;

   ZeroMemory(&si, sizeof(SYSTEM_INFO));
   ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));

   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

   if( !( GetVersionEx ((OSVERSIONINFO *) &osvi)) )
   {
      return;
   }

   // Call GetNativeSystemInfo if supported or GetSystemInfo otherwise.

   pGNSI = (PGNSI) GetProcAddress( GetModuleHandle("kernel32.dll"), "GetNativeSystemInfo");

   if(NULL != pGNSI)
   {
      pGNSI(&si);
   }
   else
   {
      GetSystemInfo(&si);
   }

   if ( VER_PLATFORM_WIN32_NT==osvi.dwPlatformId && osvi.dwMajorVersion > 4 )
   {
      char buf[80];

      strcat(pszOS, "Microsoft ");

      // Test for the specific product.

      if ( osvi.dwMajorVersion == 6 )
      {
         if( osvi.dwMinorVersion == 0 )
         {
            if( osvi.wProductType == VER_NT_WORKSTATION )
            {
               strcat(pszOS, "Windows Vista ");
            }
            else
            {
               strcat(pszOS, "Windows Server 2008 " );
            }
         }
         if( osvi.dwMinorVersion == 1 )
         {
            if( osvi.wProductType == VER_NT_WORKSTATION )
            {
               strcat(pszOS, "Windows 7 ");
            }
            else
            {
               strcat(pszOS, "Windows Server 2008 R2" );
            }
         }

         if( osvi.dwMinorVersion == 2 )
         {
            if( osvi.wProductType == VER_NT_WORKSTATION )
            {
               strcat(pszOS, "Windows 8 ");
            }
            else
            {
               strcat(pszOS, "Windows Server 2012" );
            }
         }

         pGPI = (PGPI) GetProcAddress( GetModuleHandle("kernel32.dll"), "GetProductInfo");
         pGPI( 6, 0, 0, 0, &dwType);

         switch( dwType )
         {
            case PRODUCT_BUSINESS:
            case PRODUCT_BUSINESS_N:
               strcat(pszOS, "Business" );
               break;

            case PRODUCT_CLUSTER_SERVER:
               strcat(pszOS, "HPC Edition" );
               break;

            case PRODUCT_DATACENTER_SERVER:
               strcat(pszOS, "Server Datacenter (full installation)" );
               break;

            case PRODUCT_DATACENTER_SERVER_CORE:
               strcat(pszOS, "Server Datacenter (core installation)" );
               break;

            case PRODUCT_DATACENTER_SERVER_CORE_V:
               strcat(pszOS, "Server Datacenter without Hyper-V (core installation)" );
               break;

            case PRODUCT_DATACENTER_SERVER_V:
               strcat(pszOS, "Server Datacenter without Hyper-V (full installation)" );
               break;

            case PRODUCT_ENTERPRISE:
            case PRODUCT_ENTERPRISE_E:
            case PRODUCT_ENTERPRISE_N:
               strcat(pszOS, "Enterprise" );
               break;

            case PRODUCT_ENTERPRISE_SERVER:
               strcat(pszOS, "Server Enterprise (full installation)" );
               break;

            case PRODUCT_ENTERPRISE_SERVER_CORE:
               strcat(pszOS, "Server Enterprise (core installation)" );
               break;

            case PRODUCT_ENTERPRISE_SERVER_CORE_V:
               strcat(pszOS, "Server Enterprise without Hyper-V (core installation)" );
               break;

            case PRODUCT_ENTERPRISE_SERVER_IA64:
               strcat(pszOS, "Server Enterprise for Itanium-based Systems" );
               break;

            case PRODUCT_ENTERPRISE_SERVER_V:
               strcat(pszOS, "Server Enterprise without Hyper-V (full installation)" );
               break;

            case PRODUCT_HOME_BASIC:
            case PRODUCT_HOME_BASIC_E:
            case PRODUCT_HOME_BASIC_N:
               strcat(pszOS, "Home Basic" );
               break;

            case PRODUCT_HOME_PREMIUM:
            case PRODUCT_HOME_PREMIUM_E:
            case PRODUCT_HOME_PREMIUM_N:
               strcat(pszOS, "Home Premium" );
               break;

            case PRODUCT_HYPERV:
               strcat(pszOS, "Hyper-V Server" );
               break;

            case PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT:
               strcat(pszOS, "Essential Business Server Management Server" );
               break;

            case PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING:
               strcat(pszOS, "Essential Business Server Messaging Server" );
               break;

            case PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY:
               strcat(pszOS, "Essential Business Server Security Server" );
               break;

            case PRODUCT_PROFESSIONAL:
            case PRODUCT_PROFESSIONAL_E:
            case PRODUCT_PROFESSIONAL_N:
               strcat(pszOS, "Professional" );
               break;

            case PRODUCT_SERVER_FOR_SMALLBUSINESS:
               strcat(pszOS, "Server 2008 for Windows Essential Server Solutions" );
               break;

            case PRODUCT_SERVER_FOR_SMALLBUSINESS_V:
               strcat(pszOS, "Server 2008 without Hyper-V for Windows Essential Server Solutions" );
               break;

            case PRODUCT_SERVER_FOUNDATION:
               strcat(pszOS, "Server Foundation" );
               break;

            case PRODUCT_SMALLBUSINESS_SERVER:
               strcat(pszOS, "Small Business Server" );
               break;

            case PRODUCT_STANDARD_SERVER:
               strcat(pszOS, "Server Standard (full installation)" );
               break;

            case PRODUCT_STANDARD_SERVER_CORE:
               strcat(pszOS, "Server Standard (core installation)" );
               break;

            case PRODUCT_STANDARD_SERVER_CORE_V:
               strcat(pszOS, "Server Standard without Hyper-V (core installation)" );
               break;

            case PRODUCT_STANDARD_SERVER_V:
               strcat(pszOS, "Server Standard without Hyper-V (full installation)" );
               break;

            case PRODUCT_STARTER:
            case PRODUCT_STARTER_E:
            case PRODUCT_STARTER_N:
               strcat(pszOS, "Starter" );
               break;

            case PRODUCT_STORAGE_ENTERPRISE_SERVER:
               strcat(pszOS, "Storage Server Enterprise" );
               break;

            case PRODUCT_STORAGE_EXPRESS_SERVER:
               strcat(pszOS, "Storage Server Express" );
               break;

            case PRODUCT_STORAGE_STANDARD_SERVER:
               strcat(pszOS, "Storage Server Standard" );
               break;

            case PRODUCT_STORAGE_WORKGROUP_SERVER:
               strcat(pszOS, "Storage Server Workgroup" );
               break;

            case PRODUCT_UNDEFINED:
               strcat(pszOS, "An unknown product" );
               break;

            case PRODUCT_ULTIMATE:
            case PRODUCT_ULTIMATE_E:
            case PRODUCT_ULTIMATE_N:
               strcat(pszOS, "Ultimate" );
               break;

            case PRODUCT_WEB_SERVER:
               strcat(pszOS, "Web Server (full installation)" );
               break;

            case PRODUCT_WEB_SERVER_CORE:
               strcat(pszOS, "Web Server (core installation)" );
               break;
         }

         if ( si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64 )
            strcat(pszOS, ", 64-bit" );
         else if (si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_INTEL )
            strcat(pszOS, ", 32-bit");
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 )
      {
         if( GetSystemMetrics(SM_SERVERR2) )
            strcat(pszOS,  "Windows Server 2003 R2, ");
         else if ( osvi.wSuiteMask==VER_SUITE_STORAGE_SERVER )
            strcat(pszOS, "Windows Storage Server 2003");
         else if ( osvi.wSuiteMask==VER_SUITE_WH_SERVER )
            strcat(pszOS,  "Windows Home Server");
         else if( osvi.wProductType == VER_NT_WORKSTATION &&
                  si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64)
         {
            strcat(pszOS, "Windows XP Professional x64 Edition");
         }
         else strcat(pszOS, "Windows Server 2003, ");

         // Test for the server type.
         if ( osvi.wProductType != VER_NT_WORKSTATION )
         {
            if ( si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_IA64 )
            {
                if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                   strcat(pszOS, "Datacenter Edition for Itanium-based Systems" );
                else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                   strcat(pszOS, "Enterprise Edition for Itanium-based Systems" );
            }

            else if ( si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64 )
            {
                if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                   strcat(pszOS, "Datacenter x64 Edition" );
                else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                   strcat(pszOS, "Enterprise x64 Edition" );
                else strcat(pszOS, "Standard x64 Edition" );
            }

            else
            {
                if ( osvi.wSuiteMask & VER_SUITE_COMPUTE_SERVER )
                   strcat(pszOS, "Compute Cluster Edition" );
                else if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
                   strcat(pszOS, "Datacenter Edition" );
                else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
                   strcat(pszOS, "Enterprise Edition" );
                else if ( osvi.wSuiteMask & VER_SUITE_BLADE )
                   strcat(pszOS, "Web Edition" );
                else strcat(pszOS, "Standard Edition" );
            }
         }
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
      {
         strcat(pszOS, "Windows XP ");
         if( osvi.wSuiteMask & VER_SUITE_PERSONAL )
            strcat(pszOS, "Home Edition" );
         else strcat(pszOS, "Professional" );
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
      {
         strcat(pszOS, "Windows 2000 ");

         if ( osvi.wProductType == VER_NT_WORKSTATION )
         {
            strcat(pszOS, "Professional" );
         }
         else
         {
            if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
               strcat(pszOS, "Datacenter Server" );
            else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
               strcat(pszOS, "Advanced Server" );
            else strcat(pszOS, "Server" );
         }
      }

       // Include service pack (if any) and build number.

      if( _tcslen(osvi.szCSDVersion) > 0 )
      {
          strcat(pszOS, " " );
          strcat(pszOS, osvi.szCSDVersion);
      }

      wsprintf( buf, " (build %d)", osvi.dwBuildNumber);
      strcat(pszOS, buf);

      hb_retc( pszOS );
   }

   else
   {
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
      {
         if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0)
         {
            strcat( pszOS,"Windows 95 ");
            if ( osvi.szCSDVersion[1] == 'C' || osvi.szCSDVersion[1] == 'B' )
               strcat( pszOS, "OSR2 " );
         }

         if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10)
         {
            strcat( pszOS,"Windows 98 ");
            if ( osvi.szCSDVersion[1] == 'A' )
               strcat( pszOS, "SE " );
         }

         if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90)
         {
            strcat( pszOS, "Windows Millennium Edition " );
         }
      }

      // Display version, service pack (if any), and build number.

      if ( osvi.dwMajorVersion <= 4 )
      {
         wsprintf( cExtra,"version %d.%d %s (Build %d)",
                   osvi.dwMajorVersion,
                   osvi.dwMinorVersion,
                   osvi.szCSDVersion,
                   osvi.dwBuildNumber & 0xFFFF);
      }
      else
      {
         wsprintf( cExtra,"%s (Build %d)",
                   osvi.szCSDVersion,
                   osvi.dwBuildNumber & 0xFFFF);
      }
      strcat( pszOS, cExtra );

      hb_retc( pszOS );
   }
}
/*
HB_FUNC( VXH_SETSCROLLINFO )
{
   LPCSCROLLINFO sbi;
   SetScrollInfo( (HWND) hb_parnl(1), hb_parni(2), sbi, .T. )
}
*/

typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

LPFN_ISWOW64PROCESS fnIsWow64Process;

HB_FUNC( IS64 )
{
    BOOL bIsWow64 = FALSE;
    fnIsWow64Process = (LPFN_ISWOW64PROCESS) GetProcAddress( GetModuleHandle( TEXT("kernel32") ), "IsWow64Process");
    if (NULL != fnIsWow64Process)
    {
        if( !fnIsWow64Process( GetCurrentProcess(), &bIsWow64 ) )
        {
        }
    }
    hb_retl( bIsWow64 );
}

HB_FUNC( SETSYSTEXTCOLOR )
{
   int aElements[1] = {COLOR_HIGHLIGHTTEXT};
   DWORD aNewColors[1];

   aNewColors[0] = (COLORREF) hb_parnl(1);
   hb_retnl( (long) GetSysColor( aElements[0] ) );
   SetSysColors(1, aElements, aNewColors);
}


HB_FUNC( __SETTBBTTNTTT )
{
   NMTTDISPINFO* di = (NMTTDISPINFO*) hb_parnl(1);
   di->lpszText = (char *) hb_parc(2);
}


//---------------------------------------------------------------------------//
HB_FUNC( EDITSETCUEBANNERTEXT )
{
   const char *cString = hb_parc(3);
   BSTR wString = hb_oleAnsiToWide( cString );
   if( wString )
   {
      SendMessage( (HWND) hb_parnl(1), EM_SETCUEBANNER, (WPARAM) hb_parl(2), (LPARAM) wString );
   }
}


HB_FUNC( __GETBKARRAY )
{
   HB_ITEM_NEW( Array );
   HB_ITEM_NEW( Item );
   int x, y;
   HDC hdc = (HDC) hb_parnl(1);
   HDC hdcmem = (HDC) hb_parnl(5);
   COLORREF cBack = (COLORREF) hb_parnl(4);
   COLORREF cColor;

   hb_arrayNew( &Array, 0 );
   for (y=0; y < hb_parni(3); y++)
   {
      for (x=0; x < hb_parni(2); x++)
      {
          cColor = GetPixel( hdc, x, y );
          if( cColor == cBack )
          {
             hb_arrayNew( &Item, 3 );
             hb_itemPutNI( hb_arrayGetItemPtr( &Item, 1 ), x );
             hb_itemPutNI( hb_arrayGetItemPtr( &Item, 2 ), y );
             hb_itemPutNL( hb_arrayGetItemPtr( &Item, 3 ), (long) GetPixel( hdcmem, x, y ) );
             hb_arrayAddForward( &Array, &Item );
          }
      }
   }
   hb_itemReturnForward( &Array );
}

HB_FUNC( __SETBKTRANSPARENT )
{
   int x, y;
   HDC hDC = (HDC) hb_parnl(1);
   int w = hb_parni(2);
   int h = hb_parni(3);
   COLORREF nFill  = (COLORREF) hb_parnl(4);
   COLORREF nTrans = (COLORREF) hb_parnl(5);
   for (y=0; y < h; y++)
   {
      for (x=0; x < w; x++)
      {
         if( GetPixel( hDC, x, y ) == nTrans )
         {
            SetPixel( hDC, x, y, nFill );
         }
      }
   }
}


HB_FUNC( __SETBKARRAY )
{
   HDC hdc = (HDC) hb_parnl(1);
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pSubArray;
   int iLen = pArray->item.asArray.value->ulLen;
   int i;

   for ( i = 0; i < iLen; i++ )
   {
      pSubArray = hb_arrayGetItemPtr( pArray, i + 1 );
      SetPixel( hdc, hb_arrayGetNI( pSubArray, 1 ), hb_arrayGetNI( pSubArray, 2 ), hb_arrayGetNI( pSubArray, 3 ) );
   }
}
/*
HB_FUNC( FADECOLOR )
{
   const unsigned int aClrSrc = hb_parni(1)
   unsigned int aClrDest
   const int aPercent
   register int rr,gg,bb,dr,dg,db,r1,g1,b1,r2,g2,b2;
   register int f1,f2,f3;

   r1 = (aClrSrc)&0xff;
   g1 = ((aClrSrc)>>8)&0xff;
   b1 = ((aClrSrc)>>16)&0xff;


   r2 = (aClrDest)&0xff;
   g2 = ((aClrDest)>>8)&0xff;
   b2 = ((aClrDest)>>16)&0xff;

   rr = r1 - r2;
   gg = g1 - g2;
   bb = b1 - b2;
    //aPercent = 100 means fully converted to aClrSrc;
   f1 = (rr*aPercent)/100;
   f2 = (gg*aPercent)/100;
   f3 = (bb*aPercent)/100;

   dr = r1 - f1;
   dg = g1 - f2;
   db = b1 - f3;
   aClrDest = RGB(dr,dg,db);
}
*/
HB_FUNC( LISTBOXGETSELITEMS )
{
   HWND hWnd   = (HWND) hb_parni(1);
   WORD wItems = SendMessage( hWnd, LB_GETSELCOUNT, 0, 0 );
   int * pBuffer;
   WORD w;

   if( wItems > 0 )
   {
      pBuffer = (int *) hb_xgrab( sizeof( int ) * wItems );
      SendMessage( hWnd, LB_GETSELITEMS, wItems, (LONG) pBuffer );
   }
   else
   {
      pBuffer = NULL;
   }

   hb_reta( wItems );
   for( w = 0; w < wItems; w++ )
      hb_storni( pBuffer[ w ] + 1, -1, w + 1 );

   if( wItems > 0 )
      hb_xfree( ( void * ) pBuffer );
}

HB_FUNC( __GETTREEVIEWOLDITEM )
{
   NMTREEVIEW* tv = (NMTREEVIEW*) hb_parnl(1);
   hb_retnl( (long) tv->itemOld.hItem );
}

HB_FUNC( __GETTREEVIEWNEWITEM )
{
   NMTREEVIEW* tv = (NMTREEVIEW*) hb_parnl(1);
   hb_retnl( (long) tv->itemNew.hItem );
}

HB_FUNC( __FINDPIXELCOLOR )
{
   int x, y;
   COLORREF cPixel;
   HB_ITEM_NEW( Array );
   HDC hdc = (HDC) hb_parnl(1);
   COLORREF cColor = (COLORREF) hb_parnl(2);
   hb_arrayNew( &Array, 2 );
   for (y=0; y < hb_parni(4); y++)
   {
      for (x=0; x < hb_parni(3); x++)
      {
          cPixel = GetPixel( hdc, x, y );
          if( cColor == cPixel )
          {
             hb_itemPutNI( hb_arrayGetItemPtr( &Array, 1 ), x );
             hb_itemPutNI( hb_arrayGetItemPtr( &Array, 2 ), y );
             hb_itemReturnForward( &Array );
             return;
          }
      }
   }
}

HB_FUNC( __RESOURCETOSTRING )
{
   HINSTANCE hInstance = (HINSTANCE) hb_parnl(1);
   HRSRC hRes = FindResource( hInstance, (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3) );
   if( hRes )
   {
      LONG size = SizeofResource( hInstance, hRes );
      HANDLE pt = LoadResource( hInstance, hRes );
      if( pt )
      {
         LPSTR pResult = (LPSTR) hb_xgrab( size+1 );
         memcpy( pResult, pt, size );
         hb_retclen( pResult, size );
         hb_xfree( pResult );
      }
   }
}

//------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( __RESICONTOSTRING )
{
   HINSTANCE hInst = (HINSTANCE) hb_parnl(1);
   HICON hIco = (HICON) LoadImage( hInst, (LPCTSTR) hb_parc(2), IMAGE_ICON, 0, 0, 0 );
   DWORD dwSize = GlobalSize(hIco);
   LPSTR lpGMem = (LPSTR) GlobalLock(hIco);

   LPSTR pResult = (LPSTR) hb_xgrab( dwSize+1 );
   memcpy( pResult, lpGMem, dwSize );

   hb_retclen( pResult, dwSize );
   hb_xfree( pResult );

   GlobalUnlock(hIco);
}

/*
HB_FUNC( RESTOCHAR )
{
   HINSTANCE hInst = (HINSTANCE) hb_parnl(1);
   HRSRC res = FindResource( hInst, (LPCTSTR) hb_parc(2), MAKEINTRESOURCE( hb_parnl(3) ) );
   if(res)
   {
      HGLOBAL mem = LoadResource( hInst, res);
      void *data = LockResource(mem);
      size_t sz = SizeofResource( hInst, res);

      hb_retclen( (char*) data, (long) sz );

      UnlockResource(mem);
      FreeResource(mem);
   }
}
*/
/*
static public Bitmap IconToAlphaBitmap(Icon ico)
{
ICONINFO ii = new ICONINFO();
GetIconInfo(ico.Handle, out ii);
Bitmap bmp = Bitmap.FromHbitmap(ii.hbmColor);
DestroyIcon(ii.hbmColor);
DestroyIcon(ii.hbmMask);

if (Bitmap.GetPixelFormatSize(bmp.PixelFormat) < 32)
return ico.ToBitmap();

BitmapData bmData;
Rectangle bmBounds = new Rectangle(0,0,bmp.Width,bmp.Height);

bmData = bmp.LockBits(bmBounds,ImageLockMode.ReadOnly, bmp.PixelFormat);

Bitmap dstBitmap=new Bitmap(bmData.Width, bmData.Height, bmData.Stride, PixelFormat.Format32bppArgb, bmData.Scan0);

bool IsAlphaBitmap = false;

for (int y=0; y <= bmData.Height-1; y++)
{
for (int x=0; x <= bmData.Width-1; x++)
{
Color PixelColor = Color.FromArgb(Marshal.ReadInt32(bmData.Scan0, (bmData.Stride * y) + (4 * x)));
if (PixelColor.A > 0 & PixelColor.A < 255)
{
IsAlphaBitmap = true;
break;
}
}
if (IsAlphaBitmap) break;
}

bmp.UnlockBits(bmData);

if (IsAlphaBitmap==true)
return new Bitmap(dstBitmap);
else
return new Bitmap(ico.ToBitmap());
}
}
*/

/*
   hDC := GetDC(0)
   IF ! RectVisible( hDC, {oWnd:xLeft, oWnd:xTop, ::xLeft + ::xWidth, ::xTop + ::xHeight } )
      ::CenterWindow()
   ENDIF
   ReleaseDC( 0, hDC )

#pragma BEGINDUMP
   #include "windows.h"

   extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );

   HB_FUNC( RECTVISIBLE )
   {
      RECT rc;
      Array2Rect( hb_param(2,HB_IT_ARRAY), &rc );
      hb_retl( RectVisible( (HDC) hb_parnl( 1 ), &rc ) );
   }

#pragma ENDDUMP
*/

HB_FUNC( DBGN )
{
   char buf[256];
   wsprintf( buf, "(  %d)", hb_parnl(1) );
   OutputDebugString( buf );
}

HB_FUNC( TVDISPINFOIMAGE )
{
   LPNMTVDISPINFO lptvdi = (LPNMTVDISPINFO) hb_parnl(1);

   if( lptvdi->item.mask & TVIF_IMAGE )
   {
      lptvdi->item.iImage = hb_parni(2)-1;
   }

   if( lptvdi->item.mask & TVIF_SELECTEDIMAGE )
   {
      lptvdi->item.iSelectedImage  = hb_parni(2)-1;
   }
}

HB_FUNC( TVDISPINFOGETHITEM )
{
   LPNMTVDISPINFO lptvdi = (LPNMTVDISPINFO) hb_parnl(1);
   hb_retnl( (long) lptvdi->item.hItem );
}


HB_FUNC( SET_NCCALCSIZE_PARAMS )
{
   NCCALCSIZE_PARAMS *lpncsp = (NCCALCSIZE_PARAMS*) hb_parnl(1);
   lpncsp->rgrc[0].top   += 2;
   lpncsp->rgrc[0].left  += 1;
   lpncsp->rgrc[0].right -= 1;
}

HB_FUNC( SET_CONTROL_NCCALCSIZE_PARAMS )
{
   NCCALCSIZE_PARAMS *lpncsp = (NCCALCSIZE_PARAMS*) hb_parnl(1);
   lpncsp->rgrc[0].left  += hb_parni(2);
   lpncsp->rgrc[0].top   += hb_parni(3);
}

HB_FUNC( SET_WINDOWPOS )
{
   int x = hb_parni(2);
   WINDOWPOS *lpwp = (WINDOWPOS*) hb_parnl(1);
   if( x > 0 )
      lpwp->x  = x;
   lpwp->cx -= hb_parni(3);
   lpwp->cy -= hb_parni(4);
}

HB_FUNC( TOOLSTRIP_NCCALCSIZE_PARAMS )
{
   NCCALCSIZE_PARAMS *lpncsp = (NCCALCSIZE_PARAMS*) hb_parnl(1);
   lpncsp->rgrc[0].left   += 3;
   lpncsp->rgrc[0].top    += 20;
   lpncsp->rgrc[0].right  -= 3;
   lpncsp->rgrc[0].bottom -= 3;
}

HB_FUNC( __GETMSG )
{
   MSG *ms = (MSG*) hb_parnl(1);
   HB_ITEM_NEW( aParams );

   hb_arrayNew( &aParams, 4 );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 1 ), (long) ms->hwnd );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 2 ), (long) ms->message );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 3 ), (long) ms->wParam );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 4 ), (long) ms->lParam );

   hb_itemReturnForward( &aParams );
}

HB_FUNC( __GETNMHDR )
{
   NMHDR *hdr  = (NMHDR*) hb_parnl(1);
   hb_stornl( (long) hdr->hwndFrom, 2 );
   hb_stornl( (long) hdr->idFrom, 3 );
   hb_stornl( (long) hdr->code, 4 );
}

HB_FUNC( __GETWINDOWPOS )
{
   WINDOWPOS *wp = (WINDOWPOS*) hb_parnl(1);
   HB_ITEM_NEW( aParams );

   hb_arrayNew( &aParams, 7 );

   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 1 ), (long) wp->hwnd );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 2 ), (long) wp->hwndInsertAfter );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 3 ), (long) wp->x );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 4 ), (long) wp->y );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 5 ), (long) wp->cx );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 6 ), (long) wp->cy );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 7 ), (long) wp->flags );
   hb_itemReturnForward( &aParams );
}

HB_FUNC( __GETMEASUREITEMSTRUCT )
{
   MEASUREITEMSTRUCT *mi = (MEASUREITEMSTRUCT*) hb_parnl(1);
   HB_ITEM_NEW( aParams );

   hb_arrayNew( &aParams, 6 );

   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 1 ), (long) mi->CtlType );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 2 ), (long) mi->CtlID );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 3 ), (long) mi->itemID );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 4 ), (long) mi->itemWidth );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 5 ), (long) mi->itemHeight );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 6 ), (long) mi->itemData );
   hb_itemReturnForward( &aParams );
}

HB_FUNC( KEYMENUHOOKCWP )
{
   CWPSTRUCT *cwp = (CWPSTRUCT*) hb_parnl(1);
   hb_stornl( (long) cwp->hwnd, 2 );
   hb_stornl( (long) cwp->message, 3 );
   hb_stornl( (long) cwp->wParam, 4 );
   hb_stornl( (long) cwp->lParam, 5 );
}

HB_FUNC( MENUITEMINFOITEMDATA )
{
   MENUITEMINFO mii;
   mii.cbSize = sizeof( MENUITEMINFO );
   mii.fMask  = MIIM_DATA | MIIM_STATE | MIIM_ID;
   if( GetMenuItemInfo( (HMENU) hb_parnl(1), (UINT) hb_parni(2), hb_parl(3), &mii ) )
   {
      hb_retnl( (long) mii.dwItemData );
   }
}

HB_FUNC( __GETMENUITEMINFO )
{
   MENUITEMINFO mii;

   mii.cbSize = sizeof( MENUITEMINFO );
   mii.fMask  = MIIM_DATA | MIIM_STATE | MIIM_ID | MIIM_BITMAP;

   if( GetMenuItemInfo( (HMENU) hb_parnl(1), (UINT) hb_parni(2), hb_parl(3), &mii ) )
   {
      hb_reta( 2 );
      hb_storni( (long) mii.dwItemData,   -1, 1 );
      hb_storni( (long) mii.hbmpItem,     -1, 2 );
   }
}

HB_FUNC( MENUITEMINFOITEMID )
{
   LPMENUITEMINFO mii = NULL;
   mii->cbSize = sizeof( MENUITEMINFO );
   mii->fMask  = MIIM_ID;
   if( GetMenuItemInfo( (HMENU) hb_parnl(1), hb_parni(2), FALSE, mii ) )
   {
      hb_retni( (int) mii->wID );
   }
}

HB_FUNC( __INSERTMENUSTRIPITEM )
{
   MENUITEMINFOA mii;
   memset(&mii, 0, sizeof(MENUITEMINFO));
   mii.cbSize        = sizeof( MENUITEMINFO );
   mii.fType         = MFT_OWNERDRAW;
   mii.hbmpChecked   = 0;
   mii.hbmpUnchecked = 0;
   mii.hbmpItem      = NULL;

   mii.fMask         = (UINT) hb_parnl(4);
   mii.hSubMenu      = (HMENU) hb_parnl(5);
   mii.wID           = (UINT) hb_parnl(6);
   mii.dwTypeData    = (LPTSTR) hb_parc(7);
   mii.dwItemData    = (ULONG) hb_parnl(8);
   mii.fState        = (UINT) hb_parnl(9);
   hb_retl( InsertMenuItem( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), hb_parl(3), (LPCMENUITEMINFO) &mii ) );
}


HB_FUNC( __DRAWITEMSTRUCT )
{
   DRAWITEMSTRUCT *dis = (DRAWITEMSTRUCT*) hb_parnl(1);
   HB_ITEM_NEW( aParams );
   HB_ITEM_NEW( aRect );
   hb_arrayNew( &aParams, 8 );


   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 1 ), (long) dis->CtlType );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 2 ), (long) dis->CtlID );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 3 ), (long) dis->itemID );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 4 ), (long) dis->itemAction );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 5 ), (long) dis->itemState );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 6 ), (long) dis->hwndItem );
   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 7 ), (long) dis->hDC );

   hb_itemPutNL( hb_arrayGetItemPtr( &aParams, 8 ), (long) dis->itemData );

   hb_arrayNew( &aRect, 4 );
   hb_itemPutNI( hb_arrayGetItemPtr( &aRect, 1 ), dis->rcItem.left );
   hb_itemPutNI( hb_arrayGetItemPtr( &aRect, 2 ), dis->rcItem.top );
   hb_itemPutNI( hb_arrayGetItemPtr( &aRect, 3 ), dis->rcItem.right );
   hb_itemPutNI( hb_arrayGetItemPtr( &aRect, 4 ), dis->rcItem.bottom );
   hb_arrayAddForward( &aParams, &aRect );

   hb_itemReturnForward( &aParams );
}


HB_FUNC( __DOEVENTS )
{
   MSG msg;
   long sts;

   do
   {
      if( ( sts = PeekMessage(&msg, (HWND) NULL, 0, 0, PM_REMOVE ) ) != 0 )
      {
         TranslateMessage(&msg);
         DispatchMessage(&msg);
      }
   }
   while( sts );
}

HB_FUNC( EDITGETSEL )
{
   HRESULT lResult;
   int wParam;
   int lParam;
   lResult = SendMessage( (HWND) hb_parnl(1), EM_GETSEL, (WPARAM) &wParam, (LPARAM) &lParam );

   hb_storni( wParam, 2 );
   hb_storni( lParam, 3 );

   hb_retnl( (long) lResult );
}

HB_FUNC( EDITSETSEL )
{
   hb_retnl( (long) SendMessage( (HWND) hb_parnl(1), EM_SETSEL, (WPARAM) hb_parni(2), (LPARAM) hb_parni(3) ) );
}


HB_FUNC( VXH_SETTOOLBARTOOLTIP )
{
   NMTTDISPINFO* ttdi = (NMTTDISPINFO*) hb_parnl(1);
   ttdi->lpszText = (LPSTR) hb_parc(2);
}

HB_FUNC( VXH_SETTOOLBARTOOLTIPW )
{
   NMTTDISPINFOW* ttdi = (NMTTDISPINFOW*) hb_parnl(1);
   ttdi->lpszText = (LPWSTR) hb_oleAnsiToWide( hb_parc(2) );
}

HB_FUNC( SAVEBMP )
{
   const char *filename = (const char *) hb_parc(1);
   HBITMAP hb = (HBITMAP) hb_parnl(2);

   HDC hdc=NULL;
   FILE *file=NULL;
   LPVOID buf=NULL;
   BITMAPINFO bmpInfo;
   BITMAPFILEHEADER bmpFileHeader;
   hdc=GetDC(NULL);
   ZeroMemory(&bmpInfo,sizeof(BITMAPINFO));
   bmpInfo.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
   GetDIBits(hdc,hb,0,0,NULL,&bmpInfo,DIB_RGB_COLORS);
   if(bmpInfo.bmiHeader.biSizeImage <= 0)
          bmpInfo.bmiHeader.biSizeImage=bmpInfo.bmiHeader.biWidth*abs(bmpInfo.bmiHeader.biHeight)*(bmpInfo.bmiHeader.biBitCount+7)/8;
   if((buf = malloc(bmpInfo.bmiHeader.biSizeImage)) == NULL)
          return;
   bmpInfo.bmiHeader.biCompression = BI_RGB;
   GetDIBits(hdc,hb,0,bmpInfo.bmiHeader.biHeight,buf,&bmpInfo,DIB_RGB_COLORS);
   if((file = fopen(filename,"wb")) == NULL)
          return;
   bmpFileHeader.bfReserved1=0;
   bmpFileHeader.bfReserved2=0;
   bmpFileHeader.bfSize=sizeof(BITMAPFILEHEADER)+sizeof(BITMAPINFOHEADER)+bmpInfo.bmiHeader.biSizeImage;
   bmpFileHeader.bfType=19778;
   bmpFileHeader.bfOffBits=sizeof(BITMAPFILEHEADER)+sizeof(BITMAPINFOHEADER);

   fwrite(&bmpFileHeader,sizeof(BITMAPFILEHEADER),1,file);
   fwrite(&bmpInfo.bmiHeader,sizeof(BITMAPINFOHEADER),1,file);
   fwrite(buf,bmpInfo.bmiHeader.biSizeImage,1,file);

   ReleaseDC(0,hdc);
   free(buf);
   fclose(file);
}


HB_FUNC( GETBMPSTRING )
{
   HBITMAP hb = (HBITMAP) hb_parnl(1);

   HDC hdc=NULL;
   LPVOID buf=NULL;
   BITMAPINFO bmpInfo;
   BITMAPFILEHEADER bmpFileHeader;
   hdc=GetDC(NULL);
   ZeroMemory(&bmpInfo,sizeof(BITMAPINFO));
   bmpInfo.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
   GetDIBits(hdc,hb,0,0,NULL,&bmpInfo,DIB_RGB_COLORS);
   if(bmpInfo.bmiHeader.biSizeImage <= 0)
          bmpInfo.bmiHeader.biSizeImage=bmpInfo.bmiHeader.biWidth*abs(bmpInfo.bmiHeader.biHeight)*(bmpInfo.bmiHeader.biBitCount+7)/8;
   if((buf = malloc(bmpInfo.bmiHeader.biSizeImage)) == NULL)
          return;
   bmpInfo.bmiHeader.biCompression = BI_RGB;
   GetDIBits(hdc,hb,0,bmpInfo.bmiHeader.biHeight,buf,&bmpInfo,DIB_RGB_COLORS);

   bmpFileHeader.bfReserved1=0;
   bmpFileHeader.bfReserved2=0;
   bmpFileHeader.bfSize=sizeof(BITMAPFILEHEADER)+sizeof(BITMAPINFOHEADER)+bmpInfo.bmiHeader.biSizeImage;
   bmpFileHeader.bfType=19778;
   bmpFileHeader.bfOffBits=sizeof(BITMAPFILEHEADER)+sizeof(BITMAPINFOHEADER);

   hb_storclen( (char*) &bmpFileHeader, sizeof(BITMAPFILEHEADER), 2 );
   hb_storclen( (char*) &bmpInfo.bmiHeader, sizeof(BITMAPINFOHEADER), 3 );
   hb_storclen( (char*) buf, bmpInfo.bmiHeader.biSizeImage, 4 );
   ReleaseDC(0,hdc);
   free(buf);
}
/*
HB_FUNC( HBITMAPFROMBUFFER )
{
   BITMAPINFOHEADER *bmih = (BITMAPINFOHEADER *) hb_param( 1, HB_IT_STRING )->item.asString.value ;
   BITMAPINFO *bmi  = (BITMAPINFO *) hb_param( 2, HB_IT_STRING)->item.asString.value ;
   HDC hdc = GetDC(NULL);
   hb_retnl( (LONG) CreateDIBitmap( hdc, bmih, CBM_INIT, (VOID *) hb_parcx(3), bmi, DIB_RGB_COLORS ) );
   ReleaseDC(0,hdc);
}
*/

HB_FUNC( __SETSCROLLINFO )
{
   SCROLLINFO si;
   si.cbSize = sizeof( SCROLLINFO );
   si.nMin   = hb_parni(3);
   si.nMax   = hb_parni(4);
   si.nPage  = (UINT) hb_parnl(5);
   si.nPos   = hb_parni(6);
   si.fMask  = hb_parni(7);
   SetScrollInfo( (HWND) hb_parnl(1), hb_parni(2), &si, hb_parl(8) );
}

HB_FUNC( ENUMREGDLL )
{
   CLSID clsid;
   ICatInformation *pCatInfo;
   IEnumGUID *pEnumGUID=NULL;
   CATID pcatidImpl[1];
   CATID pcatidReqd[1];
   HB_ITEM_NEW( Array );
   HB_ITEM_NEW( Item );

   hb_arrayNew( &Array, 0 );

   CoInitialize (NULL);
   CoCreateInstance ( &CLSID_StdComponentCategoriesMgr, NULL, CLSCTX_INPROC_SERVER, &IID_ICatInformation, (void **)&pCatInfo);
   pCatInfo->lpVtbl->AddRef( pCatInfo );

   pcatidImpl[0]=CATID_Control;
   pCatInfo->lpVtbl->EnumClassesOfCategories( pCatInfo, 1, pcatidImpl, 0, pcatidReqd, &pEnumGUID );

   while( ( pEnumGUID->lpVtbl->Next( pEnumGUID, 1, &clsid, NULL ))==S_OK )
   {
      char *cClass;
      BSTR bstrClassName;
      OleRegGetUserType (&clsid,USERCLASSTYPE_FULL,&bstrClassName);
      cClass = hb_oleWideToAnsi(bstrClassName);

      if( cClass != NULL )
      {
         HKEY hKey = NULL;
         LPOLESTR wProgID = NULL;
         LPOLESTR wClsID = NULL;
         char *cProgID;
         char *cClsID;
         char cReg[512];
         ProgIDFromCLSID( &clsid, &wProgID );
         StringFromCLSID( &clsid, &wClsID );

         cProgID = hb_oleWideToAnsi(wProgID);
         cClsID = hb_oleWideToAnsi(wClsID);

         if( cProgID )
         {
            TCHAR cPath[MAX_PATH];
            TCHAR cBmp[MAX_PATH];
            TCHAR cVer[MAX_PATH];
            __strcpy( cReg, "SOFTWARE\\Classes\\CLSID\\" );
            __strcat( cReg, cClsID );
            __strcat( cReg, "\\InprocServer32" );

            RegOpenKey( HKEY_LOCAL_MACHINE, cReg, &hKey);
            if( hKey )
            {
               DWORD dwSize=sizeof(cPath);
               RegQueryValueEx(hKey, NULL,NULL,NULL,(LPBYTE)cPath,&dwSize);
               RegCloseKey(hKey);
            }

            __strcpy( cReg, "SOFTWARE\\Classes\\CLSID\\" );
            __strcat( cReg, cClsID );
            __strcat( cReg, "\\ToolboxBitmap32" );

            RegOpenKey( HKEY_LOCAL_MACHINE, cReg, &hKey);
            if( hKey )
            {
               DWORD dwSize=sizeof(cBmp);
               RegQueryValueEx(hKey, NULL,NULL,NULL,(LPBYTE)cBmp,&dwSize);
               RegCloseKey(hKey);
            }

            __strcpy( cReg, "SOFTWARE\\Classes\\CLSID\\" );
            __strcat( cReg, cClsID );
            __strcat( cReg, "\\Version" );

            RegOpenKey( HKEY_LOCAL_MACHINE, cReg, &hKey);
            if( hKey )
            {
               DWORD dwSize=sizeof(cVer);
               RegQueryValueEx(hKey, NULL,NULL,NULL,(LPBYTE)cVer,&dwSize);
               RegCloseKey(hKey);
            }
            hb_arrayNew( &Item, 7 );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 1 ), cClass );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 2 ), cProgID );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 3 ), cClsID );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 4 ), cBmp );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 5 ), cVer );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 6 ), cPath );
            hb_itemPutC( hb_arrayGetItemPtr( &Item, 7 ), cReg );
            hb_arrayAddForward( &Array, &Item );
         }
      }
   }

   hb_itemReturnForward( &Array );

   pCatInfo->lpVtbl->Release(pCatInfo);
   CoUninitialize ();
}


HB_FUNC( GETOLEICON )
{
   char cReg[512];
   __strcpy( cReg, "SOFTWARE\\Classes\\CLSID\\" );
   __strcat( cReg, hb_parc(1) );
   __strcat( cReg, "\\ToolboxBitmap32" );

   HKEY hKey = NULL;
   RegOpenKey( HKEY_LOCAL_MACHINE, cReg, &hKey);
   if( hKey )
   {
      TCHAR cBmp[MAX_PATH];
      DWORD dwSize=sizeof(cBmp);
      RegQueryValueEx(hKey, NULL,NULL,NULL,(LPBYTE)cBmp,&dwSize);
      RegCloseKey(hKey);
      hb_retc( cBmp );
   }
}

typedef struct XCCLVTILEINFO {
   UINT  cbSize;
   int   iItem;
   UINT  cColumns;
   PUINT puColumns;
   #if (_WIN32_WINNT >= 0x0600)
     int   *piColFmt;
   #endif
} XCCLVTILEINFO, *PXCCLVTILEINFO;

//#define LVM_SETVIEW             (LVM_FIRST + 142)
//#define LVM_SETTILEINFO         (LVM_FIRST + 164)

HB_FUNC( __LISTVIEWSETVIEW )
{
   PHB_ITEM aParam;
   XCCLVTILEINFO lvti;
   lvti.cbSize = sizeof( XCCLVTILEINFO );
   lvti.iItem = hb_parni(2);
   lvti.cColumns = (UINT) hb_parni(3);

   if (ISARRAY( 4 ) )
   {
      int iCount, i;
      PUINT puColumn;

      iCount = (int) hb_parinfa( 4, 0 );
      puColumn = (UINT*) hb_xgrab( iCount * sizeof(UINT) );
      aParam = hb_param(4,HB_IT_ARRAY);

      for ( i = 0 ; i<iCount ; i++ )
      {
          *(puColumn+i) = *(UINT*) hb_itemArrayGet( aParam, i+1 );
      }
      lvti.puColumns = puColumn;

      SendMessage( (HWND) hb_parnl(1), LVM_SETTILEINFO, 0, (LPARAM) &lvti );
      hb_xfree(puColumn);
   }
}


HB_FUNC( SETFILETIME )
{
   FILETIME ft;
   FILETIME lft;
   SYSTEMTIME st;
   HANDLE hFile ;

   char pTime[6];
   char pDate[9];

   char szYear[5];
   char szMonth[3];
   char szDay[3];

   char szHour[3];
   char szMinute[3];

   ZeroMemory( &st, sizeof( SYSTEMTIME ) );
   hFile = CreateFile( hb_parc(1), GENERIC_READ | GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

   if (hFile == INVALID_HANDLE_VALUE)
      hb_retl( FALSE );
   else
   {
      strcpy(pTime, hb_parc(2));
      strcpy(pDate, ISDATE(3) ? hb_pards(3): hb_parc(3) );
      if( pTime )
      {
         strcpy(szMinute, pTime+3);
         szHour[0] = pTime[0]; szHour[1] = pTime[1]; szHour[2] = 0;
         st.wHour  = ( WORD ) atoi(szHour) ;
         st.wMinute= ( WORD ) atoi(szMinute);
      }

      if( pDate )
      {
         szDay[0] = pDate[6]; szDay[1] = pDate[7]; szDay[2] = 0;
         szMonth[0] = pDate[4]; szMonth[1] = pDate[5]; szMonth[2] = 0;
         szYear[0] = pDate[0]; szYear[1] = pDate[1]; szYear[2] = pDate[2]; szYear[3] = pDate[3]; szYear[4] = 0;
         st.wYear  = ( WORD ) atoi(szYear)   ;
         st.wMonth = ( WORD ) atoi(szMonth)  ;
         st.wDay   = ( WORD ) atoi(szDay)    ;
      }

      SystemTimeToFileTime(&st, &ft);
      LocalFileTimeToFileTime(&ft, &lft) ;
      hb_retl( SetFileTime(hFile, &lft, (LPFILETIME) NULL, &lft) );
      CloseHandle( hFile );
   }
}

HB_FUNC( NEXTLINE )
{
   PHB_ITEM pBuffer = hb_param( 1, HB_IT_BYREF );
   PHB_ITEM pLine = hb_param( 2, HB_IT_BYREF );
   unsigned long ulPos = 0;

   if( pBuffer && pLine && HB_IS_STRING( pBuffer ) && pBuffer->item.asString.length )
   {
      hb_retl( TRUE );
      while( ulPos < pBuffer->item.asString.length && ( pBuffer->item.asString.value[ ulPos ] != 10 ) )
      {
         ulPos++;
      }
      if( ulPos == pBuffer->item.asString.length )
      {
         if( pBuffer->item.asString.value[ ulPos - 1 ] == 13 )
         {
            hb_itemPutCL( pLine, pBuffer->item.asString.value, ulPos - 1 );
         }
         else
         {
            hb_itemForwardValue( pLine, pBuffer );
         }
         hb_storc( NULL, 1 );
         return;
      }
      else if( ulPos )
      {
         if( pBuffer->item.asString.value[ ulPos - 1 ] == 13 )
         {
            hb_itemPutCL( pLine, pBuffer->item.asString.value, ulPos - 1 );
         }
         else
         {
            hb_itemPutCL( pLine, pBuffer->item.asString.value, ulPos );
         }
      }
      else
      {
         hb_storc( NULL, 2 );
      }
      hb_itemPutCRaw( pBuffer, hb_strdup( pBuffer->item.asString.value + ulPos + 1 ), pBuffer->item.asString.length - ( ulPos + 1 ) );
   }
   else
   {
      hb_storc( NULL, 1 );
      hb_retl( FALSE );
   }
}

HB_FUNC( SENDEDITORSTRING )
{
   hb_retnl( (long) SendMessage( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), (LPARAM)(LPCTSTR) hb_parc(4) ) );
}

HB_FUNC( SENDEDITOR )
{
   SendMessage( (HWND) hb_parnl(1), hb_parni(2), (WPARAM) hb_parnl(3), (LPARAM)hb_parnl(4) );
}

HB_FUNC( SCISETPROPERTY )
{
   SendMessage( (HWND) hb_parnl(1), SCI_SETPROPERTY, (WPARAM) hb_parc(2), (LPARAM)(LPCTSTR) hb_parc(3) );
}

HB_FUNC( SCIGETFOLD )
{
   SendMessage( (HWND) hb_parnl(1), 4008, (WPARAM) hb_parc(2), (LPARAM)(LPCTSTR) hb_parc(3) );
}

HB_FUNC( SCISETKEYWORDS )
{
   SendMessage( (HWND) hb_parnl(1), 4005, (WPARAM) hb_parni(2), (LPARAM)(LPCTSTR) hb_parc(3) );
}

#define SCI_REGISTERIMAGE 2405

HB_FUNC( SCIREGISTEREVENTIMAGE )
{
   /* XPM */
   static char * Event_xpm[] = {
   "16 16 38 1",
   "  c None",
   ". c #DDCC8E",
   "+ c #E7D089",
   "@ c #E6CF89",
   "# c #DDC582",
   "$ c #C8B278",
   "% c #AC976A",
   "& c #CFC1A1",
   "* c #E7C859",
   "= c #FCE4B0",
   "- c #FFE47F",
   "; c #EEC289",
   "> c #E8A57C",
   ", c #E18782",
   "' c #58534B",
   ") c #FEF2D9",
   "! c #FAD176",
   "~ c #E9AA7E",
   "{ c #6A6248",
   "] c #EAB07F",
   "^ c #887A4A",
   "/ c #F5C06E",
   "( c #A08B46",
   "_ c #F2CC94",
   ": c #EFBD90",
   "< c #E9A081",
   "[ c #5E4730",
   "} c #AE9760",
   "| c #978353",
   "1 c #796C46",
   "2 c #8C825C",
   "3 c #6B4F29",
   "4 c #8C6822",
   "5 c #956F1F",
   "6 c #F0C692",
   "7 c #9F781D",
   "8 c #AD8220",
   "9 c #A79460",
   "                ",
   "                ",
   "       .+@#$%&  ",
   "      *=-;>,'   ",
   "      *)!~,{    ",
   "     *=!],^     ",
   "     *)/,(      ",
   "    *--_:<,[    ",
   "    }|12!,3     ",
   "       !,4      ",
   "      !,5       ",
   "     6,7        ",
   "     :8         ",
   "     9          ",
   "                ",
   "                "};
   SendMessage( (HWND) hb_parnl(1), SCI_REGISTERIMAGE, (WPARAM) hb_parni(2), (LPARAM) Event_xpm );
}

HB_FUNC( SCIREGISTERMETHODIMAGE )
{
   /* XPM */
   static char * Method_xpm[] = {
   "16 16 57 1",
   "  c None",
   ". c #CF88CF",
   "+ c #E3ACE3",
   "@ c #EEBEEE",
   "# c #FAAEFB",
   "$ c #EB91EC",
   "% c #F7F0AB",
   "& c #D5D5AC",
   "* c #ACB4AE",
   "= c #8A99AE",
   "- c #E2AFE2",
   "; c #FAABFB",
   "> c #FA98FB",
   ", c #E990EA",
   "' c #A650A7",
   ") c #BB6FBC",
   "! c #EEA5EF",
   "~ c #FA9BFB",
   "{ c #FA99FB",
   "] c #E698E7",
   "^ c #904E91",
   "/ c #E9E5AB",
   "( c #C1C4AC",
   "_ c #ACB5AD",
   ": c #98A4AE",
   "< c #A74FA8",
   "[ c #D16DD2",
   "} c #FA9FFB",
   "| c #EE9DEF",
   "1 c #965297",
   "2 c #7C2E7C",
   "3 c #A952AA",
   "4 c #D16AD2",
   "5 c #D56FD5",
   "6 c #BC70BD",
   "7 c #DD98DE",
   "8 c #8E468F",
   "9 c #9C3C9D",
   "0 c #7E2E7E",
   "a c #E5E2AB",
   "b c #CCCEAC",
   "c c #B5BBAD",
   "d c #9CA7AE",
   "e c #A850A9",
   "f c #D26BD3",
   "g c #D36ED4",
   "h c #7E307F",
   "i c #9F3C9F",
   "j c #8D358E",
   "k c #7C2D7D",
   "l c #A950AA",
   "m c #CB67CC",
   "n c #953896",
   "o c #7A2B7B",
   "p c #7F3080",
   "q c #752775",
   "r c #712472",
   "                ",
   "                ",
   "          .     ",
   "         .+.    ",
   "        .@#$.   ",
   "  %&*= .-;>>,.  ",
   "       ')!~>{]^ ",
   "%/&(_: <[)!}|12 ",
   "       34567890 ",
   " %abcd  efghijk ",
   "         lm0no  ",
   "          epq   ",
   "           r    ",
   "                ",
   "                ",
   "                "};
   SendMessage( (HWND) hb_parnl(1), SCI_REGISTERIMAGE, (WPARAM) hb_parni(2), (LPARAM) Method_xpm );
}

HB_FUNC( SCIREGISTERPROPERTYIMAGE )
{
   /* XPM */
   static char * Property_xpm[] = {
   "16 16 121 1",
   "  c None",
   ". c #000000",
   "+ c #B5A681",
   "@ c #A39780",
   "# c #A09680",
   "$ c #998F7A",
   "% c #938573",
   "& c #80715C",
   "* c #CDC8BF",
   "= c #9CB899",
   "- c #A6C1AE",
   "; c #7F6D5B",
   "> c #FFEB6E",
   ", c #FAE272",
   "' c #F4D767",
   ") c #DCBD54",
   "! c #8B7251",
   "~ c #899C69",
   "{ c #85CC85",
   "] c #806B57",
   "^ c #FFED68",
   "/ c #AE9858",
   "( c #F9E172",
   "_ c #F7DD6A",
   ": c #F2CF56",
   "< c #EDC74B",
   "[ c #FABA22",
   "} c #A6A25F",
   "| c #53AC41",
   "1 c #7D9DDF",
   "2 c #AABADA",
   "3 c #ACA48F",
   "4 c #F9E37F",
   "5 c #958453",
   "6 c #4F3A30",
   "7 c #F6DB69",
   "8 c #F3D258",
   "9 c #EDC340",
   "0 c #EABB31",
   "a c #ECA811",
   "b c #8C9050",
   "c c #419E32",
   "d c #A4C5FF",
   "e c #B9C9E5",
   "f c #F6E183",
   "g c #70614B",
   "h c #E2CD7A",
   "i c #676052",
   "j c #503D41",
   "k c #DFB232",
   "l c #C8991D",
   "m c #C49315",
   "n c #8E6214",
   "o c #516640",
   "p c #3F9729",
   "q c #B7CAF1",
   "r c #E5D386",
   "s c #8992A7",
   "t c #70614A",
   "u c #DEC161",
   "v c #594D57",
   "w c #FFD01F",
   "x c #332715",
   "y c #0A0710",
   "z c #2E2102",
   "A c #65564F",
   "B c #5B7859",
   "C c #328C18",
   "D c #C1C8D1",
   "E c #FFFFFF",
   "F c #E9E9E2",
   "G c #84755E",
   "H c #E5E4DF",
   "I c #CECAC2",
   "J c #D6A92E",
   "K c #1B100B",
   "L c #605C5D",
   "M c #969DA4",
   "N c #A7BEA4",
   "O c #29661C",
   "P c #FFFDFB",
   "Q c #FCFEFE",
   "R c #ACADB0",
   "S c #725C41",
   "T c #D3D9E3",
   "U c #E3EDFF",
   "V c #464E54",
   "W c #C0C7D1",
   "X c #FDFEFE",
   "Y c #F8FBFE",
   "Z c #F2F6FD",
   "` c #E8F0FC",
   "  c #DCE9FF",
   ". c #ffffff",
   "+ c #A2AFC4",
   "@ c #FEFFFF",
   "# c #D6E5FF",
   "$ c #C1C8D2",
   "% c #FCFEFF",
   "& c #F8FAFD",
   "* c #EFF4FB",
   "= c #E6EEFB",
   "- c #DEE9FA",
   "; c #D2E2F8",
   "> c #D0E2FF",
   ", c #F9FBFF",
   "' c #CBDEFD",
   ") c #C8CFD9",
   "! c #F6FFFF",
   "~ c #ECF9FF",
   "{ c #E1F0FF",
   "] c #D5EAFF",
   "^ c #D2EAFF",
   "/ c #73757C",
   "( c #4B4B4E",
   "_ c #4D4F54",
   ": c #4D5156",
   "< c #4C5053",
   "[ c #4B4F52",
   "} c #4C4F52",
   "| c #4B4E51",
   "                ",
   "       +@#$%&*=-",
   "      ;>>>,')!~{",
   "     ]^/>(_:<[}|",
   "1112345^67890abc",
   "1de3fghi^jklmnop",
   "1q3rgstuvwxyzABC",
   "DEFGHEItJKLM  NO",
   "DEPPEEQRSTUV    ",
   "WEPPEXYZ`` V    ",
   ".E++@+++++#V    ",
   "$EPP%&*=-;>V    ",
   "$E++,+++++'V    ",
   ")EPP!!!~{]^V    ",
   "/(___:<[}[|V    ",
   "                "};
   SendMessage( (HWND) hb_parnl(1), SCI_REGISTERIMAGE, (WPARAM) hb_parni(2), (LPARAM) Property_xpm );
}

/*
HB_FUNC( AWE_WEBCORE_INITIALIZE )
{
   BOOL enable_plugins = TRUE;
   BOOL enable_javascript = TRUE;
   BOOL enable_databases = TRUE;
   const awe_string* package_path;
   const awe_string* locale_path;
   const awe_string* user_data_path;
   const awe_string* plugin_path;
   const awe_string* log_path;
   awe_loglevel log_level;
   BOOL force_single_process = TRUE;
   const awe_string* child_process_path;
   BOOL enable_auto_detect_encoding = TRUE;
   const awe_string* accept_language_override;
   const awe_string* default_charset_override;
   const awe_string* user_agent_override;
   const awe_string* proxy_server;
   const awe_string* proxy_config_script;
   const awe_string* auth_server_whitelist;
   BOOL save_cache_and_cookies = TRUE;
   int max_cache_size;
   BOOL disable_same_origin_policy = TRUE;
   BOOL disable_win_message_pump = TRUE;
   const awe_string* custom_css;

   awe_webcore_initialize( enable_plugins,
                           enable_javascript,
                           enable_databases,
                           package_path,
                           locale_path,
                           user_data_path,
                           plugin_path,
                           log_path,
                           log_level,
                           force_single_process,
                           child_process_path,
                           enable_auto_detect_encoding,
                           accept_language_override,
                           default_charset_override,
                           user_agent_override,
                           proxy_server,
                           proxy_config_script,
                           auth_server_whitelist,
                           save_cache_and_cookies,
                           max_cache_size,
                           disable_same_origin_policy,
                           disable_win_message_pump,
                           custom_css );
}

HB_FUNC( AWE_WEBCORE_INITIALIZE_DEFAULT )
{
   awe_webcore_initialize_default();
}

HB_FUNC( AWE_WEBCORE_CREATE_WEBVIEW )
{
   hb_retc( (char*) awe_webcore_create_webview( hb_parni(1), hb_parni(2), hb_parl(3) ) );
}

#pragma comment( lib, "Awesomium.lib" )
*/

#ifndef __ITaskbarList3_FWD_DEFINED__
#define __ITaskbarList3_FWD_DEFINED__
typedef interface ITaskbarList3 ITaskbarList3;
#endif /* __ITaskbarList2_FWD_DEFINED__ */

#ifndef __ITaskbarList3_INTERFACE_DEFINED__
#define __ITaskbarList3_INTERFACE_DEFINED__


typedef enum TBPFLAG
{
   TBPF_NOPROGRESS    = 0x00000000,
   TBPF_INDETERMINATE = 0x00000001,
   TBPF_NORMAL        = 0x00000002,
   TBPF_ERROR         = 0x00000004,
   TBPF_PAUSED        = 0x00000008
}  TBPFLAG;

DEFINE_GUID(CLSID_TaskbarList, 0x56fdf344,0xfd6d,0x11d0,0x95,0x8a,0x00,0x60,0x97,0xc9,0xa0,0x90);
static const GUID IID_ITaskbarList3 = { 0xea1afb91, 0x9e28, 0x4b86, { 0x90, 0xE9, 0x9e, 0x9f, 0x8a, 0x5e, 0xef, 0xaf } };

struct ITaskbarList3Vtbl
{
   BEGIN_INTERFACE
      HRESULT ( STDMETHODCALLTYPE *QueryInterface )(ITaskbarList3 * This, REFIID riid, void **ppvObject);
      ULONG ( STDMETHODCALLTYPE *AddRef )(ITaskbarList3 * This);
      ULONG ( STDMETHODCALLTYPE *Release )(ITaskbarList3 * This);
      HRESULT ( STDMETHODCALLTYPE *HrInit )(ITaskbarList3 * This);
      HRESULT ( STDMETHODCALLTYPE *AddTab )(ITaskbarList3 * This, HWND hwnd);
      HRESULT ( STDMETHODCALLTYPE *DeleteTab )(ITaskbarList3 * This, HWND hwnd);
      HRESULT ( STDMETHODCALLTYPE *ActivateTab )(ITaskbarList3 * This, HWND hwnd);
      HRESULT ( STDMETHODCALLTYPE *SetActiveAlt )(ITaskbarList3 * This, HWND hwnd);
      HRESULT ( STDMETHODCALLTYPE *MarkFullscreenWindow )(ITaskbarList3 * This, HWND hwnd, BOOL fFullscreen);
      HRESULT ( STDMETHODCALLTYPE *SetProgressValue )(ITaskbarList3 * This, HWND hwnd, ULONGLONG ullCompleted, ULONGLONG ullTotal);
      HRESULT ( STDMETHODCALLTYPE *SetProgressState )(ITaskbarList3 * This, HWND hwnd, TBPFLAG tbpFlags);
      HRESULT ( STDMETHODCALLTYPE *RegisterTab )(ITaskbarList3 * This, HWND hwndTab, HWND hwndMDI);
      HRESULT ( STDMETHODCALLTYPE *UnregisterTab )(ITaskbarList3 * This, HWND hwndTab);
      HRESULT ( STDMETHODCALLTYPE *SetTabOrder )(ITaskbarList3 * This, HWND hwndTab, HWND hwndInsertBefore);
      HRESULT ( STDMETHODCALLTYPE *SetTabActive )(ITaskbarList3 * This, HWND hwndTab, HWND hwndMDI, DWORD dwReserved);
      HRESULT ( STDMETHODCALLTYPE *ThumbBarAddButtons )(ITaskbarList3 * This, HWND hwnd, UINT cButtons, void * pButton);
      HRESULT ( STDMETHODCALLTYPE *ThumbBarUpdateButtons )(ITaskbarList3 * This, HWND hwnd, UINT cButtons, void * pButton);
      HRESULT ( STDMETHODCALLTYPE *ThumbBarSetImageList )(ITaskbarList3 * This, HWND hwnd, void * himl);
      HRESULT ( STDMETHODCALLTYPE *SetOverlayIcon )(ITaskbarList3 * This, HWND hwnd, HICON hIcon, LPCWSTR pszDescription);
      HRESULT ( STDMETHODCALLTYPE *SetThumbnailTooltip )(ITaskbarList3 * This, HWND hwnd, LPCWSTR pszTip);
      HRESULT ( STDMETHODCALLTYPE *SetThumbnailClip )(ITaskbarList3 * This, HWND hwnd, RECT *prcClip);
   END_INTERFACE
};

interface ITaskbarList3 {
    CONST_VTBL struct ITaskbarList3Vtbl *lpVtbl;
};
#endif   /* __ITaskbarList3_INTERFACE_DEFINED__ */


HB_FUNC( TASKBARPROGRESSVALUE )
{
   ITaskbarList3 *pTL;
   BOOL bRet = FALSE;
   CoInitialize( NULL );
   if( SUCCEEDED( CoCreateInstance( &CLSID_TaskbarList, NULL, CLSCTX_ALL, &IID_ITaskbarList3, (void**)&pTL) ) )
   {
      bRet = SUCCEEDED( pTL->lpVtbl->SetProgressValue(pTL, (HWND) hb_parnl(1), hb_parni(2), hb_parni(3) ) );
      pTL->lpVtbl->Release( pTL );
   }
   CoUninitialize();
   hb_retl( bRet );
}

HB_FUNC( TASKBARPROGRESSSTATE )
{
   ITaskbarList3 *pTL;
   BOOL bRet = FALSE;
   CoInitialize( NULL );
   if( SUCCEEDED( CoCreateInstance( &CLSID_TaskbarList, NULL, CLSCTX_ALL, &IID_ITaskbarList3, (void**)&pTL) ) )
   {
      bRet = SUCCEEDED( pTL->lpVtbl->SetProgressState( pTL, (HWND) hb_parnl(1), (TBPFLAG) hb_parni(2) ) );
      pTL->lpVtbl->Release( pTL );
   }
   CoUninitialize();
   hb_retl( bRet );
}
//#define WM_FORWARDMSG 0x037F

//-----------------------------------------------------------------------------------------------------------------------------------------
BOOL AxTranslateMessageEx( MSG *pMsg )
{
   BOOL bRet = FALSE;
   if( (pMsg->message >= WM_KEYFIRST) && (pMsg->message <= WM_KEYLAST) )
   {
      if( pMsg->wParam == VK_RETURN || pMsg->wParam == VK_TAB )
      {
         LPUNKNOWN pUnk;
         HWND hParent;
         HWND hWnd = pMsg->hwnd;

         while( ! ( pUnk = (LPUNKNOWN) GetWindowLong( hWnd, GWL_USERDATA ) ) )
         {
            hParent = GetParent( hWnd );
            if( ! hParent )
            {
               break;
            }
            hWnd = hParent;
         }

         if( pUnk )
         {
            bRet = AxTranslateMessage( pUnk, pMsg );
         }
      }
      else
      {
         if( pMsg->message == WM_KEYUP )
         {
            LPUNKNOWN pUnk = (LPUNKNOWN) GetWindowLong( pMsg->hwnd, GWL_USERDATA );
            if( pUnk )
            {
               AxTranslateMessage( pUnk, pMsg );
            }
         }
      }
   }
   return bRet;
}

BOOL TranslateVXHAccel( PHB_ITEM aAccel, MSG msg, BOOL bAccEnabled )
{
   BOOL bRet = FALSE;
   if( msg.message == WM_KEYDOWN && bAccEnabled )
   {
      int i;
      int iCount = aAccel->item.asArray.value->ulLen;
      HWND hActive = GetActiveWindow();
      HWND hPrev;

      for ( i = 0; i < iCount; i++ )
      {
         PHB_ITEM pItem = hb_itemArrayGet( aAccel, i+1 );
         HWND hWnd = (HWND) hb_arrayGetNL( pItem, 1 );
         HACCEL hAccel = (HACCEL) hb_arrayGetNL( pItem, 2 );
         hb_itemRelease( pItem );

         if( hWnd != 0 && hAccel != 0 )
         {
            hPrev = hActive;
            if( ! ( hWnd == hActive ) )
            {
               while( hActive != 0 && ( ! IsChild( hWnd, hActive ) ) )
               {
                  hActive = GetParent( hActive );
               }
            }
            if( hWnd == hPrev || IsChild( hWnd, hActive ) )
            {
               if( TranslateAccelerator( hWnd, hAccel, &msg ) )
               {
                  bRet = TRUE;
                  break;
               }
            }
            hActive = hPrev;
         }
      }
   }
   return bRet;
}

HB_FUNC( __VXHYIELD )
{
   MSG msg;
   if( PeekMessage(&msg, (HWND) NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }
}

HB_FUNC( VXH_MAINLOOP )
{
   MSG msg;
   HWND hMDI  = (HWND) hb_parnl(2);
   PHB_ITEM aAccel = (PHB_ITEM) hb_param( 3, HB_IT_ARRAY );
   BOOL bAccEnabled = (BOOL) hb_parl(4);

   while( GetMessage( &msg, NULL, 0, 0 ) )
   {
      //if( ! AxTranslateMessageEx( &msg ) )
      //{
         if( ! TranslateVXHAccel( aAccel, msg, bAccEnabled ) )
         {
            if( (hMDI == 0) || (! TranslateMDISysAccel( hMDI, &msg )) )
            {
               if( ! IsDialogMessage( GetActiveWindow(), &msg ) )
               {
                  TranslateMessage( &msg );
                  DispatchMessage( &msg );
               }
            }
         }
      //}
   }
}

#ifdef __XCC__
#if 0
   typedef struct _PROCESS_MEMORY_COUNTERS_EX {
     DWORD  cb;
     DWORD  PageFaultCount;
     SIZE_T PeakWorkingSetSize;
     SIZE_T WorkingSetSize;
     SIZE_T QuotaPeakPagedPoolUsage;
     SIZE_T QuotaPagedPoolUsage;
     SIZE_T QuotaPeakNonPagedPoolUsage;
     SIZE_T QuotaNonPagedPoolUsage;
     SIZE_T PagefileUsage;
     SIZE_T PeakPagefileUsage;
     SIZE_T PrivateUsage;
   } PROCESS_MEMORY_COUNTERS_EX, *PPROCESS_MEMORY_COUNTERS_EX;
#endif
#endif

HB_FUNC( GETPROCESSMEMORY )
{
   //DWORD processID;
   //HANDLE hProcess;
   PROCESS_MEMORY_COUNTERS pmc;
   //HINSTANCE hProcHandle = GetModuleHandle( ISNIL(1)?NULL:hb_parc(1) );

   //hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, processID );

   GetProcessMemoryInfo( /*hProcHandle*/ GetCurrentProcess(), &pmc, sizeof(pmc) );
   hb_retnl( (long) pmc.WorkingSetSize );

   //CloseHandle( hProcess );
}

HB_FUNC( ISWOW64 )
{
    BOOL bIsWow64 = FALSE;
    LPFN_ISWOW64PROCESS fnIsWow64Process = (LPFN_ISWOW64PROCESS) GetProcAddress( GetModuleHandle( TEXT("kernel32") ), "IsWow64Process");
    if (NULL != fnIsWow64Process)
    {
        if( !fnIsWow64Process( GetCurrentProcess(), &bIsWow64 ) )
        {
            bIsWow64 = FALSE;
        }
    }
    hb_retl( bIsWow64 );
}


/*
MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    GlobalMemoryStatusEx(&memInfo);
    DWORDLONG totalVirtualMem = memInfo.ullTotalPageFile;

Note: The name "TotalPageFile" is a bit misleading here. In reality this parameter gives the "Virtual Memory Size", which is size of swap file plus installed RAM.

    Virtual Memory currently used:

Same code as in "Total Virtual Memory" and then


    DWORDLONG virtualMemUsed = memInfo.ullTotalPageFile - memInfo.ullAvailPageFile
*/

HB_FUNC( __GETPROPERTYCASE )
{
   PHB_DYNS pMessage = hb_dynsymGetCase( hb_parcx(1) );
   hb_retc( pMessage->pSymbol->szName );
}


HB_FUNC( RUNASADMIN )
{
   OSVERSIONINFO osvi;

   BOOL bRet;
   BOOL bWait = ISNIL(5) ? TRUE : hb_parl(5);
   SHELLEXECUTEINFO sei;
   ZeroMemory ( &sei, sizeof(sei) );

   sei.cbSize       = sizeof(SHELLEXECUTEINFO);
   sei.hwnd         = GetActiveWindow();
   sei.fMask        = 0;
   if( bWait )
      sei.fMask     = SEE_MASK_NOASYNC;
   sei.lpVerb       = TEXT("runas");
   sei.lpFile       = (LPTSTR) hb_parc(1);
   sei.lpParameters = (LPTSTR) hb_parc(2);
   sei.lpDirectory  = (LPSTR) hb_parc(3);
   sei.nShow        = ISNIL(4)?SW_SHOWNORMAL:hb_parni(4);

   osvi.dwOSVersionInfoSize = sizeof( osvi );
   GetVersionExA( &osvi ) ;

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion >= 6 && osvi.dwMinorVersion >= 2 )
   {
      HKEY hKey;
      const BYTE mydata[] = TEXT("RUNASADMIN");

      RegOpenKeyEx(HKEY_CURRENT_USER, TEXT("Software\\Microsoft\\Windows NT\\CurrentVersion\\AppCompatFlags\\Layers"), NULL, KEY_SET_VALUE, &hKey);
      RegSetValueEx(hKey, hb_parc(1), NULL, REG_SZ, mydata, sizeof(mydata) );
      RegCloseKey(hKey);
   }

   bRet = ShellExecuteEx( &sei );
   if( bWait )
   {
      WaitForSingleObject( sei.hProcess, INFINITE );
   }
   hb_retl( bRet );
}

HB_FUNC( _GETOPENFILENAME )   // ( cFileMask, cTitle, nDefaultMask, cInitDir, lSave, lLongNames, nFlags, cDefFile, cDefExt, lMultiFile, lOverwritePrompt )  --> cFileName
{
   OPENFILENAME ofn;
   LPSTR pFile, pFilter, pDir, pTitle, pTmp, pStart;
   int  iCount = 1, iNext = 2;
   WORD w = 0, wLen;
   BYTE bIndex = hb_parni( 3 );
   BOOL bSave = (hb_pcount() > 4 && ISLOG( 5 )) ? hb_parl( 5 ): FALSE;
   BOOL bLongNames = ISNIL(6) ? TRUE : hb_parl( 6 );
   BOOL bMultiFile = ISNIL(10) ? FALSE : hb_parl( 10 );
   BOOL bOverwritePrompt = ISNIL(11) ? FALSE : hb_parl( 11 );
   DWORD dwFlags = hb_parnl( 7 );
   DWORD dwFileBufSize = bMultiFile ? 1024 * 32 : 256;

   char pDefExt[ 4 ];
   char cMsg[25];

   if( hb_pcount() < 1 )
   {
      hb_retc( "" );
       return;
   }

   // alloc for title

   pTitle = (LPSTR)hb_xgrab( MAX_PATH_LEN+1 );

   wLen   = min( MAX_PATH_LEN, hb_parclen( 2 ) );
   _bcopy( pTitle, (char*)hb_parc( 2 ), wLen );
   * ( pTitle + wLen ) = 0;

   // alloc for initial dir

   pDir = (LPSTR) hb_xgrab( MAX_PATH_LEN+1 );

   if ( hb_pcount() > 3 && ISCHAR( 4 ) )
   {
      wLen  = min( MAX_PATH_LEN, hb_parclen( 4 ) );
      _bcopy( pDir, (char*)hb_parc( 4 ), wLen );
      * ( pDir + wLen ) = 0;
   }
   else
   {
      * ( pDir ) = 0;
   }

   // alloc for file

   pFile   = (LPSTR) hb_xgrab( dwFileBufSize );
   if ( hb_pcount() > 7 && ISCHAR( 8 ) )
   {
      wLen = min( dwFileBufSize - 2, hb_parclen( 8 ) );
      _bcopy( pFile, (char*) hb_parc( 8 ), wLen );
   }else
   {
      wLen    = min( dwFileBufSize - 2, hb_parclen( 1 ) );
      _bcopy( pFile, (char*) hb_parc( 1 ), wLen );
   }
   * ( pFile + wLen ) = 0;

   // alloc for mask

   pFilter = (LPSTR) hb_xgrab( 400 );
   wLen    = min( 398, hb_parclen( 1 ) );
   _bcopy( pFilter, (char*) hb_parc( 1 ), wLen );
   * ( pFilter + wLen ) = 0;

   while( * ( pFilter + w ) )
   {
      if( * ( pFilter + w ) == '|' )
      {
         * ( pFilter + w ) = 0;
         if( ! ISCHAR( 8 ) )
            * (pFile) = 0;    // if no default name was passed, terminate at end of first spec
      }
      w++;
   }
   * ( pFilter + wLen  ) = 0;
   * ( pFilter + wLen + 1 ) = 0;

   // Default Extension
   if ( hb_pcount() > 8 && ISCHAR( 9 ) )
      _bcopy( pDefExt, (char*) hb_parc( 9 ), 3 );
   else
      * pDefExt = 0;

   _bset( ( char * ) &ofn, 0, sizeof( OPENFILENAME ) );

   ofn.lStructSize     = sizeof( OPENFILENAME );
   ofn.hwndOwner       = GetActiveWindow();
   ofn.lpstrFilter     = pFilter;
   ofn.lpstrCustomFilter = 0; // NIL;
   ofn.nFilterIndex    = bIndex ? bIndex: 1;
   ofn.lpstrFile       = pFile;
   ofn.nMaxFile        = dwFileBufSize ; //MAX_PATH_LEN;
   ofn.lpstrFileTitle  = 0; // NIL;
   ofn.lpstrInitialDir = pDir;
   ofn.lpstrTitle      = pTitle;
   ofn.lpstrDefExt     = pDefExt[0] ? pDefExt : NULL ;
   ofn.Flags           = OFN_PATHMUSTEXIST | OFN_NOCHANGEDIR |
                         OFN_HIDEREADONLY |
                         bOverwritePrompt ? OFN_OVERWRITEPROMPT : 0 |
                         bMultiFile ? (OFN_ALLOWMULTISELECT | OFN_EXPLORER) : 0 |
                         bLongNames ? OFN_LONGNAMES : 0;

   if( dwFlags )
      ofn.Flags = dwFlags;

   if( bSave )
   {
      if( GetSaveFileName( &ofn ) )
         hb_retc( pFile );
      else
         hb_retc( "" );
   }
   else
   {
      if( GetOpenFileName( &ofn ) )
      {
         if( bMultiFile )
         {
            /*
             *    Multi-Select returns an array if any are selected (Cancel returns an empty string).
             *    If ONE file is selected, you get 1 element with full path to the file.
             *    If more are selected you get an array sized at (nNumFiles + 1)
             *    with the FIRST element as the path to the FOLDER,
             *    plus an additional element for each filename.
             */

            pTmp = pFile + ofn.nFileOffset;
            // first must get count of files to allocate the array
            while ( *pTmp || *(pTmp + 1) )
            {
               if ( *pTmp == NULL )
               {
                  iCount++;             // represents the number of FILES selected, not array size
               }
               pTmp++;
            }
//            TraceLog( NULL, "iCount %d \n", iCount );
            hb_reta( iCount + ( iCount == 1 ? 0 : 1) );
            hb_storc( pFile, -1, 1 );     // the directory, or full path if a single file was selected

            if ( iCount > 1 )
            {
               pStart = pFile + ofn.nFileOffset;
               pTmp = pStart + 1;
               while ( *pTmp || *(pTmp + 1) )
               {
                  if ( *pTmp == NULL )  // curChar as NULL marks end of a string
                  {
                     //TraceLog( NULL, "pStart  file? %s \n", pStart);
                     hb_storc( pStart, -1, iNext++ );      // the directory, or full path if a single file was selected
                     pStart = pTmp + 1;
                  }
                  pTmp++;
               }
               if ( iNext <= iCount + 1 )   // loop controller won't see the end of the last string
               {
                  //TraceLog( NULL, "pStart last file? %s \n", pStart);
                  hb_storc( pStart, -1, iNext++ );
               }
            }
         }
         else
            hb_retc( pFile );
      }
      else
      {
         //wsprintf(cMsg, "%d", sizeof( OPENFILENAME ) );
         if ( CommDlgExtendedError() )
         {
            wsprintf(cMsg, "CommDlg Error# %d", CommDlgExtendedError() );
            MessageBox( GetActiveWindow(), cMsg, "Dialog Error", MB_ICONSTOP );
         }
         hb_retc( "" );
      }
   }

   hb_xfree( pFilter );
   hb_xfree( pFile );
   hb_xfree( pDir );
   hb_xfree( pTitle );
}

HB_FUNC( ISRGB )
{
   hb_retl( sizeof(COLORREF) == sizeof(hb_parnl(1)) );
}

HB_FUNC( SYSTEMTIMETOLOCALTIME )
{
   SYSTEMTIME lt;
   SYSTEMTIME utc;
   char *dateTimeString = (char *) hb_parc(1);
   char cbuffer[9];

   sscanf( dateTimeString, "%4d%2d%2d%2d%2d%2d",
                           &utc.wYear, &utc.wMonth, &utc.wDay,
                           &utc.wHour, &utc.wMinute, &utc.wSecond );

   if( SystemTimeToTzSpecificLocalTime( NULL, &utc, &lt ) )
   {
      char cTime[9];

      hb_reta(2);
      hb_stord( lt.wYear, lt.wMonth, lt.wDay, -1, 1 );

      wsprintf( cTime, "%02i:%02i:%02i", lt.wHour, lt.wMinute, lt.wSecond );
      hb_storclen( cTime, 8, -1, 2 );
   }
}

HB_FUNC( KILLPROCESS )
{
   HANDLE hSnapShot = CreateToolhelp32Snapshot( TH32CS_SNAPALL, 0 );
   PROCESSENTRY32 pEntry;
   pEntry.dwSize = sizeof( pEntry );

   BOOL hRes = Process32First( hSnapShot, &pEntry );

   while( hRes )
   {
      if( stricmp( pEntry.szExeFile, hb_parc(1) ) == 0 )
      {
         HANDLE hProcess = OpenProcess( PROCESS_TERMINATE, 0, (DWORD) pEntry.th32ProcessID );
         if( hProcess != NULL )
         {
            TerminateProcess(hProcess, 9);
            CloseHandle( hProcess );
         }
      }
      hRes = Process32Next( hSnapShot, &pEntry );
   }
   CloseHandle( hSnapShot );
}

HB_FUNC( ISPROCESSRUNNING )
{
   BOOL bRet = FALSE;
   HANDLE hSnapShot = CreateToolhelp32Snapshot( TH32CS_SNAPALL, 0 ); //th32ProcessID [in] The process identifier of the process to be included in the snapshot. This parameter can be zero to indicate the current process
   PROCESSENTRY32 pEntry;
   pEntry.dwSize = sizeof( pEntry );

   BOOL hRes = Process32First( hSnapShot, &pEntry );

   while( hRes )
   {
      if( stricmp( pEntry.szExeFile, hb_parc(1) ) == 0 )
      {
         bRet = TRUE;
         break;
      }
      hRes = Process32Next( hSnapShot, &pEntry );
   }
   CloseHandle( hSnapShot );
   hb_retl( bRet );
}


HB_FUNC( ASPTR )
{
   hb_retptr( ( void * ) hb_parnl( 1 ) );
}

HB_FUNC( ASLONG )
{
   hb_retnl( ( HB_LONG ) hb_parptr( 1 ) );
}

HB_FUNC( VXH_FREECALLBACKPOINTER )
{
   void *pCallback = (void *) hb_parnl( 1 );
   if( pCallback )
   {
      PHB_ITEM pSelf = hb_param( 2, HB_IT_ANY );

      hb_retl( VirtualFree( (LPVOID) pCallback, 0, MEM_RELEASE ) );

      if( pSelf && HB_IS_OBJECT( pSelf ) )
      {
         hb_gcUnlock( pSelf->item.asArray.value );
      }

      return;
   }

   hb_retl( FALSE );
}

HB_FUNC( VXH_FREECALLBACKOBJECT )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_ANY );
   if( pSelf && HB_IS_OBJECT( pSelf ) )
   {
      hb_gcUnlock( pSelf->item.asArray.value );
   }
   hb_retl( FALSE );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDISPLAYWORKAREA )
{
   RECT rc;
   PHB_ITEM aRect = hb_itemArrayNew(4);
   PHB_ITEM element = hb_itemNew(NULL);

   SystemParametersInfo( SPI_GETWORKAREA, 0, &rc, 0 );

   hb_arraySet(aRect, 1, hb_itemPutNL(element, rc.left));
   hb_arraySet(aRect, 2, hb_itemPutNL(element, rc.top));
   hb_arraySet(aRect, 3, hb_itemPutNL(element, rc.right));
   hb_arraySet(aRect, 4, hb_itemPutNL(element, rc.bottom));
   hb_itemRelease( element );
   hb_itemRelease( hb_itemReturnForward( aRect ) );
}

HB_FUNC( GETDEVMODEFIELDS )
{
   PDEVMODE dm = (PDEVMODE) GlobalLock( (HGLOBAL) hb_parnl(1) );
   hb_reta( 8 );
   hb_storni( (long) dm->dmOrientation,   -1, 1 );
   hb_storni( (long) dm->dmPaperSize,     -1, 2 );
   hb_storni( (long) dm->dmPaperLength,   -1, 3 );
   hb_storni( (long) dm->dmPaperWidth,    -1, 4 );
   hb_storni( (long) dm->dmScale,         -1, 5 );
   hb_storni( (long) dm->dmCopies,        -1, 6 );
   hb_storni( (long) dm->dmDefaultSource, -1, 7 );
   hb_storni( (long) dm->dmPrintQuality,  -1, 8 );
   GlobalUnlock( (HGLOBAL) hb_parnl(1) );
}

#define ILS_ALPHA 0x0008

HB_FUNC( CREATEIMAGELISTBITMAP )
{
   HIMAGELIST hImageList = (HIMAGELIST) hb_parnl(1);
   HBITMAP hBitmap;
   BITMAPINFO bi = { 0 };
   bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
   bi.bmiHeader.biWidth = hb_parni(5);
   bi.bmiHeader.biHeight = hb_parni(6);
   bi.bmiHeader.biPlanes = 1;
   bi.bmiHeader.biBitCount = 32;
   bi.bmiHeader.biCompression = BI_RGB;
   bi.bmiHeader.biSizeImage = 0;
   bi.bmiHeader.biXPelsPerMeter = 0;
   bi.bmiHeader.biYPelsPerMeter = 0;
   bi.bmiHeader.biClrUsed = 0;
   bi.bmiHeader.biClrImportant = 0;

   hBitmap = CreateDIBSection( (HDC) hb_parnl(3), &bi, DIB_RGB_COLORS, NULL, NULL, 0 );

   // Select bitmap into target DC and draw from image list to it
   if(hBitmap != NULL)
   {
      HBITMAP hOldBmp = (HBITMAP) SelectObject((HDC) hb_parnl(4), hBitmap);

      IMAGELISTDRAWPARAMS ildp = { 0 };
      ildp.cbSize = sizeof(IMAGELISTDRAWPARAMS);
      ildp.himl = hImageList;
      ildp.i = hb_parni(2);
      ildp.hdcDst = (HDC) hb_parnl(4);
      ildp.x = 0;
      ildp.y = 0;
      ildp.cx = 0;
      ildp.cy = 0;
      ildp.xBitmap = 0;
      ildp.yBitmap = 0;
      ildp.fStyle = ILD_TRANSPARENT;
      ildp.fState = ILS_ALPHA;
      ildp.Frame = 255;
      ImageList_DrawIndirect(&ildp);
      SelectObject( (HDC) hb_parnl(3), hOldBmp);
      hb_retnl( (LONG) hBitmap );
   }
}


HB_FUNC( VXH_GETNOTIFYCODE )
{
   hb_retnl( ((LPNMHDR)hb_parnl(1))->code );
}

HB_FUNC ( VXH_SETMENUBACKCOLOR )
{
   MENUINFO mi;
   mi.cbSize   = sizeof(MENUINFO);
   mi.fMask    = MIM_BACKGROUND | MIM_APPLYTOSUBMENUS;
   mi.hbrBack  = (HBRUSH) hb_parnl(2);
   hb_retl( SetMenuInfo( (HMENU) hb_parnl(1), &mi ) );
}

HB_FUNC( GETOLECOLOR )
{
   OLE_COLOR* oColor = (OLE_COLOR*) hb_parnl(1);
   hb_retnl( (long) oColor );
}

/*
#include <shobjidl.h>

HB_FUNC( CREATEHPREVIEW ) //preview
{

#ifndef _WIN64
  #define RET hb_retnl( (long) pPreview );
#else
  #define RET hb_retnll( (long long) pPreview );
#endif
 LPWSTR cFileName = UTF8toUTF16( hb_parc( 1 ) );
  LPCWSTR cExtension = wcsrchr(cFileName,L'.');

 CLSID cls;
   wchar_t clsStr[250];
   wchar_t cKey[250];
   LONG nLen=250;
   IPreviewHandler* pPreview=0;
   HRESULT hr;

   if(cExtension == 0 ) {
      pPreview = 0;
      RET
      return;
   }
   wcscpy_s(cKey,250, cExtension);
   wcscat_s(cKey,250, L"\\ShellEx\\{8895b1c6-b41f-4c1c-a562-0d564250836f}");
   if( ERROR_SUCCESS != RegQueryValueW(HKEY_CLASSES_ROOT, cKey, clsStr, &nLen) )
   {
      RegQueryValueW(HKEY_CLASSES_ROOT, cExtension, cKey, &nLen);
      wcscat_s(cKey,250, L"\\ShellEx\\{8895b1c6-b41f-4c1c-a562-0d564250836f}");
      // MessageBoxW(0,cKey,cExtension,MB_OK);
      if( ERROR_SUCCESS != RegQueryValueW(HKEY_CLASSES_ROOT, cKey, clsStr, &nLen) )
      {
       pPreview = 0;
       RET
       return;
      }
    }

    CLSIDFromString( clsStr,&cls);

   if( S_OK == CoCreateInstance(cls, NULL, CLSCTX_INPROC_SERVER |CLSCTX_LOCAL_SERVER, __uuidof(IPreviewHandler),(LPVOID*)&pPreview) )
   {
      RET
      return;
   }

   MessageBoxW(0,cKey,cExtension,MB_OK);
   pPreview = 0;
   RET
return;
}


HB_FUNC( ADDPREVIEWH ) // hWnd, {nTop, nLeft, nBottom, nRight}, cFileName, pPreview
{
#ifndef _WIN64
   HWND hWnd = ( HWND ) hb_parnl( 1 );
    IPreviewHandler* pPreview = ( IPreviewHandler* ) hb_parnl( 4 );
#else
   HWND hWnd = ( HWND ) hb_parnll( 1 );
   IPreviewHandler* pPreview = ( IPreviewHandler* ) hb_parnll( 4 );
#endif

   RECT rectPreview;

   LPWSTR cFileName = UTF8toUTF16( hb_parc( 3 ) );
   LPCWSTR cExtension = wcsrchr(cFileName,L'.');

   IInitializeWithFile* pFile;
   IInitializeWithStream* pIStream;

   if(pPreview==0)
   {
      MessageBoxW(0,L"no pPreview",cExtension,MB_OK);
      RET
      return;
   }

   pPreview->QueryInterface(__uuidof( IInitializeWithFile ), (LPVOID*)&pFile );
   pPreview->QueryInterface(__uuidof( IInitializeWithStream ), (LPVOID*)&pIStream );

   if(pFile==0 && pIStream==0)
   {
      MessageBoxW(0,L"no pfile no pStream",cExtension,MB_OK);
      pPreview->Unload();
      pPreview = 0;
      RET
      return;
   }
   if(pFile!=0)
   {
      pFile->Initialize( cFileName, STGM_READ);
   } else
   { //pIStream!=0
      HANDLE hFile = CreateFileW(cFileName,FILE_READ_DATA,FILE_SHARE_READ,NULL,OPEN_EXISTING,0,NULL );
     if( INVALID_HANDLE_VALUE != hFile )
     {
         DWORD dwSize = GetFileSize( hFile, NULL );
         HGLOBAL hGlobal= GlobalAlloc(GPTR, dwSize );
         BYTE * pByte = (BYTE *)GlobalLock(hGlobal);

         if( pByte )
         {
            LPSTREAM pStream;
            ReadFile(hFile,pByte,dwSize,&dwSize,NULL);
            GlobalUnlock(hGlobal);

             CreateStreamOnHGlobal(hGlobal, TRUE, &pStream);
             pIStream->Initialize( pStream, STGM_READ);
         }

         CloseHandle( hFile );
     }
   }
   hb_xfree( cFileName );

   rectPreview.top    = hb_parvni(2,1);
   rectPreview.left   = hb_parvni(2,2);
   rectPreview.bottom = hb_parvni(2,3);
   rectPreview.right  = hb_parvni(2,4);

   pPreview->SetWindow( hWnd, &rectPreview );
   pPreview->DoPreview();

 //  pPreview->Unload();
   delete pIStream ;

}

HB_FUNC( UNLOADHPREVIEW ) //preview
{
#ifndef _WIN64
   IPreviewHandler* pPreview = ( IPreviewHandler* ) hb_parnl( 1 );
  #else
   IPreviewHandler* pPreview = ( IPreviewHandler* ) hb_parnll( 1 );
  #endif
   pPreview->Unload();

 }

*/

int CALLBACK EnumFontProc( const LOGFONT *lplf, const TEXTMETRIC *lptm, DWORD dwType, LPARAM lpData )
{
   int iResponse = 0;
   if( pFontDynSym )
   {
      hb_vmPushSymbol( pFontDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (long) lplf );
      hb_vmPushLong( (long) lptm );
      hb_vmPushLong( (long) dwType );
      hb_vmPushLong( (long) lpData );
      hb_vmDo( 4 );
      iResponse = hb_itemGetNI( hb_stackReturnItem() );
   }
   return iResponse;
}


//------------------------------------------------------------------------------------------------
HB_FUNC( _ENUMFONTS )
{
   pFontDynSym = hb_dynsymFindName( hb_parc(3) );
   hb_retni( EnumFonts( (HDC) hb_parnl(1), (LPCTSTR) hb_parc(2), (FONTENUMPROC) EnumFontProc, (LPARAM) hb_parnl(4) ) );
}

int CALLBACK ChooseColorProc( HWND hdlg, UINT uiMsg, WPARAM wParam, LPARAM lParam )
{
   int iResponse = 0;
   if( pColorDynSym )
   {
      hb_vmPushSymbol( pColorDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (long) hdlg );
      hb_vmPushLong( (long) uiMsg );
      hb_vmPushLong( (long) wParam );
      hb_vmPushLong( (long) lParam );
      hb_vmDo( 4 );
      iResponse = hb_itemGetNI( hb_stackReturnItem() );
   }
   return iResponse;
}

//----------------------------------------------------------------------------
HB_FUNC( _CHOOSECOLOR )
{
   CHOOSECOLOR cc ;
   COLORREF crCustClr[16] ;
   int i ;
   BOOL bRet ;

   //pColorDynSym = hb_dynsymFindName( hb_parc(5) );

   for( i = 0 ; i <16 ; i++ )
     crCustClr[i] = (ISARRAY(3) ? hb_parnl(3,i+1) : RGB(0,0,0)) ;// GetSysColor(COLOR_BTNFACE)) ;

   cc.lStructSize    = sizeof( CHOOSECOLOR ) ;
   cc.hwndOwner      = ISNIL(1) ? GetActiveWindow():(HWND) hb_parnl(1) ;
   cc.rgbResult      = ISNIL(2) ? (COLORREF) 0 : (COLORREF) hb_parnl(2) ;
   cc.lpCustColors   = crCustClr ;
   cc.Flags          = (WORD) (ISNIL(4) ? CC_ANYCOLOR | CC_FULLOPEN | CC_RGBINIT : hb_parnl(4) ) ;
//   cc.lpfnHook       = (LPCCHOOKPROC) ChooseColorProc;

   bRet = (BOOL) ChooseColor( &cc );
   if ( bRet )
   {
      hb_stornl( (LONG) cc.rgbResult, 2 );
      if ( ISARRAY(3) && hb_parinfa(3,0) >= 16 )
         for( i = 0 ; i <16 ; i++ )
             hb_stornl( (LONG) cc.lpCustColors[i], 3, i+1 );
   }
   hb_retl( bRet );
}

HB_FUNC( VXH_WSASTARTUP )
{
   WSADATA wsa;
   hb_retni( WSAStartup( 0x101, &wsa ) );
}

HB_FUNC( VXH_WSACLEANUP )
{
   hb_retni( WSACleanup() );
}

HB_FUNC( VXH_GETHOSTNAME )
{
   BYTE Name[ 255 ];
   gethostname( ( char * ) Name, 255 );
   hb_retc( ( char * ) Name );
}

HB_FUNC( VXH_GETHOSTBYNAME )
{
   struct hostent * pHost;
   BYTE addr[20];

   strcpy( (char *) addr, "0.0.0.0" );

   if( pHost = gethostbyname( hb_parc(1) ) )
   {
       wsprintf( (char *) addr, "%i.%i.%i.%i",
                 (BYTE) pHost->h_addr[0], (BYTE) pHost->h_addr[1],
                 (BYTE) pHost->h_addr[2], (BYTE) pHost->h_addr[3] );
   }

   hb_retc( (char *) addr );
}

HB_FUNC( MESSAGEBOXINDIRECT )
{
   MSGBOXPARAMSW pMsgBox;
   DWORD dwStyle = (DWORD) hb_parni(4);
   LPCWSTR wString = NULL;
   LPCWSTR wText = hb_oleAnsiToWide( hb_parc(2) );
   LPCWSTR wCaption = hb_oleAnsiToWide( hb_parc(3) );

   if( ! ISNIL(5) )
   {
      dwStyle |= MB_USERICON;

      if( ISCHAR(5) )
      {
         wString = hb_oleAnsiToWide( hb_parc(5) );
         pMsgBox.lpszIcon  = (LPCWSTR) wString;
      }
      else
      {
         pMsgBox.lpszIcon  = MAKEINTRESOURCEW( hb_parni(5) );
      }
   }

   pMsgBox.cbSize          = sizeof(pMsgBox);
   pMsgBox.hwndOwner       = (HWND) hb_parnl(1);
   pMsgBox.hInstance       = GetModuleHandle( NULL );
   pMsgBox.lpszText        = wText;
   pMsgBox.lpszCaption     = wCaption;
   pMsgBox.dwStyle         = dwStyle;
   pMsgBox.dwContextHelpId = hb_parni(6);
   pMsgBox.lpfnMsgBoxCallback = NULL;
   pMsgBox.dwLanguageId    = LANG_SYSTEM_DEFAULT;

   hb_retni( MessageBoxIndirectW( &pMsgBox ) );

   if( wString != NULL )
   {
      hb_xfree( (void *) wString );
   }
   hb_xfree( (void *) wText );
   hb_xfree( (void *) wCaption );
}

HB_FUNC( GETSPECIALFOLDER )
{
   LPITEMIDLIST pidlBrowse;
   char *lpBuffer = (char*) hb_xgrab( MAX_PATH + 1 );
   SHGetSpecialFolderLocation( GetActiveWindow(), ISNIL(1) ? CSIDL_DRIVES : hb_parni(1), &pidlBrowse);
   SHGetPathFromIDList( pidlBrowse, lpBuffer );
   hb_retc( lpBuffer );
   hb_xfree( lpBuffer);
}

HB_FUNC( LISTVIEWSETBKIMAGE )
{
   HWND hWnd   = ( HWND ) hb_parnl( 1 );

   LVBKIMAGE lvbki;
   memset( ( char * ) &lvbki, 0, sizeof( lvbki ) );

   if( HB_ISCHAR(2) )
   {
      lvbki.ulFlags     = LVBKIF_SOURCE_URL; //|LVBKIF_FLAG_ALPHABLEND | LVBKIF_TYPE_WATERMARK | LVBKIF_FLAG_ALPHABLEND;
      lvbki.pszImage    = (LPTSTR) hb_parc(2);
   }
   else
   {
      lvbki.ulFlags     = LVBKIF_SOURCE_HBITMAP ; //| LVBKIF_TYPE_WATERMARK; // | LVBKIF_FLAG_ALPHABLEND;
      lvbki.hbm         = (HBITMAP) hb_parnl(2);
   }

   lvbki.xOffsetPercent = HB_ISNUM(3) ? hb_parni(3) : 0;
   lvbki.yOffsetPercent = HB_ISNUM(4) ? hb_parni(4) : 0;

   CoInitialize(NULL);
   hb_retl( ListView_SetBkImage( hWnd, &lvbki ) );
}


BOOL GetAppVersion( char *LibName, WORD *MajorVersion, WORD *MinorVersion, WORD *BuildNumber, WORD *RevisionNumber )
{
   DWORD dwHandle, dwLen;
   UINT BufLen;
   LPTSTR lpData;
   VS_FIXEDFILEINFO *pFileInfo;
   dwLen = GetFileVersionInfoSize( LibName, &dwHandle );
   if (!dwLen)
      return FALSE;

   lpData = (LPTSTR) malloc (dwLen);
   if (!lpData)
      return FALSE;

   if( !GetFileVersionInfo( LibName, dwHandle, dwLen, lpData ) )
   {
      free (lpData);
      return FALSE;
   }
   if( VerQueryValue( lpData, "\\", (LPVOID *) &pFileInfo, (PUINT)&BufLen ) )
   {
      *MajorVersion = HIWORD(pFileInfo->dwFileVersionMS);
      *MinorVersion = LOWORD(pFileInfo->dwFileVersionMS);
      *BuildNumber = HIWORD(pFileInfo->dwFileVersionLS);
      *RevisionNumber = LOWORD(pFileInfo->dwFileVersionLS);
      free (lpData);
      return TRUE;
   }
   free (lpData);
   return FALSE;
}

HB_FUNC( GETFILEVERSION )
{
   WORD MajorVersion;
   WORD MinorVersion;
   WORD BuildNumber;
   WORD RevisionNumber;
   if( GetAppVersion( (char*) hb_parc(1), &MajorVersion, &MinorVersion, &BuildNumber, &RevisionNumber ) )
   {
      char cVer[80];
      wsprintf( cVer, "%d.%d.%d.%d", MajorVersion, MinorVersion, BuildNumber, RevisionNumber );
      hb_retc( cVer );
   }
}
