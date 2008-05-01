

// Header control functions
// What32

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400
#ifndef _WIN32_IE
#define _WIN32_IE 0x0500
#endif
#include <windows.h>
#include <shlobj.h>
#include <commctrl.h>

#include "item.api"
#include "hbapi.h"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );

#if ( (defined(_MSC_VER) && _MSC_VER<=1200 && !defined(__POCC__)) || (defined(__GNUC__) || defined(__WATCOMC__)) || defined(__DMC__))
#define HDM_SETBITMAPMARGIN          (HDM_FIRST + 20)
#define Header_SetBitmapMargin(hwnd, iWidth) \
        (int)SNDMSG((hwnd), HDM_SETBITMAPMARGIN, (WPARAM)(iWidth), 0)
#define HDM_GETBITMAPMARGIN          (HDM_FIRST + 21)
#define Header_GetBitmapMargin(hwnd) \
        (int)SNDMSG((hwnd), HDM_GETBITMAPMARGIN, 0, 0)
#define HDM_SETFILTERCHANGETIMEOUT  (HDM_FIRST+22)
#define Header_SetFilterChangeTimeout(hwnd, i) \
        (int)SNDMSG((hwnd), HDM_SETFILTERCHANGETIMEOUT, 0, (LPARAM)(i))
#define HDM_EDITFILTER          (HDM_FIRST+23)
#define Header_EditFilter(hwnd, i, fDiscardChanges) \
        (int)SNDMSG((hwnd), HDM_EDITFILTER, (WPARAM)(i), MAKELPARAM(fDiscardChanges, 0))
#define HDM_CLEARFILTER         (HDM_FIRST+24)
#define Header_ClearFilter(hwnd, i) \
        (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)(i), 0)
#define Header_ClearAllFilters(hwnd) \
        (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)-1, 0)
#if (defined(__GNUC__) || defined(__WATCOMC__) || defined(__DMC__) )
#if defined(__DMC__)
#define HDM_SETHOTDIVIDER          (HDM_FIRST + 19)
#endif
#define Header_SetHotDivider(hwnd, fPos, dw) \
        (int)SNDMSG((hwnd), HDM_SETHOTDIVIDER, (WPARAM)(fPos), (LPARAM)(dw))
#endif
#if defined(__DMC__)
typedef struct _HD_ITEM
{
    UINT    mask;
    int     cxy;
    LPSTR   pszText;
    HBITMAP hbm;
    int     cchTextMax;
    int     fmt;
    LPARAM  lParam;
#if (_WIN32_IE >= 0x0300)
    int     iImage;
    int     iOrder;
#endif
#if (_WIN32_IE >= 0x0500)
    UINT    type;
    LPVOID  pvFilter;
#endif
} HDITEM, FAR * LPHDITEM;
#define HDM_CREATEDRAGIMAGE     (HDM_FIRST + 16)
#define Header_CreateDragImage(hwnd, i) \
        (HIMAGELIST)SNDMSG((hwnd), HDM_CREATEDRAGIMAGE, (WPARAM)(i), 0)
#define HDM_GETORDERARRAY       (HDM_FIRST + 17)
#define Header_GetOrderArray(hwnd, iCount, lpi) \
        (BOOL)SNDMSG((hwnd), HDM_GETORDERARRAY, (WPARAM)(iCount), (LPARAM)(lpi))
#define HDM_SETORDERARRAY       (HDM_FIRST + 18)
#define Header_SetOrderArray(hwnd, iCount, lpi) \
        (BOOL)SNDMSG((hwnd), HDM_SETORDERARRAY, (WPARAM)(iCount), (LPARAM)(lpi))
#define HDM_GETITEMRECT         (HDM_FIRST + 7)
#define Header_GetItemRect(hwnd, iItem, lprc) \
        (BOOL)SNDMSG((hwnd), HDM_GETITEMRECT, (WPARAM)(iItem), (LPARAM)(lprc))
#define Header_GetImageList(hwnd) \
        (HIMAGELIST)SNDMSG((hwnd), HDM_GETIMAGELIST, 0, 0)
#define HDM_SETIMAGELIST        (HDM_FIRST + 8)
#define Header_SetImageList(hwnd, himl) \
        (HIMAGELIST)SNDMSG((hwnd), HDM_SETIMAGELIST, 0, (LPARAM)(himl))
#define HDM_GETIMAGELIST        (HDM_FIRST + 9)
#define Header_GetImageList(hwnd) \
        (HIMAGELIST)SNDMSG((hwnd), HDM_GETIMAGELIST, 0, 0)
#define HDM_ORDERTOINDEX        (HDM_FIRST + 15)
#define Header_OrderToIndex(hwnd, i) \
        (int)SNDMSG((hwnd), HDM_ORDERTOINDEX, (WPARAM)(i), 0)
#define CCM_FIRST               0x2000
#define HDM_SETUNICODEFORMAT    CCM_SETUNICODEFORMAT
#define CCM_SETUNICODEFORMAT    (CCM_FIRST + 5)
#define CCM_GETUNICODEFORMAT    (CCM_FIRST + 6)
#define Header_SetUnicodeFormat(hwnd, fUnicode)  \
    (BOOL)SNDMSG((hwnd), HDM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0)
#define HDM_GETUNICODEFORMAT   CCM_GETUNICODEFORMAT
#define Header_GetUnicodeFormat(hwnd)  \
    (BOOL)SNDMSG((hwnd), HDM_GETUNICODEFORMAT, 0, 0)
#endif
#endif

//-----------------------------------------------------------------------------

// SYNTAX:
// Header_Create( nStyle, x, y, nWidth, nHeight, hWndParent, nId )

HB_FUNC( HEADER_CREATE )
{

   hb_retnl( (LONG) CreateWindow(  "SysHeader32"         ,
                                   ""                    ,
                                   (DWORD) hb_parnl( 1 ) ,
                                   hb_parni( 2 )         ,
                                   hb_parni( 3 )         ,
                                   hb_parni( 4 )         ,
                                   hb_parni( 5 )         ,
                                   (HWND) hb_parnl( 6 )  ,
                                   (HMENU) hb_parni( 7 ) ,
                                   GetModuleHandle(NULL) ,
                                   NULL ) ) ;
}


//-----------------------------------------------------------------------------
// Header_GetItemCount(hwndHD) (int)SNDMSG((hwndHD), HDM_GETITEMCOUNT, 0, 0L)

HB_FUNC( HEADER_GETITEMCOUNT )
{
   hb_retni( Header_GetItemCount( (HWND) hb_parnl(1) ) );
}


//-----------------------------------------------------------------------------
// Header_InsertItem(hwndHD, i, phdi) (int)SNDMSG((hwndHD), HDM_INSERTITEM, (WPARAM)(int)(i), (LPARAM)(const HD_ITEM FAR*)(phdi))

HB_FUNC( HEADER_INSERTITEM )
{
   HDITEM *hdi = ( HDITEM *) hb_param( 3, HB_IT_STRING )->item.asString.value ;
   hb_retni( Header_InsertItem( (HWND) hb_parnl(1), hb_parni(2), hdi ) ) ;
}


//-----------------------------------------------------------------------------
// Header_DeleteItem(hwndHD, i) (BOOL)SNDMSG((hwndHD), HDM_DELETEITEM, (WPARAM)(int)(i), 0L)

HB_FUNC( HEADER_DELETEITEM  )
{
   hb_retl( Header_DeleteItem( (HWND) hb_parnl(1), hb_parni(2) ) ) ;
}


//-----------------------------------------------------------------------------
// Header_GetItem(hwndHD, i, phdi) (BOOL)SNDMSG((hwndHD), HDM_GETITEM, (WPARAM)(int)(i), (LPARAM)(HD_ITEM FAR*)(phdi))

// SYNTAX
// structbuffer := Header_GetItem(hWnd,i) -> cBuff or NIL

HB_FUNC( HEADER_GETITEM )
{
   HDITEM hdi ;
   BOOL lRet = Header_GetItem( (HWND) hb_parnl(1), hb_parni(2), &hdi ) ;
   if ( lRet )
       hb_retclen( (char*) &hdi, sizeof(HDITEM) ) ;
      //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) hdi, sizeof( HDITEM ) );

}


//-----------------------------------------------------------------------------
// Header_SetItem(hwndHD, i, phdi) (BOOL)SNDMSG((hwndHD), HDM_SETITEM, (WPARAM)(int)(i), (LPARAM)(const HD_ITEM FAR*)(phdi))

HB_FUNC( HEADER_SETITEM )
{
   HDITEM *hdi = ( HDITEM * ) hb_param( 3, HB_IT_STRING )->item.asString.value ;
   hb_retl( Header_SetItem( (HWND) hb_parnl(1), hb_parni(2), hdi ) ) ;
}


//-----------------------------------------------------------------------------
// Header_CreateDragImage(hwnd, i) (HIMAGELIST)SNDMSG((hwnd), HDM_CREATEDRAGIMAGE, (WPARAM)(i), 0)

HB_FUNC( HEADER_CREATEDRAGIMAGE )
{

   hb_retnl( (ULONG) Header_CreateDragImage( (HWND) hb_parnl(1), hb_parni(2) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_GetOrderArray(hwnd, iCount, lpi) (BOOL)SNDMSG((hwnd), HDM_GETORDERARRAY, (WPARAM)(iCount), (LPARAM)(lpi))

// SYNTAX:
// aOrder := Header_GetOrderArray( hWnd )

HB_FUNC( HEADER_GETORDERARRAY )
{

  UINT iCount = Header_GetItemCount((HWND)hb_parnl(1) );
  PHB_ITEM aInt ;
  PHB_ITEM temp ;
  INT *lpi = (INT*) hb_xgrab( iCount*sizeof(INT)) ;
  BOOL lRet = Header_GetOrderArray((HWND) hb_parnl(1), iCount, lpi )  ;
  UINT i;

  if ( lRet )
  {
      aInt  = _itemArrayNew(iCount ) ;
      for ( i = 0; i<iCount ; i++)
      {
        temp = _itemPutNL( NULL, lpi[i] );
        hb_arraySet( aInt, i+1, temp );
        _itemRelease( temp );
      }

      _itemReturn(aInt);
      _itemRelease(aInt);
      hb_xfree( lpi ) ;
  }
}


//-----------------------------------------------------------------------------
// Header_SetOrderArray(hwnd, iCount, lpi) (BOOL)SNDMSG((hwnd), HDM_SETORDERARRAY, (WPARAM)(iCount), (LPARAM)(lpi))
// lparam = int array of size HDM_GETITEMCOUNT
// the array specifies the order that all items should be displayed.
// e.g.  { 2, 0, 1}
// says the index 2 item should be shown in the 0ths position
//      index 0 should be shown in the 1st position
//      index 1 should be shown in the 2nd position

// SYNTAX:
// Header_SetOrderArray(hWnd,aOrder)


HB_FUNC(  HEADER_SETORDERARRAY )
{
   UINT iCount ;
   INT *lpi    ;
   UINT i      ;

   if( hb_parinfo( 2 ) == HB_IT_ARRAY  )
      {

          iCount = hb_parinfa( 2, 0 );
          lpi = (INT*) hb_xgrab( iCount*sizeof(INT) ) ;
          for ( i= 0 ; i<iCount ; i++ )
          {
             lpi[i] = hb_parni(2,i+1) ;
          }

          hb_retl( Header_SetOrderArray((HWND) hb_parnl(1), iCount, lpi ) ) ;
      }
   else
      hb_retl( 0 );

}


//-----------------------------------------------------------------------------
// Header_GetItemRect(hwnd, iItem, lprc) (BOOL)SNDMSG((hwnd), HDM_GETITEMRECT, (WPARAM)(iItem), (LPARAM)(lprc))


// SYNTAX:
// aRect:=Header_GetItemRect(hWnd,i)

HB_FUNC( HEADER_GETITEMRECT )
{
   RECT rc ;
   PHB_ITEM aRc ;

   if ( Header_GetItemRect((HWND) hb_parnl(1), (WPARAM) hb_parni(2), &rc ) )
   {
      aRc = Rect2Array( &rc );
      _itemReturn( aRc );
      _itemRelease( aRc );

   }

}


//-----------------------------------------------------------------------------
// Header_GetImageList(hwnd) (HIMAGELIST)SNDMSG((hwnd), HDM_GETIMAGELIST, 0, 0)


HB_FUNC( HEADER_GETIMAGELIST )
{

    hb_retnl( (ULONG)Header_GetImageList((HWND) hb_parnl(1) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_SetImageList(hwnd, himl) (HIMAGELIST)SNDMSG((hwnd), HDM_SETIMAGELIST, 0, (LPARAM)(himl))

HB_FUNC( HEADER_SETIMAGELIST )
{

   hb_retnl( (ULONG) Header_SetImageList((HWND) hb_parnl(1), (LPARAM) hb_parnl(2) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_OrderToIndex(hwnd, i) (int)SNDMSG((hwnd), HDM_ORDERTOINDEX, (WPARAM)(i), 0)

HB_FUNC( HEADER_ORDERTOINDEX )
{

   hb_retni( Header_OrderToIndex((HWND) hb_parnl(1), hb_parni(2) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_SetHotDivider(hwnd, fPos, dw) (int)SNDMSG((hwnd), HDM_SETHOTDIVIDER, (WPARAM)(fPos), (LPARAM)(dw))
// convenience message for external dragdrop
// wParam = BOOL  specifying whether the lParam is a dwPos of the cursor
//              position or the index of which divider to hotlight
// lParam = depends on wParam  (-1 and wParm = FALSE turns off hotlight)

HB_FUNC( HEADER_SETHOTDIVIDER )
{

   hb_retni( Header_SetHotDivider((HWND) hb_parnl(1), hb_parl(2), (LPARAM) hb_parnl(3)));

}


//-----------------------------------------------------------------------------
// Header_SetBitmapMargin(hwnd, iWidth) (int)SNDMSG((hwnd), HDM_SETBITMAPMARGIN, (WPARAM)(iWidth), 0)

HB_FUNC( HEADER_SETBITMAPMARGIN )
{

   hb_retni( Header_SetBitmapMargin( (HWND) hb_parnl(1), hb_parni(2) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_GetBitmapMargin(hwnd) (int)SNDMSG((hwnd), HDM_GETBITMAPMARGIN, 0, 0)

HB_FUNC( HEADER_GETBITMAPMARGIN )
{

   hb_retni( Header_GetBitmapMargin( (HWND) hb_parnl(1) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_SetUnicodeFormat(hwnd, fUnicode) (BOOL)SNDMSG((hwnd), HDM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0)

HB_FUNC( HEADER_SETUNICODEFORMAT )
{

   hb_retl( Header_SetUnicodeFormat( (HWND) hb_parnl(1), hb_parl( 2 ) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_GetUnicodeFormat(hwnd) (BOOL)SNDMSG((hwnd), HDM_GETUNICODEFORMAT, 0, 0)

HB_FUNC( HEADER_GETUNICODEFORMAT )
{

   hb_retl( Header_GetUnicodeFormat((HWND) hb_parnl(1) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_SetFilterChangeTimeout(hwnd, i) (int)SNDMSG((hwnd), HDM_SETFILTERCHANGETIMEOUT, 0, (LPARAM)(i))

HB_FUNC( HEADER_SETFILTERCHANGETIMEOUT )
{

   hb_retni( Header_SetFilterChangeTimeout((HWND) hb_parnl(1), hb_parni( 2 ) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_EditFilter(hwnd, i, fDiscardChanges) (int)SNDMSG((hwnd), HDM_EDITFILTER, (WPARAM)(i), MAKELPARAM(fDiscardChanges, 0))

HB_FUNC( HEADER_EDITFILTER )
{

   hb_retni( Header_EditFilter( (HWND) hb_parnl(1), hb_parni( 2 ), hb_parl( 3 ) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_ClearAllFilters(hwnd) (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)-1, 0)

HB_FUNC( HEADER_CLEARALLFILTERS )
{

   hb_retni( Header_ClearAllFilters( (HWND) hb_parnl(1) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_ClearFilter(hwnd, i) (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)(i), 0)
// Clear filter takes -1 as a column value to indicate that all
// the filter should be cleared.  When this happens you will
// only receive a single filter changed notification.

HB_FUNC( HEADER_CLEARFILTER )
{

   hb_retni( Header_ClearFilter( (HWND) hb_parnl(1), hb_parni( 2 ) ) ) ;

}


//-----------------------------------------------------------------------------
// Header_Layout(hwndHD, playout) (BOOL)SNDMSG((hwndHD), HDM_LAYOUT, 0, (LPARAM)(HD_LAYOUT FAR*)(playout))

HB_FUNC( HEADER_LAYOUT )
{
   HD_LAYOUT *hdLayout = ( HD_LAYOUT *) hb_param( 2, HB_IT_STRING )->item.asString.value ;
   hb_retl( Header_Layout( (HWND) hb_parnl(1), hdLayout ) );
}


//--------- eof.
//

