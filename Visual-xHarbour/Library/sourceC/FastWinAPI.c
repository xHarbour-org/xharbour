/*
 * $Id$
 */
#define HB_OS_WIN_32_USED

#include <windows.h>
#include <commdlg.h>
#include <shlobj.h>
#include <wininet.h>

#include "item.api"
#include "hbdefs.h"
#include "hbvmpub.h"
#include "hbinit.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbvm.h"
#include "hbapierr.h"
#include "hbpcode.h"
#include "winreg.h"

//-----------------------------------------------------------------------------
//---------------------------- INTERNAL API -----------------------------------
//-----------------------------------------------------------------------------

PHB_ITEM Rect2Array( RECT *rc  )
{
   PHB_ITEM aRect = hb_itemArrayNew(4);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aRect, 1, hb_itemPutNL(element, rc->left));
   hb_arraySet(aRect, 2, hb_itemPutNL(element, rc->top));
   hb_arraySet(aRect, 3, hb_itemPutNL(element, rc->right));
   hb_arraySet(aRect, 4, hb_itemPutNL(element, rc->bottom));
   hb_itemRelease(element);
   return aRect;
}

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

BOOL Array2Point(PHB_ITEM aPoint, POINT *pt )
{
   if (HB_IS_ARRAY(aPoint) && hb_arrayLen(aPoint) == 2) {
      pt->x = hb_arrayGetNL(aPoint,1);
      pt->y = hb_arrayGetNL(aPoint,2);
      return TRUE ;
   }
   return FALSE;
}

PHB_ITEM Point2Array( POINT *pt  )
{
   PHB_ITEM aPoint = hb_itemArrayNew(2);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aPoint, 1, hb_itemPutNL(element, pt->x));
   hb_arraySet(aPoint, 2, hb_itemPutNL(element, pt->y));
   hb_itemRelease(element);
   return aPoint;
}

BOOL Array2Size(PHB_ITEM aSize, SIZE *siz )
{
   if (HB_IS_ARRAY(aSize) && hb_arrayLen(aSize) == 2) {
      siz->cx = hb_arrayGetNL(aSize,1);
      siz->cy = hb_arrayGetNL(aSize,2);
      return TRUE ;
   }
   return FALSE;
}

PHB_ITEM Size2Array( SIZE *siz  )
{
   PHB_ITEM aSize = hb_itemArrayNew(2);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aSize, 1, hb_itemPutNL(element, siz->cx));
   hb_arraySet(aSize, 2, hb_itemPutNL(element, siz->cy));
   hb_itemRelease(element);
   return aSize;
}

void  Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect )
{
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aRect, 1, hb_itemPutNL(element, rc->left));
   hb_arraySet(aRect, 2, hb_itemPutNL(element, rc->top));
   hb_arraySet(aRect, 3, hb_itemPutNL(element, rc->right));
   hb_arraySet(aRect, 4, hb_itemPutNL(element, rc->bottom));
   hb_itemRelease(element);
}

void Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint)
{

   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aPoint, 1, hb_itemPutNL(element, pt->x));
   hb_arraySet(aPoint, 2, hb_itemPutNL(element, pt->y));
   hb_itemRelease(element);

}

void Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize )
{
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aSize, 1, hb_itemPutNL(element, siz->cx));
   hb_arraySet(aSize, 2, hb_itemPutNL(element, siz->cy));
   hb_itemRelease(element);

}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

HB_FUNC( _COPYRECT )
{
   RECT   lprcDst ;
   RECT   lprcSrc ;
   if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc ))
      {
      if ( CopyRect( &lprcDst, &lprcSrc ) ){
          hb_itemRelease(hb_itemReturnForward(Rect2Array( &lprcDst)));

      }
      else
         hb_ret();
      }
   else
     hb_ret();

}

//-----------------------------------------------------------------------------
HB_FUNC( _INFLATERECT )
{
   RECT lprc ;
   PHB_ITEM pArray=hb_param( 1, HB_IT_ARRAY );

   if ( Array2Rect( pArray , &lprc ))
   {
      if ( InflateRect( &lprc, hb_parni( 2 ), hb_parni( 3 ) ) )
      {
         Rect2ArrayEx( &lprc,pArray );
         hb_retl( TRUE ) ;
      }
      else
         hb_retl(FALSE);
   }
   else
      hb_retl(FALSE);

}

//-----------------------------------------------------------------------------
HB_FUNC( _INTERSECTRECT )
{
   RECT   lprcDst  ;
   RECT   lprcSrc1 ;
   RECT   lprcSrc2 ;

   if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc1 )  && Array2Rect(hb_param( 2, HB_IT_ARRAY ) , &lprcSrc2 ))
   {
      if (IntersectRect( &lprcDst, &lprcSrc1, &lprcSrc2 ) )
        hb_itemRelease(hb_itemReturnForward(Rect2Array( &lprcDst)));
     else
        hb_ret();

   }
   else
      hb_ret();

}

//-----------------------------------------------------------------------------
HB_FUNC( _UNIONRECT )
{
   RECT lprcDst  ;
   RECT   lprcSrc1 ;
   RECT   lprcSrc2 ;


  if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc1 )  && Array2Rect(hb_param( 2, HB_IT_ARRAY ) , &lprcSrc2 ))
   {
      if (UnionRect( &lprcDst, &lprcSrc1, &lprcSrc2 ) )
         {
         hb_itemRelease(hb_itemReturnForward(Rect2Array( &lprcDst)));
      }
      else
         hb_ret();
   }
   else
      hb_ret();
}

//-----------------------------------------------------------------------------
HB_FUNC( _SUBTRACTRECT )
{
   RECT lprcDst  ;
   RECT   lprcSrc1 ;
   RECT   lprcSrc2 ;

  if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc1 )  && Array2Rect(hb_param( 2, HB_IT_ARRAY ) , &lprcSrc2 ))
   {
      if (SubtractRect( &lprcDst, &lprcSrc1, &lprcSrc2 ))
      {
         hb_itemRelease(hb_itemReturnForward(Rect2Array(&lprcDst)));

      }
      else
         hb_ret();
   }
   else
      hb_ret();
}

//-----------------------------------------------------------------------------
HB_FUNC( _OFFSETRECT )
{
   RECT lprc ;
   PHB_ITEM pSrc1=hb_param( 1, HB_IT_ARRAY );

   if (ISARRAY(1) && Array2Rect( pSrc1, &lprc))
   {
       if(OffsetRect( &lprc, hb_parni( 2 ), hb_parni( 3 ) ))
         {
           Rect2ArrayEx(&lprc,pSrc1);
           hb_retl(TRUE);
         }
       else
           hb_retl(FALSE);
      }
   else
      hb_retl(FALSE);
}

//-----------------------------------------------------------------------------
HB_FUNC( _ISRECTEMPTY )
{
   RECT lprc ;
   PHB_ITEM pSrc1=hb_param( 1, HB_IT_ARRAY );

   if (ISARRAY(1) && Array2Rect( pSrc1, &lprc))
   {
      hb_retl( IsRectEmpty( &lprc ) ) ;
   }
   else
      hb_retl(FALSE);

}

//-----------------------------------------------------------------------------
HB_FUNC( _EQUALRECT )
{
   RECT lprc1 ;
   RECT lprc2 ;
   PHB_ITEM pSrc1=hb_param( 1 ,HB_IT_ARRAY ),pSrc2=hb_param( 2 ,HB_IT_ARRAY );

   if (Array2Rect( pSrc1, &lprc1) && Array2Rect( pSrc2, &lprc2))
   {
      hb_retl( EqualRect( &lprc1, &lprc2 ) ) ;
   }
   else
      hb_retl(FALSE);
}

//-----------------------------------------------------------------------------
HB_FUNC( _PTINRECT )
{
   RECT  lprc ;
   POINT pt   ;
   PHB_ITEM pSrc1=hb_param( 1, HB_IT_ARRAY ),pSrc2=hb_param( 2, HB_IT_ARRAY );

   if (Array2Rect( pSrc1, &lprc) && Array2Point( pSrc2, &pt))
   {
      hb_retl( (BOOL) PtInRect( &lprc, pt ) ) ;

   }
   else
      hb_retl( FALSE) ;

}

//-----------------------------------------------------------------------------
HB_FUNC( _WINDOWFROMPOINT )
{
   POINT pt;
   PHB_ITEM pSrc = hb_param( 1, HB_IT_ARRAY );
   Array2Point( pSrc, &pt );
   hb_retnl( (long) WindowFromPoint( pt ) ) ;
}

//-----------------------------------------------------------------------------
HB_FUNC( _RECTVISIBLE )
{
   RECT rc;

   if ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
      hb_retl( RectVisible( (HDC) hb_parnl( 1 ), &rc ) ) ;
   else
      hb_retl(0);

}

//-----------------------------------------------------------------------------
HB_FUNC( _VALIDATERECT )
{
   RECT rc ;

   if (ISARRAY( 2 ) && Array2Rect( hb_param( 2, HB_IT_ARRAY ), &rc ) )
      hb_retl( ValidateRect( (HWND) hb_parnl( 1 ), &rc) ) ;
   else
      hb_retl(ValidateRect( (HWND) hb_parnl( 1 ),NULL));
}

//-----------------------------------------------------------------------------
HB_FUNC ( _INVALIDATERECT )
{
   RECT rc;
   BOOL bRectOk ;

   bRectOk = ( ISARRAY( 2 )  &&   Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ) ) ;

   hb_retl( InvalidateRect( ISNIL(1) ? NULL : (HWND) hb_parnl( 1 ), bRectOk ? &rc : NULL,
                           ISLOG(3) ? hb_parl( 3 ) : TRUE ) );
}


//-----------------------------------------------------------------------------
HB_FUNC ( _GETCLIENTRECT )
{
   RECT rc;
   PHB_ITEM aMetr ;
   GetClientRect( (HWND) hb_parnl( 1 ), &rc );

   aMetr = Rect2Array( &rc  );
   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}

//-----------------------------------------------------------------------------
HB_FUNC ( _GETWINDOWRECT )
{
   RECT rc;
   PHB_ITEM aMetr ;
   GetWindowRect( (HWND) hb_parnl( 1 ),   &rc );

   aMetr = Rect2Array( &rc  );
   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}


//-----------------------------------------------------------------------------
HB_FUNC( _SETSCROLLINFO )
{
   hb_retni( SetScrollInfo( (HWND) hb_parnl(1), hb_parni(2), (SCROLLINFO*) hb_parc(3), hb_parl(4) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC ( _GETTEXTEXTENTPOINT32 )
{
   const char * pstr = hb_parcx(2);
   SIZE sz;
   PHB_ITEM aMetr ;
   if ( GetTextExtentPoint32( (HDC) hb_parnl(1), pstr, strlen(pstr), &sz ) )
   {
      aMetr = Size2Array( &sz ) ;
      hb_itemReturn( aMetr );
      hb_itemRelease( aMetr );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC ( _GETTEXTEXTENTEXPOINT )
{
   int iFit;
   SIZE sz;
   PHB_ITEM aMetr ;

   if ( GetTextExtentExPoint( (HDC) hb_parnl(1), (LPCTSTR) hb_parc(2), (int) hb_parclen(2), hb_parni(3), &iFit, NULL, &sz ) )
   {
      hb_storni( iFit, 4 );
      aMetr = Size2Array( &sz ) ;
      hb_itemReturn( aMetr );
      hb_itemRelease( aMetr );
   }
}
//-----------------------------------------------------------------------------
HB_FUNC( _POLYLINE )
{

   POINT * Point ;
   POINT pt ;
   DWORD iCount ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
       iCount = (DWORD) hb_parinfa( 2, 0 ) ;
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) ) ;
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
               hb_retl(0);
               hb_xfree(Point);
               hb_itemRelease( aSub );
               return ;
           }
       }

       hb_retl( Polyline( (HDC) hb_parnl( 1 ), Point, iCount ) ) ;
       hb_xfree(Point);

   }
   else
   {
      hb_retl( 0 );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _POLYGON )
{
   POINT * Point ;
   POINT pt ;
   int iCount ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
      iCount = (int) hb_parinfa( 2, 0 ) ;
      Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) ) ;
      aParam = hb_param(2,HB_IT_ARRAY);

      for ( i = 0 ; i<iCount ; i++ )
      {
         aSub = hb_itemArrayGet( aParam, i+1 );

         if ( Array2Point(aSub, &pt ))
         {
              *(Point+i) = pt ;
              hb_itemRelease( aSub );
         }
         else
         {
           hb_retl(0);
           hb_xfree(Point);
           hb_itemRelease( aSub );
           return ;
         }
      }

      hb_retl( Polygon( (HDC) hb_parnl( 1 ), Point, iCount ) ) ;
      hb_xfree(Point);
   }
   else
    hb_retl( 0 );
}

//-----------------------------------------------------------------------------
HB_FUNC( _CREATEPOLYGONRGN )
{

   POINT * pPoint ;
   POINT pt ;
   DWORD iCount ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;
   if (ISARRAY( 1 ) )
   {
       iCount = (DWORD) hb_parinfa( 1, 0 ) ;
       pPoint = (POINT *) hb_xgrab( iCount * sizeof (POINT) ) ;
       aParam = hb_param(1,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(pPoint+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
               hb_xfree(pPoint);
               hb_itemRelease( aSub );
               return ;
           }
       }

       hb_retnl( (LONG) CreatePolygonRgn( pPoint, iCount, hb_parni(2) ) ) ;
       hb_xfree(pPoint);
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _EXTTEXTOUT )
{
   RECT  rc    ;
   INT * lpDx  ;
   BOOL rcOk   ;
   UINT iCount ;
   UINT i      ;
   const char * cText = hb_parcx( 6 );

   rcOk = ( ISARRAY(5) && Array2Rect(hb_param(5, HB_IT_ARRAY), &rc) ) ;

   if ( ISARRAY(7) )
   {
       iCount = hb_parinfa(7,0) ;
       lpDx = (INT *) hb_xgrab( iCount * sizeof( INT ) ) ;
       for ( i=0 ; i < iCount ; i++ )
       {
          *(lpDx+i) = hb_parni( 7,i+1) ;
       }
   }

   hb_retl( ExtTextOut( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (UINT) hb_parni(4), rcOk ? &rc : NULL, (LPCSTR) cText, (UINT) strlen( cText ), ISARRAY(7) ? lpDx : NULL ) );

   if (ISARRAY(7))
   {
       hb_xfree(lpDx) ;
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _DRAWFRAMECONTROL )
{
   RECT lpRect ;
   PHB_ITEM pArray=hb_param(2,HB_IT_ARRAY);

   if (Array2Rect(pArray,&lpRect))
   {
      if( DrawFrameControl( (HDC) hb_parnl(1), &lpRect, (UINT) hb_parni(3), (UINT) hb_parni(4) ) )
      {
         Rect2ArrayEx(&lpRect,pArray);
         hb_retl(TRUE);
      }
      else
         hb_retl( FALSE ) ;
   }
   else
   {
      hb_retl( FALSE ) ;
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _CLIENTTOSCREEN )
{
   POINT Point ;
   PHB_ITEM pArray;
   pArray=  hb_param( 2 , HB_IT_ARRAY );
   if (Array2Point( pArray ,&Point  ) )
   {
      if (ClientToScreen( (HWND) hb_parnl( 1 ), &Point ))
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE ) ;
      }
      else
         hb_retl( FALSE ) ;
   }
   else
   {
      hb_retl( FALSE ) ;
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _SCREENTOCLIENT )
{
   POINT Point ;
   PHB_ITEM pArray = hb_param( 2 , HB_IT_ARRAY );

   if (Array2Point(pArray, &Point ) )
   {
      if( ScreenToClient( (HWND) hb_parnl( 1 ), &Point ) >0)
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE ) ;
      }
      else
      {
         hb_retl( FALSE ) ;
      }
   }
   else
   {
      hb_retl( FALSE ) ;
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _DRAWFOCUSRECT )
{
   RECT lprc ;

   if (ISARRAY(2) && Array2Rect( hb_param( 2 ,HB_IT_ARRAY ) , &lprc ) )
   {
      hb_retl( DrawFocusRect( (HDC) hb_parnl( 1 ), &lprc ) ) ;
   }
   else
   {
      hb_retl(FALSE);
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _GETSCROLLINFO )
{
   SCROLLINFO si ;
   si.cbSize = sizeof(SCROLLINFO) ;
   si.fMask  = SIF_ALL ;

   if ( GetScrollInfo( (HWND) hb_parnl( 1 ), hb_parni( 2 ), &si ) )
   {
      hb_retclen( (char *) &si, sizeof( SCROLLINFO ) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _SCROLLWINDOW )
{
   RECT lpRect     ;
   RECT lpClipRect ;

   if (ISARRAY( 4 ) )
   Array2Rect( hb_param( 4 , HB_IT_ARRAY ) , &lpRect ) ;
   if (ISARRAY( 5 ) )
   Array2Rect( hb_param( 5 , HB_IT_ARRAY ) , &lpClipRect ) ;

   hb_retl( ScrollWindow( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), ISARRAY(4) ? &lpRect : NULL, ISARRAY(5) ? &lpClipRect : NULL ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( _GETCURSORPOS )
{
   POINT Point ;
   PHB_ITEM gcPos ;
   if ( GetCursorPos( &Point ) )
   {
      gcPos = Point2Array( &Point);
      hb_itemReturn( gcPos );
      hb_itemRelease( gcPos );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _BEGINPAINT )
{
   PAINTSTRUCT pps ;

   hb_retnl( (LONG) BeginPaint( (HWND) hb_parnl( 1 ), &pps ) ) ;
   hb_storclen( (char *) &pps, sizeof(PAINTSTRUCT), 2 );
}

//-----------------------------------------------------------------------------
HB_FUNC( _ENDPAINT )
{
   hb_retl( EndPaint( (HWND) hb_parnl( 1 ), (PAINTSTRUCT*) hb_parcx( 2 ) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( _GETTEXTMETRICS )
{
   TEXTMETRIC tm;

   if ( GetTextMetrics( (HDC) hb_parnl( 1 ), &tm ) )
   {
      hb_retclen( (char *) &tm, sizeof( TEXTMETRIC ) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _SENDMESSAGE )
{
   char *cText ;

   if( ISBYREF(4) )
   {
      cText = (char*) hb_xgrab( hb_parcsiz(4) );

      hb_xmemcpy( cText, hb_parcx(4), hb_parcsiz(4) );
   }

   hb_retnl( (ULONG) SendMessage( (HWND) hb_parnl(1), (UINT) hb_parni(2), (ISNIL(3) ? 0 : (WPARAM) hb_parnl(3)),
                                  (ISNIL(4) ? 0 : ( ISBYREF(4)? (LPARAM) (LPSTR) cText : ( ISCHAR(4) ? (LPARAM)(LPSTR) hb_parcx(4) : (LPARAM) hb_parnl(4) ) ) ) ) );

   if( ISBYREF( 4 ) )
   {
      hb_storclen( cText, hb_parcsiz(4), 4 ) ;
      hb_xfree( cText );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _POSTMESSAGE )
{
   char *cText ;
   if (ISBYREF(4))
   {
      cText = (char*) hb_xgrab( hb_parcsiz(4) );
      hb_xmemcpy( cText, hb_parcx(4), hb_parcsiz(4) );
   }
   hb_retnl( (ULONG) PostMessage( (HWND) hb_parnl(1), (UINT) hb_parni(2), (ISNIL(3) ? 0 : (WPARAM) hb_parnl(3)),
                                  (ISNIL(4) ? 0 : ( ISBYREF(4)? (LPARAM) (LPSTR) cText : ( ISCHAR(4) ? (LPARAM)(LPSTR) hb_parcx(4) : (LPARAM) hb_parnl(4) ) ) ) ) );

   if( ISBYREF( 4 ) )
   {
      hb_storclen( cText, hb_parcsiz(4), 4 ) ;
      hb_xfree( cText );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _FILLRECT )
{
  RECT rc ;
  if ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
  {
     hb_retni( FillRect((HDC) hb_parnl( 1 ), &rc, (HBRUSH) hb_parnl( 3 ) ) ) ;
  }
  else
  {
     hb_retni( FillRect((HDC) hb_parnl( 1 ), (RECT*) hb_parc(2), (HBRUSH) hb_parnl( 3 ) ) ) ;
  }
}

//-----------------------------------------------------------------------------
HB_FUNC( _DRAWEDGE )
{
  RECT rc ;
  if ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
  {
     hb_retni( DrawEdge( (HDC) hb_parnl(1), &rc, (UINT) hb_parnl(3), (UINT) hb_parnl(4) ) ) ;
  }
  else
  {
     hb_retni(0);
  }
}

//-----------------------------------------------------------------------------
HB_FUNC( _DRAWTEXT )
{
   const char *cText = hb_parcx( 2 );
   RECT rc;

   if ( ISARRAY( 3 ) && Array2Rect( hb_param( 3, HB_IT_ARRAY ), &rc ) ) {
      hb_retl( DrawText( (HDC) hb_parnl(1), (LPCTSTR) cText, strlen( cText ), &rc, ISNIL(4) ? DT_LEFT : hb_parni(4) ) );
      if ( ISBYREF( 3 )) {
         hb_stornl( rc.left  , 3, 1 ) ;
         hb_stornl( rc.top   , 3, 2 ) ;
         hb_stornl( rc.right , 3, 3 ) ;
         hb_stornl( rc.bottom, 3, 4 ) ;
      }
   }
   else
   {
       hb_retl(0);
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _GETSCROLLBARINFO )
{
   SCROLLBARINFO sbi     ;
   sbi.cbSize = sizeof( sbi) ;
   if ( GetScrollBarInfo( (HWND) hb_parnl( 1 ), hb_parnl( 2 ), &sbi ) )
   {
       hb_retclen( (char *) &sbi, sizeof( SCROLLBARINFO ) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( _CREATEFONT )
{

   if ( ISARRAY(1))
   {
      hb_retnl( (LONG) CreateFont( hb_parni(1,1), hb_parni(1,2), hb_parni(1,3), hb_parni(1,4), hb_parni(1,5),
                                  (DWORD) hb_parnl(1,6), (DWORD) hb_parnl(1,7), (DWORD) hb_parnl(1,8),
                                  (DWORD) hb_parnl(1,9), (DWORD) hb_parnl(1,10), (DWORD) hb_parnl(1,11),
                                  (DWORD) hb_parnl(1,12), (DWORD) hb_parnl(1,13), (LPCSTR) hb_parcx(1,14) ) );
   }
   else
   {
      hb_retnl( (LONG) CreateFont( ISNIL(1) ? 0 : hb_parni(1), ISNIL(2) ? 0 : hb_parni(2), ISNIL(3) ? 0 : hb_parni(3),
                                    ISNIL(4) ? 0 : hb_parni(4), ISNIL(5) ? 0 : hb_parni(5), (DWORD) hb_parnl(6),
                                    (DWORD) hb_parnl(7), (DWORD) hb_parnl(8), (DWORD) hb_parnl(9), (DWORD) hb_parnl(10),
                                    (DWORD) hb_parnl(11), (DWORD) hb_parnl(12), (DWORD) hb_parnl(13), (LPCSTR) hb_parcx(14) ) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC ( _REDRAWWINDOW )
{
   RECT rc ;
   BOOL bRectOk ;
   bRectOk = ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ) ) ;
   hb_retl( RedrawWindow( (HWND) hb_parnl(1), bRectOk ? &rc : NULL, ISNIL(3) ? NULL : (HRGN) hb_parnl(3), hb_parni(4) ) );
}

//---------------------------------------------------------------------
HB_FUNC( _CHOOSEFONT )
{
  CHOOSEFONT *cf = (CHOOSEFONT * ) hb_parc(1);
  cf->lStructSize = sizeof(CHOOSEFONT);

  if (ChooseFont( cf ) )
  {
     hb_retclen( (char *) cf, sizeof( CHOOSEFONT )) ;
     hb_retl(TRUE);
  }
  else
     hb_retl(FALSE);
}

//----------------------------------------------------------------------------
HB_FUNC( _CHOOSECOLOR )
{
   CHOOSECOLOR cc ;
   COLORREF crCustClr[16] ;
   int i ;
   BOOL bRet ;

   for( i = 0 ; i <16 ; i++ )
     crCustClr[i] = (ISARRAY(3) ? hb_parnl(3,i+1) : RGB(0,0,0)) ;// GetSysColor(COLOR_BTNFACE)) ;

   cc.lStructSize    = sizeof( CHOOSECOLOR ) ;
   cc.hwndOwner      = ISNIL(1) ? GetActiveWindow():(HWND) hb_parnl(1) ;
   cc.rgbResult      = ISNIL(2) ? (COLORREF) 0 : (COLORREF) hb_parnl(2) ;
   cc.lpCustColors   = crCustClr ;
   cc.Flags          = (WORD) (ISNIL(4) ? CC_ANYCOLOR | CC_FULLOPEN | CC_RGBINIT : hb_parnl(4) ) ;
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

//----------------------------------------------------------------------------
HB_FUNC( _GETWINDOWTEXT )
{
   int   iLen  = GetWindowTextLength( (HWND) hb_parnl(1) );
   char *cText = (char*) hb_xgrab( iLen + 1 );
   int   iRet  = GetWindowText( (HWND) hb_parnl(1), (LPSTR) cText, iLen + 1 );

   hb_retclenAdopt( cText, iRet );
}

//TODO - standardize!!!
//----------------------------------------------------------------------------
HB_FUNC( DATETIME_GETRANGE )
{
   LPSYSTEMTIME lpSysTimeArray = (SYSTEMTIME *) hb_xgrab( 2 * sizeof(SYSTEMTIME)) ;
   PHB_ITEM aMinMaxDate, aMinDate, aMaxDate ;
   PHB_ITEM temp ;
   DWORD dwRet ;

   dwRet = DateTime_GetRange( (HWND) hb_parnl( 1 ), (SYSTEMTIME *)lpSysTimeArray ) ;

   if( ISBYREF(2) )
   {
      hb_stornl(dwRet,2);
   }

   aMinMaxDate = hb_itemArrayNew( 2 ) ;
   aMinDate    = hb_itemArrayNew( 8 ) ;

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wYear );
   hb_arraySet( aMinDate, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wMonth );
   hb_arraySet( aMinDate, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wDayOfWeek );
   hb_arraySet( aMinDate, 3, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wDay );
   hb_arraySet( aMinDate, 4, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wHour );
   hb_arraySet( aMinDate, 5, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wMinute );
   hb_arraySet( aMinDate, 6, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wSecond );
   hb_arraySet( aMinDate, 7, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[0].wMilliseconds );
   hb_arraySet( aMinDate, 8, temp );
   hb_itemRelease( temp );

   hb_arraySet( aMinMaxDate, 1, aMinDate );

   aMaxDate    = hb_itemArrayNew( 8 ) ;

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wYear );
   hb_arraySet( aMaxDate, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wMonth );
   hb_arraySet( aMaxDate, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wDayOfWeek );
   hb_arraySet( aMaxDate, 3, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wDay );
   hb_arraySet( aMaxDate, 4, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wHour );
   hb_arraySet( aMaxDate, 5, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wMinute );
   hb_arraySet( aMaxDate, 6, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wSecond );
   hb_arraySet( aMaxDate, 7, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lpSysTimeArray[1].wMilliseconds );
   hb_arraySet( aMaxDate, 8, temp );
   hb_itemRelease( temp );

   hb_arraySet( aMinMaxDate, 2, aMaxDate );

   hb_itemReturnForward( aMinMaxDate );
   hb_itemRelease( aMinMaxDate );
   hb_itemRelease( aMinDate );
   hb_itemRelease( aMaxDate );

   hb_xfree(lpSysTimeArray);
}

HB_FUNC( DATETIME_SETRANGE )
{
   LPSYSTEMTIME SysTime1 = (LPSYSTEMTIME) hb_param( 3, HB_IT_STRING )->item.asString.value;
   LPSYSTEMTIME SysTime2 = (LPSYSTEMTIME) hb_param( 4, HB_IT_STRING )->item.asString.value;

   SYSTEMTIME lpSysTimeArray[2];
   DWORD dwRet;

   lpSysTimeArray[0].wYear         = SysTime1->wYear        ;
   lpSysTimeArray[0].wMonth        = SysTime1->wMonth       ;
   lpSysTimeArray[0].wDayOfWeek    = SysTime1->wDayOfWeek   ;
   lpSysTimeArray[0].wDay          = SysTime1->wDay         ;
   lpSysTimeArray[0].wHour         = SysTime1->wHour        ;
   lpSysTimeArray[0].wMinute       = SysTime1->wMinute      ;
   lpSysTimeArray[0].wSecond       = SysTime1->wSecond      ;
   lpSysTimeArray[0].wMilliseconds = SysTime1->wMilliseconds;

   lpSysTimeArray[1].wYear         = SysTime2->wYear        ;
   lpSysTimeArray[1].wMonth        = SysTime2->wMonth       ;
   lpSysTimeArray[1].wDayOfWeek    = SysTime2->wDayOfWeek   ;
   lpSysTimeArray[1].wDay          = SysTime2->wDay         ;
   lpSysTimeArray[1].wHour         = SysTime2->wHour        ;
   lpSysTimeArray[1].wMinute       = SysTime2->wMinute      ;
   lpSysTimeArray[1].wSecond       = SysTime2->wSecond      ;
   lpSysTimeArray[1].wMilliseconds = SysTime2->wMilliseconds;

   dwRet = DateTime_SetRange( (HWND) hb_parnl( 1 ), hb_parni(2), (LPSYSTEMTIME) lpSysTimeArray ) ;
   hb_retnl(dwRet);
}

HB_FUNC( MONTHCAL_SETRANGE )
{
   LPSYSTEMTIME SysTime1 = (LPSYSTEMTIME) hb_param( 3, HB_IT_STRING )->item.asString.value;
   LPSYSTEMTIME SysTime2 = (LPSYSTEMTIME) hb_param( 4, HB_IT_STRING )->item.asString.value;

   SYSTEMTIME lpSysTimeArray[2];
   DWORD dwRet;

   lpSysTimeArray[0].wYear         = SysTime1->wYear        ;
   lpSysTimeArray[0].wMonth        = SysTime1->wMonth       ;
   lpSysTimeArray[0].wDayOfWeek    = SysTime1->wDayOfWeek   ;
   lpSysTimeArray[0].wDay          = SysTime1->wDay         ;
   lpSysTimeArray[0].wHour         = SysTime1->wHour        ;
   lpSysTimeArray[0].wMinute       = SysTime1->wMinute      ;
   lpSysTimeArray[0].wSecond       = SysTime1->wSecond      ;
   lpSysTimeArray[0].wMilliseconds = SysTime1->wMilliseconds;

   lpSysTimeArray[1].wYear         = SysTime2->wYear        ;
   lpSysTimeArray[1].wMonth        = SysTime2->wMonth       ;
   lpSysTimeArray[1].wDayOfWeek    = SysTime2->wDayOfWeek   ;
   lpSysTimeArray[1].wDay          = SysTime2->wDay         ;
   lpSysTimeArray[1].wHour         = SysTime2->wHour        ;
   lpSysTimeArray[1].wMinute       = SysTime2->wMinute      ;
   lpSysTimeArray[1].wSecond       = SysTime2->wSecond      ;
   lpSysTimeArray[1].wMilliseconds = SysTime2->wMilliseconds;

   dwRet = MonthCal_SetRange( (HWND) hb_parnl( 1 ), hb_parni(2), (LPSYSTEMTIME) lpSysTimeArray ) ;
   hb_retnl(dwRet);
}

HB_FUNC( ISWINDOWCOVERED )
{
  HWND hwnd = (HWND) hb_parnl(1);
  HDC hdc = GetWindowDC(hwnd);
  RECT rcClip;
  int iClip;
  iClip = GetClipBox(hdc, &rcClip);
  ReleaseDC(hwnd, hdc);
  hb_retnl(iClip);
}


HB_FUNC( _INVERTRECT )
{
   RECT rc ;
   Array2Rect( hb_param( 2, HB_IT_ARRAY ), &rc );
   hb_retni( InvertRect( (HDC) hb_parnl(1), &rc ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( _MAKEPOINTS )
{
   hb_retnl( (LONG) LOWORD( (DWORD) hb_parnl(1) ) );
}

HB_FUNC( _CLIPCURSOR )
{
   RECT rc ;
   BOOL bRectOk ;

   bRectOk = ( ISARRAY( 2 )  &&   Array2Rect( hb_param(1,HB_IT_ARRAY), &rc ) );

   hb_retl(  ClipCursor( bRectOk ? &rc : NULL ) );

}

HB_FUNC( _GETMENUITEMINFO )
{
   LPCMENUITEMINFO lpcmenuitemInfo = (LPCMENUITEMINFO) hb_param( 4, HB_IT_STRING )->item.asString.value;
   hb_retl( GetMenuItemInfo( (HMENU) hb_parnl(1), (UINT) hb_parni(2), hb_parl(3), (MENUITEMINFO *) lpcmenuitemInfo ) );
//   hb_storclen( (char *) &lpcmenuitemInfo, sizeof( MENUITEMINFO ), 4 );
}

HB_FUNC( _SETWINDOWORGEX )
{
   POINT lpPoint ;
   PHB_ITEM pArray;
   if( SetWindowOrgEx( (HDC) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), &lpPoint ) >0)
   {
      pArray = Point2Array(&lpPoint) ;
      hb_itemReturn( pArray );
      hb_itemRelease( pArray );
   }
}

HB_FUNC( _SETWINDOWEXTEX )
{
   SIZE lpSize ;
   PHB_ITEM pArray;
   if( SetWindowExtEx( (HDC) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), &lpSize ) >0)
   {
      pArray = Size2Array(&lpSize) ;
      hb_itemReturn( pArray );
      hb_itemRelease( pArray );
   }
}

HB_FUNC( _SETVIEWPORTEXTEX )
{
   SIZE siz ;
   PHB_ITEM aSize ;
   if ( SetViewportExtEx( (HDC) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), &siz ) )
   {
       aSize = Size2Array( &siz );
       hb_itemReturn( aSize );
       hb_itemRelease( aSize );
   }
}

HB_FUNC( _SETVIEWPORTORGEX )
{
   POINT pt ;
   PHB_ITEM aPoint ;
   if ( SetViewportOrgEx( (HDC) hb_parnl( 1 ),hb_parni( 2 ), hb_parni( 3 ), &pt ) )
   {
       aPoint = Point2Array( &pt );
       hb_itemReturn( aPoint );
       hb_itemRelease( aPoint );
   }
}


HB_FUNC( _FTPFINDFIRSTFILE )
{
   WIN32_FIND_DATA FindFileData ;
   HINTERNET hResult ;

   hResult = FtpFindFirstFile( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2), &FindFileData, (DWORD) hb_parnl(4), (DWORD_PTR) hb_parnl(5) );

   if ( hResult )
   {
      hb_storclen( (char *) &FindFileData , sizeof( WIN32_FIND_DATA ), 3 );
   }
   hb_retnl( (ULONG) hResult ) ;
}

HB_FUNC( _INTERNETFINDNEXTFILE )
{
   WIN32_FIND_DATA FindFileData ;
   BOOL bRet;

   bRet = InternetFindNextFile( (HINTERNET) hb_parnl(1), &FindFileData );
   
   if( bRet )
   {
      hb_storclen( (char *) &FindFileData, sizeof( WIN32_FIND_DATA ), 2 );
   }
   hb_retl( bRet );
}

HB_FUNC( _FILETIMETOSYSTEMTIME )
{
   FILETIME *FileTime  = (FILETIME *) hb_param( 1, HB_IT_STRING )->item.asString.value ;
   SYSTEMTIME SystemTime ;
   BOOL bRet;
   
   bRet = FileTimeToSystemTime( FileTime, &SystemTime );
   
   if( bRet )
   {
      hb_storclen( (char *) &SystemTime , sizeof(SYSTEMTIME), 2 );
   }
   hb_retl( bRet );
}

HB_FUNC( _ALPHABLEND )
{
   BLENDFUNCTION bf;
   bf.BlendOp     = AC_SRC_OVER;
   bf.BlendFlags  = 0;
   bf.AlphaFormat = 0;
   bf.SourceConstantAlpha = (BYTE) hb_parni(11);

   hb_retl( AlphaBlend( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (HDC) hb_parnl(6), hb_parni(7), hb_parni(8), hb_parni(9), hb_parni(10), bf ) );
}

HB_FUNC( _MONITORFROMPOINT )
{
   POINT pt;
   PHB_ITEM pSrc = hb_param( 1, HB_IT_ARRAY );
   Array2Point( pSrc, &pt );

   hb_retnl( (long) MonitorFromPoint( pt, (DWORD) hb_parnl(2) ) );
}

HB_FUNC( _GETMONITORINFO )
{
   BOOL bRet=FALSE;
   MONITORINFO pMi;
   if( GetMonitorInfo( (HMONITOR) hb_parnl(1), &pMi ) );
   {
      hb_storni( pMi.rcMonitor.left, 2 );
      hb_storni( pMi.rcMonitor.right, 3 );
      bRet = TRUE;
   }
   hb_retl( bRet );
}