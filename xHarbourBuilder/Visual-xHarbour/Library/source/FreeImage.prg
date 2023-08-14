/*
 * $Id$
 */

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#include "debug.ch"
#include "vxh.ch"
#include "FiVxh.ch"

#ifdef VXH_PROFESSIONAL

CLASS FreeImage INHERIT TitleControl, FreeImageRenderer
   PROPERTY Transparent SET ::Update()                 DEFAULT .F.
   PROPERTY Alignment   SET ::Update()                 DEFAULT 1

   ACCESS DesignMode    INLINE IIF( ::Parent != NIL, ::Parent:DesignMode, .F. )

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnDestroy()           INLINE ::TitleControl:OnDestroy(), IIF( ::hDIB != NIL, FreeImageUnload( ::hDIB ),), ::hDIB := NIL, NIL

   METHOD OnLButtonUp()         INLINE IIF( HGetPos( ::EventHandler, "OnClick" ) != 0, ::Form:&( ::EventHandler[ "OnClick" ] )( Self ), )
   METHOD OnGetDlgCode()        INLINE DLGC_WANTMESSAGE
   METHOD OnEraseBkGnd()
   METHOD Destroy()             INLINE ::TitleControl:Destroy()
   METHOD __CreateBkBrush()
   METHOD Save(cFile)           INLINE MemoWrit( cFile, ::__cData )
ENDCLASS

//--------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS FreeImage
   LOCAL cSupp := ""
   ::__xCtrlName  := "FreeImage"
   ::ClsName      := "PanelBox"
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle      := WS_EX_CONTROLPARENT

   ::FreeImageRenderer:Init( Self )
   ::TitleControl:Init( oParent )
   ::Width         := 100
   ::Height        := 100
   ::IsContainer   := .T.
   ::__IsStandard  := .F.
   IF ::Parent:DesignMode
      ::__ExplorerFilter := __GetSystem():FreeImageFormats
   ENDIF
   ::bSetValue := {|cValue| IIF( ValType(::bOnSetValue)=="B", Eval( ::bOnSetValue, Self, cValue ), ::LoadFromString( cValue ) ) }
   ::bGetValue := {|| ::__cData }
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Create() CLASS FreeImage
   ::FreeImageRenderer:Create()
   ::TitleControl:Create()
   ::__SetImageName( ::xImageName )
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD __CreateBkBrush( hDC ) CLASS FreeImage
   LOCAL hMemBitmap, hOldBitmap, hMemDC, hBrush, nLeftBorder, nBorder, lDC := hDC != NIL
   IF ! lDC
      hDC := GetDC( ::hWnd )
   ENDIF

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   IF ::Transparent .AND. ::Parent:BkBrush != NIL

      nBorder     := (::Height - ( ::ClientHeight + IIF( ! Empty(::Text), ::TitleHeight, 0 ) ) ) / 2
      nLeftBorder := (::Width-::ClientWidth)/2

      SetBrushOrgEx( hMemDC, ::Parent:ClientWidth-::Left-nLeftBorder, ::Parent:ClientHeight-::Top-IIF( ! Empty(::Text), ::TitleHeight, 0 )-nBorder )
      _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, ::Parent:BkBrush )
   ELSE
      IF ::xBackColor != NIL
         hBrush := CreateSolidBrush( ::xBackColor )
      ENDIF
      _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, IIF( hBrush != NIL, hBrush, GetSysColorBrush(COLOR_BTNFACE) ) )
      IF hBrush != NIL
         DeleteObject( hBrush )
      ENDIF
   ENDIF

   ::Draw( hMemDC )

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
   ENDIF
   ::BkBrush := CreatePatternBrush( hMemBitmap )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

   IF ! lDC
      ReleaseDC( ::hWnd, hDC )
   ENDIF

RETURN NIL

//--------------------------------------------------------------------------------------------------------
METHOD OnEraseBkGnd( hDC ) CLASS FreeImage
   ::__CreateBkBrush( hDC )
   _FillRect( hDC, {0, 0, ::ClientWidth, ::ClientHeight}, ::BkBrush )
RETURN 1

#endif

//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------


CLASS FreeImageRenderer
   PROPERTY ImageName        SET ::__SetImageName( @v )
   PROPERTY Opacity          SET ::Update() DEFAULT 100
   PROPERTY Alignment        SET ::Update() DEFAULT 1
   PROPERTY KeepAspectRatio  SET ::Update() DEFAULT .F.
   PROPERTY Stretch          SET ::Update() DEFAULT .F.
   PROPERTY Margins          SET ::SetMargins(v)

   DATA LeftMargin      EXPORTED INIT 0
   DATA TopMargin       EXPORTED INIT 0
   DATA RightMargin     EXPORTED INIT 0
   DATA BottomMargin    EXPORTED INIT 0
   DATA lTransparentSet EXPORTED INIT .F.
   DATA Owner           EXPORTED
   DATA Type            EXPORTED
   DATA ImageType       EXPORTED
   DATA Transparent     EXPORTED INIT .F.
   DATA ClsName         EXPORTED INIT "FreeImageRenderer"
   DATA __xCtrlName     EXPORTED INIT "FreeImage"
   DATA __IsInstance    EXPORTED INIT .F.
   DATA __cData         PROTECTED
   DATA hDIB            EXPORTED

   DATA EnumAlignment   EXPORTED INIT { { "None",;
                                          "Center",;
                                          "Left - Top",;
                                          "Top - Center",;
                                          "Top - Right",;
                                          "Right - Center",;
                                          "Right - Bottom",;
                                          "Bottom - Center",;
                                          "Bottom - Left",;
                                          "Left - Center" }, {1,2,3,4,5,6,7,8,9,10} }
   DATA __ExplorerFilter INIT {}

   ACCESS ImageWidth    INLINE IIF( ::hDIB != NIL, FreeImageGetWidth( ::hDIB ), 0 )
   ACCESS ImageHeight   INLINE IIF( ::hDIB != NIL, FreeImageGetHeight( ::hDIB ), 0 )
   ACCESS DesignMode    INLINE IIF( ::Owner != NIL, ::Owner:DesignMode, .F. )

   METHOD Init()   CONSTRUCTOR
   METHOD Kill()   INLINE IIF( ::hDIB != NIL, FreeImageUnload( ::hDIB ),), ::hDIB := NIL
   METHOD Update()
   METHOD __SetImageName()
   METHOD Create()
   METHOD Draw()
   METHOD LoadResource()
   METHOD LoadFromString()
   METHOD Destroy()            INLINE ::Kill(), ::Owner := NIL,NIL
   METHOD SetMargins()
   METHOD ValidImage()         INLINE ::hDIB != NIL
   METHOD Reload()             INLINE ::LoadFromString( ::__cData ), ::InvalidateRect()
   METHOD Rescale()
ENDCLASS

//--------------------------------------------------------------------------------------------------------
METHOD SetMargins( cMargins ) CLASS FreeImageRenderer
   LOCAL oApp, n, aMargins := hb_atokens( cMargins, "," )
   oApp := __GetApplication()
   ::LeftMargin   := 0
   ::TopMargin    := 0
   ::RightMargin  := 0
   ::BottomMargin := 0

   IF LEN( aMargins ) == 1
      ASIZE( aMargins, 4 )
      FOR n := 2 TO 4
          IF aMargins[n] == NIL
             aMargins[n] := aMargins[1]
          ENDIF
      NEXT
    ELSE
      ASIZE( aMargins, 4 )
      FOR n := 1 TO 4
          IF aMargins[n] == NIL
             aMargins[n] := "0"
          ENDIF
      NEXT
   ENDIF

   ::LeftMargin   := VAL( aMargins[1] )
   ::TopMargin    := VAL( aMargins[2] )
   ::RightMargin  := VAL( aMargins[3] )
   ::BottomMargin := VAL( aMargins[4] )

   ::xMargins := Alltrim( Str( ::LeftMargin ) )+ "," + Alltrim( Str( ::TopMargin ) ) + "," + Alltrim( Str( ::RightMargin ) ) + "," + Alltrim( Str( ::BottomMargin ) )

   ::Update()

   IF ::DesignMode .AND. oApp:ObjectManager != NIL
      oApp:ObjectManager:PostMessage( WM_USER + 4766 )
   ENDIF

RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS FreeImageRenderer
   ::Owner := oOwner
   __SetInitialValues( Self )
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Create() CLASS FreeImageRenderer
   IF !EMPTY( ::ImageName )
      ::__SetImageName( @::xImageName )
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Rescale() CLASS FreeImageRenderer
   LOCAL cx, cy, nRatio, iy, nHeight, nWidth, tmp

   cx := FreeImageGetWidth( ::hDIB )
   cy := FreeImageGetHeight( ::hDIB )

   IF ::KeepAspectRatio
      nRatio := ::Owner:ClientWidth / cx
      iy     := cy * nRatio

      nHeight := ::Owner:ClientHeight
      nWidth  := ::Owner:ClientWidth

      IF iy < nHeight
         nHeight := iy
       ELSE
         nRatio := nHeight / cy
         nWidth := cx * nRatio
      ENDIF
      cy := nHeight
      cx := nWidth

    ELSEIF ::Stretch
      cy := ::Owner:ClientHeight
      cx := ::Owner:ClientWidth
   ENDIF

   tmp := FreeImageRescale( ::hDIB, cx, cy, FILTER_BICUBIC )
   IF tmp != NIL
      FreeImageUnload( ::hDIB )
      ::hDIB := tmp
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD LoadFromString( cData ) CLASS FreeImageRenderer
   ::xImageName := NIL

   ::__cData := cData
   IF ::Owner:BkBrush != NIL
      DeleteObject( ::Owner:BkBrush )
      ::Owner:BkBrush := NIL
   ENDIF

   ::lTransparentSet := .F.
   IF ::hDIB != NIL
      FreeImageUnload( ::hDIB )
      ::hDIB := NIL
   ENDIF

   IF ! EMPTY( cData )
      ::hDIB := FreeImageStringToDib( cData )
   ENDIF

   ::Owner:InvalidateRect()
   AEVAL( ::Children, {|o| o:Reload()} )
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Draw( hMemDC, hBitmap ) CLASS FreeImageRenderer
   LOCAL cx, cy, hDIBMemBitmap, display_dib, hDib, nRatio, iy, nHeight, nWidth, hTmp
   LOCAL hMemBitmap1, hOldBitmap1, hMemDC1, x, y
   IF ::hDIB == NIL
      RETURN NIL
   ENDIF

   cx := FreeImageGetWidth( ::hDIB )
   cy := FreeImageGetHeight( ::hDIB )

   IF ::KeepAspectRatio
      nRatio := ::Owner:ClientWidth / cx
      iy     := cy * nRatio

      nHeight := ::Owner:ClientHeight
      nWidth  := ::Owner:ClientWidth

      IF iy < nHeight
         nHeight := iy
       ELSE
         nRatio := nHeight / cy
         nWidth := cx * nRatio
      ENDIF
      cy := nHeight
      cx := nWidth

    ELSEIF ::Stretch
      cy := ::Owner:ClientHeight
      cx := ::Owner:ClientWidth
   ENDIF
   x := 0
   y := 0
   IF ! ::Stretch
      SWITCH ::xAlignment
         CASE 2 // Center
            x := ( ::Owner:ClientWidth - cx ) / 2
            y := ( ::Owner:ClientHeight - cy ) / 2
            EXIT

         CASE 3 // Left Top
            x := ::LeftMargin
            y := ::TopMargin
            EXIT

         CASE 4 // Top - Center
            x := ( ::Owner:ClientWidth - cx ) / 2
            y := ::TopMargin
            EXIT

         CASE 5 // Top - Right
            x := ::Owner:ClientWidth - cx - ::RightMargin
            y := ::TopMargin
            EXIT

         CASE 6 // Right - Center
            x := ::Owner:ClientWidth - cx - ::RightMargin
            y := ( ::Owner:ClientHeight - cy ) / 2
            EXIT

         CASE 7 // Right - Bottom
            x := ::Owner:ClientWidth  - cx - ::RightMargin
            y := ::Owner:ClientHeight - cy - ::BottomMargin
            EXIT

         CASE 8 // Bottom - Center
            x := ( ::Owner:ClientWidth - cx ) / 2
            y := ::Owner:ClientHeight - cy - ::BottomMargin
            EXIT

         CASE 9 // Bottom - Left
            x := ::LeftMargin
            y := ::Owner:ClientHeight - cy - ::BottomMargin
            EXIT

         CASE 10 // "Left - Center
            x := ::LeftMargin
            y := ( ::Owner:ClientHeight - cy ) / 2
            EXIT
      END
   ENDIF

   x -= ::Owner:HorzScrollPos
   y -= ::Owner:VertScrollPos

   IF ! ::lTransparentSet
      ::lTransparentSet := .T.

      IF FreeImageIsTransparent( ::hDIB ) .OR. FreeImageHasBackgroundColor( ::hDIB )
         IF .T. //! ::KeepAspectRatio .AND. ! ::Stretch
            hMemDC1     := CreateCompatibleDC( hMemDC )
            hMemBitmap1 := CreateCompatibleBitmap( hMemDC, cx, cy )
            hOldBitmap1 := SelectObject( hMemDC1, hMemBitmap1 )
            BitBlt( hMemDC1, 0, 0, cx, cy, hMemDC, x, y, SRCCOPY )

            hDIBMemBitmap := FreeImagehBitmapToDib( hMemBitmap1 )

            IF hDIBMemBitmap != NIL
               IF FreeImageGetBPP( hDIBMemBitmap ) == 32
                  hTmp := FreeImageConvertTo24Bits( hDIBMemBitmap )
                  FreeImageUnload( hDIBMemBitmap )
                ELSE
                  hTmp := hDIBMemBitmap
               ENDIF

               IF ( display_dib := FreeImageComposite( ::hDIB, .F., , hTmp ) ) != NIL
                  FreeImageUnload( ::hDIB )
                  ::hDIB := display_dib
               ENDIF

               FreeImageUnload( hTmp )

            ENDIF
            SelectObject( hMemDC1, hOldBitmap1 )
            DeleteObject( hMemBitmap1 )
            DeleteDC( hMemDC1 )
          ELSE
            hDIBMemBitmap := FreeImagehBitmapToDib( hBitmap )
            IF FreeImageGetBPP( hDIBMemBitmap ) == 32
               hDib := FreeImageConvertTo24Bits( hDIBMemBitmap )
               FreeImageUnload( hDIBMemBitmap )
             ELSE
               hDib := hDIBMemBitmap
            ENDIF
            IF ( display_dib := FreeImageComposite( ::hDIB, .F., , hDib ) ) != NIL
               FreeImageUnload( ::hDIB )
               ::hDIB := display_dib
            ENDIF
            FreeImageUnload( hDib )
         ENDIF
      ENDIF
   ENDIF

   IF ::hDIB == NIL
      RETURN 0
   ENDIF

   IF ::KeepAspectRatio .OR. ::Stretch
      hDib := FreeImageRescale( ::hDIB, cx, cy, FILTER_BICUBIC )
   ENDIF
   IF ::xOpacity < 100
      hMemDC1     := CreateCompatibleDC( hMemDC )
      hMemBitmap1 := CreateCompatibleBitmap( hMemDC, cx, cy )
      hOldBitmap1 := SelectObject( hMemDC1, hMemBitmap1 )

      FreeImageDraw( IIF( hDib != NIL, hDib, ::hDIB ), hMemDC1, 0, 0, cx, cy )

      _AlphaBlend( hMemDC, x, y, cx, cy, hMemDC1, 0, 0, cx, cy, ( 255 * ::xOpacity ) / 100 )

      SelectObject( hMemDC1, hOldBitmap1 )
      DeleteObject( hMemBitmap1 )
      DeleteDC( hMemDC1 )

    ELSE //IF ! ::Transparent
      FreeImageDraw( IIF( hDib != NIL, hDib, ::hDIB ), hMemDC, x, y, cx, cy )
   ENDIF
   IF hDib != NIL
      FreeImageUnload( hDIB )
   ENDIF
RETURN NIL

//--------------------------------------------------------------------------------------------------------
METHOD LoadResource( cResource, cType ) CLASS FreeImageRenderer
   LOCAL lOK := .F., hBmp, hInst
   hInst := ::Owner:AppInstance
   IF cType != "BMP" .AND. cType != "ICO"

      ::__cData := __ResourceToString( hInst, cResource, cType )
      IF !EMPTY( ::__cData )
         IF ::hDIB != NIL
            FreeImageUnload( ::hDIB )
         ENDIF
         ::hDIB := FreeImageStringToDib( ::__cData )
         lOK := ::hDIB != NIL
      ENDIF

    ELSE

      IF cType == "BMP"
         hBmp := LoadImage( hInst, cResource, IMAGE_BITMAP, 0, 0, (LR_DEFAULTCOLOR | LR_DEFAULTSIZE | LR_CREATEDIBSECTION) )
       ELSE
         hBmp := LoadImage( hInst, cResource, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE )
      ENDIF

      IF ! Empty( hBmp )
         IF ::hDIB != NIL
            FreeImageUnload( ::hDIB )
         ENDIF
         ::hDIB := FreeImagehBitmapToDib( hBmp )
         lOK := ::hDIB != NIL
         DeleteObject( hBmp )
      ENDIF

   ENDIF
   ::Owner:InvalidateRect()
   AEVAL( ::Owner:Children, {|o| o:Reload()} )
RETURN lOK

//--------------------------------------------------------------------------------------------------------
METHOD Update() CLASS FreeImageRenderer
   IF ::__cData == NIL
      ::__SetImageName( @::xImageName )
   ENDIF
   IF ::Owner != NIL
      ::Owner:InvalidateRect()
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD __SetImageName( cFile ) CLASS FreeImageRenderer
   LOCAL cType, cPrev
   IF VALTYPE( cFile ) == "A"
      cFile := IIF( ::DesignMode .AND. VALTYPE( cFile[1] ) == "C", cFile[1], cFile[2] )
   ENDIF
   IF ::Owner:BkBrush != NIL
      DeleteObject( ::Owner:BkBrush )
      ::Owner:BkBrush := NIL
   ENDIF
   ::lTransparentSet := .F.
   IF ::hDIB != NIL
      FreeImageUnload( ::hDIB )
      ::hDIB := NIL
   ENDIF
   cPrev := ::xImageName
   IF !EMPTY( cFile )
      IF AT( ".", cFile ) > 0
         ::ImageType := FreeImageGetFileType( cFile )
         ::hDIB := FreeImageLoad( ::ImageType, cFile, 0 )
         ::__cData := MemoRead( cFile )
       ELSE
         DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
         ::LoadResource( cFile, cType )
      ENDIF
    ELSE
      ::xImageName := NIL
      ::__cData := ""
   ENDIF

   IF ::DesignMode
      IF !EMPTY( cPrev )
         ::Owner:Application:Project:RemoveImage( cPrev, Self )
      ENDIF
      IF !EMPTY( cFile )
         DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
         ::Owner:Application:Project:AddImage( cFile, cType, Self )
      ENDIF
   ENDIF
   ::Owner:InvalidateRect()
   AEVAL( ::Owner:Children, {|o| o:Reload()} )

RETURN Self

//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------

#pragma BEGINDUMP

#include <windows.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "FreeImage.h"

//--------------------------------------------------------------------------------------------------------
HB_FUNC_INIT( FREEIMAGEINIT )
{
   FreeImage_Initialise( hb_parl(1) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC_EXIT( FREEIMAGEEND )
{
   FreeImage_DeInitialise();
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGECOMPOSITE )
{
   COLORREF rgb;
   RGBQUAD appBkColor;
   FIBITMAP *fi;
   if( !ISNIL(3) )
   {
      rgb = (COLORREF) hb_parnl(3);
      appBkColor.rgbRed = GetRValue( rgb );
      appBkColor.rgbGreen = GetGValue( rgb );
      appBkColor.rgbBlue = GetBValue( rgb );
   }
   fi = FreeImage_Composite( (FIBITMAP*) hb_parptr(1),
                                         hb_parl(2),
                                         ISNIL(3) ? NULL : &appBkColor,
                                         ISNIL(4) ? NULL : (FIBITMAP*) hb_parptr(4) );
   if( fi )
      hb_retptr( fi );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEADJUSTCONTRAST )
{
   hb_retl( FreeImage_AdjustContrast( (FIBITMAP *) hb_parptr(1), hb_parni(2) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEHBITMAPTODIB )
{
   FIBITMAP *dib = NULL;
   HBITMAP hBmp = (HBITMAP) hb_parnl(1);
   if( hBmp )
   {
      BITMAP bm;
      HDC hDC;
      GetObject( hBmp, sizeof(BITMAP), (LPSTR) &bm );
      dib = FreeImage_Allocate(bm.bmWidth, bm.bmHeight, bm.bmBitsPixel, 0, 0, 0);
      hDC = GetDC( NULL );
      GetDIBits( hDC, hBmp, 0, FreeImage_GetHeight(dib), FreeImage_GetBits(dib), FreeImage_GetInfo(dib), DIB_RGB_COLORS);
      ReleaseDC( NULL, hDC );
   }
   if ( dib != NULL )
   {
      hb_retptr( dib );
   }
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGESTRINGTODIB )
{
   FIMEMORY *hMem = FreeImage_OpenMemory( (BYTE *) hb_parc(1), hb_parclen(1) );
   FREE_IMAGE_FORMAT fiSource = FreeImage_GetFileTypeFromMemory( hMem, 0 );
   FIBITMAP *dib = FreeImage_LoadFromMemory( fiSource, hMem, 0 );
   FreeImage_CloseMemory( hMem );
   hb_retptr( dib );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGECONVERTTO24BITS )
{
   FIBITMAP *dib = NULL;
   dib = FreeImage_ConvertTo24Bits( (FIBITMAP*) hb_parptr(1) );
   if ( dib != NULL )
   {
      hb_retptr( dib );
   }
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGECONVERTTO32BITS )
{
   FIBITMAP *dib = NULL;
   dib = FreeImage_ConvertTo32Bits( (FIBITMAP*) hb_parptr(1) );
   if ( dib != NULL )
   {
      hb_retptr( dib );
   }
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEDRAW )
{
   FIBITMAP *dib = (FIBITMAP *) hb_parptr(1);
   HDC hDC = (HDC) hb_parnl(2);
   SetStretchBltMode(hDC, COLORONCOLOR);

   hb_retni( StretchDIBits( hDC, hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6),
                                   0, 0, FreeImage_GetWidth(dib), FreeImage_GetHeight(dib),
                                   FreeImage_GetBits(dib), FreeImage_GetInfo(dib),
                                   DIB_RGB_COLORS, SRCCOPY) );

}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGELOAD )
{
   FIBITMAP *dib = FreeImage_Load( (FREE_IMAGE_FORMAT) hb_parni(1), hb_parc(2), hb_parni(3) );
   if ( dib != NULL )
   {
      hb_retptr( dib );
   }
}


//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGERESCALE )
{
   FIBITMAP *dib = FreeImage_Rescale( (FIBITMAP *) hb_parptr(1), hb_parni(2), hb_parni(3), (FREE_IMAGE_FILTER) hb_parni(4) );
   if ( dib != NULL )
   {
      hb_retptr( dib );
   }
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEUNLOAD )
{
   FreeImage_Unload( (FIBITMAP *) hb_parptr(1) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEGETWIDTH )
{
   hb_retni( FreeImage_GetWidth( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEGETHEIGHT )
{
   hb_retni( FreeImage_GetHeight( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEISTRANSPARENT )
{
   hb_retl( FreeImage_IsTransparent( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGESETTRANSPARENT )
{
   FreeImage_SetTransparent( (FIBITMAP*) hb_parptr(1), hb_parl(2) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEHASBACKGROUNDCOLOR )
{
   hb_retl( FreeImage_HasBackgroundColor( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEGETBPP )
{
   hb_retnl( FreeImage_GetBPP( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEGETFILETYPE )
{
   hb_retni( FreeImage_GetFileType( hb_parc(1), 0 ) );
}

#pragma comment( lib, "freeimage.lib" )

#pragma ENDDUMP
