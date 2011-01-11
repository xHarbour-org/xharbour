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

CLASS FreeImage INHERIT FreeImageRenderer, Panel

   PROPERTY Transparent READ xTransparent WRITE Update DEFAULT .F.
   PROPERTY Alignment   READ xAlignment   WRITE Update DEFAULT 1

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnDestroy()          INLINE ::FreeImageRenderer:Destroy(), NIL
   METHOD OnGetDlgCode()       INLINE DLGC_WANTMESSAGE
   METHOD OnPaint()
   METHOD OnEraseBkGnd()       INLINE 1
   //METHOD OnSize()             INLINE ::Update(), NIL
   //METHOD OnMove()             INLINE ::Update(), NIL
   METHOD SetBackColor(n)      INLINE ::Panel:SetBackColor(n), ::Update()
   METHOD __SetSmallCaption(l) INLINE ::Panel:__SetSmallCaption(l), ::Update()
ENDCLASS

//--------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS FreeImage
   LOCAL aFormat, cSupp := ""
   ::__xCtrlName  := "FreeImage"
   ::FreeImageRenderer:Init( Self )
   ::Panel:Init( oParent )
   ::Width         := 100
   ::Height        := 100
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::IsContainer   := .T.
   ::ControlParent := .T.
   IF ::Parent:__ClassInst != NIL
      ::__ExplorerFilter := __GetSystem():FreeImageFormats
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Create() CLASS FreeImage
   ::FreeImageRenderer:Create()
   ::Panel:Create()
   ::__SetImageName( ::xImageName )
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC, hMemDC ) CLASS FreeImage
   LOCAL hParBitmap, hOldParBitmap, hParDC
   LOCAL hMemBitmap, hOldBitmap, hBrush
   LOCAL oChild, hMemBitmap1, hOldBitmap1, hMemDC1, pPt := (struct POINT)

   IF !::Transparent
      hBrush := ::BkBrush
    ELSE
      hBrush := ::__hBrush
      DEFAULT hBrush TO ::Parent:BkBrush
   ENDIF
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )

   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF

   _FillRect( hMemDC, {0, 0, ::ClientWidth, ::ClientHeight}, hBrush )

   ::Draw( hMemDC )

   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF
          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin+oChild:CaptionHeight, SRCCOPY )
          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )
      NEXT
   ENDIF

   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF

RETURN 0
#endif

//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------


CLASS FreeImageRenderer
   PROPERTY ImageName        READ xImageName        WRITE __SetImageName  INVERT
   PROPERTY Opacity          READ xOpacity          WRITE Update DEFAULT 100
   PROPERTY Alignment        READ xAlignment        WRITE Update DEFAULT 1
   PROPERTY KeepAspectRatio  READ xKeepAspectRatio  WRITE Update DEFAULT .F.

   DATA LeftMargin   EXPORTED INIT 0
   DATA TopMargin    EXPORTED INIT 0
   DATA RightMargin  EXPORTED INIT 0
   DATA BottomMargin EXPORTED INIT 0

   PROPERTY Margins         READ xMargins WRITE SetMargins PROTECTED


   DATA lTransparentSet EXPORTED INIT .F.
   DATA Owner           EXPORTED
   DATA Type            EXPORTED
   DATA Transparent     EXPORTED INIT .F.
   DATA ClsName         EXPORTED INIT "FreeImageRenderer"
   DATA __IsInstance    EXPORTED INIT .F.
   DATA __ClassInst     EXPORTED
   DATA hDIB            EXPORTED
   ACCESS Instance      INLINE __GetApplication():Instance
   DATA __Alignments    EXPORTED INIT { "None",;
                                        "Center",;
                                        "Left - Top",;
                                        "Top - Center",;
                                        "Top - Right",;
                                        "Right - Center",;
                                        "Right - Bottom",;
                                        "Bottom - Center",;
                                        "Bottom - Left",;
                                        "Left - Center" }
   DATA __ExplorerFilter INIT {}
   
   ACCESS ImageWidth       INLINE IIF( ::hDIB != NIL, FreeImageGetWidth( ::hDIB ), 0 )
   ACCESS ImageHeight      INLINE IIF( ::hDIB != NIL, FreeImageGetHeight( ::hDIB ), 0 )
   
   METHOD Init()   CONSTRUCTOR
   METHOD Kill()   INLINE FreeImageUnload( ::hDIB ), ::hDIB := NIL
   METHOD Update()
   METHOD __SetImageName()
   METHOD Create()
   METHOD Draw()
   METHOD LoadResource()
   METHOD LoadFromString()
   METHOD Destroy()            INLINE IIF( ::hDIB != NIL, FreeImageUnload( ::hDIB ), ), NIL
   METHOD SetMargins()
ENDCLASS

//--------------------------------------------------------------------------------------------------------
METHOD SetMargins( cMargins ) CLASS FreeImageRenderer
   LOCAL oApp, oItem, n, aMargins := hb_atokens( cMargins, "," )
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
   
   IF ::__ClassInst != NIL .AND. oApp:ObjectManager != NIL
      oApp:ObjectManager:PostMessage( WM_USER + 4766 )
   ENDIF

RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS FreeImageRenderer
   LOCAL cSupp := ""
   ::Owner := oOwner
   IF ::Owner:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance  := .T.
      ::__ExplorerFilter := __GetSystem():FreeImageFormats
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Create() CLASS FreeImageRenderer
   LOCAL n, aFormat, cSupp

   IF !EMPTY( ::ImageName )
      ::__SetImageName( ::xImageName )
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD LoadFromString( cData ) CLASS FreeImageRenderer
   IF !EMPTY( cData )
      IF ::hDIB != NIL
         FreeImageUnload( ::hDIB )
      ENDIF
      ::hDIB := FreeImageStringToDib( cData )
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Draw( hMemDC ) CLASS FreeImageRenderer
   LOCAL cx, cy, hDIBMemBitmap, display_dib, hBitmap, hDib, nRatio, iy, nHeight, nWidth
   LOCAL hMemBitmap1, hOldBitmap1, hMemDC1, hDC, x, y, hBackColor
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
   ENDIF
   x := 0
   y := 0
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
   
   IF !::lTransparentSet
      ::lTransparentSet := .T.
      IF FreeImageIsTransparent( ::hDIB ) .OR. FreeImageHasBackgroundColor( ::hDIB )
         IF !::KeepAspectRatio
            hMemDC1     := CreateCompatibleDC( hMemDC )
            hMemBitmap1 := CreateCompatibleBitmap( hMemDC, cx, cy )
            hOldBitmap1 := SelectObject( hMemDC1, hMemBitmap1 )
            BitBlt( hMemDC1, 0, 0, cx, cy, hMemDC, x, y, SRCCOPY )

            hDIBMemBitmap := FreeImagehBitmapToDib( hMemBitmap1 )

            IF hDIBMemBitmap != NIL
               IF FreeImageGetBPP( hDIBMemBitmap ) == 32
                  hDib := FreeImageConvertTo24Bits( hDIBMemBitmap )
                  FreeImageUnload( hDIBMemBitmap )
                ELSE 
                  hDib := hDIBMemBitmap
               ENDIF

               display_dib := FreeImageComposite( ::hDIB, .F., , hDib )

               IF display_dib != NIL
                  FreeImageUnload( ::hDIB )
                  ::hDIB := display_dib
               ENDIF

               FreeImageUnload( hDib )

            ENDIF
            SelectObject( hMemDC1, hOldBitmap1 )
            DeleteObject( hMemBitmap1 )
            DeleteDC( hMemDC1 )
          ELSE
            IF ( display_dib := FreeImageComposite( ::hDIB, .F., ::Owner:BackColor ) ) != NIL
               FreeImageUnload( ::hDIB )
               ::hDIB := display_dib
            ENDIF
         ENDIF   
      ENDIF
   ENDIF

   IF ::hDIB == NIL
      RETURN 0
   ENDIF

   IF ::xOpacity < 100
      hMemDC1     := CreateCompatibleDC( hMemDC )
      hMemBitmap1 := CreateCompatibleBitmap( hMemDC, cx, cy )
      hOldBitmap1 := SelectObject( hMemDC1, hMemBitmap1 )
      
      FreeImageDraw( ::hDIB, hMemDC1, 0, 0, cx, cy )

      _AlphaBlend( hMemDC, x, y, cx, cy, hMemDC1, 0, 0, cx, cy, ( 255 * ::xOpacity ) / 100 )

      SelectObject( hMemDC1, hOldBitmap1 )
      DeleteObject( hMemBitmap1 )
      DeleteDC( hMemDC1 )

    ELSE
      FreeImageDraw( ::hDIB, hMemDC, x, y, cx, cy )
   ENDIF

RETURN NIL

//--------------------------------------------------------------------------------------------------------
METHOD LoadResource( cResource, cType ) CLASS FreeImageRenderer
   LOCAL display_dib, lOK := .F., cData, hBmp, n, hMem, hRes, hInst
   hInst := ::Owner:AppInstance
   IF cType != "BMP" .AND. cType != "ICO"
   
      cData := __ResourceToString( hInst, cResource, cType )
      IF !EMPTY( cData )
         IF ::hDIB != NIL
            FreeImageUnload( ::hDIB )
         ENDIF
         ::hDIB := FreeImageStringToDib( cData )
         lOK := ::hDIB != NIL
      ENDIF
      
    ELSE
    
      IF cType == "BMP"
         hBmp := LoadImage( hInst, cResource, IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR | LR_DEFAULTSIZE | LR_CREATEDIBSECTION )
       ELSE
         hBmp := LoadImage( hInst, cResource, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE )
      ENDIF
      
      IF hBmp != NIL 
         IF ::hDIB != NIL
            FreeImageUnload( ::hDIB )
         ENDIF
         ::hDIB := FreeImagehBitmapToDib( hBmp )
         lOK := ::hDIB != NIL
         DeleteObject( hBmp )
      ENDIF
      
   ENDIF

RETURN lOK

//--------------------------------------------------------------------------------------------------------
METHOD Update() CLASS FreeImageRenderer
   ::__SetImageName( ::xImageName )
   IF ::Owner != NIL
      ::Owner:InvalidateRect()
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD __SetImageName( cFile ) CLASS FreeImageRenderer
   LOCAL n, cType, cPrev
   IF VALTYPE( cFile ) == "A"
      cFile := IIF( ::__ClassInst != NIL .AND. VALTYPE( cFile[1] ) == "C", cFile[1], cFile[2] )
   ENDIF
   ::lTransparentSet := .F.
   IF ::hDIB != NIL
      FreeImageUnload( ::hDIB )
      ::hDIB := NIL
   ENDIF
   cPrev := ::xImageName
   IF !EMPTY( cFile )
      IF AT( ".", cFile ) > 0
         ::hDIB := FreeImageLoad( FreeImageGetFileType( cFile ), cFile, 0 )
       ELSE
         DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
         ::LoadResource( cFile, cType )
      ENDIF
    ELSE
      ::xImageName := NIL
   ENDIF

   IF ::__ClassInst != NIL 
      IF !EMPTY( cPrev )
         ::Owner:Application:Project:RemoveImage( cPrev, Self )
      ENDIF
      IF !EMPTY( cFile )
         DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
         ::Owner:Application:Project:AddImage( cFile, cType, Self )
      ENDIF
   ENDIF
   ::Owner:InvalidateRect()
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
   FIBITMAP *dib;
   dib = FreeImage_Load( (FREE_IMAGE_FORMAT) hb_parni(1), hb_parc(2), hb_parni(3) );
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
   hb_retnl( FreeImage_GetWidth( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEGETHEIGHT )
{
   hb_retnl( FreeImage_GetHeight( (FIBITMAP*) hb_parptr(1) ) );
}

//--------------------------------------------------------------------------------------------------------
HB_FUNC( FREEIMAGEISTRANSPARENT )
{
   hb_retl( FreeImage_IsTransparent( (FIBITMAP*) hb_parptr(1) ) );
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
