/*
 * $Id$
 */

#include "debug.ch"
#include "vxh.ch"

#define AC_SRC_OVER  0x00
#define AC_SRC_ALPHA  0x01
#define MAKEROP4(fore,back)  (  (back<<8) & 0xFF000000| fore )

#define _PIXELBYPIXEL_

static aDef := {"BMP","ICO"}

//CreateMappedBitmap

CLASS PictureBox INHERIT Panel
   PROPERTY TransparencyByPixel DEFAULT .F.
   PROPERTY ImageName        SET ::SetImageName(@v)
   PROPERTY Stretch          SET ::Update()         DEFAULT .F.
   PROPERTY KeepAspectRatio  SET ::Update()         DEFAULT .F.
   PROPERTY Transparent      SET ::InvalidateRect() DEFAULT .F.
   PROPERTY BlackAndWhite    SET ::InvalidateRect() DEFAULT .F.
   PROPERTY InvertedColors   SET ::InvalidateRect() DEFAULT .F.
   PROPERTY Opacity          SET ::InvalidateRect() DEFAULT 100

   PROPERTY Alignment        SET ::Update()         DEFAULT 0


   DATA pPicture        EXPORTED
   DATA ResourceName    EXPORTED
   DATA PictureWidth    EXPORTED
   DATA PictureHeight   EXPORTED
   DATA Type            EXPORTED INIT ""
   DATA ImageIndex      EXPORTED INIT -1
   DATA __IsGif         PROTECTED
   DATA __aPixels       PROTECTED
   DATA EnumAlignment   EXPORTED INIT { { "None",;
                                          "Center",;
                                          "Left - Top",;
                                          "Top - Center",;
                                          "Top - Right",;
                                          "Right - Center",;
                                          "Right - Bottom",;
                                          "Bottom - Center",;
                                          "Bottom - Left",;
                                          "Left - Center" }, {0,1,2,3,4,5,6,7,8,9} }
   DATA __ExplorerFilter INIT {;
                              { "All Supported Graphics", "*.bmp;*.jpg;*.jif;*.jpeg;*.jpe;*.gif;*.ico;*.ani;*.cur;*.wmf;*.emf" },;
                              { "Windows Bitmap (*.bmp)", "*.bmp" },;
                              { "Icon Files (*.ico)", "*.ico" },;
                              { "CompuServe Graphics Interchange (*.gif)", "*.gif" },;
                              { "Cursors (*.ani,*.cur)", "*.ani;*.cur" },;
                              { "JPEG (*.jpg,*.jif,*.jpeg,*.jpe)", "*.jpg;*.jif;*.jpeg;*.jpe" },;
                              { "Windows Metafile (*.wmf)", "*.wmf" },;
                              { "Windows Enhanced Metafile (*.emf)", "*.emf" };
                              }

   DATA TransparentColor //compatibility only

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Draw()
   METHOD Kill() INLINE IIF( ::pPicture != NIL .AND. ::pPicture > 0, PictureRemove( ::pPicture ), ), ::pPicture := NIL
   METHOD SetImageName()
   METHOD SetBitmap( hBmp ) INLINE ::pPicture := PictureLoadBitmap( hBmp )
   METHOD SetIcon( hIcon )  INLINE ::pPicture := PictureLoadIcon( hIcon )
   METHOD OnGetDlgCode()    INLINE DLGC_WANTMESSAGE
   METHOD OnEraseBkGnd()    INLINE 1
   METHOD OnPaint()
   METHOD Update()          INLINE IIF( ::hWnd != NIL, ::InvalidateRect(), )
   METHOD Destroy()
   METHOD __CreateBkBrush()
ENDCLASS

METHOD Init( oParent ) CLASS PictureBox
   DEFAULT ::__xCtrlName TO "PictureBox"
   ::Super:Init( oParent )
   ::Width        := 100
   ::Height       := 100
   ::Style        := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   //::__IsStandard   := .F.
   ::IsContainer   := .T.
   ::ControlParent := .T.
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Create() CLASS PictureBox
   LOCAL aSize, cType, n
   SWITCH VALTYPE( ::ImageName )
      CASE "A"
           IF EMPTY( ::Type )
              cType := UPPER( RIGHT( ::ImageName[1], 3 ) )
            ELSE
              cType := ::Type
           ENDIF
           IF ! ::DesignMode .OR. EMPTY( ::ImageName[1] )
              IF ( n := ASCAN( aDef, cType,,,.T.) ) > 0
                 ::pPicture := PictureLoadImageFromResource( ::AppInstance, UPPER(::ImageName[2]), n )
               ELSE
                 ::pPicture := PictureLoadFromResource( ::AppInstance, UPPER(::ImageName[2]), cType )
              ENDIF
              ::xImageName := ::ImageName[2]
            ELSE
              IF FILE( ::ImageName[1] )
                 ::pPicture := PictureLoadFromFile( ::ImageName[1] )
              ENDIF
              ::xImageName := ::ImageName[1]
           ENDIF
           ::__IsGif := cType == "GIF"
           EXIT

      CASE "C"
           IF EMPTY( ::Type )
              cType := UPPER( RIGHT( ::ImageName, 3 ) )
            ELSE
              cType := ::Type
           ENDIF
           IF AT( "\", ::ImageName ) > 0
              IF FILE( ::ImageName )
                 ::pPicture := PictureLoadFromFile( ::ImageName )
              ENDIF
            ELSE
              IF ( n := ASCAN( aDef, cType,,,.T.) ) > 0
                 ::pPicture := PictureLoadImageFromResource( ::AppInstance, ::ImageName, n )
                ELSE
                 ::pPicture := PictureLoadFromResource( ::AppInstance, ::ImageName, cType )
              ENDIF
           ENDIF
           ::__IsGif := cType == "GIF"

           EXIT
   END

   IF ::DesignMode .AND. !EMPTY( ::xImageName )
      ::Application:Project:AddImage( ::xImageName, ::Type, Self )
   ENDIF

   IF ::pPicture != NIL .AND. ::pPicture > 0
      aSize := PictureGetSize( ::pPicture )
      ::PictureWidth  := aSize[1]
      ::PictureHeight := aSize[2]
   ENDIF
   IF ::KeepAspectRatio .OR. ::Stretch
      ::VertScroll := .F.
      ::HorzScroll := .F.
   ENDIF
   ::Super:Create()
   IF aSize != NIL .AND. !::KeepAspectRatio .AND. !::Stretch
      ::OriginalRect[3] := aSize[1]
      ::OriginalRect[4] := aSize[2]
      ::__SetScrollBars()
   ENDIF
   ::InvalidateRect()
RETURN Self

//--------------------------------------------------------------------------------------------------------

METHOD Draw( hDC, x, y, hBmp ) CLASS PictureBox
   LOCAL hMemBitmap, hOldBitmap, hMemDC, aSize, nFill
   LOCAL aPixels
   DEFAULT x TO 0
   DEFAULT y TO 0

   IF ::TransparencyByPixel .AND. ::Transparent
      nFill := GetPixel( hDC,0,0)
   ENDIF

   IF ::xOpacity < 100
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)

      TransparentBlt( hMemDC, 0, 0, ::ClientWidth, ::ClientHeight, hDC, 0, 0, ::ClientWidth, ::ClientHeight, GetPixel( hMemDC,0,0) )

      PicturePaint( ::pPicture, hMemDC, x-::HorzScrollPos, y-::VertScrollPos, ::ClientWidth, ::ClientHeight, ::Stretch, ::KeepAspectRatio )

      aSize := {::PictureWidth, ::PictureHeight}
      IF ::KeepAspectRatio
         aSize := PictureSize( ::pPicture )
      ENDIF
      IF ::BlackAndWhite
         PictureDisplayBlackAndWhite( hMemDC, hMemBitmap, aSize[1], aSize[2] )
      ENDIF
      IF ::InvertedColors
         PictureInvertColors( hMemDC, hMemBitmap, aSize[1], aSize[2] )
      ENDIF

      aSize[1] := MIN( aSize[1], ::ClientWidth )
      aSize[2] := MIN( aSize[2], ::ClientHeight )

      aPixels := {}
      IF ::TransparencyByPixel .AND. ::Transparent
         ::__aPixels := __GetBkArray( hMemDC, ::ClientWidth, ::ClientHeight, GetPixel( hMemDC,0,0), hDC )
      ENDIF

      _AlphaBlend( hDC, x, y, aSize[1], aSize[2], hMemDC, x, y, aSize[1], aSize[2], ( 255 * ::xOpacity ) / 100 )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )

    ELSE
      PicturePaint( ::pPicture, hDC, x-::HorzScrollPos, y-::VertScrollPos, ::ClientWidth, ::ClientHeight, ::Stretch, ::KeepAspectRatio )

      aSize := {::PictureWidth, ::PictureHeight}
      IF ::KeepAspectRatio
         aSize := PictureSize( ::pPicture )
      ENDIF

      IF ::BlackAndWhite
         PictureDisplayBlackAndWhite( hDC, hBmp, aSize[1], aSize[2] )
      ENDIF
      IF ::InvertedColors
         PictureInvertColors( hDC, hBmp, aSize[1], aSize[2] )
      ENDIF
   ENDIF
RETURN NIL

METHOD __CreateBkBrush( hDC ) CLASS PictureBox
   LOCAL x := 0, y := 0
   LOCAL hMemBitmap, hOldBitmap, hBrush, hMemDC

   IF ::xBackColor != NIL
      hBrush := CreateSolidBrush( ::xBackColor )
   ENDIF

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   _FillRect( hMemDC, { ::LeftMargin, ::TopMargin, ::ClientWidth, ::ClientHeight }, IIF( hBrush != NIL, hBrush, GetSysColorBrush(COLOR_BTNFACE) ) )

   IF ::xBackColor != NIL
      DeleteObject( hBrush )
   ENDIF

   IF ::pPicture != NIL .AND. ::pPicture != 0
      IF ::xAlignment == 1
         x := ( ::ClientWidth - ::PictureWidth ) / 2
         y := ( ::ClientHeight - ::PictureHeight ) / 2
      ENDIF
      ::Draw( hMemDC, x, y, hMemBitmap )
   ENDIF

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
   ENDIF

   ::BkBrush := CreatePatternBrush( hMemBitmap )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnPaint() CLASS PictureBox
   LOCAL hDC := ::BeginPaint()
   ::__CreateBkBrush( hDC )
   _FillRect( hDC, { 0, 0, ::ClientWidth, ::ClientHeight }, ::BkBrush )
   ::EndPaint()
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD Destroy() CLASS PictureBox
   IF ::DesignMode
      ::Kill()
      ::Application:Project:RemoveImage( ::xImageName, Self )
   ENDIF
RETURN ::Super:Destroy()

METHOD SetImageName( cFile, cType ) CLASS PictureBox
   LOCAL aSize, n, cPrev
   IF VALTYPE( cFile ) == "A"
      cFile := IIF( ::DesignMode .AND. VALTYPE( cFile[1] ) == "C", cFile[1], cFile[2] )
   ENDIF

   cPrev := ::xImageName

   IF ::hWnd != NIL
      IF ::pPicture != NIL
         ::Kill()
      ENDIF
      IF !EMPTY( cFile )
         IF AT( ".", cFile ) > 0
            ::pPicture := PictureLoadFromFile( cFile )
          ELSE
            DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
            IF ( n := ASCAN( aDef, cType,,,.T.) ) > 0
               ::pPicture := PictureLoadImageFromResource( ::AppInstance, cFile, n )
              ELSE
               ::pPicture := PictureLoadFromResource( ::AppInstance, cFile, cType )
            ENDIF
         ENDIF
         IF ::pPicture != NIL
            aSize := PictureGetSize( ::pPicture )
            ::PictureWidth  := aSize[1]
            ::PictureHeight := aSize[2]
         ENDIF
       ELSE
         ::xImageName := NIL
      ENDIF
      ::InvalidateRect()

   ENDIF
   IF ::DesignMode
      IF !EMPTY( cPrev )
         ::Application:Project:RemoveImage( cPrev, Self )
      ENDIF
      IF !EMPTY( cFile )
         DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
         ::Application:Project:AddImage( cFile, cType, Self )
      ENDIF
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

