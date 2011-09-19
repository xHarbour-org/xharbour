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
   DATA TransparencyByPixel PUBLISHED INIT .F.
   DATA pPicture        EXPORTED
   DATA ResourceName    EXPORTED
   DATA PictureWidth    EXPORTED
   DATA PictureHeight   EXPORTED
   DATA Type            EXPORTED INIT ""
   DATA ImageIndex      EXPORTED INIT -1
   DATA __IsGif         PROTECTED
   DATA __aPixels       PROTECTED
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
   
   PROPERTY ImageName        READ xImageName        WRITE SetImageName   INVERT
   PROPERTY Stretch          READ xStretch          WRITE Update         DEFAULT .F.
   PROPERTY KeepAspectRatio  READ xKeepAspectRatio  WRITE Update         DEFAULT .F.
   PROPERTY Transparent      READ xTransparent      WRITE InvalidateRect DEFAULT .F.
   PROPERTY BlackAndWhite    READ xBlackAndWhite    WRITE InvalidateRect DEFAULT .F.
   PROPERTY InvertedColors   READ xInvertedColors   WRITE InvalidateRect DEFAULT .F.
   PROPERTY Opacity          READ xOpacity          WRITE InvalidateRect DEFAULT 100

   PROPERTY Alignment        READ xAlignment        WRITE Update         DEFAULT 1

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Draw()
   METHOD Kill() INLINE IIF( ::pPicture != NIL .AND. ::pPicture > 0, PictureRemove( ::pPicture ), ), ::pPicture := NIL
   METHOD OnPaint()
   METHOD SetImageName()
   METHOD SetBitmap( hBmp ) INLINE ::pPicture := PictureLoadBitmap( hBmp )
   METHOD OnGetDlgCode() INLINE DLGC_WANTMESSAGE
   METHOD OnEraseBkGnd() INLINE 1
   METHOD OnSize() INLINE ::InvalidateRect(), 0
   METHOD Update() INLINE IIF( ::hWnd != NIL, ::InvalidateRect(), )
   METHOD Destroy()
ENDCLASS

METHOD Init( oParent ) CLASS PictureBox
   DEFAULT ::__xCtrlName TO "PictureBox"
   ::Super:Init( oParent )
   ::Width        := 100
   ::Height       := 100
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
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
           IF ::__ClassInst == NIL .OR. EMPTY( ::ImageName[1] )
              IF ( n := ASCAN( aDef, cType,,,.T.) ) > 0
                 ::pPicture := PictureLoadImageFromResource( ::AppInstance, UPPER(::ImageName[2]), n )
               ELSE
                 ::pPicture := PictureLoadFromResource( ::AppInstance, UPPER(::ImageName[2]), "" )
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

   IF ::__ClassInst != NIL .AND. !EMPTY( ::xImageName )
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
   IF !EMPTY( ::Caption )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
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
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)

      PicturePaint( ::pPicture, hMemDC, x-::HorzScrollPos, y-::VertScrollPos, ::Width, ::Height, ::Stretch, ::KeepAspectRatio )

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

      aSize[1] := MIN( aSize[1], ::Width )
      aSize[2] := MIN( aSize[2], ::Height )

      aPixels := {}
      IF ::TransparencyByPixel .AND. ::Transparent
         ::__aPixels := __GetBkArray( hMemDC, ::Width, ::Height, GetPixel( hMemDC,0,0), hDC )
      ENDIF

      _AlphaBlend( hDC, 0, 0, aSize[1], aSize[2], hMemDC, 0, 0, aSize[1], aSize[2], ( 255 * ::xOpacity ) / 100 )

      //IF ::TransparencyByPixel
      //   __SetBkArray( hDC, ::__aPixels )
      //ENDIF
      
      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )

    ELSE
      PicturePaint( ::pPicture, hDC, x-::HorzScrollPos, y-::VertScrollPos, ::Width, ::Height, ::Stretch, ::KeepAspectRatio )

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

METHOD OnPaint( hDC, hMemDC ) CLASS PictureBox
   LOCAL x := 0, y := 0
   LOCAL hMemBitmap, hOldBitmap, hBrush, lTransparent, nFill
   LOCAL oChild, hOldBitmap1, hMemDC1, rgbTransparent := RGB(255,255,255)
   LOCAL pt := (struct POINT)

   IF !::IsWindow()
      RETURN 0
   ENDIF

   lTransparent := ::Transparent
   
   hBrush := ::BkBrush
   DEFAULT hBrush TO ::__hBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )

   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF

   _FillRect( hMemDC, {0, 0, ::Width, ::Height}, hBrush )

   IF hDC != NIL .AND. lTransparent
      BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
   ENDIF

   nFill := GetPixel( hMemDC, 0, 0 )
   
   IF ::pPicture != NIL .AND. ::pPicture != 0
      IF ::xAlignment == 2
         x := ( ::Width / 2 ) - ( ::PictureWidth / 2 )
         y := ( ::Height / 2 ) - ( ::PictureHeight / 2 )
      ENDIF
      ::Draw( hMemDC, x, y, hMemBitmap )
   ENDIF

   IF lTransparent .AND. ( !::TransparencyByPixel .OR. ::xOpacity == 100 )
      TransparentBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, ::Width, ::Height, GetPixel( hMemDC,0,0) )
   ENDIF

   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF
          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin, SRCCOPY )
          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )
      NEXT
   ENDIF

   IF hDC != NIL
      IF !lTransparent .OR. ( ::TransparencyByPixel .AND. ::xOpacity < 100 )
         BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
      ENDIF

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )

   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------

METHOD Destroy() CLASS PictureBox
   IF ::__ClassInst != NIL
      ::Kill()
      ::Application:Project:RemoveImage( ::xImageName, Self )
   ENDIF
RETURN ::Super:Destroy()

METHOD SetImageName( cFile, cType ) CLASS PictureBox
   LOCAL aSize, n, cPrev
   IF VALTYPE( cFile ) == "A"
      cFile := IIF( ::__ClassInst != NIL .AND. VALTYPE( cFile[1] ) == "C", cFile[1], cFile[2] )
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
   IF ::__ClassInst != NIL 
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

