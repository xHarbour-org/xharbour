/*
 * $Id$
 */

// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!

#xtranslate Ceil( <x> ) => ( Iif( <x> - Int( <x> ) > 0, Int( <x> ) + 1, <x> ) )

#include "vxh.ch"
#include "debug.ch"
#include "winuser.ch"

CLASS RepEdit INHERIT Panel
   DATA Objects      EXPORTED INIT {}
   DATA FlatCaption  EXPORTED INIT .T.
   DATA FlatBorder   EXPORTED INIT .T.
   DATA xGrid        EXPORTED INIT 8
   DATA yGrid        EXPORTED INIT 8
   DATA hBmpGrid     EXPORTED 
   DATA xBmpSize     EXPORTED 
   DATA yBmpSize     EXPORTED 
   DATA aSelect      EXPORTED 
   DATA aPrevSel     EXPORTED 
   DATA nDownPos     EXPORTED
   DATA oPs          EXPORTED
   
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnPaint()
   METHOD Create()
   METHOD OnMouseMove()
   METHOD OnDestroy() INLINE DeleteObject( ::hBmpGrid ), NIL
   METHOD CreateControl()
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS RepEdit
   LOCAL cBits, xSize, ySize, aSize, hDC, n
   ::oPs := PageSetup( ::Application:MainForm )
   ::oPs:ReturnDefault := .T.
   ::oPs:Show()
   
   n := ( ::Parent:ClientWidth / ::oPs:PageWidth ) * 100
   
   ::Width := ::oPs:PageWidth - ( ( ::oPs:PageWidth * n ) / 100 )
   ::Parent:OriginalRect[3] := ::Width + 6
   
   Super:Create()
   ::ForeColor := ::System:Color:LightGray
   cBits := MakeGridTile( ::xGrid, ::yGrid, @xSize, @ySize )
   IF !Empty(::hBmpGrid)
      DeleteObject(::hBmpGrid)
   ENDIF
   ::hBmpGrid := CreateBitmap( xSize, ySize, 1, 1, cBits )
   ::xBmpSize := xSize
   ::yBmpSize := ySize
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnMouseMove( nwParam, x, y ) CLASS RepEdit
   LOCAL oCtrl, hDC, aPt, aRect[4]
   IF nwParam == MK_LBUTTON 
      IF ::aSelect != NIL
         hDC := GetDC( ::hWnd )
         IF ::aPrevSel != NIL
            _DrawFocusRect( hDC, ::aPrevSel )
         ENDIF

         aRect[1] := MIN( ::aSelect[1], x )
         aRect[2] := MIN( ::aSelect[2], y )

         aRect[3] := MAX( ::aSelect[1], x )
         aRect[4] := MAX( ::aSelect[2], y )

         ::aPrevSel := aRect
         _DrawFocusRect( hDC, aRect )
         ReleaseDC( ::hWnd, hDC )
       ELSEIF ::nDownPos != NIL
         oCtrl := ::Application:Props:PropEditor:ActiveObject
         IF ::Application:Props[ "ViewMenuGrid" ]:Checked
            oCtrl:Left := Snap( x-::nDownPos[1], ::xGrid )
            oCtrl:Top  := Snap( y-::nDownPos[2], ::xGrid )
          ELSE
            oCtrl:Left := x-::nDownPos[1]
            oCtrl:Top  := y-::nDownPos[2]
         ENDIF
         oCtrl:MoveWindow()
      ENDIF
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD CreateControl( cControl, x, y ) CLASS RepEdit
   EXTERN VrLabel, VrLine, VrImage, VrDataTable
   LOCAL oControl, hPointer := HB_FuncPtr( cControl )
   IF hPointer != NIL
      DEFAULT x TO 0
      DEFAULT y TO 0
      oControl := HB_Exec( hPointer,, Self )
      oControl:__ClsInst := __ClsInst( oControl:ClassH )
      oControl:Left := x 
      oControl:Top  := y 
      oControl:Create()
      
      IF ::Application:Props:ToolBox:ActiveItem != NIL
         ::Application:Props:ToolBox:ActiveItem:PointerItem:Select()
         ::Application:Report:Modified := .T.
         ::Application:Props:PropEditor:ResetProperties( {{ oControl }} )
      ENDIF
      
      IF !oControl:lUI
         ::Application:Props:Components:AddButton( oControl )
      ENDIF
   ENDIF
RETURN oControl

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown( nwParam, x, y ) CLASS RepEdit
   LOCAL pt
   ::SetCapture()
   IF ::Application:Props:ToolBox:ActiveItem != NIL
      ::CreateControl( "Vr"+::Application:Props:ToolBox:ActiveItem:Caption, x, y )
    ELSE
      pt := (struct POINT)
      pt:x := x
      pt:y := y
      ClientToScreen( ::hWnd, @pt )
      ::aSelect := {x,y}
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp( nwParam, x, y ) CLASS RepEdit
   ::aSelect  := NIL
   ::aPrevSel := NIL
   ::nDownPos := NIL
   ::InvalidateRect(, .F. )
   ::ReleaseCapture()
   TRY
      WITH OBJECT ::Application:Props:PropEditor
         :CheckValue( "Left",   "Position", :ActiveObject:Left )
         :CheckValue( "Top",    "Position", :ActiveObject:Top )
      END
   CATCH
   END
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC ) CLASS RepEdit
   LOCAL hOldBrush, hOldPen, aRect, oCtrl, cx, cy, nX, nY
   LOCAL hMemDC, hMemBitmap, hOldBitmap, nBColor, nFColor
    
   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   nBColor := SetBkColor( hMemDC, ::BackColor )
   nFColor := SetTextColor( hMemDC, ::ForeColor )
   SetBkMode( hMemDC, TRANSPARENT )
   IF ::Application:Props[ "ViewMenuGrid" ]:Checked
      DrawGrid( hMemDC, ::hBmpGrid, ::xBmpSize, ::yBmpSize, ::Width, ::Height, SRCCOPY )
    ELSE
      _Fillrect( hMemDC, {0,0,::Width,::Height}, ::BkBrush )
   ENDIF
   SetTextColor( hMemDC, nFColor )
   SetBkColor( hMemDC, nBColor )
   
   cx := ::Width
   cy := ::Height
   /*
   IF ::Application:Report:VrReport != NIL .AND. ::Application:Report:VrReport:oPDF != NIL
      nX := GetDeviceCaps( hDC, LOGPIXELSX )
      nY := GetDeviceCaps( hDC, LOGPIXELSY )
      cx := Int( ( ::Application:Report:VrReport:oPDF:PageWidth / 1440 ) * nX )
      cy := Int( ( ::Application:Report:VrReport:oPDF:PageLength / 1440 ) * nY )
      _Fillrect( hMemDC, {cx,0,cx+::Width, cy}, GetStockObject( LTGRAY_BRUSH ) )
   ENDIF
   */
   IF ::Application:Props:PropEditor:ActiveObject != NIL  .AND. ::nDownPos == NIL
      oCtrl := ::Application:Props:PropEditor:ActiveObject:EditCtrl
      IF ::Application:Props:PropEditor:ActiveObject:lUI .AND. oCtrl:Parent:hWnd == ::hWnd
         aRect := oCtrl:GetRectangle()
         _DrawFocusRect( hMemDC, {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1} )
      ENDIF
   ENDIF
   BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
RETURN 0

//--------------------------------------------------------------------------------------------------------------------------------------
CLASS HeaderEdit INHERIT RepEdit
   DATA Type INIT "Header"
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------------
CLASS BodyEdit INHERIT RepEdit
   DATA Type INIT "Body"
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------------
CLASS FooterEdit INHERIT RepEdit
   DATA Type INIT "Footer"
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------------
CLASS RepHeaderEdit INHERIT RepEdit
   DATA Type INIT "RepHeader"
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------------
CLASS RepFooterEdit INHERIT RepEdit
   DATA Type INIT "RepFooter"
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------------
CLASS ExtraPageEdit INHERIT RepEdit
   DATA Type INIT "ExtraPage"
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION MakeGridTile( nxGrid, nyGrid, Width, Height)

   LOCAL nWidth   := 256 - 256 % nxGrid
   LOCAL nHeight  := 256 - 256 % nyGrid
   LOCAL cBits    := ""
   LOCAL nByte    := 1
   LOCAL nVal     := 0
   LOCAL cDotted
   LOCAL cEmpty
   LOCAL nBits
   LOCAL nPos
   LOCAL i

   Width   := nWidth
   Height  := nHeight
   nWidth  := Ceil( nWidth/8 )
   nWidth  := If( nWidth%2==0, nWidth, nWidth+1 )
   cDotted := Replicate( Chr(255), nWidth )
   cEmpty  := cDotted
   nBits   := ( 8*nWidth ) - 1

   FOR i:=0 TO nBits STEP nxGrid
      IF i >= nByte*8
         cDotted[ nByte ] := 255-nVal
         nVal  := 2^( 7-(i%8) )
         nByte := Int(i/8) + 1
      ELSE
         nVal += 2^( 7-(i%8) )
      ENDIF
   NEXT
   cDotted[nByte] := 255-nVal

   nHeight--
   FOR i:= 0 TO nHeight
      IF i % nyGrid == 0
         cBits += cDotted
      ELSE
         cBits += cEmpty
      ENDIF
   NEXT
RETURN cBits

FUNCTION Snap( x, nGrain )
RETURN ROUND( ( x / nGrain ), 0) * nGrain
