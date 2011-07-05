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
   CLASSDATA aCursor EXPORTED
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
   DATA nMove        EXPORTED INIT 0
   DATA oLast        EXPORTED

   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnPaint()
   METHOD Create()
   METHOD OnMouseMove()
   METHOD CreateControl()
   METHOD OnDestroy()  INLINE DeleteObject( ::hBmpGrid ), NIL
   METHOD Snap( nPos ) INLINE IIF( ::Application:Props[ "ViewMenuGrid" ]:Checked, Snap( nPos, ::xGrid ), nPos )
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS RepEdit
   LOCAL cBits, xSize, ySize, aSize, hDC, n
   ::oPs := PageSetup( ::Application:MainForm )
   ::oPs:ReturnDefault := .T.
   ::oPs:Show()

   DEFAULT ::aCursor TO {::System:Cursor:SizeNWSE,;
                         ::System:Cursor:SizeWE,;
                         ::System:Cursor:SizeNESW,;
                         ::System:Cursor:SizeNS }
   
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
   LOCAL aPoint, aPoints, oCtrl, n, hDC, aPt, aRect[4], nx, ny
   oCtrl := ::Application:Props:PropEditor:ActiveObject
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
         DO CASE
            CASE ::nMove == 0 // move
                 oCtrl:Left := IIF( oCtrl:ClsName != "Group", ::Snap( x-::nDownPos[1] ), -1 )
                 oCtrl:Top  := MAX( 0, ::Snap( y-::nDownPos[2] ) )

            CASE ::nMove == 1 // Top-Left
                 nx := oCtrl:Left
                 ny := oCtrl:Top
                 oCtrl:Left   := ::Snap( x-::nDownPos[1] )
                 oCtrl:Top    := ::Snap( y-::nDownPos[2] )
                 oCtrl:Width  += nx-oCtrl:Left
                 oCtrl:Height += ny-oCtrl:Top
                 oCtrl:EditCtrl:Width  += nx-oCtrl:Left
                 oCtrl:EditCtrl:Height += ny-oCtrl:Top

            CASE ::nMove == 2 // Left
                 nx := oCtrl:Left
                 oCtrl:Left := ::Snap( x-::nDownPos[1] )
                 oCtrl:Width += nx-oCtrl:Left
                 oCtrl:EditCtrl:Width += nx-oCtrl:Left

             CASE ::nMove == 3 // Left-Bottom
                 nx := oCtrl:Left
                 ny := oCtrl:Height
                 oCtrl:Left   := ::Snap( x-::nDownPos[1] )
                 oCtrl:Height := ::Snap( y-oCtrl:Top )
                 oCtrl:Width  += nx-oCtrl:Left
                 oCtrl:EditCtrl:Width  += nx-oCtrl:Left
                 oCtrl:EditCtrl:Height := oCtrl:Height

            CASE ::nMove == 4 // Bottom
                 oCtrl:Height := ::Snap( y-oCtrl:Top )
                 oCtrl:EditCtrl:Height := oCtrl:Height

            CASE ::nMove == 5 // Right-Bottom
                 oCtrl:Width  := ::Snap( x-oCtrl:Left )
                 oCtrl:Height := ::Snap( y-oCtrl:Top )
                 oCtrl:EditCtrl:Width := oCtrl:Width
                 oCtrl:EditCtrl:Height := oCtrl:Height

            CASE ::nMove == 6 // Right
                 oCtrl:Width := ::Snap( x-oCtrl:Left )
                 oCtrl:EditCtrl:Width := oCtrl:Width

            CASE ::nMove == 7 // Top-Right
                 ny := oCtrl:Top
                 oCtrl:Top    := ::Snap( y-::nDownPos[2] )
                 oCtrl:Width  := ::Snap( x-oCtrl:Left )
                 oCtrl:Height += ny-oCtrl:Top
                 oCtrl:EditCtrl:Width  := oCtrl:Width
                 oCtrl:EditCtrl:Height += ny-oCtrl:Top

            CASE ::nMove == 8 // Top
                 ny := oCtrl:Top
                 oCtrl:Top    := ::Snap( y-::nDownPos[2] )
                 oCtrl:Height += ny-oCtrl:Top
                 oCtrl:EditCtrl:Height += ny-oCtrl:Top

         ENDCASE
         oCtrl:MoveWindow()
         ::Application:Report:Modified := .T.
      ENDIF
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD CreateControl( hControl, x, y, oParent ) CLASS RepEdit
   EXTERN VrLabel, VrLine, VrImage, VrDataTable, VrTheme, VrTotal, VrFormula, VrGroupHeader, VrGroupFooter, VrTotal
   LOCAL xValue, xVar, hWnd, n, oControl, hPointer := HB_FuncPtr( IIF( VALTYPE( hControl ) == "C", hControl, hControl:ClsName ) )
   
   IF hPointer != NIL
      DEFAULT oParent TO Self
      DEFAULT x TO 0
      DEFAULT y TO 0
      oControl := HB_Exec( hPointer,, oParent )
      oControl:__ClsInst := __ClsInst( oControl:ClassH )
      oControl:Left := x 
      oControl:Top  := y 
      oControl:Create()

      IF VALTYPE( hControl ) == "H"
         FOR n := 1 TO LEN( oControl:aProperties )
             IF HGetPos( hControl, oControl:aProperties[n][1] ) > 0 .AND. UPPER( oControl:aProperties[n][1] ) != "FONT"

                xVar := __objSendMsg( oControl, oControl:aProperties[n][1] )
                xValue := hControl[ oControl:aProperties[n][1] ]
                IF VALTYPE( xVar ) != VALTYPE( xValue )
                   DO CASE
                      CASE VALTYPE( xVar ) == "N"
                           xValue := VAL( xValue )

                      CASE VALTYPE( xVar ) == "D"
                           xValue := DTOC( xValue )

                      CASE VALTYPE( xVar ) == "L"
                           xValue := xValue == "True"
                   ENDCASE
                ENDIF
                __objSendMsg( oControl, "_" + oControl:aProperties[n][1], xValue )
             ENDIF
         NEXT
      ENDIF
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
      IF UPPER( ::Application:Props:ToolBox:ActiveItem:Caption ) == "GROUP" .AND. ::Type != "Body"
         RETURN NIL
      ENDIF
      ::CreateControl( "Vr"+::Application:Props:ToolBox:ActiveItem:Caption, x, y )
    ELSEIF ::Type != "ExtraPage"
      pt := (struct POINT)
      pt:x := x
      pt:y := y
      ClientToScreen( ::hWnd, @pt )
      ::aSelect := {x,y}
    ELSE
      ::Application:Props:PropEditor:ResetProperties( {{ Self }} )
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
         IF ASCAN( :ActiveObject:aProperties, {|a| a[1]=="Width"} ) > 0
            :CheckValue( "Width",  "Size",     :ActiveObject:Width )
         ENDIF
         IF ASCAN( :ActiveObject:aProperties, {|a| a[1]=="Height"} ) > 0
            :CheckValue( "Height", "Size",     :ActiveObject:Height )
         ENDIF
      END
   CATCH
   END
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC ) CLASS RepEdit
   LOCAL hOldBrush, hOldPen, aRect, oCtrl, cx, cy, nX, nY
   LOCAL hMemDC, hMemBitmap, hOldBitmap, nBColor, nFColor, lMarkers := .F.

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

   IF ::Application:Props:PropEditor:ActiveObject != NIL  .AND. ::nDownPos == NIL
      oCtrl := ::Application:Props:PropEditor:ActiveObject:EditCtrl
      IF ::Application:Props:PropEditor:ActiveObject:lUI .AND. oCtrl:Parent:hWnd == ::hWnd
         aRect := oCtrl:GetRectangle()
         _DrawFocusRect( hMemDC, {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1} )
         lMarkers := .T.
      ENDIF
   ENDIF
   BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
   
   IF lMarkers
      hDC := GetDCEx( ::hWnd )
      PaintMarkers( hDC, oCtrl )
      ReleaseDC( ::hWnd, hDC )
      ::oLast := oCtrl
    ELSEIF ::oLast != NIL
      ::oLast:InvalidateRect()
      ::oLast := NIL
   ENDIF
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
   DATA PagePosition EXPORTED
   DATA aProperties  EXPORTED INIT { { "PagePosition", "Position" } }
   DATA EditCtrl     EXPORTED
   DATA lUI          EXPORTED INIT .F.
   METHOD GetValue( cVal ) INLINE ::&cVal
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

FUNCTION KeyDown( oCtrl, nKey )
   LOCAL lShift, aRect, nMove := 1, lMod := .T.
   IF oCtrl != NIL .AND. oCtrl:Cargo != NIL
      IF oCtrl:Cargo:lUI
         aRect := oCtrl:GetRectangle()
         lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
         IF CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
            nMove := 8
         ENDIF
         aRect[1]-=(nMove+4)
         aRect[2]-=(nMove+4)
         aRect[3]+=(nMove+4)
         aRect[4]+=(nMove+4)
      ENDIF
      IF nKey == VK_DELETE
         oCtrl:Cargo:Delete()
       ELSEIF nKey == VK_LEFT
         IF lShift .AND. oCtrl:aSize[2]
            oCtrl:Cargo:Width -= nMove
            oCtrl:Width -= nMove
          ELSE
            oCtrl:Cargo:Left -= nMove
            oCtrl:Left -= nMove
         ENDIF
       ELSEIF nKey == VK_UP
         IF lShift .AND. oCtrl:aSize[8]
            oCtrl:Cargo:Height -= nMove
            oCtrl:Height -= nMove
          ELSE
            oCtrl:Cargo:Top -= nMove
            oCtrl:Top -= nMove
         ENDIF
       ELSEIF nKey == VK_RIGHT
         IF lShift .AND. oCtrl:aSize[6]
            oCtrl:Cargo:Width += nMove
            oCtrl:Width += nMove
          ELSE
            oCtrl:Cargo:Left += nMove
            oCtrl:Left += nMove
         ENDIF
       ELSEIF nKey == VK_DOWN
         IF lShift .AND. oCtrl:aSize[4]
            oCtrl:Cargo:Height += nMove
            oCtrl:Height += nMove
          ELSE
            oCtrl:Cargo:Top += nMove
            oCtrl:Top += nMove
         ENDIF
       ELSE
         lMod := .F.
      ENDIF
      IF lMod
         IF !oCtrl:Application:Report:Modified
            oCtrl:Application:Report:Modified := .T.
         ENDIF
         IF oCtrl:Cargo:lUI
            oCtrl:Parent:InvalidateRect( aRect, .T.)
          ELSEIF oCtrl:Cargo:Button != NIL
            oCtrl:Cargo:Button:Delete()
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------------

CLASS __VrGroup INHERIT RepEdit
   DATA aSize EXPORTED INIT {.F.,.F.,.F.,.T.,.F.,.F.,.F.,.T.}
   DATA FlatCaption EXPORTED INIT .T.
   DATA Type INIT "Group"
   METHOD OnLButtonDown()
   METHOD OnMouseMove()
   METHOD OnMouseLeave()     INLINE ::Parent:Cursor := NIL, NIL
   METHOD OnKeyDown(n)       INLINE KeyDown( Self, n )
   METHOD OnGetDlgCode()     INLINE DLGC_WANTMESSAGE + DLGC_WANTCHARS + DLGC_WANTARROWS + DLGC_HASSETSEL
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown(n,x,y) CLASS __VrGroup 
   LOCAL aRect, oCtrl
   ::Parent:SetCapture()
   IF ::Application:Props:ToolBox:ActiveItem != NIL
      ::Parent:CreateControl( "Vr"+::Application:Props:ToolBox:ActiveItem:Caption, x, y, ::Cargo )
    
    ELSEIF ::Application:Props:PropEditor:ActiveObject != NIL
      oCtrl := ::Application:Props:PropEditor:ActiveObject:EditCtrl
      TRY
         IF oCtrl != NIL
            aRect := oCtrl:GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            oCtrl:Parent:InvalidateRect( aRect, .F. )
            aRect := ::GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            ::Parent:InvalidateRect( aRect, .F. )
            ::Parent:nDownPos := {x,y}
         ENDIF
      CATCH
      END
      ::SetFocus()
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------

METHOD OnMouseMove(n,x,y) CLASS __VrGroup 
   LOCAL oCtrl
   IF n == MK_LBUTTON
      oCtrl := ::Application:Props:PropEditor:ActiveObject
      IF !(oCtrl == Self)
         Super:OnMouseMove(n,x,y)
      ENDIF
    ELSE
      MouseMove( Self, n, x, y )
   ENDIF
RETURN NIL
