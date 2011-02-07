/*
 * $Id$
 */

// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!

#include "vxh.ch"
#include "debug.ch"

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ComponentPanel INHERIT Panel
   DATA Current   EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD AddButton()
   METHOD Reset()
   METHOD Close()
   METHOD Refresh()
   METHOD OnSize()        INLINE ::InvalidateRect(,.T.), NIL
   METHOD OnEraseBkGnd(h) INLINE _FillRect( h, {0,0,::ClientWidth,::ClientHeight}, ::System:CurrentScheme:Brush:ToolStripGradientBegin ), 1
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ComponentPanel

   DEFAULT ::__xCtrlName  TO "ComponentPanel"

   ::Super:Init( oParent )
   ::LeftMargin   := 10
   ::TopMargin    := 10
   ::Width        := 200
   ::AllowUndock  := .T.
   //::StaticEdge   := .T.
   ::HorzScroll   := .T.
   ::TabStop      := .T.
RETURN Self
//------------------------------------------------------------------------------------------

METHOD AddButton( oComponent ) CLASS ComponentPanel
   LOCAL n, x, lExit, nBtn, aButtons, hIcon, oItem, aSize, oBtn := CompButton( Self )
   oComponent:Button := oBtn
   WITH OBJECT oBtn
      :Component   := oComponent
      :Left        := :Parent:LeftMargin - :Parent:HorzScrollPos
      :OwnerDraw   := .T.
      :Caption     := oComponent:Name
      :ImageList   := ::Application:MainForm:ToolBox1:ImageList
      :Action      := {|o| o:SelectComponent()}
      
      aButtons := ::Application:MainForm:ToolBox1:aButtons
      nBtn  := 1
      lExit := .F.
      
     
      FOR n := 1 TO LEN( aButtons )
          FOR x := 1 TO LEN( aButtons[n][2] )
              //IF ( hIcon := LoadImage( ::Application:Instance, "ICO_" + UPPER( aButtons[n][2][x][1] ), IMAGE_ICON ) ) != 0
              //   DestroyIcon( hIcon )
                 nBtn ++
              //ENDIF
              IF aButtons[n][2][x][1] == oComponent:__xCtrlName
                 n := LEN( aButtons )+1 // force exit first loop
                 EXIT
              ENDIF
          NEXT
      NEXT

      IF oComponent:__xCtrlName == "Report"
         nBtn := ::Application:MainForm:ToolBox1:ImageList:Count
      ENDIF

      :ImageIndex  := nBtn //oComponent:ImageIndex

      :ImageAlign  := DT_LEFT
      :ImageIndent := 8
      :DrawTheme   := .F.

      :Font:Bold     := .T.

      aSize        := :Drawing:GetTextExtentPoint32( oComponent:Name )
      :Width       := aSize[1] + :ImageList:IconWidth + 15
      :Parent:LeftMargin += :Width //+ 15
      ::OriginalRect[3] := ::LeftMargin + 10
      :Create()
   END
   ::InvalidateRect()
   ::MoveWindow()
   ::__SetScrollBars()
RETURN oBtn

//------------------------------------------------------------------------------------------

METHOD Reset() CLASS ComponentPanel
   LOCAL n, Component, Btn, aChildren, lUpdate := .T.
   
   aChildren := {}
   FOR n := 1 TO LEN( ::Children )
       IF ::Children[n]:ClsName == "Button"
          AADD( aChildren, ::Children[n] )
       ENDIF
   NEXT
   AEVAL( aChildren, {|o| o:Destroy() } )

   ::LeftMargin := 10
RETURN Self

METHOD Refresh() CLASS ComponentPanel
   LOCAL oBtn
   IF ::Current != NIL
      FOR EACH oBtn IN ::Children
         IF !(oBtn:Component == ::Application:ObjectManager:ActiveObject )
            oBtn:Selected := .F.
         ENDIF
      NEXT
   ENDIF
   ::InvalidateRect(,.F.)
RETURN NIL

//------------------------------------------------------------------------------------------

METHOD Close() CLASS ComponentPanel
   LOCAL n, Component, Btn, aChildren, lUpdate := .T.
   aChildren := {}
   FOR n := 1 TO LEN( ::Children )
       IF ::Children[n]:ClsName == "Button"
          AADD( aChildren, ::Children[n] )
       ENDIF
   NEXT
   AEVAL( aChildren, {|o| o:Destroy() } )
//   FOR n := 1 TO LEN( ::Children )
//       IF ::Children[n]:ClsName == "Button"
//          ::Children[n]:Destroy()
//          n--
//       ENDIF
//   NEXT
   ::Hide()
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS CompButton INHERIT Button
   DATA Selected  EXPORTED INIT .F.
   DATA Component EXPORTED
   DATA DropCtrl  EXPORTED
   DATA ImageList EXPORTED

   METHOD OnParentDrawItem()
   METHOD OnLButtonDown()
   METHOD OnKeyDown()
   METHOD Select()
   METHOD Delete()
   METHOD SelectComponent()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Select() CLASS CompButton
   IF ::Parent:Current != NIL
      ::Parent:Current:Selected := .F.
      ::Parent:Current:InvalidateRect(,.F.)
   ENDIF
   ::Parent:Current := Self
   ::Selected := .T.
   ::InvalidateRect(,.F.)
   ::Application:Report:EditReset(1)
RETURN Self

METHOD OnLButtonDown() CLASS CompButton
   ::Select()
RETURN NIL

METHOD SelectComponent() CLASS CompButton
   ::Application:Props:PropEditor:ResetProperties( {{ ::Component }} )
   ::Application:Report:EditReset(1)
RETURN 0

//------------------------------------------------------------------------------------------
METHOD Delete() CLASS CompButton
   LOCAL oBtn, Control
   ::Destroy()
   ::Parent:LeftMargin := 10
   FOR EACH oBtn IN ::Parent:Children
       IF oBtn:ClsName == "Button"
          oBtn:Left := ::Parent:LeftMargin - ::Parent:HorzScrollPos
          ::Parent:LeftMargin += oBtn:Width //+ 15
          oBtn:MoveWindow()
       ENDIF
   NEXT
   ::Parent:OriginalRect[3] := ::Parent:LeftMargin + 10
   ::Parent:MoveWindow(,,,,.T.)
   ::Parent:__SetScrollBars()
RETURN NIL

METHOD OnKeyDown( nwParam, nlParam ) CLASS CompButton
   LOCAL oBtn, Control
   IF nwParam == VK_DELETE
   ENDIF
RETURN 0

//------------------------------------------------------------------------------------------

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS CompButton

   LOCAL nLeft, nTop, hDC, cPaint, aRect, nStyle, lDisabled, lSelected, lFocus, hTheme, aTextRect, nTextFlags

   IF dis:CtlType & ODT_BUTTON != 0 .AND. ( ( ::ImageList != NIL .AND. ::ImageIndex != NIL ) .OR. ::bkBrush != NIL .OR. ::ForeColor != NIL )
      nTop := 5
      nLeft:= 3
      aRect := { dis:rcItem:Left, dis:rcItem:Top, dis:rcItem:Right, dis:rcItem:Bottom }

      lDisabled := dis:itemState & ODS_DISABLED != 0
      lSelected := dis:itemState & ODS_SELECTED != 0
      lFocus    := dis:itemState & ODS_FOCUS != 0

      aTextRect  := aClone( aRect )
      nTextFlags := DT_CENTER + DT_VCENTER + DT_SINGLELINE

      IF ::ImageList != NIL .AND. ::ImageIndex != NIL
         DO CASE
            CASE ::ImageAlign == DT_LEFT
                 nTop  := ( aRect[4] / 2 ) - ( ::ImageList:IconHeight / 2 )
                 aTextRect[1] += ::ImageList:IconWidth + ::ImageIndent
                 nTextFlags := DT_LEFT + DT_VCENTER + DT_SINGLELINE

            CASE ::ImageAlign == DT_RIGHT
                 nLeft := ( aRect[3] - ::ImageList:IconWidth ) + ::ImageIndent
                 nTop  := ( aRect[4] / 2 ) - ( ::ImageList:IconHeight / 2 )
                 aTextRect[3] -= ::ImageList:IconWidth
                 nTextFlags := DT_RIGHT  + DT_VCENTER + DT_SINGLELINE

            CASE ::ImageAlign == DT_CENTER
                 nLeft := ( aRect[3] / 2 ) - ( ::ImageList:IconWidth / 2 )

                 aTextRect[2] += ::ImageList:IconHeight
                 nTextFlags := DT_CENTER + DT_VCENTER + DT_SINGLELINE

         ENDCASE
         IF EMPTY( ::Caption )
            nTop := ( aRect[4] / 2 ) - ( ::ImageList:IconHeight / 2 )
         ENDIF
      ENDIF

      _FillRect( dis:hDC, aRect, ::System:CurrentScheme:Brush:ToolStripGradientBegin )

      IF ::Selected
         DrawFocusRect( dis:hDC, dis:rcItem )
      ENDIF
      IF ::ImageList != NIL .AND. ::ImageIndex != NIL
         ::ImageList:DrawImage( dis:hDC, ::ImageIndex, nLeft, nTop )
      ENDIF

      SetBkMode( dis:hDC, TRANSPARENT )

      _DrawText( dis:hDC, ::Caption, aTextRect, nTextFlags )
   ENDIF

RETURN NIL


