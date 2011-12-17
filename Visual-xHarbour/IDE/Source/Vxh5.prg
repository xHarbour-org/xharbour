/*
 * $Id$
 */
#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

#define PBS_NORMAL     1
#define PBS_HOT        2
#define PBS_PRESSED    3
#define PBS_DISABLED   4
#define PBS_DEFAULTED  5
#define BP_PUSHBUTTON  1

#define DG_ADDCONTROL      1
#define DG_DELCONTROL      2
#define DG_PROPERTYCHANGED 3
#define DG_MOVESELECTION   4
#define DG_FONTCHANGED     5
#define DG_DELCOMPONENT    6

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS DebugTab INHERIT TabStrip
   METHOD OnSysCommand(n) INLINE IIF( n==SC_CLOSE, (::Hide(),0),)
   METHOD Close() INLINE ::Hide()
ENDCLASS

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS DebugBuild INHERIT ListBox
   METHOD Init() CONSTRUCTOR
   METHOD Create()
ENDCLASS

//------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS DebugBuild

   DEFAULT ::__xCtrlName  TO "DebugBuild"

   #ifdef DLL
      Application := GetApplication()
   #endif

   ::Super:Init( oParent )
   ::HorzScroll   := .T.
   ::VertScroll   := .T.
   ::ClientEdge   := .F.
   ::StaticEdge   := .T.
   ::ClipChildren := .F.
   ::ClipSiblings := .F.
   ::HasStrings   := .T.
   ::Border       := .F.
RETURN Self

//------------------------------------------------------------------------------------------
METHOD Create() CLASS DebugBuild
   ::Super:Create()
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ErrorListView INHERIT ListView
   METHOD OnParentNotify()
   METHOD ProcessErrors()
ENDCLASS

//------------------------------------------------------------------------------------------
METHOD ProcessErrors( oError, aErrors ) CLASS ErrorListView
   LOCAL n
   ( oError )
   ::ResetContent()
   
   FOR n := 1 TO LEN( aErrors )
       ::InsertItem( aErrors[n][1],, n )
       ::SetItemText( n-1, 1, aErrors[n][2] )
       ::SetItemText( n-1, 2, aErrors[n][3] )
       ::SetItemText( n-1, 3, aErrors[n][4] )
   NEXT
   ::EnsureVisible( 0, .F. )
   ::EnsureVisible( 1, .F. )
   ::EnsureVisible( 2, .F. )
   ::EnsureVisible( 3, .F. )
RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnParentNotify() CLASS ErrorListView
   LOCAL iFile, lvi, nLine, cFile, oEditor, nFor

   SWITCH ::Parent:hdr:code
      CASE NM_DBLCLK
         //PopupEditor
         IF SendMessage( ::hWnd, LVM_GETSELECTEDCOUNT, 0, 0 ) > 0
            iFile := SendMessage( ::hWnd, LVM_GETNEXTITEM, -1, LVNI_SELECTED )

            lvi := (struct LVITEM)
            lvi:iSubItem   := 0
            lvi:cchTextMax := 512
            lvi:pszText    := SPACE( 512 )

            SendMessage( ::hWnd, LVM_GETITEMTEXT, iFile, @lvi )
            cFile := Upper( Left( lvi:pszText, At( Chr(0), lvi:pszText ) - 1 ) )

            lvi:iSubItem   := 1
            lvi:cchTextMax := 512
            lvi:pszText    := SPACE( 512 )

            SendMessage( ::hWnd, LVM_GETITEMTEXT, iFile, @lvi )
            nLine := Val( Left( lvi:pszText, At( Chr(0), lvi:pszText ) - 1 ) )

            nFor := 1
            FOR EACH oEditor IN xEdit_GetEditors()
               IF Upper( oEditor:cPath + oEditor:cFile ) == cFile
                  ::Application:EditorPage:Select()
                  ::Application:SourceTabs:SetCurSel( nFor )
                  ::Application:SourceEditor:oEditor := oEditor
                  oEditor:SetDisplay( ::Application:SourceEditor:oEditor:oDisplay, .T. )
                  oEditor:GoLine( nLine )
                  ::Application:SourceEditor:SetFocus()
                  EXIT
               ENDIF
               nFor++
            NEXT
         ENDIF
         EXIT
   END
RETURN 0

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
   ::HorzScroll   := .T.
   ::TabStop      := .T.
RETURN Self

//------------------------------------------------------------------------------------------
METHOD AddButton( oComponent ) CLASS ComponentPanel
   LOCAL n, x, lExit, nBtn, aButtons, aSize, oBtn := CompButton( Self )
   oComponent:Button := oBtn
   WITH OBJECT oBtn
      :Component := oComponent
      :Left      := :Parent:LeftMargin - :Parent:HorzScrollPos
      :OwnerDraw := .T.
      :Caption   := oComponent:Name
      :ImageList := ::Application:MainForm:ToolBox1:ImageList
      :Action    := {|o| o:SelectComponent()}

      aButtons   := ::Application:MainForm:ToolBox1:aButtons
      nBtn       := 1
      lExit      := .F.
      
      FOR n := 1 TO LEN( aButtons )
          FOR x := 1 TO LEN( aButtons[n][2] )
              nBtn ++
              IF aButtons[n][2][x][1] == oComponent:__xCtrlName
                 n := LEN( aButtons )+1 // force exit first loop
                 EXIT
              ENDIF
          NEXT
      NEXT

      :ImageIndex  := nBtn //oComponent:ImageIndex
      :ImageAlign  := DT_LEFT
      :ImageIndent := 8
      :DrawTheme   := .F.

      :Font:Bold   := .T.

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
METHOD Reset( oOwner ) CLASS ComponentPanel
   LOCAL n, Component, aChildren, lUpdate := .T.
   
   DEFAULT oOwner TO ::Application:Project:CurrentForm
   
   IF EMPTY( oOwner:Components )
      ::Hide()
   ENDIF

   aChildren := {}
   FOR n := 1 TO LEN( ::Children )
       IF ::Children[n]:ClsName == "Button"
          AADD( aChildren, ::Children[n] )
       ENDIF
   NEXT
   AEVAL( aChildren, {|o| o:Destroy() } )

   ::LeftMargin := 10

   FOR EACH Component IN ::Application:Project:AppObject:Components
       ::AddButton( Component )
   NEXT
   FOR EACH Component IN oOwner:Components
       ::AddButton( Component )
   NEXT
   IF !::IsWindowVisible() .AND. !EMPTY( ::Children )
      ::Show()
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------
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
   LOCAL n, aChildren, lUpdate := .T.
   aChildren := {}
   FOR n := 1 TO LEN( ::Children )
       IF ::Children[n]:ClsName == "Button"
          AADD( aChildren, ::Children[n] )
       ENDIF
   NEXT
   AEVAL( aChildren, {|o| o:Destroy() } )
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
   ::Application:Project:EditReset(1)
RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnLButtonDown() CLASS CompButton
   ::Select()
RETURN NIL

//------------------------------------------------------------------------------------------
METHOD SelectComponent() CLASS CompButton
   ::Application:MainForm:FormEditor1:InvalidateRect()
   ::Application:Project:CurrentForm:SelectControl( ::Component, .F., .F. )
   ::Application:Project:EditReset(1)
RETURN 0

//------------------------------------------------------------------------------------------
METHOD Delete() CLASS CompButton
   LOCAL oBtn
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

//------------------------------------------------------------------------------------------
METHOD OnKeyDown( nwParam ) CLASS CompButton
   IF nwParam == VK_DELETE
      ::Application:Project:SetAction( { { DG_DELCONTROL, NIL, 0, 0, .F., ::Component:Owner, ::Component:__xCtrlName, ::Component, , 1, , Self } }, ::Application:Project:aUndo )
   ENDIF
RETURN 0

//------------------------------------------------------------------------------------------
METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS CompButton
   LOCAL nLeft, nTop, aRect, lDisabled, lSelected, lFocus, aTextRect, nTextFlags
   ( nwParam )
   ( nlParam )
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
