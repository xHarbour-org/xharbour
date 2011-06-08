/*
 * $Id$
 */

// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!

static aGroupProp := {}
static cCurFolder

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS PropEditor INHERIT TreeView
   DATA ActiveObject         EXPORTED
   DATA ReleaseEditSelection EXPORTED INIT .F.
   DATA ActiveControl        EXPORTED
   DATA ActiveItem           EXPORTED
   DATA lPaint               EXPORTED INIT .T.

   DATA nScroll              PROTECTED INIT 0
   DATA LevelFont            PROTECTED
   DATA CurCol               PROTECTED INIT 1
   DATA ItemRect             PROTECTED
   DATA Colors               PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD ResetProperties()
   METHOD OnUserMsg()
   METHOD Create()
   METHOD OnHScroll( n, nPos ) INLINE ::nScroll := n
   METHOD OnDestroy()          INLINE ::LevelFont:Delete(), NIL

   METHOD OnEraseBkGnd()
   
   METHOD OnParentNotify()
   METHOD DrawItem()
   
   METHOD GetEditBuffer()
   METHOD SetValue()
   METHOD OnGetDlgCode()
   METHOD OnSize()
   METHOD GetValue()
   METHOD OnLButtonDown()
   METHOD OnVertScroll()       INLINE IIF( ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow(),;
                                           ( ::ActiveControl:Destroy(), ::ActiveControl := NIL ), ), NIL
   METHOD GetPropertiesAndValues( oObj ) INLINE __ClsGetPropertiesAndValues( oObj )
   METHOD CheckValue()
ENDCLASS

//---------------------------------------------------------------------------------------------------

METHOD CheckValue( cProp, cRoot, xValue ) CLASS PropEditor
   LOCAL xVal, lDiff, oItem := ::SearchString( cProp, cRoot )
   IF oItem != NIL
      xVal := oItem:ColItems[1]:Value
      IF !( VALTYPE( xVal ) == VALTYPE( xValue ) )
         xVal := oItem:ColItems[1]:SetValue
      ENDIF
      IF ! ( xValue == xVal ) .AND. VALTYPE( xVal ) == VALTYPE( xValue )
         IF UPPER( cProp ) == "LEFT" .AND. __ObjHasMsg( ::ActiveObject:Parent, "HorzScrollPos" )
            xValue := ::ActiveObject:Left + ::ActiveObject:Parent:HorzScrollPos
          ELSEIF UPPER( cProp ) == "TOP" .AND. __ObjHasMsg( ::ActiveObject:Parent, "VertScrollPos" )
            xValue := ::ActiveObject:Top + ::ActiveObject:Parent:VertScrollPos
         ENDIF

         IF ( VALTYPE( oItem:ColItems[1]:Value ) == VALTYPE( xValue ) )
            oItem:ColItems[1]:Value := xValue
          ELSE
            oItem:ColItems[1]:SetValue := xValue
         ENDIF
         ::InvalidateRect(,.F.)
         RETURN .F.
      ENDIF
   ENDIF
RETURN .T.

//------------------------------------------------------------------------------------------

METHOD GetEditBuffer( oItem, nCol ) CLASS PropEditor
RETURN oItem:ColItems[nCol-1]:Value

METHOD OnGetDlgCode() CLASS PropEditor
   IF ::wParam == VK_RETURN
      ::PostMessage( WM_USER+4768 )
      RETURN 0
   ENDIF
RETURN DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS

METHOD Init( oParent ) CLASS PropEditor
   LOCAL cColor
   ::ReleaseEditSelection := .F.
   DEFAULT ::__xCtrlName  TO "PropEditor"
   ::Super:Init( oParent )
   ::SmallCaption := .F.
RETURN Self
//------------------------------------------------------------------------------------------

METHOD Create() CLASS PropEditor
   ::ClipChildren := .T.
   ::Tooltip:Text  := "-"
   ::Tooltip:Track := .T.
   ::ToolTip:CloseOnClick := .T.
   ::ExStyle := 0
   ::Super:Create()
   ::FullRowSelect := .T.
   ::SetScrollTime(0)
   ::LevelFont := Font()
   ::LevelFont:Weight     := 700
   ::LevelFont:Create()
   ::SetIndent( 15 )
   ::SetItemHeight( ABS( ::Font:Height ) + 6 )
RETURN Self

//---------------------------------------------------------------------------------------------------
METHOD OnSize( n, x, y ) CLASS PropEditor
   ::Columns[1][1] := Int(::ClientWidth/2)-11
   ::Columns[2][1] := Int(::ClientWidth/2)-7
   ::InvalidateRect(,.f.)
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD OnEraseBkGnd( hDC ) CLASS PropEditor
   LOCAL nTop, rc, hItem := ::GetLastVisibleItem()
   nTop := 0
   IF !EMPTY( hItem )
      rc := ::GetItemRect( hItem )
      nTop := rc:bottom
      IF nTop >= ::Height .OR. !::lPaint
         RETURN 1
      ENDIF
   ENDIF
   _FillRect( hDC, {0,nTop,::Width, ::Height}, ::Parent:BkBrush )
RETURN 1

//---------------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS PropEditor
   LOCAL oItem, tvcd, tvkd
   DO CASE
      CASE hdr:code == NM_CUSTOMDRAW
           tvcd := (struct NMTVCUSTOMDRAW)
           tvcd:Pointer( nlParam )
           DO CASE
              CASE tvcd:nmcd:dwDrawStage == CDDS_PREPAINT
                   SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_NOTIFYITEMDRAW )
                   RETURN CDRF_NOTIFYITEMDRAW

              CASE tvcd:nmcd:dwDrawStage == CDDS_ITEMPREPAINT
                   ::DrawItem( tvcd )
                   SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_SKIPDEFAULT  )
                   RETURN CDRF_SKIPDEFAULT 
           ENDCASE
   ENDCASE
RETURN 0

//------------------------------------------------------------------------------------------
METHOD SetValue( xValue, cCaption ) CLASS PropEditor
   LOCAL oObj, cProp, cProp2, xVal, n, xProp, oItem
   
   oItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
   IF oItem == NIL
      RETURN Self
   ENDIF
   IF ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow()
      ::ActiveControl:OnWMKillFocus := NIL
      ::ActiveControl:Destroy()
      ::ActiveControl := NIL
   ENDIF
   cProp := oItem:ColItems[1]:Prop
   cProp2:= oItem:ColItems[1]:Prop2

   IF cProp IN {"Column"}
      __objSendMsg( ::ActiveObject, "_" + UPPER( cProp ), cCaption )
      RETURN NIL
   ENDIF
   IF cProp IN {"DataSource"}
      IF xValue != NIL
         xValue := oItem:ColItems[1]:Value[2][ xValue ]
         IF cCaption != NIL
            oItem:ColItems[1]:Value[1] := cCaption
          ELSE
            oItem:ColItems[1]:Value[1] := xValue:Name
         ENDIF
       ELSE
         oItem:ColItems[1]:Value[1] := ""
         oItem:ColItems[1]:Value[2][1] := NIL
      ENDIF
      __objSendMsg( ::ActiveObject, "_" + UPPER( cProp ), xValue )
      RETURN NIL
   ENDIF
   
   IF !EMPTY( cProp2 )
      IF ::ActiveObject:lUI
         oObj := __objSendMsg( ::ActiveObject:EditCtrl, cProp2 )
         __objSendMsg( oObj, "_" + UPPER( cProp ), xValue )
      ENDIF
      oObj := __objSendMsg( ::ActiveObject, cProp2 )
      __objSendMsg( oObj, "_" + UPPER( cProp ), xValue )

      IF cProp2 == "Font" .AND. ::ActiveObject:lUI
         ::ActiveObject:EditCtrl:Font:Set( ::ActiveObject:EditCtrl )
         ::ActiveObject:SetText( ::ActiveObject:Text )
      ENDIF
    ELSE
      xProp := cProp
      IF xProp == "Text"
         xProp := "Caption"
       ELSEIF xProp == "FileName" .AND. ::ActiveObject:ClsName == "Image"
         xProp := "ImageName"
      ENDIF
      IF cProp IN {"GroupBy","Field","Order"}
         xValue := cCaption
      ENDIF
      __objSendMsg( ::ActiveObject, "_" + UPPER( cProp ), xValue )

      IF ::ActiveObject:lUI
         IF ! ( UPPER( xProp ) == "WIDTH" .AND. UPPER( ::ActiveObject:ClassName ) == "VRLABEL" )
            TRY
               __objSendMsg( ::ActiveObject:EditCtrl, "_" + UPPER( xProp ), xValue )
            CATCH
            END
         ENDIF
         ::ActiveObject:Parent:InvalidateRect( , .F. )
      ENDIF

   ENDIF
   IF ( VALTYPE( oItem:ColItems[1]:Value ) == VALTYPE( xValue ) )
      oItem:ColItems[1]:Value := xValue
   ENDIF
   
   IF cProp == "BackColor" .OR. cProp == "ForeColor"
      oItem:ColItems[1]:Color := xValue
      n := hScan( ::System:Color, xValue )
      oItem:ColItems[1]:Value := IIF( n > 0, ::System:Color:Keys[n], "Custom..." )
      _InvalidateRect( ::hWnd, oItem:GetItemRect():Array() ,.F.)
   ENDIF

RETURN Self

METHOD DrawItem( tvcd ) CLASS PropEditor
   LOCAL hItem, n, nState, nBack, nFore, lExpanded, rc, nWidth, nLeft, nRight, nBottom, aAlign, x, y, lHasChildren
   LOCAL aRow, aCol, hOldPen, nAlign, cType, nColor, hOld, hBrush, aRest, cText, nfHeight := ABS( ::Font:Height )+3
   LOCAL nPos, cCap, xValue, cProp, nLevel, hDC, oItem
   LOCAL hOldFont, hIcon, lDisabled := .F.
   LOCAL hMemBitmap, hOldBitmap, lEnabled, hBoldFont

   rc       := tvcd:nmcd:rc
   oItem    := FindTreeItem( ::Items, tvcd:nmcd:dwItemSpec )
   IF oItem == NIL .OR. rc:Right == 0 .OR. !IsWindowVisible( ::hWnd )
      RETURN NIL
   ENDIF
   hDC      := tvcd:nmcd:hdc
   nLevel   := tvcd:iLevel
   lEnabled := IsWindowEnabled( ::hWnd )
   nBack := IIF( lEnabled, ::Columns[1][2], /*GetSysColor( COLOR_BTNFACE )*/ ::System:CurrentScheme:ToolStripPanelGradientEnd )
   nFore := IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) )

   nState := ::GetItemState( tvcd:nmcd:dwItemSpec, TVIF_STATE )

   IF nState & TVIS_SELECTED == TVIS_SELECTED .AND. ::CurCol == 1 .AND. ::HasFocus
      nBack := ::System:CurrentScheme:ButtonSelectedGradientEnd //GetSysColor( COLOR_HIGHLIGHT )
      nFore := IIF( lEnabled, RGB(0,0,0), GetSysColor( COLOR_BTNSHADOW ) )
   ENDIF

   lExpanded := nState & TVIS_EXPANDED != 0

   cText := oItem:Caption
   cCap  := cText
   SetBkColor( hDC, nBack )

   SelectObject( hDC, ::Font:Handle )

   lHasChildren := LEN( oItem:Items ) > 0

   IF nLevel == 0
      nBack := C_WHITE
      nFore := IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) )
      SelectObject( hDC, ::LevelFont:Handle )
      SetBkColor( hDC, ::BackColor )
    ELSEIF lHasChildren
      SelectObject( hDC, ::LevelFont:Handle )
   ENDIF

   lDisabled := ::Application:Report == NIL

   IF lDisabled
      nFore := GetSysColor( COLOR_BTNSHADOW )
   ENDIF

   SetTextColor( hDC, nFore )

   IF !Empty( oItem:ColItems ) .AND. oItem:ColItems[1]:ColType == "MENUEDITOR"
      SetTextColor( hDC, IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) ) )
      SelectObject( hDC, ::LevelFont:Handle )
   ENDIF

   DEFAULT ::nScroll TO 0
   nWidth  := ::Columns[1][1]
   nLeft   := -::nScroll+ nfHeight
   nRight  := nLeft + nWidth
   nBottom := rc:bottom

   aAlign  := _GetTextExtentPoint32( hDC, cText )
   IF aAlign == NIL
      aAlign := {0,0}
   ENDIF

   x := nLeft
   y := rc:top + ( ( rc:bottom - rc:top )/2 ) - ( aAlign[2]/2 )

   n:=0
   IF lHasChildren
      n++
   ENDIF

   x := x*(nLevel+n)

   IF nLevel == 0
      SetBkColor( hDC, ::System:CurrentScheme:ToolStripPanelGradientEnd )
      _ExtTextOut( hDC, x+2, y, ETO_CLIPPED+ETO_OPAQUE, { rc:left, rc:top, rc:right, rc:bottom }, cText  )

      IF lHasChildren .AND. !lExpanded
         MoveToEx( hDC, rc:left, rc:bottom-1 )
         LineTo( hDC, rc:right, rc:bottom-1 )
      ENDIF
    ELSE
      _FillRect( hDC, { rc:left, rc:top, nLeft, rc:bottom }, /*GetSysColorBrush( COLOR_BTNFACE )*/ ::System:CurrentScheme:Brush:ToolStripPanelGradientEnd )
      _FillRect( hDC, { nRight+::Columns[2][1], rc:top, rc:right, rc:bottom }, /*GetSysColorBrush( COLOR_BTNFACE )*/ ::System:CurrentScheme:Brush:ToolStripPanelGradientEnd )

      _ExtTextOut( hDC, x+2, y, ETO_CLIPPED+ETO_OPAQUE, { nLeft, rc:top, nRight, rc:bottom }, cText  )
   ENDIF

   aRow := { {nLeft,rc:bottom}, {nRight,rc:bottom} }
   aCol := { {nRight-1,rc:bottom}, {nRight-1,rc:top} }

   IF nLevel > 0
      hOldPen := SelectObject( hDC, ::System:CurrentScheme:Pen:ToolStripPanelGradientEnd )
      _PolyLine( hDC, { {nLeft,rc:bottom-1}, {nLeft,rc:top} } )
      _PolyLine( hDC, { {nLeft,rc:top}, {nRight,rc:top} } )
      _PolyLine( hDC, aRow )
      _PolyLine( hDC, aCol )
      SelectObject( hDC, hOldPen )
   ENDIF

   aRest := { rc:Left, rc:bottom, rc:right, ::ClientHeight }
   
   IF VALTYPE( oItem:ColItems ) == "A"
      FOR n := 1 TO LEN( oItem:ColItems )
          nLeft   += nWidth

          nWidth  := ::Columns[n+1][1]
          nRight  := nLeft + nWidth + 2
          nBottom := rc:bottom

          nColor  := oItem:ColItems[n]:Color
          nAlign  := oItem:ColItems[n]:Align

          nAlign  := TA_LEFT
 
          cText   := oItem:ColItems[n]:Value

          IF oItem:ColItems[n]:ColType == "ENUM"
             cText := cText[ oItem:ColItems[n]:SetValue ]
          ENDIF

          cType   := VALTYPE( cText )

          SWITCH cType
             CASE "O"
                  cText  := "(object)"
                  EXIT

             CASE "A"
                  // DataSource falls here
                  cText  := cText[1]
                  IF ::ActiveObject != NIL
                     IF oItem:ColItems[n]:ColType == "DATASOURCE" .AND. !EMPTY( ::ActiveObject:DataSource )
                        cText := ::ActiveObject:DataSource:Name
                      ELSEIF oItem:ColItems[n]:ColType == "FORMULA"
                        cText := ::ActiveObject:Formula
                      ELSEIF oItem:ColItems[n]:ColType == "COLUMN"
                        cText := ::ActiveObject:Column
                      ELSEIF oItem:ColItems[n]:ColType IN "GROUPBY"
                        cText := ::ActiveObject:GroupBy
                      ELSEIF oItem:ColItems[n]:ColType IN "FIELD"
                        cText := ::ActiveObject:Field
                      ELSEIF oItem:ColItems[n]:ColType IN "ORDER"
                        cText := ::ActiveObject:Order
                     ENDIF
                  ENDIF
                  EXIT

             CASE "N"
                  cText  := StrTrim( cText )
                  nAlign := TA_RIGHT
                  EXIT

             CASE "D"
                  cText  := DTOC( cText )
                  nAlign := TA_LEFT
                  EXIT

             CASE "L"
                  cText  := IIF( cText, "True", "False" )
                  EXIT
          END
          DEFAULT cText TO ""

          aAlign  := _GetTextExtentPoint32( hDC, cText )
          IF aAlign == NIL
             aAlign := {0,0}
          ENDIF

          oItem:ColItems[n]:Text      := cText
          oItem:ColItems[n]:TextWidth := aAlign[1]

          x := nLeft+2+IIF( nColor != NIL, 24, 0 ) + IIF( oItem:ColItems[n]:ColType == "ICONS", 20, 0 )
          y := rc:top + ( ( rc:bottom - rc:top )/2 ) - ( aAlign[2]/2 )

          SWITCH nAlign
             CASE TA_RIGHT
                  x := nRight - aAlign[1] - 2
                  EXIT

             CASE TA_CENTER
                  x:= nLeft + ( ( nRight - nLeft )/2 ) - ( aAlign[1]/2 )
                  EXIT
          END
          nBack := IIF( lEnabled, ::Columns[n+1][2], /*GetSysColor( COLOR_BTNFACE )*/ ::System:CurrentScheme:ToolStripPanelGradientEnd ) 
          nFore := IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) )
          IF nState & TVIS_SELECTED != 0 .AND. ::CurCol == n+1 .AND. ::HasFocus
             nBack := ::System:CurrentScheme:ButtonSelectedGradientEnd //GetSysColor( COLOR_HIGHLIGHT )
             nFore := RGB(0,0,0)
          ENDIF
          SetBkColor( hDC, nBack )
          SetTextColor( hDC, nFore )

          IF oItem:ColItems[n]:ColType == "O" .AND. lHasChildren
             SetTextColor( hDC, IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) ) )
          ENDIF

          lDisabled := lDisabled .OR. oItem:ColItems[n]:ReadOnly

          IF lDisabled
             nFore := GetSysColor( COLOR_BTNSHADOW )
             SetTextColor( hDC, nFore )
          ENDIF

          _ExtTextOut( hDC, x, y, ETO_CLIPPED+ETO_OPAQUE, { nLeft, rc:top, nRight, rc:bottom }, cText  )

          SelectObject( hDC, ::Font:Handle )

          IF oItem:ColItems[n]:ColType == "ICONS" .AND. ::ActiveObject:__hIcon != NIL
             DrawIconEx( hDC, x-20, y, ::ActiveObject:__hIcon, 16, 16,,,DI_NORMAL )
          ENDIF
          IF oItem:ColItems[n]:ColType == "IMAGEINDEX" .AND. ::ActiveObject:Parent:ImageList != NIL .AND. ::ActiveObject:&cCap != NIL .AND. ::ActiveObject:&cCap > 0
             hIcon := ::ActiveObject:Parent:ImageList:GetImage( ::ActiveObject:&cCap )
             DrawIconEx( hDC, nLeft, y, hIcon, ::ActiveObject:Parent:ImageList:IconWidth, 16,,,DI_NORMAL )
          ENDIF

          IF nColor != NIL
             hBrush := CreateSolidBrush( nColor )
             hOld := SelectObject( hDC, hBrush )
             Rectangle( hDC, x-24, rc:top+3, x-2, rc:bottom-2 )
             SelectObject( hDC, hOld )
             DeleteObject( hBrush )
          ENDIF

          aRow := { {nLeft,rc:bottom}, {nRight,rc:bottom} }
          aCol := { {nRight,rc:bottom}, {nRight,rc:top} }

          hOldPen := SelectObject( hDC, ::System:CurrentScheme:Pen:ToolStripPanelGradientEnd )
          _PolyLine( hDC, { {nLeft,rc:top}, {nRight,rc:top} } )
          _PolyLine( hDC, aRow )
          _PolyLine( hDC, aCol )
          SelectObject( hDC, hOldPen )

      NEXT
   ENDIF
   IF lHasChildren
      y := rc:top + ( ( rc:bottom - rc:top ) / 2 ) - ( 9/2 )
      n := nfHeight *(nLevel)
      x := -::nScroll+(16/2)-(11/2)+n

      n := nfHeight-5
      IF ( n / 2 ) == Int( n / 2 )
         n --
      ENDIF
      DrawMinusPlus( hDC, x, y, lExpanded, nLevel>0, RGB(0,0,0), n, n/2 )
   ENDIF
RETURN 0

//---------------------------------------------------------------------------------------------------

METHOD GetValue( cProp, cRoot ) CLASS PropEditor
   LOCAL lDiff, oItem := ::SearchString( cProp, cRoot )
   IF oItem != NIL
      RETURN oItem:ColItems[1]:Value
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD OnLButtonDown(n,x,y) CLASS PropEditor
   LOCAL oItem, rc, pt, aSize, cText
   LOCAL nCol:=0, z, nLeft

   IF ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow()
      ::ActiveControl:Destroy()
      ::ActiveControl := NIL
   ENDIF

   oItem := ::HitTest( x, y )
   IF oItem != NIL
      oItem:Select()

      nLeft := 14
      FOR z := 1 TO LEN( ::Columns )
          nLeft += ::Columns[z][1]
          IF nLeft > x
             nCol := z
             EXIT
          ENDIF
      NEXT

      IF nCol > 1
         ::PostMessage( WM_USER+4768 )
         RETURN 0
       ELSE
         ::CurCol := 1
         IF !::HasFocus
            ::SetFocus()
         ENDIF
         _InvalidateRect( ::hWnd, oItem:GetItemRect():Array() ,.F.)
      ENDIF
   ENDIF
RETURN NIL

METHOD OnUserMsg( hWnd, nMsg, nCol, nLeft ) CLASS PropEditor
   LOCAL oItem, rc, cType, n, cProp, cFont, cText
   DO CASE
      CASE nMsg == WM_USER + 4768
           ::ActiveItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
           IF ::ActiveItem != NIL
              rc := (struct RECT)
              rc:left := ::ActiveItem:hItem
              SendMessage( ::hWnd, TVM_GETITEMRECT, .F., @rc )

              nCol  := 2
              nLeft := ::Columns[1][1] + ( ABS( ::Font:Height ) + 3 )

              cProp := ::ActiveItem:Caption
              ::CurCol := nCol
              _InvalidateRect( ::hWnd, ::ActiveItem:GetItemRect():Array() ,.F.)
              oItem := ::ActiveItem
              IF ::CurCol > 1 .AND. ::ActiveItem:ColItems != NIL .AND. LEN( ::ActiveItem:ColItems ) >= nCol-1
                 cType := ::ActiveItem:ColItems[nCol-1]:ColType
                 IF ::ActiveItem:ColItems[nCol-1]:ReadOnly
                    RETURN 0
                 ENDIF
                 DO CASE
                    CASE cType == "ENUM"
                         WITH OBJECT ::ActiveControl := ObjCombo( Self )
                            :Left   := nLeft-1
                            :Top    := rc:top
                            :Width  := ::Columns[ nCol ][ 1 ]+4
                            :Height := 70
                            :Border := .F.
                            :Height := 200
                            :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                            :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                            :Action := oItem:ColItems[nCol-1]:Action
                            :Cargo  := { cProp, oItem }
                            :Create()
                            FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value )
                                :AddItem( oItem:ColItems[nCol-1]:Value[n] )
                            NEXT
                            :SetCurSel( oItem:ColItems[nCol-1]:SetValue )
                            :SetItemHeight( -1, ::GetItemHeight()-5 )
                            :SetFocus()
                            :ShowDropDown()
                         END

                    CASE cType == "FACENAME"
                         ::ActiveControl := FontComboBox( Self )
                         WITH OBJECT ::ActiveControl
                            :Left   := nLeft-1
                            :Top    := rc:top
                            :Width  := ::Columns[ nCol ][ 1 ]+4
                            :Height := 200
                            :Border := .F.
                            :Cargo  := ::ActiveItem:Owner:Caption
                            :Owner  := ::ActiveObject
                            :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                            :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                            :Action := {|o, n, oPar, cSel| cSel := o:GetSelString(), oPar := o:Parent, o:Destroy(), oPar:SetValue( cSel ) }
                            :Create()

                            cFont := :Cargo

                            :SetItemHeight( -1, ::GetItemHeight()-5 )
                            :SetItemHeight( 1, ABS( :Owner:&cFont:Height ) )
                            :SetFocus()
                            :ShowDropDown()
                         END

                    CASE cType == "COLORREF"
                         ::ActiveControl := ColorPicker( Self )
                         WITH OBJECT ::ActiveControl
                            :AllowSystemDefault := .T.
                            :AllowCustomColor   := .T.
                            :Left   := nLeft-1
                            :Top    := rc:top+1
                            :Width  := ::Columns[ nCol ][ 1 ]+4
                            :Height := 120
                            
                            :SysDefault := ::ActiveObject:Sys&cProp

                            IF ::ActiveItem:ColItems[nCol-1]:Value == "Custom..."
                               :Custom := ::ActiveItem:ColItems[nCol-1]:Color
                            ENDIF

                            :ColorSelected := ::ActiveItem:ColItems[nCol-1]:Color

                            :Create()
                            :Action := {|o, c, oPar| c := o:ColorSelected, oPar := o:Parent, o:Destroy(), oPar:SetValue( c ) }
                            :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                            :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }

                            n := :FindStringExact( 0, ::ActiveItem:ColItems[nCol-1]:Value )
                            :SetCurSel(n)

                            :SetItemHeight( -1, ::GetItemHeight()-5 )
                            :SetItemHeight( 1, 14 )
                            :SetFocus()
                            :ShowDropDown()
                         END

                    CASE cType == "C" .OR. cType == "U" .OR. cType == "IMAGENAME"
                         ::ActiveControl := EditBox( Self )
                         WITH OBJECT ::ActiveControl
                            :Left   := nLeft
                            :Top    := rc:top+1
                            :Width  := ::Columns[ nCol ][ 1 ]+2 - IIF( cType == "ICONS", 20, 0 )
                            :Height := rc:bottom-rc:top-1
                            :SetExStyle( WS_EX_CLIENTEDGE, .F. )
                            :Style := :Style | ES_AUTOHSCROLL | ES_MULTILINE & NOT( WS_BORDER )

                            IF __ObjHasMsg( ::ActiveObject, "__ExplorerFilter" ) .OR. ::ActiveItem:Caption == "ImageName"
                               IF ::ActiveItem:Caption == "FileName" .OR. ::ActiveItem:Caption == "ImageName"
                                  :Button := .T.
                                  :ButtonAction := {|o| BrowseForFile( o, Self, ::ActiveObject ) }
                               ENDIF
                             ELSEIF cType == "C"
                               :Button := .T.
                               :ButtonAction := {|o| ::EditText( o ) }
                            ENDIF

                            :Create()
                            :Caption := ::GetEditBuffer( ::ActiveItem, nCol )
                            :Cargo   := :Caption

                            :OnWMKillFocus := {|o,cText,oPar| cText := o:Caption,;
                                                              oPar  := o:Parent,;
                                                              oPar:SetValue( cText ),;
                                                              o:Destroy(), 0 }
                            :OnWMKeyDown   := {|o,n| CheckKeyDown(o,n)}

                            IF !::ReleaseEditSelection
                               :SetSel( 0, -1 )
                            ENDIF
                            :SetFocus()
                         END

                    CASE cType == "N"
                         ::ActiveControl := EditBox( Self )
                         WITH OBJECT ::ActiveControl
                            :Caption := ALLTRIM( STR( ::ActiveItem:ColItems[nCol-1]:Value ) )
                            :Left   := nLeft
                            :Top    := rc:top+1
                            :Width  := ::Columns[ nCol ][ 1 ]+2
                            :Height := rc:bottom-rc:top-1

                            :ClientEdge  := .F.
                            :Alignment   := 3
                            :MultiLine   := .T.
                            :AutoHScroll := .T.
                            :Border      := .F.
                            :Cargo       := :Caption
                            :OnWMKillFocus := {|o,cText,oPar| cText := o:Caption,;
                                                                             oPar  := o:Parent,;
                                                                             IIF( ::__xCtrlName == "PropEditor", oPar:SetValue( VAL( cText ) ), ),;
                                                                             o:Destroy(), 0 }

                            :OnWMKeyDown  := {|o,n| CheckChar( o, n, ::ActiveItem )}
                            :OnWMChar     := {|o,n| CheckChar( o, n, ::ActiveItem )}
                            :OnWMPaste    := {|o,n| CheckCharPaste( o, n, ::ActiveItem )}

                            :Create()
                            IF !::ReleaseEditSelection
                               :SetSel( 0, -1 )
                            ENDIF
                            :SetFocus()
                         END

                    CASE cType == "L"
                         ::ActiveControl := ObjCombo( Self )
                         WITH OBJECT ::ActiveControl
                            :Left   := nLeft-1
                            :Top    := rc:top
                            :Width  := ::Columns[ nCol ][ 1 ]+4
                            :Height := 70
                            :Border := .F.
                            :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                            :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                            :Create()
                            :Action := {|o, c, oPar| c := o:GetSelString(),;
                                                          oPar := o:Parent,;
                                                          o:Destroy(),;
                                                          oPar:SetValue( IIF( c == "True", .T., .F. ), o ) }
                            :AddItem( "True" )
                            :AddItem( "False" )
                            IF ::ActiveItem:ColItems[nCol-1]:Value
                               :SetCurSel(1)
                             ELSE
                               :SetCurSel(2)
                            ENDIF
                            :SetItemHeight( -1, ::GetItemHeight()-5 )
                            :SetFocus()
                            :ShowDropDown()
                         END

                   CASE cType IN { "DATASOURCE", "FORMULA", "COLUMN", "GROUPBY", "FIELD", "ORDER" }
                        ::ActiveControl := ObjCombo( Self )
                        WITH OBJECT ::ActiveControl
                           :Left   := nLeft-1
                           :Top    := rc:top
                           :Width  := ::Columns[ nCol ][ 1 ]+4
                           :Height := 70
                           :Border := .F.

                           :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                           :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ), NIL }
                           :Action := {|o, n, oPar, c| c := o:GetSelString(), n := o:GetCurSel()-1, oPar := o:Parent, o:Destroy(), oPar:SetValue( n + 1, c ) }
                           :Create()

                           :AddItem( "" )

                           FOR n := 1 TO LEN( ::ActiveItem:ColItems[nCol-1]:Value[2] )
                               IF ::ActiveItem:ColItems[nCol-1]:Value[2][n] != NIL
                                  IF ! cType IN {"GROUPBY","FIELD","ORDER"}
                                     :AddItem( ::ActiveItem:ColItems[nCol-1]:Value[2][n]:Name )
                                   ELSE
                                     :AddItem( ::ActiveItem:ColItems[nCol-1]:Value[2][n] )
                                  ENDIF
                               ENDIF
                           NEXT

                           IF ( n := :FindStringExact( ::ActiveItem:ColItems[nCol-1]:Value[1] ) ) > 0
                              :SetCurSel(n)
                            ELSE
                              :SetCurSel(1)
                           ENDIF
                           :SetItemHeight( -1, ::GetItemHeight()-5 )
                           :ShowDropDown()
                           :SetFocus()
                        END

                 ENDCASE
              ENDIF
           ENDIF
   ENDCASE
RETURN NIL

METHOD ResetProperties( aSel, lPaint, lForce, aSubExpand, lRefreshComp ) CLASS PropEditor
   LOCAL cProp, cProp2, aProp, xValue, n, oItem, nColor, aSub, aCol, oSub, oObj, xValue2
   LOCAL aObj, aProperties, cProperty, aProperty, aSubProp, cType, Child, xProp
   LOCAL aField

   IF ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow()
      ::ActiveControl:Destroy()
      ::ActiveControl := NIL
   ENDIF

   DEFAULT lPaint TO ::lPaint
   IF !::lPaint .OR. !lPaint
      RETURN NIL
   ENDIF

   DEFAULT lForce TO .F.
   DEFAULT lRefreshComp TO .T.
   
   DEFAULT aSel TO { { ::ActiveObject,, } }

   IF aSel[1][1] == NIL
      ::ResetContent()
      RETURN NIL
   ENDIF
   
   IF ::ActiveObject != NIL .AND. ::ActiveObject == aSel[1][1] .AND. !lForce
      RETURN Self
   ENDIF

   ::ActiveObject := aSel[1][1]
   ::lPaint := .F.
   ::SetRedraw( .F. )
   ::ResetContent()

   aProperties := ::ActiveObject:aProperties

   aSort( aProperties,,,{|x, y| x[1] < y[1]})

   FOR EACH aProperty IN aProperties
       ::Application:Yield()
       cProp  := aProperty[1]
       xValue := ::ActiveObject:&cProp
       cType  := VALTYPE( xValue )
       nColor := NIL

       oItem := ::SearchString( aProperty[2] )

       IF oItem == NIL
          oItem := TreeViewItem( Self )
          oItem:Caption    := aProperty[2]
          oItem:InsertAfter:= TVI_SORT
          oItem:Create()
       ENDIF

       aCol := { TreeColItem( xValue, cType, , nColor, cProp, , ,NIL ) }
       aCol[1]:ColType := cType
       IF __ObjHasMsg( ::ActiveObject, "Enum"+cProp )
          aCol[1]:Value    := ::ActiveObject:Enum&cProp[1]
          aCol[1]:ColType  := "ENUM"
          aCol[1]:SetValue := ASCAN( ::ActiveObject:Enum&cProp[2], ::ActiveObject:&cProp,,, .T. )
          aCol[1]:Action   := {|o, n, oPar, c| n := o:GetCurSel()-1,;
                                                 oPar := o:Parent,;
                                                 o:Destroy(),;
                                                 c := o:Cargo[1],;
                                                 o:Cargo[2]:ColItems[1]:SetValue := n+1,;
                                                 oPar:SetValue( ::ActiveObject:Enum&c[2][n+1] ) }
          xValue := NIL

        ELSEIF UPPER(cProp) IN {"ORDER"} .AND. ::ActiveObject:ClsName == "DataTable"
          aCol[1]:ColType := "ORDER"
          aCol[1]:Value   := { "", { NIL } }
          IF ::ActiveObject:EditCtrl:IsOpen
             FOR n := 1 TO ::ActiveObject:EditCtrl:OrdCount()
                 AADD( aCol[1]:Value[2], ::ActiveObject:EditCtrl:OrdName(n) )
             NEXT
          ENDIF
          xValue := NIL

        ELSEIF UPPER(cProp) IN {"GROUPBY"}
          aCol[1]:ColType := UPPER(cProp)
          aCol[1]:Value   := { "", { NIL } }
          IF !EMPTY( ::ActiveObject:DataSource )
             FOR EACH aField IN ::ActiveObject:DataSource:EditCtrl:Struct()
                 AADD( aCol[1]:Value[2], aField[1] )
             NEXT
          ENDIF
          xValue := NIL

        ELSEIF UPPER(cProp) IN {"FIELD"}
          aCol[1]:ColType := UPPER(cProp)
          aCol[1]:Value   := { "", { NIL } }
          IF !EMPTY( ::Application:Report:VrReport:DataSource )
             FOR EACH aField IN ::Application:Report:VrReport:DataSource:EditCtrl:Struct()
                 AADD( aCol[1]:Value[2], aField[1] )
             NEXT
          ENDIF
          xValue := NIL

        ELSEIF UPPER(cProp) == "DATASOURCE"
          aCol[1]:ColType := "DATASOURCE"
          aCol[1]:Value   := { "", { NIL } }
          FOR EACH Child IN ::Application:Props:Components:Children
              IF Child:Component:ClsName == "DataTable"
                 AADD( aCol[1]:Value[2], Child:Component )
              ENDIF
          NEXT
          xValue := NIL
        
        ELSEIF UPPER(cProp) == "FORMULA"
          aCol[1]:ColType := "FORMULA"
          aCol[1]:Value   := { "", { NIL } }
          FOR EACH Child IN ::Application:Props:Components:Children
              IF Child:Component:ClsName == "Formula"
                 AADD( aCol[1]:Value[2], Child:Component )
              ENDIF
          NEXT
          xValue := NIL
        
        ELSEIF UPPER(cProp) == "COLUMN"
          aCol[1]:ColType := "COLUMN"
          aCol[1]:Value   := { "", { NIL } }
          FOR EACH Child IN ::Application:Props:Body:Objects
              IF Child:ClsName IN {"Label","Total"}
                 AADD( aCol[1]:Value[2], Child )
              ENDIF
          NEXT
          xValue := NIL

        ELSEIF UPPER(cProp) == "BACKCOLOR" .OR. UPPER(cProp) == "FORECOLOR"
          xValue := ::ActiveObject:&cProp
          n := hScan( ::System:Color, xValue )
          aCol[1]:Color   := xValue
          aCol[1]:Value   := IIF( n > 0, ::System:Color:Keys[n], "Custom..." )
          aCol[1]:ColType := "COLORREF"

        ELSEIF UPPER(cProp) == "FILENAME" .AND. UPPER(::ActiveObject:ClsName) == "IMAGE"
          xValue := NIL
          aCol[1]:Value   := ::ActiveObject:FileName
          aCol[1]:ColType := "IMAGENAME"
        ELSE
          aCol[1]:Value    := ::ActiveObject:GetValue( cProp )
          //aCol[1]:ColType  := ::ActiveObject:ClsName
          //aCol[1]:SetValue := ::ActiveObject:Value
          aCol[1]:Action   := {|o, n, oPar, c| oPar := o:Parent,;
                                               o:Destroy(),;
                                               c := o:Cargo[1],;
                                               oPar:SetValue( c ) }
       ENDIF

       oSub := oItem:AddItem( cProp, 0, aCol )

       IF VALTYPE( xValue ) == "O"
          aSub := __ClsGetPropertiesAndValues( xValue )
          FOR EACH aSubProp IN aSub
              ::Application:Yield()

              cProp2  := aSubProp[1]
              xValue2 := __objSendMsg( xValue, cProp2 )

              cType   := VALTYPE( xValue2 )
              nColor  := NIL

              cProp2  := GetProperCase( __Proper( cProp2 ) )[1]
              aCol    := { TreeColItem( IIF( VALTYPE(xValue2)=="O", "", xValue2 ), cType, , nColor, cProp2, cProp ) }

              IF UPPER(cProp2) == "FACENAME"
                 aCol[1]:ColType  := "FACENAME"
                 xValue2 := NIL
              ENDIF

              oSub:AddItem( cProp2, 0, aCol )
          NEXT
       ENDIF

       oItem:SortChildren( .T. )
       oItem:Expand()
   NEXT

   ::lPaint := .T.
   ::SetRedraw( .T. )
   ::Application:Yield()

RETURN NIL

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS TreeColItem
   DATA SetValue  EXPORTED
   DATA Value     EXPORTED
   DATA Align     EXPORTED
   DATA ColType   EXPORTED
   DATA Action    EXPORTED
   DATA Color     EXPORTED
   DATA Prop      EXPORTED
   DATA Prop2     EXPORTED
   DATA Text      EXPORTED
   DATA TextWidth EXPORTED
   DATA ReadOnly  EXPORTED INIT .F.
   DATA Help
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( xValue, cType, nAlign, nColor, cProp, cProp2, lReadOnly, cHelp ) CLASS TreeColItem
   DEFAULT lReadOnly TO ::ReadOnly
   ::Value    := xValue
   ::Align    := nAlign
   ::ColType  := cType
   ::Color    := nColor
   ::Prop     := cProp
   ::Prop2    := cProp2
   ::ReadOnly := lReadOnly
   ::Help     := cHelp
RETURN Self


FUNCTION GetProperCase( cProp )
   LOCAL n := ASCAN( aGroupProp, {|a| UPPER( a[1] ) == UPPER( cProp ) } )
   IF n > 0
      RETURN aGroupProp[n]
   ENDIF
RETURN { Upper(cProp[1])+Lower(SubStr(cProp,2)), "", NIL }

//------------------------------------------------------------------------------------------

CLASS ObjCombo INHERIT ComboBox
   DATA ColWidth
   DATA ColFont
   DATA ColFont
   METHOD Init() CONSTRUCTOR
   METHOD OnParentDrawItem()
   METHOD Create()
ENDCLASS

METHOD Init( oParent ) CLASS ObjCombo
   ::Super:Init( oParent )
   ::Style := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST
   ::ColFont := Font()
RETURN Self

METHOD Create() CLASS ObjCombo
   ::Super:Create()
   ::ColFont:Weight := 700
   ::ColFont:Create()
RETURN Self

METHOD OnParentDrawItem( nwParam, nlParam ) CLASS ObjCombo

   LOCAL n, x, lSelected, aRect, aClip, nLen, itemTxt, cText, nField, hBrush, hOld, z, aAlign, y

   IF ::Parent:DrawItemStruct:hwndItem == ::hWnd
      DEFAULT ::ColWidth TO ::ClientWidth

      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0
      aClip     := { ::Parent:DrawItemStruct:rcItem:Left,  ::Parent:DrawItemStruct:rcItem:Top, ;
                     ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }

      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0 //.OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
         SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )
         itemTxt := left( itemTxt, nLen )

         z := ::Parent:DrawItemStruct:rcItem:Top
         IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT != 0 .AND. ::Parent:DrawItemStruct:itemAction & ODA_SELECT == 0
            z := ::Parent:DrawItemStruct:rcItem:Top - 2
         ENDIF

         cText   := ""
         aRect   := ACLONE(aClip)
         nField  := 1
         aRect[3]:= aRect[1] + ::ColWidth

         FOR n := 1 to nLen + 1
             IF SubStr( itemTxt, n, 1) == chr(9) .or. n == nLen + 1
                x := aRect[1] + 2

                ::Font:Select( ::Parent:DrawItemStruct:hDC )
                IF nField == 1
                   ::ColFont:Select( ::Parent:DrawItemStruct:hDC )
                ENDIF

                aAlign  := _GetTextExtentPoint32( ::Parent:DrawItemStruct:hDC, cText )
                y := ::Parent:DrawItemStruct:rcItem:top + ( ( ::Parent:DrawItemStruct:rcItem:bottom - ::Parent:DrawItemStruct:rcItem:top )/2 ) - ( aAlign[2]/2 )

                _ExtTextOut( ::Parent:DrawItemStruct:hDC, x, y, ETO_OPAQUE + ETO_CLIPPED, aRect, cText )
                cText := ""
                aRect[1] += ::ColWidth
                nField ++

                IF nField  > 1
                   aRect[3] := ::Parent:DrawItemStruct:rcItem:Right
                  ELSE
                   aRect[3] += ::ColWidth
                ENDIF
                LOOP
             ENDIF

             cText += SubStr( itemTxt, n, 1 )
         NEXT



         //_ExtTextOut( ::Parent:DrawItemStruct:hDC, 24, y, ETO_OPAQUE + ETO_CLIPPED, { ::Parent:DrawItemStruct:rcItem:Left, ::Parent:DrawItemStruct:rcItem:Top, ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom }, itemTxt )

      ENDIF

      IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT == 0
         IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
            aClip := { ::Parent:DrawItemStruct:rcItem:Left,  ::Parent:DrawItemStruct:rcItem:Top, ;
                       ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
            _DrawfocusRect( ::Parent:DrawItemStruct:hDC, aclip )
         ENDIF
      ENDIF
   ENDIF
RETURN 0

//--------------------------------------------------------------------------------------------------------------------------------------------
#pragma BEGINDUMP

#include <windows.h>
#include <shlobj.h>
#include <commctrl.h>
#include "hbapi.h"

HB_FUNC( DRAWMINUSPLUS )
{
   HDC hdc  = (HDC) hb_parnl(1);
   LONG x   = hb_parnl(2);
   LONG y   = hb_parnl(3);
   BOOL bExpand = hb_parl(4);
   BOOL bFill   = hb_parl(5);
   int  cxy   = hb_parni(7);
   int  cxyh  = hb_parni(8);
   COLORREF cColor = ISNIL(6) ? GetSysColor(COLOR_BTNSHADOW) : (COLORREF) hb_parnl(6);

   HPEN hPen;
   HPEN oPen;
   HBRUSH oBrush;

   hPen = CreatePen(PS_SOLID, 1, cColor);
   oPen = (HPEN)SelectObject(hdc, hPen);
   oBrush = (HBRUSH)SelectObject(hdc, GetStockObject(bFill ? WHITE_BRUSH : NULL_BRUSH));

   Rectangle(hdc, x, y, x + cxy, y + cxy);
   SelectObject(hdc, GetStockObject(BLACK_PEN));

   if (!bExpand)
   {
      MoveToEx(hdc, x + cxyh, y + 2, NULL);
      LineTo(hdc, x + cxyh, y + cxy - 2);
   }

   MoveToEx(hdc, x + 2, y + cxyh, NULL);
   LineTo(hdc, x + cxy - 2, y + cxyh);

   SelectObject(hdc, oPen);
   SelectObject(hdc, oBrush);
   DeleteObject(hPen);
}

#pragma ENDDUMP

STATIC FUNCTION BrowseForFile( oEdit, oMan, oObj, lIcon, aFilter )
   LOCAL cName, oFile := CFile( oEdit:Caption )
   oEdit:OnWMKillFocus := NIL
   
   DEFAULT lIcon TO .F.
   IF aFilter == NIL
      IF lIcon
         oFile:AddFilter( "Icon Files (*.ico)", "*.ico" )
       ELSE
         TRY
            FOR EACH aFilter IN oObj:__ExplorerFilter
                oFile:AddFilter( aFilter[1], aFilter[2] )
            NEXT
          CATCH
            TRY
               FOR EACH aFilter IN oObj:BackgroundImage:__ExplorerFilter
                   oFile:AddFilter( aFilter[1], aFilter[2] )
               NEXT
            CATCH
            END
         END
      ENDIF
    ELSE
      oFile:AddFilter( aFilter[1], aFilter[2] )
   ENDIF
   IF __ObjHasMsg( oMan:ActiveObject, "Path" ) .AND. !EMPTY( oMan:ActiveObject:Path )
      oFile:Path := oMan:ActiveObject:Path
   ENDIF
   oFile:OpenDialog()
   oEdit:OnWMKillFocus := {|o|o:Destroy() }
   IF oFile:Result != IDCANCEL
      oEdit:Destroy()
      
      IF __ObjHasMsg( oMan:ActiveObject, "Path" )
         oMan:ActiveObject:Path := oFile:Path
         oMan:SetValue( oFile:Name )
       ELSE
         oMan:SetValue( oFile:Path + "\" + oFile:Name )
      ENDIF

      oMan:InvalidateRect(,.F.)
    ELSE
      oEdit:SetFocus()
   ENDIF
RETURN NIL

STATIC FUNCTION BrowseForFolder( oEdit, oMan, oItem )
   LOCAL cDir
   static pCallBack
   DEFAULT pCallBack TO WinCallBackPointer( @BrowseForFolderCallBack() )
   cCurFolder := oEdit:Caption
   cDir := SHBrowseForFolder(,,BIF_NEWDIALOGSTYLE | BIF_BROWSEINCLUDEURLS, CSIDL_DESKTOP, pCallBack )
   oEdit:OnWMKillFocus := {|o|o:Destroy() }
   IF !EMPTY( cDir )
      IF oItem:Caption == "IncludePath" .OR. oItem:Caption == "SourcePath" 
         oEdit:Caption += "; " + cDir
         oEdit:SetFocus()
         RETURN NIL
       ELSE
         oEdit:Destroy()
         oMan:SetValue( cDir )
      ENDIF
      oMan:InvalidateRect(,.F.)
    ELSE
      oEdit:SetFocus()
   END
RETURN NIL

STATIC FUNCTION BrowseForFolderCallBack( hWnd, nMsg, lp, pData )
   LOCAL cBuffer
   SWITCH nMsg
      CASE BFFM_INITIALIZED
         SendMessage( hWnd, BFFM_SETSELECTION, 1, cCurFolder )
         EXIT

      CASE BFFM_SELCHANGED
         cBuffer := SHGetPathFromIDList( lp )
         SendMessage( hWnd, BFFM_SETSTATUSTEXT, 0, cBuffer )
   END
RETURN 0

STATIC FUNCTION CheckKeyDown( o, n )
   LOCAL oPar, cText
   SWITCH n
      CASE 13
         cText := o:Caption
         oPar  := o:Parent
         oPar:SetValue( cText )
         o:Destroy()
         EXIT

      CASE 27
         o:Caption := o:Cargo
         o:Destroy()
         EXIT
   END
RETURN NIL

STATIC FUNCTION CheckChar( o, n, oItem )
   LOCAL oPar, cText, nStart, nEnd, cCaption
   SWITCH n
      CASE 13
         cText := o:Caption
         oPar  := o:Parent
         oPar:SetValue( CompileValue( cText ) )
         o:Destroy()
         EXIT

      CASE 27
         o:Caption := o:Cargo
         o:Destroy()
         EXIT

      DEFAULT
         IF !EMPTY( o:Caption ) .AND. CHR(n) $ "+*/"
            RETURN 0
         ENDIF
         IF ! ( oItem:Caption IN { "Contrast", "Brightness", "PagePosition" } ) .AND. CHR(n) == "-"
            RETURN 0
         ENDIF
         
         IF !CHR(n) $ "-0123456789"
            IF ! ( n >= 35 .AND. n <= 40 ) .AND. n != VK_BACK
               RETURN 0
            ENDIF
          ELSEIF CHR(n) $ "0123456789" .AND. oItem:Caption == "Opacity"

            nStart := LoWord( o:SendMessage( EM_GETSEL, 0, 0 ) ) + 1
            nEnd   := HiWord( o:SendMessage( EM_GETSEL, 0, 0 ) ) + 1

            cCaption := o:Caption+CHR(n)

            IF nEnd > nStart
               cCaption := IIF( nStart > 1, SUBSTR( cCaption, 1, nStart ), "" ) + SUBSTR( cCaption, nEnd )
            ENDIF

            IF VAL( cCaption ) > 100 .OR. VAL( cCaption ) < 0
               RETURN 0
            ENDIF

          ELSEIF CHR(n) == "-" .AND. oItem:Caption $ "WidthHeight" .AND. o:Parent:ActiveObject:ClsName != "StatusBarPanel"
            RETURN 0
         ENDIF
   END
RETURN NIL

STATIC FUNCTION CheckCharPaste( o, n, oItem )
RETURN 0

STATIC FUNCTION CompileValue( cVal )
   IF !EMPTY( cVal )
      cVal := &cVal
   ENDIF
RETURN cVal

