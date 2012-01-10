/*
 * $Id$
 */


static aGroupProp
static cCurFolder

static __aProps := {=>}

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

#include "xEditConstants.ch"

#define DG_ADDCONTROL      1
#define DG_DELCONTROL      2
#define DG_PROPERTYCHANGED 3
#define DG_MOVESELECTION   4
#define DG_FONTCHANGED     5
#define DG_DELCOMPONENT    6

#define PLUSFRAME 9

#define MAPVK_VK_TO_VSC    0
#define MAPVK_VSC_TO_VK    1
#define MAPVK_VK_TO_CHAR   2
#define MAPVK_VSC_TO_VK_EX 3

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ObjManager INHERIT TreeView
   DATA nScroll   PROTECTED INIT 0
   DATA LevelFont PROTECTED
   DATA CurCol    PROTECTED INIT 1
   DATA ItemRect  PROTECTED
   DATA ActiveObject EXPORTED
   DATA Colors       PROTECTED
   DATA ReleaseEditSelection EXPORTED INIT .F.
   DATA ActiveControl EXPORTED
   DATA lPaint        EXPORTED INIT .T.
   METHOD Init() CONSTRUCTOR
   METHOD ResetProperties()
   METHOD Create()
   METHOD OnHScroll( n)        INLINE ::nScroll := n
   METHOD OnDestroy()          INLINE ::LevelFont:Delete(), NIL
   METHOD OnKeyDown()
   METHOD OnUserMsg()
   METHOD OnLButtonDown()
   METHOD OnMouseMove()
   METHOD SetValue()
   METHOD ChangeCtrlFont()
   METHOD SetActiveObjectFont()
   METHOD SetObjectValue()

   METHOD OnEraseBkGnd()
   
   METHOD OnParentNotify()
   METHOD DrawItem()
   
   METHOD GetEditBuffer()
   METHOD OnGetDlgCode()
   METHOD OnSize()
   METHOD EditCaption()
   METHOD CheckValue()
   METHOD GetValue()
   METHOD RenameForm()
   METHOD OnVertScroll()       INLINE IIF( ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow(),;
                                           ( ::ActiveControl:Destroy(), ::ActiveControl := NIL ), ), NIL
   METHOD SetPropDesc()
   METHOD GetPropertiesAndValues( oObj ) INLINE __ClsGetPropertiesAndValues( oObj )
   METHOD EditText()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD OnGetDlgCode() CLASS ObjManager
   IF ::wParam == VK_RETURN
      ::PostMessage( WM_USER+4768 )
      RETURN 0
   ENDIF
RETURN DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS

METHOD Init( oParent ) CLASS ObjManager
   LOCAL cColor
   ::ReleaseEditSelection := .F.

   DEFAULT ::__xCtrlName  TO "ObjManager"
   ::Super:Init( oParent )
   ::SmallCaption := .F.

   ::Colors := {}
   FOR EACH cColor IN ::System:Color:Keys
       AADD( ::Colors, { ::System:Color[ cColor ], cColor } )
   NEXT
RETURN Self
//------------------------------------------------------------------------------------------

METHOD Create() CLASS ObjManager
   ::ClipChildren := .T.
   ::Tooltip:Text  := "-"
   ::Tooltip:Track := .T.
   ::ToolTip:CloseOnClick := .T.
   ::ExStyle := 0
   ::Super:Create()
   ::SetScrollTime(0)
   ::LevelFont := Font()
   ::LevelFont:Weight     := 700
   ::LevelFont:Create()
   ::SetIndent( 15 )
   ::SetItemHeight( ABS( ::Font:Height ) + 6 )
   ::Action := {|o| ::SetPropDesc(o) }
RETURN Self

//---------------------------------------------------------------------------------------------------
METHOD OnSize() CLASS ObjManager
   ::Columns[1][1] := Int(::ClientWidth/2)-11
   ::Columns[2][1] := Int(::ClientWidth/2)-7
   ::InvalidateRect(,.f.)
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD OnEraseBkGnd( hDC ) CLASS ObjManager
   LOCAL nTop, rc, hItem := ::GetLastVisibleItem()
   nTop := ::GetExpandedCount() * ::GetItemHeight()
   nTop := 0
   IF !EMPTY( hItem )
      rc := ::GetItemRect( hItem )
      nTop := rc:bottom
      IF nTop >= ::Height .OR. !::lPaint
         RETURN 1
      ENDIF
   ENDIF
   _FillRect( hDC, {0,nTop,::Width, ::Height}, ::Parent:BkBrush )//::System:CurrentScheme:Brush:ToolStripPanelGradientEnd )
RETURN 1

//---------------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS ObjManager
   LOCAL tvcd
   ( nwParam )
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

METHOD DrawItem( tvcd ) CLASS ObjManager
   LOCAL n, nState, nBack, nFore, lExpanded, rc, nWidth, nLeft, nRight, nBottom, aAlign, x, y, lHasChildren
   LOCAL aRow, aCol, hOldPen, nAlign, cType, nColor, hOld, hBrush, aRest, cText, nfHeight := ABS( ::Font:Height )+3
   LOCAL nPos, cCap, cProp, nLevel, hDC, oItem
   LOCAL hIcon, lDisabled := .F.
   LOCAL lEnabled, hBoldFont

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

   TRY
      lDisabled := oItem:ColItems[1]:ReadOnly
     catch
      lDisabled := .F.
   END
   IF cText == "Height" .AND. ::ActiveObject:__xCtrlName == "Expando" .AND. !::ActiveObject:Expanded
      lDisabled := .T.
   ENDIF   

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
//   TRY
      FOR n := 1 TO LEN( oItem:ColItems )
          nLeft   += nWidth

          cText   := oItem:ColItems[n]:Value

          nWidth  := ::Columns[n+1][1]
          nRight  := nLeft + nWidth + 2
          nBottom := rc:bottom

          nColor  := oItem:ColItems[n]:Color
          nAlign  := oItem:ColItems[n]:Align

          nAlign := TA_LEFT

          IF oItem:ColItems[n]:ColType == "ANIMATIONSTYLE"
             IF ( nPos := hScan( ::System:WindowAnimation, ::ActiveObject:AnimationStyle ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "STOPBITS"
             IF ( nPos := hScan( ::System:StopBits, ::ActiveObject:StopBits ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "PARITY"
             IF ( nPos := hScan( ::System:Parity, ::ActiveObject:Parity ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "HANDSHAKE"
             IF ( nPos := hScan( ::System:HandShake, ::ActiveObject:HandShake ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "OLEVERB"
             IF ( nPos := hScan( ::System:OleVerb, ::ActiveObject:OleVerb ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "ROLE"
             cText := cText[ ::ActiveObject:Role ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "DROPDOWN"
             cText := cText[ ::ActiveObject:DropDown ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "SYSFOLDERS"
             IF ( nPos := hScan( ::System:Folders, ::ActiveObject:SysFolder ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "CBDROPDOWN"
             IF ( nPos := hScan( ::System:DropDownStyle, ::ActiveObject:DropDownStyle ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "SHORTCUTKEY_KEY"
             GetRealKeyName(::ActiveObject:ShortcutKey:Key, @cText, 40)
          ENDIF

          IF oItem:ColItems[n]:ColType == "TEXTALIGNMENT"
             IF ( nPos := hScan( ::System:TextAlignment, ::ActiveObject:ImageAlign ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "DTSFORMATS"
             IF ( nPos := hScan( ::System:DateTimeFormat, ::ActiveObject:Format ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "LVGALIGNMENT"
             IF ( nPos := hScan( ::System:ListViewGroupAlign, ::ActiveObject:Alignment ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "ADSDATADRIVERS"
             IF ( nPos := hScan( ::System:AdsDataDrivers, ::ActiveObject:Driver ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "DATADRIVERS"
             IF ( nPos := hScan( ::System:DataDrivers, ::ActiveObject:Driver ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "CURSORS"
             IF ( nPos := ASCAN( ::ActiveObject:__CursorValues, ::ActiveObject:Cursor ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "SERVICES"
             IF ( nPos := ASCAN( oItem:ColItems[n]:Value, ::ActiveObject:ServiceName ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "ENUM"
             TRY
               cText := cText[ oItem:ColItems[n]:SetValue ]
             CATCH
             END
          ENDIF

          IF oItem:ColItems[n]:ColType == "SERVERTYPE"
             cText := cText[ oItem:ColItems[n]:SetValue ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "TARGETTYPE"
             cText := cText[ oItem:ColItems[n]:SetValue ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "REPRESENTATION"
             cText := cText[ oItem:ColItems[n]:SetValue ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "ALIGNMENT"
             cText := cText[ oItem:ColItems[n]:SetValue ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "SHOWMODE"
             cText := cText[ ::ActiveObject:ShowMode ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "FRAMESTYLES"
             cText := cText[ ::ActiveObject:FrameStyle ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "IMAGETYPE"
             cText := cText[ ::ActiveObject:ImageType ]
          ENDIF

          //IF oItem:ColItems[n]:ColType == "STATES"
          //   cText := cText[ ::ActiveObject:InitialState+1 ]
          //ENDIF

          IF oItem:ColItems[n]:ColType == "PAGE_POSITIONS"
             cText := cText[ ::ActiveObject:Position+1 ]
          ENDIF

          //IF oItem:ColItems[n]:ColType == "VIEWSTYLES"
          //   cText := cText[ ::ActiveObject:ViewStyle+1 ]
          //ENDIF

          IF oItem:ColItems[n]:ColType == "CHECKSTYLES"
             cText := cText[ ::ActiveObject:CheckStyle ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "DRAWSTYLES"
             cText := cText[ ::ActiveObject:OwnerDraw ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "BALLOONICONS"
             cText := cText[ ::ActiveObject:BalloonTipIcon+1 ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "CASETYPES"
             cText := cText[ ::ActiveObject:Case ]
          ENDIF

          IF oItem:ColItems[n]:ColType == "ENUMERATION"
             cProp := oItem:ColItems[n]:Prop
             x := ::ActiveObject:__OleVars[ cProp ][4]

             IF x != NIL
                TRY
                   cText := cText[x+1]:Name
                  catch
                   cText := XSTR(x)
                END
              ELSE
                cText := ""
             ENDIF
          ENDIF

          cType   := VALTYPE( cText )

          SWITCH cType
             CASE "O"
                  cText  := "(object)"
                  EXIT

             CASE "A"
                  cText  := IIF( LEN(cText)>0, cText[1], "" )
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
             nFore := RGB(0,0,0) //GetSysColor( COLOR_HIGHLIGHTTEXT )
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

          IF ASCAN( m->aChangedProps, {|a| a[1] == ::ActiveObject .AND. a[2] == oItem:ColItems[n]:Prop } ) > 0
             SelectObject( hDC, hBoldFont )
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
//   CATCH
//   END 
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

METHOD CheckValue( cProp, cRoot, xValue ) CLASS ObjManager
   LOCAL xVal, oItem := ::SearchString( cProp, cRoot )
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

//---------------------------------------------------------------------------------------------------

METHOD GetValue( cProp, cRoot ) CLASS ObjManager
   LOCAL oItem := ::SearchString( cProp, cRoot )
   IF oItem != NIL
      RETURN oItem:ColItems[1]:Value
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD RenameForm( cOldName, cNewName, lProject ) CLASS ObjManager
   LOCAL n, lForce
   LOCAL cNewBuffer, lComment, oEditor, Line

   IF !( cOldName == cNewName )
      
      DEFAULT lProject TO .F.
      
      IF lProject
         oEditor := ::Application:ProjectPrgEditor
         cNewBuffer := ""
         lComment   := .F.
         Line := oEditor:FirstLine
         WHILE Line != NIL
            cNewBuffer += ChangePrgLine( Line[ ED_BUFFER ], cOldName, cNewName, @lComment  ) + CHR(13) + CHR(10)
            IF Len( Line ) == 4
               Line := Line[ ED_NEXTLINE ]
             ELSE
               Line := Line[ ED_COLLAPSED_START ]
            ENDIF
         ENDDO
         lForce := ::Application:SourceTabs:CurSel == 1 .AND. IsWindowVisible( oEditor:oDisplay:hWnd )
         oEditor:Load( ,cNewBuffer, .T., .T. )
         oEditor:lModified := .T.
         cNewName := ::Application:Project:properties:Name
         ::Application:SourceTabs:SetCurSel( 1 )
       ELSE
         oEditor := ::Application:ProjectPrgEditor

         IF ::ActiveObject:lCustom
            ::ActiveObject:__NewName := cNewName
            ::ActiveObject:__OldName := cOldName
         ENDIF

         FOR n := 1 TO 2
             cNewBuffer := ""
             lComment   := .F.
             Line := oEditor:FirstLine
             WHILE Line != NIL
                cNewBuffer += ChangePrgLine( Line[ ED_BUFFER ], cOldName, cNewName, @lComment  ) + CHR(13) + CHR(10)
                IF Len( Line ) == 4
                   Line := Line[ ED_NEXTLINE ]
                 ELSE
                   Line := Line[ ED_COLLAPSED_START ]
                ENDIF
             ENDDO
             lForce := ::Application:SourceEditor:oEditor == oEditor .AND. n == ::Application:SourceTabs:CurSel
             oEditor:Load( ,cNewBuffer, lForce, lForce )
             oEditor:lModified := .T.
             
             // Loop to rename the caller __Project( NIL ):Run( RenamedForm( NIL ) )
             oEditor := ::ActiveObject:Form:Editor
         NEXT
         oEditor := ::ActiveObject:Form:Editor
      ENDIF
      
      IF lProject .OR. ::ActiveObject:ClsName == "VXH_FORM_IDE" .OR. ::ActiveObject:ClsName == "CCTL" 
         TRY
            IF ( n := aScan( xEdit_GetEditors(), oEditor, , , .T. ) ) > 0
               ::Application:SourceTabs:SetItemText( n, cNewName +".prg", .F. )
            ENDIF
            IF ::ActiveObject:ClsName == "VXH_FORM_IDE" .OR. ::ActiveObject:ClsName == "CCTL" 
               IF ( n := aScan( ::Application:Project:Forms, ::ActiveObject, , , .T. ) ) > 0
                  ::Application:FormsTabs:SetItemText( n, cNewName, .F. )
               ENDIF
            ENDIF
          catch
         END
      ENDIF
      ::Application:Project:Modified := .T.
   ENDIF
RETURN Self

METHOD ChangeCtrlFont( cf, oItem, oFont ) CLASS ObjManager
   LOCAL aFont1, aFont2, aProps
   
   IF cf != NIL
      aProps := { "xHeight", "xWidth", "xEscapement", "xOrientation", "xWeight", "xnItalic", "xnUnderline", "xnStrikeOut",;
                  "xCharSet", "OutPrecision", "ClipPrecision", "Quality", "PitchAndFamily" }

      aFont1 := {}                                
      AADD( aFont1, cf:lpLogFont:lfFaceName:AsString() )
      AADD( aFont1, cf:lpLogFont:lfHeight              )
      AADD( aFont1, cf:lpLogFont:lfWidth               )
      AADD( aFont1, cf:lpLogFont:lfEscapement          )
      AADD( aFont1, cf:lpLogFont:lfOrientation         )
      AADD( aFont1, cf:lpLogFont:lfWeight              )
      AADD( aFont1, cf:lpLogFont:lfItalic              )
      AADD( aFont1, cf:lpLogFont:lfUnderline           )
      AADD( aFont1, cf:lpLogFont:lfStrikeOut           )
      AADD( aFont1, cf:lpLogFont:lfCharSet             )
      AADD( aFont1, cf:lpLogFont:lfOutPrecision        )
      AADD( aFont1, cf:lpLogFont:lfClipPrecision       )
      AADD( aFont1, cf:lpLogFont:lfQuality             )
      AADD( aFont1, cf:lpLogFont:lfPitchAndFamily      )

      aFont2 := {}
      AADD( aFont2, oFont:xFaceName      )
      AADD( aFont2, oFont:xHeight        )
      AADD( aFont2, oFont:xWidth         )
      AADD( aFont2, oFont:xEscapement    )
      AADD( aFont2, oFont:xOrientation   )
      AADD( aFont2, oFont:xWeight        )
      AADD( aFont2, oFont:xnItalic       )
      AADD( aFont2, oFont:xnUnderline    )
      AADD( aFont2, oFont:xnStrikeOut    )
      AADD( aFont2, oFont:xCharSet       )
      AADD( aFont2, oFont:OutPrecision   )
      AADD( aFont2, oFont:ClipPrecision  )
      AADD( aFont2, oFont:Quality        )
      AADD( aFont2, oFont:PitchAndFamily )

//      IF IsWindow( oFont:Parent:hWnd )
         ::Application:Project:SetAction( { { DG_FONTCHANGED, oFont, aFont1, aFont2, oItem } }, ::Application:Project:aUndo )
//      ENDIF
   ENDIF
RETURN Self

METHOD SetActiveObjectFont( oFont, aFont1, aFont2, oItem ) CLASS ObjManager
   LOCAL aProps, n
   ( aFont2 )
   aProps := { "xFaceName", "xHeight", "xWidth", "xEscapement", "xOrientation", "xWeight", "xnItalic", "xnUnderline", "xnStrikeOut",;
               "xCharSet", "OutPrecision", "ClipPrecision", "Quality", "PitchAndFamily" }
   FOR n := 1 TO LEN( aProps )
       __objSendMsg( oFont, "_" + UPPER( aProps[n] ), aFont1[n] )
   NEXT
   oFont:Modify()
   oFont:Set( oFont:Parent )
   ::ResetProperties(,,.T., IIF( oItem:Expanded, {oItem:Caption},) )
RETURN Self

//------------------------------------------------------------------------------------------
METHOD SetValue( xValue, cCaption, oItem ) CLASS ObjManager
   LOCAL oObj, cProp, cProp2, xVal
    
   DEFAULT oItem TO FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
   IF oItem == NIL
      RETURN Self
   ENDIF
   IF oItem:Caption == "ImageName" .AND. !EMPTY( xValue ) .AND. !FILE( xValue )
      IF ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow()
         ::ActiveControl:OnWMKillFocus := NIL
         ::ActiveControl:Destroy()
         ::ActiveControl := NIL
      ENDIF
      ::MessageBox( "File not found " + xValue, "ImageName", MB_ICONINFORMATION )
      ::SetFocus()
      RETURN Self
   ENDIF
   
   cProp := oItem:ColItems[1]:Prop
   cProp2:= oItem:ColItems[1]:Prop2


   IF cProp == "Name" .AND. IsValidIdentifier( xValue )
      xValue := LEFT( xValue, 25 ) // Truncate at 25
   ENDIF

   IF (cProp == "Name" .OR. cProp == "Alias" ).AND. !IsValidIdentifier( xValue )
      IF ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow()
         ::ActiveControl:OnWMKillFocus := NIL
         ::ActiveControl:Destroy()
         ::ActiveControl := NIL
      ENDIF
      MessageBox( ::Application:MainForm:hWnd, "Property must be a valid identifier", "Visual xHarbour", MB_OK | MB_ICONEXCLAMATION | MB_APPLMODAL )
      ::SetFocus()
      RETURN NIL
   ENDIF

   IF cProp == "Name" .AND. !( ::ActiveObject:Name == xValue ) .AND. !CheckControlName( ::ActiveObject:Form, xValue, ::ActiveObject )
      IF ::ActiveControl != NIL .AND. ::ActiveControl:IsWindow()
         ::ActiveControl:OnWMKillFocus := NIL
         ::ActiveControl:Destroy()
         ::ActiveControl := NIL
      ENDIF
      ::Application:MainForm:MessageBox( "The name " + xValue +" is already in use by another component", "Property Value is not valid", MB_ICONEXCLAMATION )
      ::SetFocus()
      RETURN NIL
   ENDIF
  
   IF cProp2 != NIL
      oObj := ::ActiveObject:&cProp2
    ELSE
      oObj := ::ActiveObject
   ENDIF

   IF cProp2 == "MDIClient"
      IF cProp == "AlignLeft" .OR. cProp == "AlignTop" .OR. cProp == "AlignRight" .OR. cProp == "AlignBottom"
         xValue := oItem:ColItems[1]:Value[2][ xValue ]
      ENDIF
   ENDIF

   IF cProp2 != NIL .AND. cProp2 == "Dock" .AND. AT( "Margin", cProp ) == 0
      xValue := oItem:ColItems[1]:Value[2][ xValue ]
    ELSEIF cProp IN {"ImageList","HotImageList","BandChild","DataSource","ImageListSmall","HeaderMenu","ButtonMenu","ContextMenu","PageChild","Socket","BindingSource","SqlConnector"}
      xValue := oItem:ColItems[1]:Value[2][ xValue ]
   ENDIF

   IF cProp == "DataSource" .AND. ::ActiveObject:ClsName == "DataGrid" .AND. ::ActiveObject:DataSource == xValue
      RETURN NIL
   ENDIF

   IF cProp == "Cursor"
      xValue := ::ActiveObject:__CursorValues[ xValue ]
   ENDIF
   TRY
      xVal := oObj:&cProp
    CATCH
      xVal := xValue
   END
   ::Application:Project:SetAction( { { DG_PROPERTYCHANGED, ::ActiveObject, xValue, cCaption, oItem, xVal, cProp } }, ::Application:Project:aUndo )
RETURN Self

METHOD SetObjectValue( oActiveObject, xValue, cCaption, oItem ) CLASS ObjManager
   STATIC s_bSetting := .F.

   LOCAL n, cProp, cProp2, oObj, nColor, Topic, Event, nCurr, nHost, cVal
   LOCAL oError
   
   DEFAULT oItem TO FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )

   IF oItem == NIL .OR. Empty( oItem:ColItems )
      RETURN NIL
   ENDIF

   // No change!!!
   IF VALTYPE( oItem:ColItems[1]:Value ) != "O" .AND. VALTYPE( oItem:ColItems[1]:Value ) != "A" .AND. VALTYPE( oItem:ColItems[1]:Value ) == VALTYPE( xValue ) .AND. oItem:ColItems[1]:Value == xValue
      RETURN NIL
   ENDIF

   IF s_bSetting
      RETURN NIL
   ELSE
      s_bSetting := .T.
   ENDIF

   cProp := oItem:ColItems[1]:Prop
   cProp2:= oItem:ColItems[1]:Prop2

   IF cProp2 != NIL
      oObj  := oActiveObject:&cProp2
      AADD( m->aChangedProps, {oActiveObject, cProp2} )
    ELSE
      oObj  := oActiveObject
      AADD( m->aChangedProps, {oActiveObject, cProp} )
   ENDIF
   IF VALTYPE( xValue )=="C"
      xValue := ALLTRIM( xValue )
   ENDIF

   IF cProp2 != NIL .AND. cProp2 == "MDIClient" .AND. ( cProp == "AlignLeft" .OR. cProp == "AlignTop" .OR. cProp == "AlignRight" .OR. cProp == "AlignBottom" )
      __objSendMsg( oActiveObject:MDIClient, "_"+cProp, xValue )
      IF xValue != NIL
         oItem:ColItems[1]:Value[1] := xValue:Name
       ELSE
         oItem:ColItems[1]:Value[1] := ""
         oItem:ColItems[1]:Value[2][1] := NIL
      ENDIF
      PostMessage( oActiveObject:hWnd, WM_SIZE, 0, MAKELPARAM( oActiveObject:ClientWidth, oActiveObject:ClientHeight ) )

    ELSEIF cProp2 != NIL .AND. cProp2 == "Dock" .AND. AT( "Margins", cProp ) == 0

      __objSendMsg( oActiveObject:Dock, "_"+cProp, xValue )
      IF xValue != NIL
         oItem:ColItems[1]:Value[1] := xValue:Name
       ELSE
         oItem:ColItems[1]:Value[1] := ""
         oItem:ColItems[1]:Value[2][1] := NIL
      ENDIF

    ELSEIF cProp IN {"ImageList","HotImageList","BandChild","DataSource","ImageListSmall","HeaderMenu","ButtonMenu","ContextMenu","PageChild","Socket","BindingSource","SqlConnector"}
      TRY
         IF VALTYPE( xValue ) == "O" .AND. xValue:ComponentType == "DataSource" .AND. !xValue:IsOpen
            xValue:Create()
         ENDIF
      CATCH
      END
      __objSendMsg( oActiveObject, "_"+cProp, xValue )

      IF cProp == "BandChild" .AND. xValue != NIL
         // Reposition child Z order before CoolBar
         nCurr := ASCAN( oActiveObject:Parent:Parent:Children, {|o|o:hWnd == oActiveObject:Parent:hWnd } )
         nHost := ASCAN( oActiveObject:Parent:Parent:Children, {|o|o:hWnd == xValue:hWnd } )

         IF nHost > nCurr
            ADEL( oActiveObject:Parent:Parent:Children, nHost, .T. )
            AINS( oActiveObject:Parent:Parent:Children, nCurr, xValue, .T. )
         ENDIF
       ELSEIF cProp == "DataSource" .AND. oActiveObject:ClsName == "DataGrid"
         IF xValue == NIL .OR. xValue:IsOpen
            oActiveObject:AutoAddColumns()
         ENDIF
      ENDIF

      IF xValue != NIL
         IF cCaption != NIL
            oItem:ColItems[1]:Value[1] := cCaption
          ELSE
            oItem:ColItems[1]:Value[1] := xValue:Name
         ENDIF
       ELSE
         oItem:ColItems[1]:Value[1] := ""
         oItem:ColItems[1]:Value[2][1] := NIL
      ENDIF

    ELSE
      IF cProp == "Caption" .AND. xValue == "" .AND. oObj:ClsName == "ToolButton"
         xValue := NIL
      ENDIF
      
      IF oItem:Owner:Caption == "COM Properties"
         IF HGetPos( oObj:__OleVars, cProp ) != 0
            IF VALTYPE( oObj:__OleVars[cProp][1] ) == "A"
               oObj:__OleVars[cProp][4] := xValue
             ELSE
               oObj:__OleVars[cProp][1] := xValue
            ENDIF   
         ENDIF
       ELSE
         __objSendMsg( oObj, "_"+cProp, xValue )
      ENDIF
      
      IF cProp == "Name"
         IF oActiveObject:ClsName == "ProjProp" .AND. !FILE( oActiveObject:Path + "\" + oActiveObject:Name + ".vxh" )
            n := RAT( "\", oActiveObject:Path )
            oActiveObject:Path := SUBSTR( oActiveObject:Path, 1, n ) + xValue
            ::CheckValue( "Path", "Path", oActiveObject:Path )
         ENDIF
      ENDIF

      IF cProp == "Name" .OR. cProp == "Icon" .OR. cProp == "ImageIndex"
         ::Application:ObjectTree:Set( oActiveObject )
      ENDIF
      
      IF cProp2 != NIL .AND. cProp2 == "Font" .AND. __clsParent( oActiveObject:ClassH, "COMPONENT" )
         oObj:Set( oActiveObject )
      ENDIF
      
      IF cProp IN {"Left","Top","Width","Height"}
         oActiveObject:MoveWindow()
         oActiveObject:Parent:InvalidateRect()
      ENDIF
      IF cProp == "Caption" .AND. __ObjHasMsg( oActiveObject, "SmallCaption" ) .AND. oActiveObject:SmallCaption
         oActiveObject:SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
      ENDIF

      IF cProp != "ColorScheme" .AND. AT( "Color", cProp ) > 0 .AND. VALTYPE( xValue ) != "L"
         nColor := xValue
         IF ( n := ASCAN( ::Colors, {|a|a[1]==xValue} ) ) > 0
            IF nColor == NIL .AND. UPPER( cProp ) == "MASKCOLOR"
               xValue := "None"
             ELSE
               xValue := ::Colors[n][2]
            ENDIF
            TRY
               IF xValue == "System Default..." .AND. nColor == NIL
                  nColor := oObj:&( STRTRAN( cProp, "Color", "SysColor" ) )
               ENDIF
             catch
            END
          ELSE
            IF cProp == "BackColor" .AND. ( xValue == oObj:BackSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:BackSysColor
            ENDIF
            IF cProp == "ForeColor" .AND. ( xValue == oObj:ForeSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:ForeSysColor
            ENDIF
            IF cProp == "SelBackColor" .AND. ( xValue == oObj:BackSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:BackSysColor
            ENDIF
            IF cProp == "SelForeColor" .AND. ( xValue == oObj:ForeSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:ForeSysColor
            ENDIF
            IF cProp == "HighlightColor" .AND. ( xValue == oObj:HighlightSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:HighlightSysColor
            ENDIF
            IF cProp == "HighlightTextColor" .AND. ( xValue == oObj:HighlightTextSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:HighlightTextSysColor
            ENDIF
            IF cProp == "HeaderBackColor" .AND. ( xValue == oObj:HeaderBackSysColor .OR. xValue == NIL )
               xValue := "System Default..."
               nColor := oObj:HeaderBackSysColor
            ENDIF
            IF cProp == "TransparentColor" .AND. ( xValue == NIL )
               xValue := "System Default..."
               nColor := -1
            ENDIF
         ENDIF
         IF VALTYPE( xValue ) != "C"
            xValue := "Custom..."
         ENDIF
         TRY
            IF nColor == NIL
               nColor := oObj:&( STRTRAN( RIGHT(cProp,9), "Color", "SysColor" ))
               xValue := "System Default..."
            ENDIF
          catch
         END
            
         oItem:ColItems[1]:Color := nColor
         ::InvalidateRect(,.F.)
      ENDIF

      cVal := oItem:ColItems[1]:Value

      IF !( VALTYPE( oItem:ColItems[1]:Value ) == "A" ) 
         oItem:ColItems[1]:Value := xValue
      ENDIF

   ENDIF

   IF cProp == "MDIContainer" .OR. cProp == "MDIChild"
      ::ResetProperties()
   ENDIF

   ::Application:Project:Modified := .T.
   IF oActiveObject:ClsName != "ProjProp"
      ::Application:Project:CurrentForm:UpdateSelection()

      TRY
         IF cProp == "Name"
            ::RenameForm( cVal, xValue,, oActiveObject )
            IF oActiveObject:ClsName == "VXH_FORM_IDE"
               ::Application:FileTree:UpdateView()
               IF FILE( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + cVal +".prg" )
                  FRENAME( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + cVal +".prg",;
                           ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + xValue +".prg" )
               ENDIF
               IF FILE( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + cVal +".xfm" )
                  FRENAME( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + cVal +".xfm",;
                           ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + xValue +".xfm" )
               ENDIF
            ENDIF
            
            FOR EACH Topic IN oActiveObject:Events
                FOR EACH Event IN Topic[2]
                    IF Empty( Event[2] )
                       LOOP
                    ENDIF

                    ::Application:EventManager:RenameEvent( Event[1], Event[2], IIF( cProp != "Name", xValue, STRTRAN( Event[2], cVal, xValue ) ), .F. )
                    
                    Event[2] := STRTRAN( Event[2], cVal, xValue )

                NEXT
            NEXT

            ::Application:EventManager:ResetEvents()
         ENDIF
      CATCH oError
         IF oError != NIL
            view oError:ProcName, oError:ProcLine, oError:Description, oError:Operation
         ENDIF
      END
    ELSEIF cProp == "Name"
      ::RenameForm( "__"+cVal, "__"+xValue, .T., oActiveObject )
      IF FILE( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + cVal +".xfm" )
         FRENAME( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + cVal +".xfm",;
                  ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source + "\" + xValue +".xfm" )
      ENDIF
   ENDIF
   s_bSetting := .F.

RETURN .T.

FUNCTION IsValidIdentifier( sID, oObject )

   LOCAL cChar, aData, nFor
   IF oObject != NIL .AND. __objHasMsg( oObject, "Parent" )
      aData := __objGetMsgList( oObject:Parent )
      IF ASCAN( aData, UPPER( sID ) ) > 0
         RETURN .F.
      ENDIF
      aData := __objGetMethodList( oObject:Parent )
      IF ASCAN( aData, UPPER( sID ) ) > 0
         RETURN .F.
      ENDIF
   ENDIF
   IF ! ( sID[1] == '_' .OR. ( sID[1] >= 'a' .AND. sID[1] <= 'z' ) .OR. ( sID[1] >= 'A' .AND. sID[1] <= 'Z' ) )
      RETURN .F.
   ENDIF
   
   nFor := 1
   FOR EACH cChar IN sID
      IF ! ( sID[ nFor ] == '_' .OR. ( sID[ nFor ] >= 'a' .AND. sID[ nFor ] <= 'z' ) .OR. ( sID[ nFor ] >= 'A' .AND. sID[ nFor ] <= 'Z' ) .OR. ( sID[ nFor ] >= '0' .AND. sID[ nFor ] <= '9' ) )
         RETURN .F.
      ENDIF
      nFor++
   NEXT
   IF oObject != NIL .AND. __objHasMsg( oObject, "Parent" )
      IF ASCAN( oObject:Parent:Children, {|o| o:Name == sID } ) > 0
         RETURN .F.
      ENDIF
      IF __objHasMsg( oObject:Parent, "Components" )
         IF ASCAN( oObject:Parent:Components, {|o| o:Name == sID } ) > 0
            RETURN .F.
         ENDIF
      ENDIF
   ENDIF
RETURN .T.
//---------------------------------------------------------------------------------------------------

METHOD EditCaption() CLASS ObjManager
   LOCAL oItem := ::SearchString( "Caption" )
   IF oItem != NIL
      oItem:Select()
      ::PostMessage( WM_USER+4768 )
   ENDIF
RETURN Self

METHOD SetPropDesc( oItem ) CLASS ObjManager
   LOCAL cHelp
   IF ::Form:HasProperty( "Label1" )
      WITH OBJECT ::Form
         IF !EMPTY( oItem:ColItems )
            IF :Label1:Visible
               :Label1:Visible := .F.
               :Label2:Visible := .F.
               :Label3:Visible := .F.
               :Label4:Visible := .F.

               :Label5:Visible := .T.
               :Label6:Visible := .T.
            ENDIF

            :Label5:Caption := oItem:ColItems[1]:Prop

            IF oItem:Owner:Caption == "COM Properties" .AND. HGetPos( ::ActiveObject:__OleVars, :Label5:Caption ) != 0
               cHelp := ::ActiveObject:__OleVars[ :Label5:Caption ][5]

            ENDIF
            DEFAULT cHelp TO "Visual xHarbour - Object Manager Property Description"
            :Label6:Caption := cHelp
          ELSE
            :Label5:Caption := ""
            :Label6:Caption := ""
         ENDIF
      END
   ENDIF
RETURN Self


//---------------------------------------------------------------------------------------------------

METHOD ResetProperties( aSel, lPaint, lForce, aSubExpand, lRefreshComp ) CLASS ObjManager
   LOCAL cProp, cProp2, aProp, xValue, n, oItem, nColor, aSub, aCol, oSub, oObj, xValue2, lReadOnly
   LOCAL aProperties, aProperty, aSubProp, cType, Child, xProp

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

   IF ::Form:HasProperty( "Label1" )
      IF ! ::Form:Label1:Visible
         WITH OBJECT ::Form
            :Label5:Visible := .F.
            :Label6:Visible := .F.

            :Label1:Visible := .T.
            :Label2:Visible := .T.
            :Label3:Visible := .T.
            :Label4:Visible := .T.
         END
      ENDIF
      IF ::ActiveObject != NIL
         ::Form:Label2:Caption := ::ActiveObject:Name
         ::Form:Label4:Caption := ::ActiveObject:__xCtrlName
      ENDIF
   ENDIF
   
   IF ::ActiveObject != NIL .AND. ::ActiveObject == aSel[1][1] .AND. !lForce
      RETURN Self
   ENDIF

   IF ::ActiveObject != NIL
      IF __clsParent( ::ActiveObject:ClassH, "COMPONENT" )
         ::Application:Components:InvalidateRect()
       ELSE
         //::Application:MainForm:FormEditor1:InvalidateRect()
      ENDIF
   ENDIF

   ::ActiveObject := aSel[1][1]

   IF ::Form:HasProperty( "Label2" )
      ::Form:Label2:Caption := ::ActiveObject:Name
      ::Form:Label4:Caption := ::ActiveObject:__xCtrlName
   ENDIF
   
   ::Application:Props[ "ComboSelect" ]:SelectControl( ::ActiveObject )

   IF lRefreshComp
      ::Application:Components:Refresh()
   ENDIF
   
   ::lPaint := .F.

   ::SetRedraw( .F. )

   //EnableScrollBar( ::hWnd, SB_VERT, ESB_DISABLE_BOTH )
   ::ResetContent()
   //EnableScrollBar( ::hWnd, SB_VERT, ESB_ENABLE_BOTH )
   
   TRY
      WITH OBJECT ::Form
         :Label5:Hide()
         :Label6:Hide()

         :Label1:Show()
         :Label2:Show()
         :Label3:Show()
         :Label4:Show()
         :Label2:Caption := ::ActiveObject:Name
         :Label4:Caption := ::ActiveObject:__xCtrlName
      END
   CATCH
   END

   aProperties := ::GetPropertiesAndValues( ::ActiveObject )

   aSort( aProperties,,,{|x, y| x[1] < y[1]})

   FOR EACH aProperty IN aProperties
       //::Application:Yield()
       //hb_gcAll()
       cProp  := aProperty[1]

       IF cProp == "RESOURCES" .AND. ::ActiveObject:ClsName == "Application"
          LOOP
       ENDIF
       IF cProp == "WIDTH" .AND. ::ActiveObject:ClsName == "ExplorerBar"
          LOOP
       ENDIF
       IF ::ActiveObject:__CustomOwner .AND. cProp IN {"LEFT","TOP","WIDTH","HEIGHT"}
          LOOP
       ENDIF
       IF __ObjHasMsg( ::ActiveObject, "__PROPFILTER" ) .AND. ASCAN( ::ActiveObject:__PropFilter, cProp ) > 0
          LOOP
       ENDIF
       IF cProp == "MDICLIENT" .AND. ( !::ActiveObject:MDIContainer .OR. ::ActiveObject:MDIChild )
          LOOP
       ENDIF
       IF cProp == "MDICHILD" .AND. ::ActiveObject:MDIContainer
          LOOP
       ENDIF
       IF cProp == "MDICONTAINER" .AND. ::ActiveObject:MDIChild
          LOOP
       ENDIF

       IF cProp IN { "PANEHEIGHT","OWNER","PROGID","CLSID" }
          LOOP
       ENDIF
       IF cProp == "POSITION" .AND. ::ActiveObject:ClsName == "ToolButton"
          LOOP
       ENDIF
       IF ( cProp == "DOCK" .OR. cProp == "ANCHOR" ).AND. ::ActiveObject:Parent:ClsName == "StatusBarPanel"
          LOOP
       ENDIF

       IF cProp == "DOCK" .AND. ::ActiveObject:ClsName == "ToolStrip" .AND. UPPER( ::ActiveObject:Parent:ClsName ) == "TOOLSTRIPCONTAINER"
          LOOP
       ENDIF

       IF cProp == "MDICHILD" .AND. ( LEN( ::Application:Project:Forms ) > 0 .AND. ( ::ActiveObject:hWnd == ::Application:Project:Forms[1]:hWnd .OR. !::Application:Project:Forms[1]:MdiContainer ) )
          IF ! ( ::Application:Project:Properties:TargetType IN {3,5} )
             LOOP
          ENDIF
       ENDIF
       IF cProp == "MDICONTAINER" .AND. LEN( ::Application:Project:Forms ) > 0 .AND. !( ::ActiveObject:hWnd == ::Application:Project:Forms[1]:hWnd )
          LOOP
       ENDIF
       hb_gcStep()

       TRY
         xValue := ::ActiveObject:&cProp
       CATCH
         xValue := NIL
       END
       cType  := VALTYPE( xValue )
       nColor := NIL

       IF cProp != "COLORSCHEME" .AND. AT( "COLOR", cProp ) > 0 .AND. VALTYPE( xValue ) != "L"
          cType  := "COLORREF"
          nColor := xValue

          IF ( n := ASCAN( ::Colors, {|a|a[1]==xValue} ) ) > 0
             IF nColor == NIL .AND. UPPER( cProp ) == "MASKCOLOR"
                xValue := "None"
              ELSE
                xValue := ::Colors[n][2]
             ENDIF
             TRY
                IF xValue == "System Default..." .AND. nColor == NIL
                   nColor := oObj:&( STRTRAN( cProp, "Color", "SysColor" ) )
                ENDIF
              catch
             END
           ELSE
             IF cProp == "LINKCOLOR" .AND. ( xValue == ::ActiveObject:LinkSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:LinkSysColor
             ENDIF
             IF cProp == "BACKCOLOR" .AND. ( xValue == ::ActiveObject:BackSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:BackSysColor
             ENDIF
             IF cProp == "FORECOLOR" .AND. ( xValue == ::ActiveObject:ForeSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:ForeSysColor
             ENDIF
             IF cProp == "SELBACKCOLOR" .AND. ( xValue == ::ActiveObject:BackSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:BackSysColor
             ENDIF
             IF cProp == "SELFORECOLOR" .AND. ( xValue == ::ActiveObject:ForeSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:ForeSysColor
             ENDIF
             IF cProp == "HIGHLIGHTCOLOR" .AND. ( xValue == ::ActiveObject:HighlightSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:HighlightSysColor
             ENDIF
             IF cProp == "HIGHLIGHTTEXTCOLOR" .AND. ( xValue == ::ActiveObject:HighlightTextSysColor .OR. xValue == NIL )
                xValue := "System Default..."
                nColor := ::ActiveObject:HighlightTextSysColor
             ENDIF
             IF cProp == "TRANSPARENTCOLOR" .AND. ( xValue == NIL )
                xValue := "System Default..."
                nColor := -1
             ENDIF
          ENDIF
          IF VALTYPE( xValue ) != "C"
             xValue := "Custom..."
          ENDIF
          TRY
             IF nColor == NIL
                nColor := ::ActiveObject:&( STRTRAN( RIGHT(cProp,9), "Color", "SysColor" ))
                xValue := "System Default..."
             ENDIF
           CATCH
          END
       ENDIF

       aProp := GetProperCase( cProp )
       cProp := aProp[1]


       IF Empty( aProp[2] )
          aProp[2] := "General"
       ENDIF
       oItem := ::SearchString( aProp[2] )

       IF oItem == NIL
          oItem := TreeViewItem( Self )
          oItem:Caption    := aProp[2]
          oItem:InsertAfter:= TVI_SORT
          oItem:Create()
       ENDIF

       aCol := { TreeColItem( IIF( VALTYPE(xValue)=="O", "", xValue ), cType, , nColor, cProp, , ,/*aProp[3]*/ ) }

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
        ELSE
          DO CASE
             CASE cProp == "TargetType"
                  aCol[1]:Value    := ::ActiveObject:__TargetTypes
                  aCol[1]:ColType  := "TARGETTYPE"
                  aCol[1]:SetValue := ::ActiveObject:TargetType
                  xValue := NIL

             CASE cProp == "ShortcutKey"
                  aCol[1]:Value   := ::ActiveObject:ShortcutKey:GetShortcutText()
                  aCol[1]:ColType := ""

             CASE cProp == "ToolTip" .AND. VALTYPE( xValue ) == "O"
                  aCol[1]:Value := ""

             CASE cProp == "Structure"
                  aCol[1]:Value := "Edit Structure"
                  aCol[1]:ColType := "STRUCTEDIT"

             CASE RIGHT( cProp, 4 ) == "Font"
                  aCol[1]:Value := "Choose Font"
                  aCol[1]:ColType := "CHOOSEFONT"

             CASE cProp == "Functable"
                  IF EMPTY( ::ActiveObject:Host )
                     LOOP
                  ENDIF
                  aCol[1]:Value   := "(Collection)"
                  aCol[1]:ColType := "ARRAYS"

             CASE cProp == "Tables"
                  aCol[1]:Value   := "(Collection)"
                  aCol[1]:ColType := "ARRAYS"
                  aCol[1]:Action  := {|o| ::MessageBox( "Under Construction", "" ), o:Destroy() }

             CASE cProp == "Table"
                  aCol[1]:Value   := "DataSource Editor"
                  aCol[1]:ColType := "ARRAYS"
                  aCol[1]:Action  := {|o| TableEditor( o:Parent:ActiveObject ), o:Destroy() }

             CASE cProp == "Columns"
                  aCol[1]:Value   := "(Collection)"
                  aCol[1]:ColType := "ARRAYS"
                  aCol[1]:Action  := {|o| ColumnManager( @o:Parent:ActiveObject ), o:Destroy() }

             CASE cProp == "Images"
                  aCol[1]:Value   := "(Collection)"
                  aCol[1]:ColType := "ARRAYS"
                  aCol[1]:Action  := {|o| ImageManager( o:Parent:ActiveObject ), o:Destroy() }

             CASE cProp == "Editor"
                  aCol[1]:Value := "Edit Menu"
                  aCol[1]:ColType  := "MENUEDITOR"

             CASE cProp == "ServiceName"
                  aCol[1]:Value   := {}
                  aSubProp := ::System:Services
                  DEFAULT aSubProp TO {}
                  FOR n := 1 TO LEN( aSubProp )
                      AADD( aCol[1]:Value, aSubProp[n][1] )
                  NEXT
                  aCol[1]:ColType := "SERVICES"

             CASE cProp IN {"BindingSource","DataSource","ImageList","HotImageList","ImageListSmall","HeaderMenu","ButtonMenu","ContextMenu","Socket","SqlConnector"}
                  aCol[1]:Value := { "", { NIL } }
                  IF cProp IN {"HeaderMenu","ButtonMenu"}
                     xProp := cProp
                     cProp := "ContextMenu"
                  ENDIF
                  IF ::ActiveObject:Form:hWnd != ::Application:Project:Forms[1]:hWnd
                     FOR EACH Child IN ::Application:Project:Forms[1]:Components
                         IF Child:ComponentType IN cProp .AND. ( Child:lCreated .OR. Child:ComponentType == "DataSource" .OR. Child:ClsName == "ImageList"  )
                            AADD( aCol[1]:Value[2], Child )
                         ENDIF
                     NEXT
                  ENDIF

                  FOR EACH Child IN ::ActiveObject:Form:Components
                      IF Child:ComponentType IN cProp
                         IF ( Child:lCreated .OR. Child:ComponentType IN {"DataSource","BindingSource","SqlConnector"} .OR. Child:ClsName == "ImageList" ) 
                            AADD( aCol[1]:Value[2], Child )
                         ENDIF
                      ENDIF
                  NEXT
                  IF cProp == "ImageList"
                     AADD( aCol[1]:Value[2], ::System:ImageList:Cursors )
                     AADD( aCol[1]:Value[2], ::System:ImageList:Small )
                     AADD( aCol[1]:Value[2], ::System:ImageList:Large )
                     AADD( aCol[1]:Value[2], ::System:ImageList:StdSmall )
                  ENDIF

                  TRY
                     IF xValue != NIL .AND. xValue:Name != "CommonImageList" .AND. xValue:Name != "CommonImageListSmall"
                        aCol[1]:Value[1] := xValue:Name
                     ENDIF
                   CATCH
                  END

                  aCol[1]:ColType  := UPPER( STRTRAN( UPPER( cProp ), "HOT" ) )
                  aCol[1]:ColType  := UPPER( STRTRAN( aCol[1]:ColType, "SMALL" ) )
                  xValue := NIL
                  IF xProp != NIL
                     cProp := xProp
                     xProp := NIL
                  ENDIF

             CASE cProp == "PageChild"

                  aCol[1]:Value := { "", { NIL } }

                  FOR EACH Child IN ::ActiveObject:Parent:Children
                      IF Child:ClsName != REBARCLASSNAME
                         AADD( aCol[1]:Value[2], Child )
                      ENDIF
                  NEXT

                  IF xValue != NIL
                     aCol[1]:Value[1] := xValue:Name
                  ENDIF

                  aCol[1]:ColType  := "BANDCHILD"
                  xValue := NIL

             CASE cProp == "BandChild"

                  aCol[1]:Value := { "", { NIL } }

                  FOR EACH Child IN ::ActiveObject:Parent:Parent:Children
                      IF Child:ClsName != REBARCLASSNAME
                         AADD( aCol[1]:Value[2], Child )
                      ENDIF
                  NEXT

                  IF xValue != NIL
                     aCol[1]:Value[1] := xValue:Name
                  ENDIF

                  aCol[1]:ColType  := "BANDCHILD"
                  xValue := NIL

             CASE cProp == "Icon"
                  IF xValue != NIL
                     aCol[1]:Value := xValue 
                     xValue := NIL
                  ENDIF
                  aCol[1]:ColType  := "ICONS"

             CASE cProp == "ServerType"
                  aCol[1]:Value    := ::ActiveObject:__ServerTypes
                  aCol[1]:ColType  := "SERVERTYPE"
                  aCol[1]:SetValue := ::ActiveObject:ServerType
                  xValue := NIL

             CASE cProp == "BitmapMask"
                  IF xValue != NIL
                     aCol[1]:Value := xValue
                     xValue := NIL
                  ENDIF
                  aCol[1]:ColType  := "BITMAPMASK"

             CASE cProp == "ImageName"
                  IF xValue != NIL
                     aCol[1]:Value := xValue
                     xValue := NIL
                  ENDIF
                  aCol[1]:ColType  := "IMAGENAME"

             CASE cProp == "MaskType"
                  aCol[1]:ColType  := "MASKTYPE"


             CASE cProp == "ImageIndex"
                  aCol[1]:ColType  := "IMAGEINDEX"
                  xValue := NIL

             CASE cProp == "HeaderImageIndex"
                  aCol[1]:ColType  := "IMAGEINDEX"
                  xValue := NIL

             CASE cProp == "Representation"
                  aCol[1]:Value := ::ActiveObject:__Representation
                  aCol[1]:ColType  := "REPRESENTATION"
                  aCol[1]:SetValue := ::ActiveObject:Representation
                  xValue := NIL

             CASE cProp == "Alignment"
                  IF ::ActiveObject:__xCtrlName == "ListViewGroup"
                     aCol[1]:Value := ::System:ListViewGroupAlign:Keys
                     aCol[1]:ColType  := "LVGALIGNMENT"
                   ELSE
                     aCol[1]:Value := ::ActiveObject:__Alignments
                     aCol[1]:ColType  := "ALIGNMENT"
                     aCol[1]:SetValue := ::ActiveObject:Alignment
                  ENDIF
                  xValue := NIL

   //          CASE cProp == "ShortCut"
   //               aCol[1]:Value   := ""
   //               aCol[1]:ColType := "SHORTCUT"
   //               xValue := NIL

             CASE cProp == "ShowMode"
                  aCol[1]:Value := ::ActiveObject:__Show_Modes
                  aCol[1]:ColType  := "SHOWMODE"
                  xValue := NIL

             CASE cProp == "FrameStyle"
                  aCol[1]:Value := ::ActiveObject:__Frame_Styles
                  aCol[1]:ColType  := "FRAMESTYLES"
                  xValue := NIL

             CASE cProp == "Case"
                  aCol[1]:Value := ::ActiveObject:CaseTypes
                  aCol[1]:ColType  := "CASETYPES"
                  xValue := NIL

             //CASE cProp == "InitialState"
             //     aCol[1]:Value := ::ActiveObject:States
             //     aCol[1]:ColType  := "STATES"
             //     xValue := NIL

             CASE cProp == "Position" .AND. __ObjHasMsg( ::ActiveObject, "Page_Positions" )
                  aCol[1]:Value := ::ActiveObject:Page_Positions
                  aCol[1]:ColType  := "PAGE_POSITIONS"
                  xValue := NIL

//              CASE cProp == "ViewStyle"
//                   aCol[1]:Value := ::ActiveObject:View_Styles
//                   aCol[1]:ColType  := "VIEWSTYLES"
//                   xValue := NIL

             CASE cProp == "CheckStyle"
                  aCol[1]:Value := ::ActiveObject:Check_Styles
                  aCol[1]:ColType  := "CHECKSTYLES"
                  xValue := NIL

             CASE cProp == "OwnerDraw" .AND. __ObjHasMsg( ::ActiveObject, "OwnerDraw_Styles" )
                  aCol[1]:Value := ::ActiveObject:OwnerDraw_Styles
                  aCol[1]:ColType  := "DRAWSTYLES"
                  xValue := NIL

             CASE cProp == "BalloonTipIcon"
                  aCol[1]:Value := ::ActiveObject:Balloon_Icons
                  aCol[1]:ColType  := "BALLOONICONS"
                  xValue := NIL

             CASE cProp == "ImageType"
                  aCol[1]:Value := ::ActiveObject:Image_Type
                  aCol[1]:ColType  := "IMAGETYPE"
                  xValue := NIL

             CASE cProp == "Cursor"
                  aCol[1]:Value   := ::ActiveObject:__Cursors
                  aCol[1]:ColType := "CURSORS"
                  aCol[1]:Action  := {|o| BrowseFile(o) }

                  xValue := NIL

             CASE cProp == "DropDown" .AND. ::ActiveObject:ClsName != "ToolButton"
                  aCol[1]:Value   := ::ActiveObject:__DropDown
                  aCol[1]:ColType := "DROPDOWN"
                  aCol[1]:Action  := {|o, n, oPar| n := o:GetCurSel(),;
                                                     oPar := o:Parent,;
                                                     o:Destroy(),;
                                                     oPar:SetValue( n ) }
                  xValue := NIL

             CASE cProp == "Role"
                  aCol[1]:Value   := ::ActiveObject:__Roles
                  aCol[1]:ColType := "ROLE"
                  aCol[1]:Action  := {|o, n, oPar| n := o:GetCurSel(),;
                                                     oPar := o:Parent,;
                                                     o:Destroy(),;
                                                     oPar:SetValue( n ) }
                  xValue := NIL

             CASE cProp == "AnimationStyle"
                  aCol[1]:Value := ::System:WindowAnimation:Keys
                  aCol[1]:ColType  := "ANIMATIONSTYLE"
                  xValue := NIL

             CASE cProp == "Format" .AND. ::ActiveObject:__xCtrlName == "DateTimePicker"
                  aCol[1]:Value := ::System:DateTimeFormat:Keys
                  aCol[1]:ColType  := "DTSFORMATS"
                  xValue := NIL

             CASE cProp == "OleVerb"
                  aCol[1]:Value := ::System:OleVerb:Keys
                  aCol[1]:ColType  := "OLEVERB"
                  xValue := NIL

             CASE cProp == "HandShake"
                  aCol[1]:Value   := ::System:HandShake:Keys
                  aCol[1]:ColType := "HANDSHAKE"
                  aCol[1]:Action  := {|o, n, oPar| n := o:GetCurSel()-1,;
                                                     oPar := o:Parent,;
                                                     o:Destroy(),;
                                                     oPar:SetValue( HGetValueAt( ::System:HandShake, n+1 )  ) }
                  xValue := NIL

             CASE cProp == "Parity"
                  aCol[1]:Value   := ::System:Parity:Keys
                  aCol[1]:ColType := "PARITY"
                  aCol[1]:Action  := {|o, n, oPar| n := o:GetCurSel()-1,;
                                                     oPar := o:Parent,;
                                                     o:Destroy(),;
                                                     oPar:SetValue( HGetValueAt( ::System:Parity, n+1 )  ) }
                  xValue := NIL

             CASE cProp == "StopBits"
                  aCol[1]:Value   := ::System:StopBits:Keys
                  aCol[1]:ColType := "STOPBITS"
                  aCol[1]:Action  := {|o, n, oPar| n := o:GetCurSel()-1,;
                                                     oPar := o:Parent,;
                                                     o:Destroy(),;
                                                     oPar:SetValue( HGetValueAt( ::System:StopBits, n+1 )  ) }
                  xValue := NIL

             CASE cProp == "SysFolder"
                  aCol[1]:Value := ::System:Folders:Keys
                  aCol[1]:ColType  := "SYSFOLDERS"
                  xValue := NIL

             CASE cProp == "Driver" .AND. ::ActiveObject:__xCtrlName == "DataTable"
                  aCol[1]:Value := ::System:DataDrivers:Keys
                  aCol[1]:ColType  := "DATADRIVERS"
                  xValue := NIL

             CASE cProp == "Driver" .AND. ::ActiveObject:__xCtrlName == "AdsDataTable"
                  aCol[1]:Value := ::System:AdsDataDrivers:Keys
                  aCol[1]:ColType  := "ADSDATADRIVERS"
                  xValue := NIL

             CASE cProp == "ImageAlign"
                  aCol[1]:Value := ::System:TextAlignment:Keys
                  aCol[1]:ColType  := "TEXTALIGNMENT"
                  xValue := NIL

             CASE cProp == "DropDownStyle"
                  aCol[1]:Value := ::System:DropDownStyle:Keys
                  aCol[1]:ColType  := "CBDROPDOWN"
                  xValue := NIL

          ENDCASE
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
              IF cProp2 != "COLORSCHEME" .AND. AT( "COLOR", cProp2 ) > 0 .AND. VALTYPE( xValue2 ) != "L"
                 cType  := "COLORREF"
                 nColor := xValue2
                 IF ( n := ASCAN( ::Colors, {|a|a[1]==xValue2} ) ) > 0
                    xValue2 := ::Colors[n][2]
                 ENDIF
                 IF cProp2 == "BACKCOLOR" .AND. ( nColor == NIL .OR. n == 0 .OR. xValue:xBackColor == NIL )
                    xValue2 := "System Default..."
                 ENDIF
                 IF cProp2 == "FORECOLOR" .AND. ( nColor == NIL .OR. n == 0 .OR. xValue:xForeColor == NIL )
                    xValue2 := "System Default..."
                 ENDIF
                 IF VALTYPE( xValue2 ) != "C"
                    xValue2 := "Custom..."
                 ENDIF
                 IF nColor == NIL
                    nColor := xValue:&( STRTRAN( RIGHT(cProp2,9), "Color", "SysColor" ))
                    xValue := "System Default..."
                 ENDIF
              ENDIF

              cProp2  := GetProperCase( cProp2 )[1]
              aCol    := { TreeColItem( IIF( VALTYPE(xValue2)=="O", "", xValue2 ), cType, , nColor, cProp2, cProp ) }

              IF cProp == "Dock"
                 IF cProp2 $ "LeftTopRightBottom"
                    aCol[1]:Value := { "", { NIL } }
                    AADD( aCol[1]:Value[2], ::ActiveObject:Parent )
                    FOR EACH Child IN ::ActiveObject:Parent:Children
                        IF Child:hWnd != ::ActiveObject:hWnd .AND. Child:__xCtrlName != "DataTable" .AND. Child:__xCtrlName != "AdsDataTable" .AND. Child:__xCtrlName != "Splitter"
                           AADD( aCol[1]:Value[2], Child )
                        ENDIF
                    NEXT
                    TRY
                       IF xValue2 != NIL .AND. VALTYPE( xValue2 ) == "O"
                          aCol[1]:Value[1] := xValue2:Name
                       ENDIF
                    catch
                    END
                    aCol[1]:ColType  := "DOCKING"
                    xValue2 := NIL
                 ENDIF
               ELSEIF cProp2 == "AlignLeft" .OR. cProp2 == "AlignTop" .OR. cProp2 == "AlignRight" .OR. cProp2 == "AlignBottom"
                 aCol[1]:Value := { "", { NIL } }
                 FOR EACH Child IN ::ActiveObject:Children
                     IF Child:hWnd != ::ActiveObject:hWnd .AND. Child:__xCtrlName != "DataTable" .AND. Child:__xCtrlName != "AdsDataTable" .AND. Child:__xCtrlName != "Splitter"
                        AADD( aCol[1]:Value[2], Child )
                     ENDIF
                 NEXT
                 TRY
                    IF xValue2 != NIL .AND. VALTYPE( xValue2 ) == "O"
                       aCol[1]:Value[1] := xValue2:Name
                    ENDIF
                 catch
                 END
                 aCol[1]:ColType  := "MDICLIENTALIGNMENT"
                 xValue2 := NIL
              ENDIF
              IF cProp2 == "FaceName"
                 aCol[1]:ColType  := "FACENAME"
                 xValue2 := NIL
               ELSEIF cProp2 == "Key"
                 aCol[1]:ColType := "SHORTCUTKEY_KEY"
                 xValue2 := NIL
               ELSEIF cProp2 == "ImageName"
                 IF xValue2 != NIL
                    aCol[1]:Value := xValue2
                    xValue2 := NIL
                 ENDIF
                 aCol[1]:ColType  := "IMAGENAME"

               ELSEIF cProp2 == "Type" .AND. cProp == "Animation"
                  xValue2 := NIL
                  aCol[1]:Value := ::System:WindowAnimation:Keys
                  aCol[1]:ColType  := "ANIMATIONSTYLE"
              ENDIF
              IF cProp2 == "Alignment"
                 aCol[1]:Value    := xValue:__Alignments
                 aCol[1]:ColType  := "ALIGNMENT"
                 aCol[1]:SetValue := xValue:Alignment
                 xValue2 := NIL
              ENDIF
              oSub:AddItem( cProp2, 0, aCol )
          NEXT
       ENDIF
       oSub:SortChildren( .T. )
       IF aSubExpand != NIL .AND. ASCAN( aSubExpand, cProp ) > 0
          oSub:Expand()
       ENDIF
       oItem:SortChildren( .T. )
       oItem:Expand()
   NEXT

   TRY
      IF ::ActiveObject:ClsName == "AtlAxWin"
         oItem := TreeViewItem( Self )
         oItem:Caption    := "COM Properties"
         oItem:InsertAfter:= TVI_SORT
         oItem:Create()

         FOR EACH cProp IN ::ActiveObject:__OleVars:Keys
             xValue := ::ActiveObject:__OleVars[cProp][1]
             cType := VALTYPE( xValue )
             lReadOnly := ::ActiveObject:__OleVars[cProp][3]

             IF cType == "A"
                cType := "ENUMERATION"
              ELSEIF cType == "O"
                cType := "COLLECTION"
                //lReadOnly := .F.
             ENDIF

             aCol  := { TreeColItem( xValue, cType, , NIL, cProp,, lReadOnly ) }
             oSub := oItem:AddItem( cProp, 0, aCol )
         NEXT 
         oItem:SortChildren( .T. )
         oItem:Expand()
      ENDIF
    catch
   END
   oItem := ::SearchString( "Appearance" )

   IF oItem == NIL
      oItem := ::SearchString( "General" )
   ENDIF

   IF oItem != NIL
      oItem:EnsureVisible()
   ENDIF
   ::lPaint := .T.
   ::SetRedraw( .T. )
   ::Application:Yield()

   IF ::ActiveObject:TreeItem != NIL
      ::ActiveObject:TreeItem:Select()
    ELSE
      ::Application:ObjectTree:Set( ::ActiveObject )
   ENDIF

   hb_gcall()
RETURN NIL

//--------------------------------------------------------------------------------------------------------------------------------

METHOD GetEditBuffer( oItem, nCol ) CLASS ObjManager
   LOCAL cProp, cText := oItem:ColItems[nCol-1]:Value
   cProp := oItem:ColItems[1]:Prop
   IF cProp == "Caption" .AND. cText == NIL .AND. ::ActiveObject:ClsName == "ToolButton"
      cText := ""
   ENDIF
RETURN cText

//--------------------------------------------------------------------------------------------------------------------------------

METHOD OnUserMsg( hWnd, nMsg, nCol, nLeft ) CLASS ObjManager
   LOCAL oItem, rc, cType, n, cProp, cText, oFont, cFont
   ( hWnd )
   IF nMsg == WM_USER + 4767
      IF ( oItem := ::GetSelected() ) != NIL .AND. ::ActiveObject:HasMessage( "Dock" ) .AND. ::ActiveObject:Dock != NIL
         oItem:ColItems[1]:Value := ::ActiveObject:Dock:Margins
         ::InvalidateRect(,.F.)
      ENDIF
      
    ELSEIF nMsg == WM_USER + 4766
      IF ( oItem := ::GetSelected() ) != NIL
         IF ::ActiveObject:HasMessage( "BackgroundImage" ) .AND. VALTYPE( ::ActiveObject:BackgroundImage ) == "O"
            oItem:ColItems[1]:Value := ::ActiveObject:BackgroundImage:Margins
          ELSE
            oItem:ColItems[1]:Value := ::ActiveObject:Margins
         ENDIF
         ::InvalidateRect(,.F.)
      ENDIF

    ELSEIF nMsg == WM_USER + 4768
      oItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )

      IF oItem != NIL
         rc := (struct RECT)
         rc:left := oItem:hItem
         SendMessage( ::hWnd, TVM_GETITEMRECT, .F., @rc )

         nCol  := 2
         nLeft := ::Columns[1][1] + ( ABS( ::Font:Height ) + 3 )

         cProp := oItem:Caption
         IF cProp == "Height" .AND. ::ActiveObject:__xCtrlName == "Expando" .AND. !::ActiveObject:Expanded
            RETURN 0
         ENDIF
         ::CurCol := nCol
         _InvalidateRect( ::hWnd, oItem:GetItemRect():Array() ,.F.)

         IF ::CurCol > 1 .AND. oItem:ColItems != NIL .AND. LEN( oItem:ColItems ) >= nCol-1
            cType := oItem:ColItems[nCol-1]:ColType
            IF oItem:ColItems[nCol-1]:ReadOnly
               RETURN 0
            ENDIF
            DO CASE

               CASE cType == "SHORTCUTKEY_KEY"
                    ::ActiveControl := EditBox( Self )
                    WITH OBJECT ::ActiveControl
                       :Left        := nLeft + IIF( cType == "ICONS", 20, 0 )
                       :Top         := rc:top+1
                       :Width       := ::Columns[ nCol ][ 1 ]+2 - IIF( cType == "ICONS", 20, 0 )
                       :Height      := rc:bottom-rc:top-1
                       :ClientEdge  := .F.
                       :Style       := :Style | ES_AUTOHSCROLL | ES_MULTILINE & NOT( WS_BORDER )

                       GetRealKeyName(::ActiveObject:ShortcutKey:Key, @cText, 40)
                       
                       :Caption     := cText
                       :Cargo       := ::ActiveObject:ShortcutKey:Key

                       :OnWMKeyDown := {|o,n,l| CheckShortCutKeyDown(o,n,l,.T.)}
                       :OnWMChar    := {|| 0}
                       :OnWMKillFocus := {|o,cText,oPar| cText := IIF( o:Cargo == NIL, 0, o:Cargo ),;
                                                         oPar  := o:Parent,;
                                                         IIF( ::__xCtrlName == "ObjManager", oPar:SetValue( cText ), ),;
                                                         o:Destroy(), 0 }

                       :Create()
                       :SetFocus()
                    END
                    RETURN 0

               CASE cType == "D"
                    ::ActiveControl := DateTimePicker( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := rc:bottom-rc:top+1
                       :Border := .F.
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Create()
                       
                       :OnDTNDateTimeChange := {|o, d| d := o:Date,;
                                                     o:Parent:SetValue( d, o ) }
                                                     
                       :Action := {|o, d, oPar| d := o:Date,;
                                                     oPar := o:Parent,;
                                                     o:Destroy(),;
                                                     oPar:SetValue( d, o ) }
                       :Date := ::GetEditBuffer( oItem, nCol )
                       :SetFocus()
                       :PostMessage( WM_LBUTTONDOWN, 1, MAKELPARAM( :Width - 10, :Height / 2 ) )
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
                       IF oItem:ColItems[nCol-1]:Value
                          :SetCurSel(1)
                        ELSE
                          :SetCurSel(2)
                       ENDIF
                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetFocus()
                       :ShowDropDown()
                    END
               CASE cType == "MASKTYPE"
                    ::ActiveControl := ObjCombo( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 70
                       :Border := .F.

                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, c, oPar| c := o:GetSelString(), oPar := o:Parent, o:Destroy(), oPar:SetValue( c ) }
                       :Create()

                       FOR n := 1 TO LEN( ::ActiveObject:MaskTypes )
                           :AddItem( ::ActiveObject:MaskTypes[n] )
                       NEXT

                       IF ( n := :FindStringExact( ::ActiveObject:MaskType ) ) > 0
                          :SetCurSel(n)
                        ELSE
                          :SetCurSel(1)
                       ENDIF

                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetFocus()
                       :ShowDropDown()
                    END

               CASE cType == "ALIGNMENT" .OR. cType == "REPRESENTATION" .OR. cType == "SERVERTYPE"  .OR. cType == "TARGETTYPE"
                    ::ActiveControl := ObjCombo( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 120
                       :Border := .F.
                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, n, oPar| n := o:GetCurSel()-1, oPar := o:Parent, o:Destroy(), oPar:SetValue( n + 1 ), oItem:ColItems[nCol-1]:SetValue := n+1 }
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
                       :Cargo  := oItem:Owner:Caption
                       :Owner  := ::ActiveObject
                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, oPar, cSel| cSel := o:GetSelString(), oPar := o:Parent, o:Destroy(), oPar:SetValue( cSel ) }
                       :Create()

                       cFont := :Cargo

                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       //:SetItemHeight( 1, ABS( :Owner:&cFont:Height ) )

                       :SetFocus()
                       :ShowDropDown()
                    END

               CASE cType == "COLLECTION"
                    ObjectManager( oItem:ColItems[nCol-1]:Value )

               CASE cType == "ENUMERATION"
                    ::ActiveControl := ObjCombo( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 200
                       :Border := .F.

                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }

                       :Action := {|o, n, oPar| n := o:GetCurSel()-1, oPar := o:Parent, o:Destroy(), oPar:SetValue(n) }
                       :Create()

                       FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value )
                           :AddItem( oItem:ColItems[nCol-1]:Value[n]:Name )
                       NEXT
                       
                       :SetCurSel( 1 )

                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetFocus()
                       :ShowDropDown()
                    END

               CASE cType == "CBDROPDOWN" .OR.;
                    cType == "TEXTALIGNMENT" .OR.;
                    cType == "DTSFORMATS" .OR.;
                    cType == "DATADRIVERS" .OR.;
                    cType == "ADSDATADRIVERS" .OR.;
                    cType == "SHOWMODE" .OR.;
                    cType == "IMAGETYPE" .OR.;
                    cType == "SERVICES" .OR.;
                    cType == "STATES" .OR.;
                    /*cType == "VIEWSTYLES" .OR.*/;
                    cType == "DRAWSTYLES" .OR.;
                    cType == "CASETYPES" .OR.;
                    cType == "CHECKSTYLES" .OR.;
                    cType == "FRAMESTYLES" .OR.;
                    cType == "CURSORS" .OR.;
                    cType == "BALLOONICONS" .OR.;
                    cType == "LVGALIGNMENT" .OR.;
                    cType == "SYSFOLDERS" .OR.;
                    cType == "OLEVERB" .OR.;
                    cType == "ROLE" .OR.;
                    cType == "DROPDOWN" .OR.;
                    cType == "HANDSHAKE" .OR.;
                    cType == "PARITY" .OR.;
                    cType == "STOPBITS" .OR.;
                    cType == "ANIMATIONSTYLE" .OR.;
                    cType == "PAGE_POSITIONS" .OR. ( cType == "IMAGEINDEX" .AND. ::ActiveObject:Parent:ImageList != NIL )
                    ::ActiveControl := IIF( cType == "CURSORS", CursorComboBox( Self ), IIF( cType == "IMAGEINDEX", ComboBoxEx( Self ), ObjCombo( Self ) ) )
                    WITH OBJECT ::ActiveControl
                       :ClientEdge := .F.
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 200
                       :Border := .F.

                       IF cType != "IMAGEINDEX"
                          :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                          :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                        ELSE
                          :Height := 300
                       ENDIF
                       :Action := oItem:ColItems[nCol-1]:Action
                       IF cType == "ROLE" .OR. cType == "DROPDOWN"

                        ELSEIF cType == "OLEVERB"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:OleVerb, n+1 )  ) }

                        ELSEIF cType == "SYSFOLDERS"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:Folders, n+1 )  ) }

                        ELSEIF cType == "ADSDATADRIVERS"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:AdsDataDrivers, n+1 )  ) }

                        ELSEIF cType == "DATADRIVERS"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:DataDrivers, n+1 )  ) }

                        ELSEIF cType == "LVGALIGNMENT"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:ListViewGroupAlign, n+1 )  ) }
                        ELSEIF cType == "DTSFORMATS"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:DateTimeFormat, n+1 )  ) }
                        ELSEIF cType == "TEXTALIGNMENT"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:TextAlignment, n+1 )  ) }
                        ELSEIF cType == "CBDROPDOWN"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:DropDownStyle, n+1 )  ) }
                        ELSEIF cType == "ANIMATIONSTYLE"
                          :Action := {|o, n, oPar| n    := o:GetCurSel()-1,;
                                                   oPar := o:Parent,;
                                                   o:Destroy(),;
                                                   oPar:SetValue( HGetValueAt( ::System:WindowAnimation, n+1 )  ) }
                        ELSEIF cType == "SERVICES"
                          :Action := {|o, oPar, cSel| cSel := o:GetSelString(),;
                                                      oPar := o:Parent,;
                                                      o:Destroy(),;
                                                      oPar:SetValue( cSel ) }
                        ELSE
                          :Action := {|o, n, oPar| n := o:GetCurSel()-1, oPar := o:Parent, o:Destroy(), oPar:SetValue( n + IIF( cType == "PAGE_POSITIONS" .OR. cType == "STATES" .OR. /*cType == "VIEWSTYLES" .OR.*/ cType == "BALLOONICONS" .OR. cType == "IMAGEINDEX", 0, 1 ) ) }
                       ENDIF
                       :Create()

                       IF cType != "CURSORS" .AND. cType != "IMAGEINDEX"
                          FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value )
                              :AddItem( oItem:ColItems[nCol-1]:Value[n] )
                          NEXT
                        ELSEIF cType == "IMAGEINDEX"
                          :AddItem( "None", 0 )
                          FOR n := 1 TO ::ActiveObject:Parent:ImageList:Count
                              :AddItem( XSTR(n), n )
                          NEXT
                          :ImageList  := ::ActiveObject:Parent:ImageList
                       ENDIF
                       
                       IF cType == "SHOWMODE"
                          :SetCurSel( ::ActiveObject:ShowMode, 0 )
                        ELSEIF cType == "IMAGETYPE"
                          :SetCurSel( ::ActiveObject:ImageType )
//                        ELSEIF cType == "VIEWSTYLES"
//                          :SendMessage( CB_SETCURSEL, ::ActiveObject:ViewStyle )
                        ELSEIF cType == "CHECKSTYLES"
                          :SetCurSel( ::ActiveObject:CheckStyle )
                        ELSEIF cType == "DRAWSTYLES"
                          :SetCurSel( ::ActiveObject:OwnerDraw )
                        ELSEIF cType == "FRAMETYLES"
                          :SetCurSel( ::ActiveObject:FrameStyle )
                        ELSEIF cType == "CASETYPES"
                          :SetCurSel( ::ActiveObject:Case )
                        ELSEIF cType == "BALLOONICONS"
                          :SetCurSel( ::ActiveObject:BalloonTipIcon )
                        ELSEIF cType == "IMAGEINDEX"
                          :SendMessage( CB_SETCURSEL, MAX( ::ActiveObject:&cProp, 0 ) )
                        //ELSEIF cType == "STATES"
                        //  :SetCurSel( ::ActiveObject:InitialState )
                        ELSEIF cType == "PAGE_POSITIONS"
                          :SetCurSel( ::ActiveObject:Position )
                        ELSEIF cType == "CURSORS"
                          n := ASCAN( ::ActiveObject:__CursorValues, ::ActiveObject:Cursor )
                          :SetCurSel( n )
                        ELSEIF cType == "DATADRIVERS"
                          n := hScan( ::System:DataDrivers, ::ActiveObject:Driver )
                          :SetCurSel( n )
                        ELSEIF cType == "ADSDATADRIVERS"
                          n := hScan( ::System:AdsDataDrivers, ::ActiveObject:Driver )
                          :SetCurSel( n )
                        ELSEIF cType == "DTSFORMATS"
                          n := hScan( ::System:DateTimeFormat, ::ActiveObject:Format )
                          :SetCurSel( n )
                        ELSEIF cType == "ANIMATIONSTYLE"
                          n := hScan( ::System:WindowAnimation, ::ActiveObject:AnimationStyle )
                          :SetCurSel( n )
                        ELSEIF cType == "LVGALIGNMENT"
                          n := hScan( ::System:ListViewGroupAlign, ::ActiveObject:Alignment )
                          :SetCurSel( n )
                        ELSEIF cType == "TEXTALIGNMENT"
                          n := hScan( ::System:TextAlignment, ::ActiveObject:ImageAlign )
                          :SetCurSel( n )
                        ELSEIF cType == "CBDROPDOWN"
                          n := hScan( ::System:DropDownStyle, ::ActiveObject:DropDownStyle )
                          :SetCurSel( n )
                        ELSEIF cType == "SYSFOLDERS"
                          n := hScan( ::System:Folders, ::ActiveObject:SysFolder )
                          :SetCurSel( n )
                        ELSEIF cType == "ROLE"
                          :SetCurSel( ::ActiveObject:Role )
                        ELSEIF cType == "DROPDOWN"
                          :SetCurSel( ::ActiveObject:DropDown )
                        ELSEIF cType == "OLEVERB"
                          n := hScan( ::System:OleVerb, ::ActiveObject:OleVerb )
                          :SetCurSel( n )
                        ELSEIF cType == "HANDSHAKE"
                          n := hScan( ::System:HandShake, ::ActiveObject:HandShake )
                          :SetCurSel( n )
                        ELSEIF cType == "PARITY"
                          n := hScan( ::System:Parity, ::ActiveObject:Parity )
                          :SetCurSel( n )
                        ELSEIF cType == "STOPBITS"
                          n := hScan( ::System:StopBits, ::ActiveObject:StopBits )
                          :SetCurSel( n )
                        ELSEIF cType == "SERVICES"
                          n := aScan( oItem:ColItems[nCol-1]:Value, ::ActiveObject:ServiceName )
                          :SetCurSel( n )
                       ENDIF

                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetFocus()
                       :ShowDropDown()
                    END

               CASE cType == "DOCKING" .OR. cType == "MDICLIENTALIGNMENT"
                    ::ActiveControl := ObjCombo( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 70
                       :Border := .F.
                       :Height := 200
                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, n, oPar| n := o:GetCurSel()-1, oPar := o:Parent, o:Destroy(), oPar:SetValue( n + 1 ) }
                       :Create()
                       :AddItem( "" )
                       FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value[2] )
                           IF VALTYPE( oItem:ColItems[nCol-1]:Value[2][n] ) == "O"
                              IF oItem:ColItems[nCol-1]:Value[2][n]:hWnd == ::ActiveObject:Parent:hWnd
                                 :AddItem( "Parent" )
                               ELSE
                                 :AddItem( oItem:ColItems[nCol-1]:Value[2][n]:Name )
                              ENDIF
                           ENDIF
                       NEXT

                       IF ::ActiveObject:Parent != NIL .AND. oItem:ColItems[nCol-1]:Value[1] == ::ActiveObject:Parent:Name
                          n := :FindStringExact( "Parent" )
                        ELSE
                          n := :FindStringExact( oItem:ColItems[nCol-1]:Value[1] )
                       ENDIF
                       IF n > 0
                          :SetCurSel(n)
                        ELSE
                          :SetCurSel(1)
                       ENDIF
                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetFocus()
                       :ShowDropDown()
                    END

               CASE cType IN { "IMAGELIST","BANDCHILD","DATASOURCE","HEADERMENU","BUTTONMENU","CONTEXTMENU","SOCKET","BINDINGSOURCE","SQLCONNECTOR" }
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

                       FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value[2] )
                           IF oItem:ColItems[nCol-1]:Value[2][n] != NIL
                              :AddItem( oItem:ColItems[nCol-1]:Value[2][n]:Name )
                           ENDIF
                       NEXT

                       IF ( n := :FindStringExact( oItem:ColItems[nCol-1]:Value[1] ) ) > 0
                          :SetCurSel(n)
                        ELSE
                          :SetCurSel(1)
                       ENDIF
                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :ShowDropDown()
                       :SetFocus()
                    END

               CASE cType == "STRUCTEDIT" .OR. cType == "CHOOSEFONT"
                    ::ActiveControl := Button( Self )
                    WITH OBJECT ::ActiveControl
                       :Caption := oItem:ColItems[nCol-1]:Value
                       :Left    := nLeft-1
                       :Top     := rc:top
                       :Width   := ::Columns[ nCol ][ 1 ]+4
                       :Height  := rc:bottom-rc:top+1

                       :OnWMLButtonUp := {|o,x,y|CheckBtnClickPos(o,x,y) }
                       :OnWMKillFocus := {|o|o:Destroy() }
                       
                       oFont := ::ActiveObject:&cProp
                       IF cType == "CHOOSEFONT"

                          IF __clsParent( ::ActiveObject:ClassH, "COMPONENT" )
                             :Action := {|o, cf| cf := o:Parent:ActiveObject:Show(o),;
                                                       o:Destroy(),;
                                                       ::Application:Project:Modified := .T.}
                           ELSE
                             :Action := {|o, cf| cf := oFont:Choose(o,.T.),;
                                                       o:Parent:ChangeCtrlFont( cf, oItem, oFont ),;
                                                       o:Destroy(),;
                                                       ::Application:Project:Modified := .T.}
                          ENDIF
                        ELSE
                          :Action := {|o| StructEditor( o:Parent:ActiveObject ),;
                                          o:Parent:ResetProperties(,,.T.),;
                                          o:Destroy(),;
                                          ::Application:Project:Modified := .T.}
                       ENDIF
                       :SetStyle( BS_LEFT )
                       :Create()
                       :RemoveWindowTheme()
                       :SendMessage( WM_LBUTTONDOWN, 0, MAKELPARAM( 2, 2 ) )
                    END
                    RETURN 0

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

               CASE cType == "ARRAYS"
                    ::ActiveControl := Button( Self )
                    WITH OBJECT ::ActiveControl
                       :Caption := oItem:ColItems[nCol-1]:Value
                       :Left    := nLeft-1
                       :Top     := rc:top
                       :Width   := ::Columns[ nCol ][ 1 ]+4
                       :Height  := rc:bottom-rc:top+1
                       :OnWMLButtonUp := {|o,x,y|CheckBtnClickPos(o,x,y) }
                       :SetStyle( BS_LEFT )
                       :Theming := .F.
                       :Action  := oItem:ColItems[nCol-1]:Action

                       IF oItem:ColItems[nCol-1]:Value == "DataSource Editor"
                          IF EMPTY( ::ActiveObject:Structure )
                             MessageBox( 0, "DataSource must contain valid fields", "Empty DataSource Structure", MB_ICONSTOP )
                             ::ActiveControl := NIL
                             RETURN 0
                          ENDIF
                       ENDIF
                       :Create()
                       :SendMessage( WM_LBUTTONDOWN, 0, 0 )
                    END

               CASE cType == "MENUEDITOR"
                    IF oItem:ColItems[nCol-1]:Value == "Edit Menu"
                       ::ActiveControl := Button( Self )
                       WITH OBJECT ::ActiveControl
                          :Caption := "Edit Menu"
                          :Left    := nLeft-1
                          :Top     := rc:top
                          :Width   := ::Columns[ nCol ][ 1 ]+4
                          :Height  := rc:bottom-rc:top+1
                          :OnWMLButtonUp := {|o,x,y|CheckBtnClickPos(o,x,y) }
                          :Action := {|o| MenuEditor( ::Application:MainForm, ::ActiveObject ), o:Destroy()  }
                          :SetStyle( BS_LEFT )
                          :Create()
                          :RemoveWindowTheme()

                          :SendMessage( WM_LBUTTONDOWN, 0, 0 )
                       END
                    ENDIF

               CASE cType == "C" .OR. cType == "U" .OR. cType == "ICONS" .OR. cType == "IMAGENAME" .OR. cType == "BITMAPMASK"
                    ::ActiveControl := EditBox( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft + IIF( cType == "ICONS", 20, 0 )
                       :Top    := rc:top+1
                       :Width  := ::Columns[ nCol ][ 1 ]+2 - IIF( cType == "ICONS", 20, 0 )
                       :Height := rc:bottom-rc:top-1
                       :SetExStyle( WS_EX_CLIENTEDGE, .F. )
                       :Style := :Style | ES_AUTOHSCROLL | ES_MULTILINE & NOT( WS_BORDER )
                       
                       IF oItem:Caption == "Path" .OR. oItem:Caption == "SelectedPath" .OR. oItem:Caption == "DefaultPath" .OR. oItem:Caption == "Default" .OR. oItem:Caption == "IncludePath"  .OR. oItem:Caption == "SourcePath" .OR. oItem:Owner:Caption == "Target Folder"
                          :Button := .T.
                          :ButtonAction := {|o| BrowseForFolder(o, Self, oItem)}
                          
                        ELSEIF __ObjHasMsg( ::ActiveObject, "__ExplorerFilter" ) .OR. oItem:Caption == "FileName" .OR. oItem:Owner:Caption == "Font" .OR. oItem:Caption == "Icon" .OR. oItem:Caption == "ImageName"
                          IF oItem:Caption == "FileName" .OR. oItem:Caption == "ImageName" .OR. oItem:Caption == "Icon"
                             :Button := .T.
                             :ButtonAction := {|o| BrowseForFile( o, Self, IIF( oItem:Owner:Caption == "Font", ::ActiveObject:Font, ::ActiveObject ), oItem:Caption == "Icon" ) }
                          ENDIF
                        ELSEIF ::ActiveObject:ClassName == "WINDOWEDIT" .AND. oItem:Caption == "ImageName"
                          :Button := .T.
                          :ButtonAction := {|o| BrowseForFile( o, Self, ::ActiveObject:BackgroundImage, .F. ) }
                        ELSEIF cType == "C"
                          :Button := .T.
                          :ButtonAction := {|o| ::EditText( o ) }
                       ENDIF
                       IF cType == "BITMAPMASK"
                          :Button := .T.
                          :ButtonAction := {|o| BrowseForFile( o, Self, ::ActiveObject:BitmapMask, .F., { "Windows Bitmap (*.bmp)", "*.bmp" } ) }
                       ENDIF
                       
                       :Create()
                       :Caption := ::GetEditBuffer( oItem, nCol )
                       :Cargo   := :Caption
                       
                       IF !( oItem:Caption == "IncludePath"  .OR. oItem:Caption == "SourcePath" )
                          :OnWMKillFocus := {|o,cText,oPar| cText := o:Caption,;
                                                            oPar  := o:Parent,;
                                                            IIF( ::__xCtrlName == "ObjManager", oPar:SetValue( cText ), ),;
                                                            o:Destroy(), 0 }
                       ENDIF
                       :OnWMKeyDown   := {|o,n| CheckKeyDown(o,n)}

                       IF !::ReleaseEditSelection
                          :SetSel( 0, -1 )
                       ENDIF
                       :SetFocus()
                    END
                    RETURN 0

               CASE cType == "N"
                    ::ActiveControl := EditBox( Self )
                    WITH OBJECT ::ActiveControl
                       :Caption := ALLTRIM( STR( oItem:ColItems[nCol-1]:Value ) )
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
                                                                        IIF( ::__xCtrlName == "ObjManager", oPar:SetValue( VAL( cText ) ), ),;
                                                                        o:Destroy(), 0 }

                       :OnWMKeyDown  := {|o,n| CheckChar( o, n, oItem )}
                       :OnWMChar     := {|o,n| CheckChar( o, n, oItem )}
                       :OnWMPaste    := {|o,n| CheckCharPaste( o, n, oItem )}

                       :Create()
                       IF !::ReleaseEditSelection
                          :SetSel( 0, -1 )
                       ENDIF
                       :SetFocus()
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
                       
                       TRY
                          IF AT( "BackColor", oItem:Caption ) > 0
                             :SysDefault := ::ActiveObject:BackSysColor
                            ELSEIF UPPER( oItem:Caption )!= "MASKCOLOR"
                             :SysDefault := ::ActiveObject:ForeSysColor
                          ENDIF
                         catch
                       END
                       
                       IF oItem:ColItems[nCol-1]:Value == "Custom..."
                          :Custom := oItem:ColItems[nCol-1]:Color
                       ENDIF

                       :ColorSelected := oItem:ColItems[nCol-1]:Color

                       :Create( UPPER( oItem:Caption ) == "MASKCOLOR" )
                       :Action := {|o, c, oPar| c := o:ColorSelected, oPar := o:Parent, o:Destroy(), oPar:SetValue( c ) }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }

                       n := :FindStringExact( 0, oItem:ColItems[nCol-1]:Value )
                       :SetCurSel(n)

                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetItemHeight( 1, 14 )
                       :SetFocus()
                       :ShowDropDown()
                    END
            ENDCASE
          ELSE
            ::InvalidateRect(,.F.)
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

METHOD EditText( oEdit )
   LOCAL oForm   := Dialog( ::Application:MainForm )
   oForm:Caption := "Enter Text"
   oForm:ToolWindow := .T.
   oForm:Modal   := .T.
   oForm:Width   := 350
   oForm:height  := 220
   oForm:Center  := .T.
   oForm:OnWMInitDialog := {|o| InitEditText(o, Self) }
   oForm:Cargo   := oEdit
   oForm:Create()
RETURN NIL

FUNCTION InitEditText( oForm, oMan )
   LOCAL oCancel, oOK, oEdit
   WITH OBJECT oCancel := Button( oForm )
      :Caption     := "&Cancel"
      :Height      := 25
      :Dock:Right  := oForm
      :Dock:Bottom := oForm
      :Action      := {|| oForm:Close()}
      :Create()
   END
   WITH OBJECT oOK := Button( oForm )
      :Caption     := "&OK"
      :Height      := 25
      :Dock:Right  := oCancel
      :Dock:Bottom := oForm
      :Action      := {|| oMan:SetValue( oEdit:Caption ),;
                          oForm:Close()}
      :Create()
   END
   WITH OBJECT oEdit := EditBox( oForm )
      :Caption     := oForm:Cargo:Caption
      :Dock:Left   := oForm
      :Dock:Top    := oForm
      :Dock:Right  := oForm
      :Dock:Bottom := oCancel
      :Height      := 160
      :VertScroll  := .T.
      :HorzScroll  := .T.
      :WantReturn   := .T.
      :Multiline   := .T.
      :Create()
   END
RETURN NIL

STATIC FUNCTION CompileValue( cVal )
   IF !EMPTY( cVal )
      cVal := &cVal
   ENDIF
RETURN cVal

STATIC FUNCTION CheckBtnClickPos( o, x, y )
   IF x < 0 .OR. y < 0 .OR. x > o:ClientWidth .OR. y > o:ClientHeight
      o:Destroy()
   ENDIF
RETURN NIL

STATIC FUNCTION CheckShortCutKeyDown( o, n )
   LOCAL nScanCode, cText, lAsc, aSys := { VK_CONTROL, VK_SHIFT, VK_MENU, VK_CAPITAL, VK_SCROLL }
   IF ASCAN( aSys, n ) == 0
      
      nScanCode := MapVirtualKey( n, 0 )

      lAsc := (n <= 32) .OR. (n >= 112 .AND. n <= 135) .OR. n == VK_MULTIPLY
      
      IF (!lAsc .AND. n != VK_DIVIDE )
         lAsc := ToAscii( n, nScanCode, cText,, 1)
      ENDIF
      
      nScanCode := nScanCode << 16
      nScanCode := nScanCode | 0x1 << 25 // <- don't care
   
      IF !lAsc
         nScanCode := nScanCode | 0x1 << 24 // <- extended bit
      ENDIF

      GetKeyNameText( nScanCode, @cText, 40 )

      o:Caption := cText
      o:Cargo   := n
    
   ENDIF
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

STATIC FUNCTION CheckCharPaste()
RETURN 0


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
         IF oItem:Caption != "Contrast" .AND. oItem:Caption != "Brightness" .AND. CHR(n) == "-"
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
//------------------------------------------------------------------------------------------

METHOD OnMouseMove( nwParam, x, y ) CLASS ObjManager
   LOCAL oItem, rc, pt, aAlign, nfHeight, nPlus := 0
   static cText
   ( nwParam )
   IF ( oItem := ::HitTest( x, y ) ) != NIL
      TRY
         nfHeight := ABS( ::Font:Height )
         rc := oItem:GetItemRect()
         IF LEN( oItem:Items ) > 0
            nPlus := nfHeight + 5
         ENDIF
         IF oItem:Caption == "Margins"
            IF ::ToolTip:Text == "Left, Top, Right, Bottom"
               break
            ENDIF
            ::ToolTip:Text  := "Left, Top, Right, Bottom"

            pt := (struct POINT)
            pt:x := ::Columns[2][1] + 9
            pt:y := rc:top + 16
            ClientToScreen( ::hWnd, @pt )
            ::ToolTip:TrackPosition( pt:x, pt:y )
            ::ToolTip:TrackActivate()
         
          ELSEIF oItem:ColItems != NIL .AND. x >= ::Columns[1][1] + nfHeight + 3
            cText := oItem:ColItems[1]:Text
            aAlign := ::Drawing:GetTextExtentPoint32( cText )
            IF aAlign[1] + nPlus > ::Columns[2][1]
               ::ToolTip:Text  := cText
               pt := (struct POINT)
               pt:x := ::Columns[1][1] + ::Columns[2][1] - aAlign[1] + nfHeight
               pt:y := rc:top
               ClientToScreen( ::hWnd, @pt )
               ::ToolTip:TrackPosition( pt:x, pt:y )
               ::ToolTip:TrackActivate()
               //SetTimer( ::hWnd, 1, 5000 )
             ELSE
               ::ToolTip:Text  := NIL
            ENDIF

          ELSEIF oItem:ColItems != NIL .AND. x < ::Columns[1][1]+nfHeight+3
            cText := oItem:Caption
            aAlign := ::Drawing:GetTextExtentPoint32( cText )
            IF aAlign[1] > ::Columns[1][1] - nPlus 
               ::ToolTip:Text  := cText
               pt := (struct POINT)
               pt:x := nfHeight - ( aAlign[1] - ::Columns[1][1] ) - 3
               pt:y := rc:top
               ClientToScreen( ::hWnd, @pt )
               ::ToolTip:TrackPosition( pt:x, pt:y )
               ::ToolTip:TrackActivate()
               //SetTimer( ::hWnd, 1, 5000 )
             ELSE
               cText := NIL
               ::ToolTip:Text  := NIL
            ENDIF
            
          ELSE
            //SetTimer( ::hWnd, 1, 5000 )
         ENDIF
      CATCH
      END
    ELSE
      ::ToolTip:Text  := NIL
      cText := NIL
      RETURN 0
   ENDIF
RETURN NIL

METHOD OnLButtonDown(n,x,y) CLASS ObjManager
   LOCAL oItem, nCol:=0, z, nLeft
   ( n )
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

//------------------------------------------------------------------------------------------

METHOD OnKeyDown( nKey ) CLASS ObjManager
   LOCAL oItem
   DO CASE
      CASE nKey == VK_LEFT
           IF ::CurCol > 1
              ::CurCol--
              oItem := FindTreeItem( ::Items, TVGetSelected( ::handle ) )
              _InvalidateRect( ::hWnd, oItem:GetItemRect():Array() ,.F.)
              RETURN 0
           ENDIF

      CASE nKey == VK_RIGHT
           IF ::CurCol < LEN( ::Columns )
              ::CurCol++
              oItem := FindTreeItem( ::Items, TVGetSelected( ::handle ) )
              _InvalidateRect( ::hWnd, oItem:GetItemRect():Array(),.F.)
              RETURN 0
           ENDIF

   ENDCASE
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
   ::Value   := xValue
   ::Align   := nAlign
   ::ColType := cType
   ::Color   := nColor
   ::Prop    := cProp
   ::Prop2   := cProp2
   ::ReadOnly:= lReadOnly
   ::Help    := cHelp
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS EventManager INHERIT ObjManager
   DATA ReleaseEditSelection EXPORTED INIT .T.
   METHOD Init() CONSTRUCTOR
   METHOD SetValue()
   METHOD ResetEvents()
   METHOD GetEditBuffer()
   METHOD EditEvent()
   METHOD GenerateEvent()
   METHOD RenameEvent()
   METHOD OnLButtonDblClk()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS EventManager
   ::__xCtrlName  := "EvtManager"
   ::Super:Init( oParent )
   //::Caption    := "Events"
   ::SmallCaption := .F.
   ::Action     := NIL
RETURN Self

//------------------------------------------------------------------------------------------

METHOD SetValue( xValue ) CLASS EventManager
   LOCAL oItem, cProp, n, i

   oItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )

   IF oItem == NIL .OR. Empty( oItem:ColItems )
      RETURN NIL
   ENDIF

   IF !EMPTY( xValue ) .AND. !IsValidIdentifier( xValue )
      MessageBox( ::Application:MainForm:hWnd, "Event Name must be a valid identifier", "Visual xHarbour", MB_OK | MB_ICONEXCLAMATION | MB_APPLMODAL )
      ::SetFocus()
      RETURN NIL
   ENDIF

   cProp := oItem:ColItems[1]:Prop

   FOR n := 1 TO LEN( ::ActiveObject:Events )
       FOR i := 1 TO LEN( ::ActiveObject:Events[n][2] )
           IF ::ActiveObject:Events[n][2][i][1] == cProp
              oItem:ColItems[1]:Value := xValue

              ::Application:Project:Modified := .T.
              ::Application:Project:CurrentForm:__lModified := .T.
              IF Empty( xValue )
                 ::ActiveObject:Events[n][2][i][2] := ""
               ELSE
                 IF Empty( ::ActiveObject:Events[n][2][i][2] )
                    ::ActiveObject:Events[n][2][i][2] := xValue
                    ::GenerateEvent( oItem:Caption, xValue, ::ActiveObject:Events[n][2][i] )
                  ELSE
                    IF ::ActiveObject:Events[n][2][i][2] == xValue
                       ::GenerateEvent( oItem:Caption, xValue, ::ActiveObject:Events[n][2][i] )
                       RETURN Self
                    ENDIF
                    ::RenameEvent( oItem:Caption, ::ActiveObject:Events[n][2][i][2], xValue )
                    ::ActiveObject:Events[n][2][i][2] := xValue
                 ENDIF
              ENDIF
              RETURN Self
           ENDIF
       NEXT
   NEXT
RETURN .T.

//------------------------------------------------------------------------------------------

METHOD GenerateEvent( cEvent, cFuncName, Event ) CLASS EventManager
   LOCAL nPos, aPos, aFind, aEvents

   WITH OBJECT ::Application:Project:CurrentForm:Editor
      aFind := :Find( " "+cFuncName, FR_FROMTOP | FR_WHOLEWORD )
      nPos  := :Position( aFind[1], aFind[2] )
      IF nPos == 0
         :AddLine( { "//----------------------------------------------------------------------------------------------------//", NIL, NIL, 0 })
         :AddLine( { "METHOD " + cFuncName + "( Sender" + IIF( ! Empty( Event[3] ), ", " + Event[3] + " ", " " ) + ") CLASS "+::ActiveObject:Form:Name, NIL, NIL, 0 } )
         
         aEvents := { "ONCHAR", "ONKEYDOWN", "ONKEYUP", "ONSYSKEYDOWN", "ONSYSKEYUP", "ONCHILDCHAR", "ONCHILDKEYDOWN", "ONSYSCHAR" }

         IF ASCAN( aEvents, UPPER( cEvent ) ) > 0
            :AddLine( { "   LOCAL nKey := Sender:wParam", NIL, NIL, 0  } )
            :AddLine( { "", NIL, NIL, 0  } )
           ELSE
            :AddLine( { "   " + IIF( ::ActiveObject:__xCtrlName == "LinkLabel", "ShellExecute( ::hWnd, 'open', Sender:Url, , , SW_SHOW )", "" ), NIL , NIL, 0 } )
         ENDIF
         
         nPos := Len( :GetBuffer() )
         :AddLine( { "RETURN Self", NIL, NIL, 0 } )
         :lModified := .T.
         TRY
         IF ::Application:SourceEditor:oEditor == HB_QWith()
            :GoLine( :nLines - 1 )
            :GoColumn( 4 )
          ELSE
            :SetPosSilent( nPos )
         ENDIF
         CATCH
         END
       ELSE
         IF ::Application:SourceEditor:oEditor == HB_QWith()
            aPos := :LineColumn( nPos )
            :GoLine( aPos[1] )
            :GoColumn( aPos[2]+1 )
          ELSE
            :SetPosSilent( nPos )
         ENDIF
      ENDIF
      ::Application:Project:Modified := .T.
      ::Application:Project:CurrentForm:__lModified := .T.
   END

   IF !::Application:SourceEditor:IsWindowVisible()
      ::Application:EditorPage:Select()
   ENDIF
   ::Application:SourceEditor:SetFocus()
   InvalidateRect( ::Application:SourceEditor:hWnd,, .F. )
   
   ::Application:MainForm:ToolBox1:Enabled := .F.
   ::Application:ObjectTree:Enabled        := .F.
RETURN Self

//------------------------------------------------------------------------------------------

METHOD RenameEvent( cEvent, cFuncName, cNewFuncName, lSwitch ) CLASS EventManager
   LOCAL cNewBuffer, lComment, Line, lForce
   DEFAULT lSwitch TO .T.
   ( cEvent )
   IF !( cFuncName == cNewFuncName )
      WITH OBJECT ::ActiveObject:Form:Editor
         cNewBuffer := ""
         lComment   := .F.
         Line := :FirstLine
         WHILE Line != NIL
            cNewBuffer += ChangePrgLine( Line[ ED_BUFFER ], cFuncName, cNewFuncName, @lComment  ) + CHR(13) + CHR(10)
            IF Len( Line ) == 4
               Line := Line[ ED_NEXTLINE ]
             ELSE
               Line := Line[ ED_COLLAPSED_START ]
            ENDIF
         ENDDO
         lForce := ::Application:SourceEditor:oEditor == HB_QWith()
         :Load( ,cNewBuffer, lForce, lForce )
         :lModified := .T.
      END
      ::Application:Project:Modified := .T.
      IF lSwitch
         ::Application:EditorPage:Select()
         ::Application:SourceEditor:SetFocus()
      ENDIF
   ENDIF
RETURN Self

STATIC FUNCTION ChangePrgLine( cLine, cOldVal, cNewVal, lComment )
   LOCAL cPrev := "", cWord := "", lRest := .T., cChar, nFor, cNewLine := ""
   nFor := 1
   FOR EACH cChar IN cLine
       IF cChar == "/" .AND. cPrev == "/"
          lRest := .F.
       ENDIF
       IF cChar == "*" .AND. cPrev == "/"
          lComment := .T.
       ENDIF
       IF cChar == "/" .AND. cPrev == "*"
          lComment := .F.
       ENDIF
       cWord += cChar

       IF !lComment .AND. lRest .AND. UPPER( ALLTRIM( cWord ) ) == UPPER( ALLTRIM( cOldVal ) )
          cWord := cNewVal
       ENDIF

       IF cChar == ":" .AND. ( cLine[ nFor-1 ] == ":" .OR. cLine[ nFor-1 ] == " " )
          cNewLine += cWord
          cWord := ""
       ENDIF

       IF cChar  == " " .OR. cChar == "." /*.OR. cChar == ":"*/ .OR. cChar == '"' .OR. cChar == "(" .OR. cChar == ")" .OR. cChar == "=" .OR. cChar == "{" .OR. cChar == ","
          cNewLine += cWord
          cWord := ""
       ENDIF
       cPrev := cChar
       nFor++
   NEXT
   cNewLine += cWord
RETURN cNewLine

//------------------------------------------------------------------------------------------
METHOD ResetEvents( aSel ) CLASS EventManager

   LOCAL oItem, Event, Topic, oSub

   DEFAULT aSel TO { { ::ActiveObject,, } }

   IF EMPTY( aSel[1] ) .OR. aSel[1][1] == NIL
      ::ResetContent()
      RETURN NIL
   ENDIF

   ::ActiveObject := aSel[1][1]

   ::lPaint := .F.

   ::SetRedraw( .F. )
   //EnableScrollBar( ::hWnd, SB_VERT, ESB_DISABLE_BOTH )
   ::ResetContent()

   IF ::ActiveObject:Events != NIL
      ::Application:Yield()
      FOR EACH Topic IN ::ActiveObject:Events
          ::Application:Yield()
          oItem := ::AddItem( Topic[1] )
          FOR EACH Event IN Topic[2]
              oSub := oItem:AddItem( Event[1], 0, { TreeColItem( Event[2], "C", ,NIL , Event[1]) } )
          NEXT
          hb_gcall()
      NEXT
      ::ExpandAll()
      IF LEN( ::Items ) > 0
         ::Items[1]:EnsureVisible()
      ENDIF
   ENDIF
   //EnableScrollBar( ::hWnd, SB_VERT, ESB_ENABLE_BOTH )
   ::lPaint := .T.
   ::SetRedraw( .T. )

RETURN Self

//------------------------------------------------------------------------------------------

METHOD GetEditBuffer( oItem, nCol ) CLASS EventManager
   LOCAL cText := oItem:ColItems[nCol-1]:Value

   IF Empty( cText )
      IF ::ActiveObject:__xCtrlName != "Application"
         RETURN ::ActiveObject:Name + "_" + oItem:Caption
       ELSE
         cText := "App_"+oItem:Caption
      ENDIF
   ENDIF
RETURN cText

//------------------------------------------------------------------------------------------

METHOD OnLButtonDblClk() CLASS EventManager
   LOCAL oItem, nCol := 0

   oItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
   IF oItem == NIL .OR. Empty( oItem:ColItems )
      RETURN NIL
   ENDIF

   IF LEN( ::Children ) == 1
      ::Children[1]:Destroy()
   ENDIF

   ::SetValue( ::GetEditBuffer( oItem, 2 ) )
   IF ::Parent:Name == "Panel1"
      ::InvalidateRect()
   ENDIF
RETURN 0

//------------------------------------------------------------------------------------------

METHOD EditEvent( cEvent ) CLASS EventManager
   LOCAL n, i, lReset := .F., cName := ""

   IF ::ActiveObject:Events != NIL
      FOR n := 1 TO LEN( ::ActiveObject:Events )
          FOR i := 1 TO LEN( ::ActiveObject:Events[n][2] )
              IF ::ActiveObject:Events[n][2][i][1] == cEvent
                 cName := ::ActiveObject:Events[n][2][i][2]

                 IF Empty( cName )
                    lReset := .T.
                    cName := ::ActiveObject:Name + "_"+ cEvent
                    ::ActiveObject:Events[n][2][i][2] := cName
                 ENDIF

                 ::GenerateEvent( cEvent, cName, ::ActiveObject:Events[n][2][i] )

                 IF lReset
                    ::ResetEvents()
                 ENDIF

                 ::Parent:Select()
                 RETURN Self
              ENDIF
          NEXT
      NEXT
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------

CLASS ObjCombo INHERIT ComboBox
   DATA ColWidth
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

METHOD OnParentDrawItem() CLASS ObjCombo
   LOCAL n, x, lSelected, aRect, aClip, nLen, itemTxt, cText, nField, z, aAlign, y

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

//------------------------------------------------------------------------------------------
CLASS ObjectManager INHERIT Dialog
   DATA Object           EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oObject ) CLASS ObjectManager
   #ifdef DLL
      Application := GetApplication()
   #endif
   ::Object  := oObject
   DEFAULT ::__xCtrlName  TO "ObjectManager"

   ::Super:Init( ::Application:MainForm )

   ::Modal      := .T.
   ::Top        := 400
   ::Width      := 500
   ::Height     := 600
   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::DlgModalFrame := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS ObjectManager
   ::Caption    := "Object Manager"
   
   Image( Self )
   WITH OBJECT ::Image1
      :Height  := 77
      :BackColor     := C_WHITE
      :ImageName     := "Banner"
      :Dock:Margin   := 0
      :Dock:Left     := :Parent
      :Dock:Top      := :Parent
      :Dock:Right    := :Parent
      :Create()
      :DockIt()
   END
   
   StatusBar( Self )
   WITH OBJECT ::StatusBar1
      StatusBarPanel( :Parent:StatusBar1, , 120 )
      StatusBarPanel( :Parent:StatusBar1, ,  -1 )
      StatusBarPanel( :Parent:StatusBar1, , 250 )
      :Create()
      :DockIt()
   END

   ObjObjManager( Self )
   WITH OBJECT ::ObjManager1
      :Dock:Left     := :Parent
      :Dock:Top      := :Parent:Image1
      :Dock:Bottom   := :Parent:StatusBar1
      :Dock:Right    := :Parent

      :FullRowSelect := .T.

      :NoHScroll     := .T.
      :HasButtons    := .T.
      :LinesAtRoot   := .T.
      :ShowSelAlways := .T.

      :Columns := { {100,C_WHITE}, {200,C_WHITE} }
      :Create()

      :DockIt()
      :ResetProperties( {{ ::Object }} )
   END

   ::CenterWindow( .T. )
RETURN NIL


CLASS ObjObjManager INHERIT ObjManager
   METHOD GetPropertiesAndValues( oObj ) INLINE __clsGetIVarNamesAndValues( oObj )
ENDCLASS

//--------------------------------------------------------------------------------------------------------------------------------

FUNCTION GetProperCase( cProp )
   LOCAL n := ASCAN( __aProps[ UPPER( cProp[1] ) ], {|a| UPPER( a[1] ) == UPPER( cProp ) } )
   IF n > 0
      RETURN __aProps[ UPPER( cProp[1] ) ][n]
   ENDIF
RETURN { __Proper( cProp ), "" }

INIT PROCEDURE GetPropArray()
__aProps["A"] := { { "AutoHScroll",             "Style"     },;
                   { "AutoVScroll",             "Style"     },;
                   { "Alias",                   ""          },;
                   { "Animate",                 ""          },;
                   { "AutoRun",                 ""          },;
                   { "AlwaysOnTop",             "Behavior"  },;
                   { "AnchorColumn",            "Behavior"  },;
                   { "AllowDragRecords",        "Behavior"  },;
                   { "AllowCurrentPage",        "Behavior"  },;
                   { "AllowPrintFile",          "Behavior"  },;
                   { "AllowSelection",          "Behavior"  },;
                   { "AllowSomePages",          "Behavior"  },;
                   { "AnimationStyle",          "Behavior"  },;
                   { "AddExtension",            "Behavior"  },;
                   { "AllowMaximize",           "Behavior"  },;
                   { "AutoOpen",                "Behavior"  },;
                   { "AlignLeft",               "Behavior"  },;
                   { "AlignTop",                "Behavior"  },; 
                   { "AlignRight",              "Behavior"  },; 
                   { "AlignBottom",             "Behavior"  },;
                   { "AlignMask",               "Behavior"  },;
                   { "AlignTop",                "Behavior"   },;
                   { "AutoArrange",             "Behavior"   },;
                   { "AutoClose",               "Behavior"   },;
                   { "AutoColumns",             "Behavior"   },;
                   { "AutoHorzScroll",          "Behavior"   },;
                   { "AutoEditHorzScroll",      "Behavior"   },;
                   { "AutoVertScroll",          "Behavior"   },;
                   { "AcceptFiles",             "Behavior"   },;
                   { "AnyColor",                "Colors"     },;
                   { "ActiveLinkColor",         "Colors"     },;
                   { "AllowClose",              "Position"   },;
                   { "AllowUnDock",             "Position"   },;
                   { "AutoLeft",                "Position"   },;
                   { "AutoTop",                 "Position"   },;
                   { "AutoSize",                "Size"       },;
                   { "AdjustX",                 "Size"       },;
                   { "AdjustY",                 "Size"       },;
                   { "Anchor",                  "Size"       },;
                   { "AutoHeight",              "Size"       },;
                   { "AutoConnect",             "Connection" },;
                   { "AutoWidth",               "Size"       } }

__aProps["B"] := { { "BandChild",               ""           },;
                   { "BaudRate",                ""           },;
                   { "ButtonMenu",              "Behavior"   },;
                   { "ButtonText",              "Appearance" },;
                   { "BoldSelection",           "Appearance" },;
                   { "BalloonTipIcon",          "Appearance" },;
                   { "BalloonTipText",          "Appearance" },;
                   { "BalloonTipTitle",         "Appearance" },;
                   { "Border",                  "Appearance" },;
                   { "BackgroundImage",         "Appearance" },;
                   { "BitmapMask",              "Appearance" },;
                   { "BitmapMaskColor",         "Appearance" },;
                   { "BtnCheck",                "Behavior"   },;
                   { "BindingSource",           "Connection" },;
                   { "BlackAndWhite",           "Colors"     },;
                   { "BackColor",               "Colors"     },;
                   { "Binary",                  "Target Folder" },;
                   { "Band",                    "Position"   },;
                   { "BitmapWidth",             "Size"       },;
                   { "BitmapHeight",            "Size"       },;
                   { "Balloon",                 ""           },;
                   { "Bottom",                  ""           },;
                   { "BottomMargin",            ""           } }

__aProps["C"] := { { "ContextMenu",             "Behavior" },;
                   { "CheckPathExists",         "Behavior" },;
                   { "CheckFileExists",         "Behavior" },;
                   { "CaptionBar",              "Style" },;
                   { "ClientEdge",              "Style" },;
                   { "ClipChildren",            "Style" },;
                   { "ClipSiblings",            "Style" },;
                   { "ControlParent",           "Style" },;
                   { "Cargo",                   "" },;
                   { "CloseButton",             "" },;
                   { "CloseOnClick",            "" },;
                   { "ConnectionString",        "Connection" },;
                   { "ContextArrow",            "" },;
                   { "CompilerFlags",           "Build" },;
                   { "Caption",                 "Appearance" },;
                   { "CheckBoxes",              "Appearance" },;
                   { "Cursor",                  "Appearance" },;
                   { "CleanBuild",              "Build" },;
                   { "ConvertOem",              "Appearance" },;
                   { "CenterImage",             "Image" },;
                   { "ColorInactiveHeader",     "Appearance" },;
                   { "CustomFormat",            "Appearance" },;
                   { "CheckGroup",              "Behavior" },;
                   { "CreatePrompt",            "Behavior" },;
                   { "CheckStyle",              "Behavior" },;
                   { "CurSel",                  "Behavior" },;
                   { "Color",                   "Colors" },;
                   { "ColorScheme",             "Colors", } }
   
__aProps["D"] := { { "DisableNoScroll",         "Style" },;
                   { "DlgModalFrame",           "Style" },;
                   { "DrawArrows",              "Style" },;
                   { "DataSource",              "" },;
                   { "DataSearchField",         "" },;
                   { "DataSearchWidth",         "" },;
                   { "DataSearchRecords",       "" },;
                   { "Driver",                  "" },;
                   { "DiscardNull",             "" },;
                   { "DtrEnabled",              "" },;
                   { "DataBits",                "" },;
                   { "Definitions",             "Build" },;
                   { "DefaultExt",              "Behavior" },;
                   { "DeferenceLinks",          "Behavior" },;
                   { "DropDownStyle",           "Behavior" },;
                   { "DropDown",                "Behavior" },;
                   { "DisableParent",           "Behavior" },;
                   { "DefaultButton",           "Behavior" },;
                   { "DefaultPath",             "Path" },;
                   { "DebugInfo",               "Debugger" },;
                   { "Dock",                    "Position", } }

__aProps["E"] := { { "EditLabels",              "Style" },;
                   { "Escapement",              "" },;
                   { "EnterNext",               "Behavior" },;
                   { "Expanded",                "Behavior" },;
                   { "ExtVertScrollBar",        "Behavior" },;
                   { "ExtendedSel",             "Behavior" },;
                   { "Enabled",                 "Behavior", } }

__aProps["F"] := { { "FullRow",                 "Style" },;
                   { "FaceName",                "" },;
                   { "Filter",                  "" },;
                   { "FirstChild",              "" },;
                   { "FlatBorder",              "Appearance" },;
                   { "FlatCaption",             "Appearance" },;
                   { "Flat",                    "Appearance" },;
                   { "Font",                    "Appearance" },;
                   { "FrameStyle",              "Appearance" },;
                   { "FullRowSelect",           "Appearance" },;
                   { "Format",                  "Appearance" },;
                   { "FromPage",                "Behavior" },;
                   { "Filter",                  "Behavior" },;
                   { "FilterIndex",             "Behavior" },;
                   { "FullOpen",                "Behavior" },;
                   { "FullSelectOnClick",       "Behavior" },;
                   { "ForeColor",               "Colors" },;
                   { "Folder",                  "Path" },;
                   { "FileName",                "Path", } }

__aProps["G"] := { { "Grid",                    "Style" },;
                   { "GridColor",               "Colors" },;
                   { "GenerateMembers",         "Object" },;
                   { "GenerateMember",          "Object", } }

__aProps["H"] := { { "HasStrings",              "Style" },;
                   { "Hidden",                  "Style" },;
                   { "HideClippedButtons",      "Style" },;
                   { "HeaderMenu",              "Behavior" },;
                   { "HandShake",               "" },;
                   { "HideInvert",              "" },;
                   { "HTMLBody",                "" },;
                   { "HorzPadding",             "Appearance" },;
                   { "HeaderFont",              "Appearance" },;
                   { "HasButtons",              "Appearance" },;
                   { "HasLines",                "Appearance" },;
                   { "HotImageList",            "Appearance" },;
                   { "HighlightCaption",        "Appearance" },;
                   { "HorzScrollSize",          "Behavior" },;
                   { "HorzScroll",              "Behavior" },;
                   { "HighlightColor",          "Colors" },;
                   { "HighlightTextColor",      "Colors" },;
                   { "Height",                  "Size" },;
                   { "HeaderBackColor",         "Header" },;
                   { "HeaderForeColor",         "Header" },;
                   { "HeaderImageIndex",        "Header" },;
                   { "HeaderHeight",            "Header" } }

__aProps["I"] := { { "Icons",                   "Style" },;
                   { "Italic",                  "" },;
                   { "ImageType",               "Image" },;
                   { "ImageName",               "Image" },;
                   { "IconWidth",               "Size" },;
                   { "IconHeight",              "Size" },;
                   { "ImageListSmall",          "Appearance" },;
                   { "Icon",                    "Appearance" },;
                   { "ImageIndex",              "Appearance" },;
                   { "ImageList",               "Appearance" },;
                   { "InitialState",            "Appearance" },;
                   { "ItemToolTips",            "Appearance" },;
                   { "InitialDirectory",        "Behavior" },;
                   { "Id",                      "Behavior" },;
                   { "InvertedColors",          "Colors" },;
                   { "ImageAlign",              "Layout" },;
                   { "ImageAlignment",          "Layout" },;
                   { "IncludePath",             "Path" },;
                   { "IntegralHeight",          "Size" },;
                   { "ItemHeight",              "Size" } }

__aProps["J"] := {}
   
__aProps["K"] := { { "KeepAspectRatio",         "Image" } }

__aProps["L"] := { { "List",                    "Style" },;
                   { "LowerCase",               "Style" },;
                   { "LeftMargin",              "" },;
                   { "LoadFromFile",            "Image" },;
                   { "LinesAtRoot",             "Appearance" },;
                   { "LocalPort",               "Connection" },;
                   { "LinkColor",               "Colors" },;
                   { "Left",                    "Position" } }

__aProps["M"] := { { "MaximizeBox",             "Style" },;
                   { "MDIChild",                "Style" },;
                   { "MDIClient",               "Style" },;
                   { "MDIContainer",            "Style" },;
                   { "MinimizeBox",             "Style" },;
                   { "MultiLine",               "Style" },;
                   { "MimeFormatted",           "" },;
                   { "MixedButtons",            "Style" },;
                   { "MaskType",                "" },;
                   { "MenuArrow",               "Appearance" },;
                   { "Message",                 "Appearance" },;
                   { "MultiColumn",             "Appearance" },;
                   { "MultiSelect",             "Behavior" },;
                   { "MaskColor",               "Colors" },;
                   { "MaxRange",                "Position" },;
                   { "MinRange",                "Position" },;
                   { "MaxHeight",               "Size" },;
                   { "MaxWidth",                "Size" },;
                   { "MinHeight",               "Size" },;
                   { "MinWidth",                "Size" } }

__aProps["N"] := { { "NoHideSel",               "Style" },;
                   { "NoLabelWrap",             "Style" },;
                   { "NoRedraw",                "Style" },;
                   { "NoScroll",                "Style" },;
                   { "NoSortHeader",            "Style" },;
                   { "Notify",                  "Style" },;
                   { "NoActivate",              "Style" },;
                   { "NoColumnHeader",          "Appearance" },;
                   { "NoHScroll",               "Behavior" },;
                   { "NoToolTips",              "Behavior" },;
                   { "Name",                    "Object" } }

__aProps["O"] := { { "OemConvert",              "Style" },;
                   { "OwnerData",               "Style" },;
                   { "OwnerDraw",               "Style" },;
                   { "OwnerDrawFixed",          "Style" },;
                   { "Orientation",             "" },;
                   { "OleVerb",                 "ActiveX" },;
                   { "OfficeXPLook",            "Appearance" },;
                   { "OwnerDrawFixed",          "Behavior" },;
                   { "OwnerDrawVariable",       "Behavior" },;
                   { "Objects",                 "Target Folder" } }

__aProps["P"] := { { "PopUp",                   "Style" },;
                   { "PageChild",               "" },;
                   { "PortName",                "" },;
                   { "Parity",                  "" },;
                   { "ParityReplace",           "" },;
                   { "PaperSize",               "Appearance" },;
                   { "PreventFullOpen",         "Behavior" },;
                   { "ProportionalLeft",        "Dock" },;
                   { "ProportionalTop",         "Dock" },;
                   { "Path",                    "Path" },;
                   { "Position",                "Position" } }

__aProps["Q"] := {} 
   
__aProps["R"] := { { "RadioCheck",              "Style" },;
                   { "ReadOnly",                "Style" },;
                   { "Report",                  "Style" },;
                   { "ReadOnly",                "" },;
                   { "Right",                   "" },;
                   { "RightMargin",             "" },;
                   { "ReadBufferSize",          "" },;
                   { "ReadTimeOut",             "" },;
                   { "ReceivedBytesThreshold",  "" },;
                   { "RtsEnabled",              "" },;
                   { "RestoreDirectory",        "Behavior" },;
                   { "ReadOnlyChecked",         "Behavior" },;
                   { "RemoteIP",                "Connection" },;
                   { "RemotePort",              "Connection" },;
                   { "Resource",                "Target Folder" },;
                   { "Resizable",               "Size" } }

__aProps["S"] := { { "SetAlternate",            "Set" },;
                   { "SetCentury",              "Set" },;
                   { "SetCancel",               "Set" },;
                   { "SetDeleted",              "Set" },;
                   { "SetDefault",              "Set" },;
                   { "SetDecimals",             "Set" },;
                   { "SetDateFormat",           "Set" },;
                   { "SetEpoch",                "Set" },;
                   { "SetExact",                "Set" },;
                   { "SetExclusive",            "Set" },;
                   { "SetFixed",                "Set" },;
                   { "SetPath",                 "Set" },;
                   { "SetSoftseek",             "Set" },;
                   { "SetUnique",               "Set" },;
                   { "SetDebug",                "Set" },;
                   { "SetTypeahead",            "Set" },;
                   { "SetCursor",               "Set" },;
                   { "SetConsole",              "Set" },;
                   { "SetAltfile",              "Set" },;
                   { "SetDevice",               "Set" },;
                   { "SetExtra",                "Set" },;
                   { "SetExtrafile",            "Set" },;
                   { "SetPrinter",              "Set" },;
                   { "SetPrintfile",            "Set" },;
                   { "SetMargin",               "Set" },;
                   { "SetBell",                 "Set" },;
                   { "SetConfirm",              "Set" },;
                   { "SetEscape",               "Set" },;
                   { "SetInsert",               "Set" },;
                   { "SetExit",                 "Set" },;
                   { "SetIntensity",            "Set" },;
                   { "SetScoreboard",           "Set" },;
                   { "SetDelimiters",           "Set" },;
                   { "SetDelimchars",           "Set" },;
                   { "SetWrap",                 "Set" },;
                   { "SetMessage",              "Set" },;
                   { "SetMcenter",              "Set" },;
                   { "SetScrollbreak",          "Set" },;
                   { "SetEventmask",            "Set" },;
                   { "SetVideomode",            "Set" },;
                   { "SetMblocksize",           "Set" },;
                   { "SetMfileext",             "Set" },;
                   { "SetStrictread",           "Set" },;
                   { "SetOptimize",             "Set" },;
                   { "SetAutopen",              "Set" },;
                   { "SetAutorder",             "Set" },;
                   { "SetAutoshare",            "Set" },;
                   { "SetCount",                "Set" },;
                   { "SetLanguage",             "Set" },;
                   { "SetIdlerepeat",           "Set" },;
                   { "SetTrace",                "Set" },;
                   { "SetTracefile",            "Set" },;
                   { "SetTracestack",           "Set" },;
                   { "SetFilecase",             "Set" },;
                   { "SetDircase",              "Set" },;
                   { "SetDirseparator",         "Set" },;
                   { "SetErrorloop",            "Set" },;
                   { "SetOutputsafety",         "Set" },;
                   { "SetDbflockscheme",        "Set" },;
                   { "SetBackgroundtasks",      "Set" },;
                   { "SetTrimfilename",         "Set" },;
                   { "SetGtmode",               "Set" },;
                   { "SetBackgroundtick",       "Set" },;
                   { "SetPrinterjob",           "Set" },;
                   { "SetHardcommit",           "Set" },;
                   { "SetForceopt",             "Set" },;
                   { "SetEol",                  "Set" },;
                   { "SetErrorlog",             "Set" },;
                   { "SmallIcon",               "Style" },;
                   { "Sort",                    "Style" },;
                   { "StaticEdge",              "Style" },;
                   { "SysMenu",                 "Style" },;
                   { "Shared",                  "" },;
                   { "StrikeOut",               "" },;
                   { "SystemCursor",            "" },;
                   { "ServerType",              "" },;
                   { "ServiceName",             "" },;
                   { "StopBits",                "" },;
                   { "SendUsing",               "" },;
                   { "SMTPServer",              "" },;
                   { "SMTPServerPort",          "" },;
                   { "SMTPAuthenticate",        "" },;
                   { "SMTPUseSSL",              "" },;
                   { "SendUserName",            "" },;
                   { "SendPassword",            "" },;
                   { "Stretch",                 "Image" },;
                   { "SelOnlyRep",              "Behavior" },;
                   { "ShowGrid",                "Appearance" },;
                   { "ShortCutText",            "Appearance" },;
                   { "SmallCaption",            "Appearance" },;
                   { "Smooth",                  "Appearance" },;
                   { "ShadowRow",               "Appearance" },;
                   { "ShowChevron",             "Appearance" },;
                   { "ShowHeaders",             "Appearance" },;
                   { "ShowTabs",                "Appearance" },;
                   { "ShowSelection",           "Appearance" },;
                   { "ShowSelectionBorder",     "Appearance" },;
                   { "Striping",                "Appearance" },;
                   { "ShowInTaskBar",           "Behavior" },;
                   { "SystemAnimation",         "Behavior" },;
                   { "ShowPlacesBar",           "Behavior" },;
                   { "ShortcutKey",             "Behavior" },;
                   { "ShowHelp",                "Behavior" },;
                   { "ShowNewFolderButton",     "Behavior" },;
                   { "ShowDragging",            "Behavior" },;
                   { "ShowMode",                "Behavior" },;
                   { "ShowSelAlways",           "Behavior" },;
                   { "ShortCut",                "Behavior" },;
                   { "ShowHelp",                "Behavior" },;
                   { "ShowReadOnly",            "Behavior" },;
                   { "SqlConnector",            "Connection" },;
                   { "SolidColor",              "Colors" },;
                   { "SelBackColor",            "Colors" },;
                   { "SelForeColor",            "Colors" },;
                   { "SysFolder",               "Path" },;
                   { "SelectedPath",            "Path" },;
                   { "SourcePath",              "Path" },;
                   { "Source",                  "Target Folder" },;
                   { "Source",                  "Target Folder" },;
                   { "Step",                    "Position" },;
                   { "State",                   "Behavior" },;
                   { "SelectionHeight",         "Size" } }
   
__aProps["T"] := { { "TabStop",                 "Style" },;
                   { "ThickFrame",              "Style" },;
                   { "Transparent",             "Style" },;
                   { "ToolWindow",              "Style" },;
                   { "TopMost",                 "Style" },;
                   { "Text",                    "" },;
                   { "Title",                   "" },;
                   { "TopMargin",               "" },;
                   { "TextBody",                "" },;
                   { "TargetName",              "Build" },;
                   { "ThemeActive",             "Build" },;
                   { "TargetType",              "Build" },;
                   { "Theming",                 "Appearance" },;
                   { "ToolTip",                 "Appearance" },;
                   { "ToolTipText",             "Appearance" },;
                   { "ToolTipTitle",            "Appearance" },;
                   { "TabStripStyle",           "Appearance" },;
                   { "ToPage",                  "Behavior" },;
                   { "Title",                   "Behavior" },;
                   { "TransparentColor",        "Colors" },;
                   { "TitleBackColor",          "Colors" },;
                   { "TrailingTextColor",       "Colors" },;
                   { "TitleForeColor",          "Colors" },;
                   { "Top",                     "Position" },;
                   { "TabPosition",             "Position" } }

__aProps["U"] := { { "UpperCase",               "Style" },;
                   { "UseTabStops",             "Style" },;
                   { "Underline",               "" },;
                   { "UseDLL",                  "Build" },;
                   { "UserVariables",           "Object" } }

__aProps["V"] := { { "Vertical",                "Style" },;
                   { "Visible",                 "Style" },;
                   { "VertPadding",             "Appearance" },;
                   { "VertScrollSize",          "Behavior" },;
                   { "VertScroll",              "Behavior" },;
                   { "VisitedColor",            "Colors" },;
                   { "ViewStyle",               "Layout" },;
                   { "Vertical",                "Layout" },;
                   { "VerticalGripper",         "Layout" } }

__aProps["W"] := { { "WantKeyboardInput",       "Style" },;
                   { "WantReturn",              "Style" },;
                   { "Weight",                  "" },;
                   { "WindowMenu",              "" },;
                   { "WriteBufferSize",         "" },;
                   { "WriteTimeOut",            "" },;
                   { "WholeDropDown",           "Appearance" },;
                   { "WindowsMenu",             "Appearance" },;
                   { "Wrap",                    "Behavior" },;
                   { "Width",                   "Size" } }

__aProps["X"] := {}
__aProps["Y"] := {}
__aProps["Z"] := {}
   
   //aSort( aSet,,,{|x, y| x[1] < y[1]})   
RETURN

STATIC FUNCTION BrowseFile( o )
   LOCAL n := o:GetCurSel()-1
   o:Parent:SetValue( n + 1 )
   o:Destroy()
RETURN NIL

STATIC FUNCTION BrowseForFile( oEdit, oMan, oObj, lIcon, aFilter )
   LOCAL oFile := CFile( oEdit:Caption )
   oEdit:OnWMKillFocus := NIL
   
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

STATIC FUNCTION BrowseForFolderCallBack( hWnd, nMsg, lp )
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
