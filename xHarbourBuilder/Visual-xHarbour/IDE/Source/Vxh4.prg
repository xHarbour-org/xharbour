/*
 * $Id$
 */


static cCurFolder

static __aProps := {=>}

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"
#include "scintilla.ch"

#define DG_ADDCONTROL      1
#define DG_PROPERTYCHANGED 3
#define DG_FONTCHANGED     5
#define DG_DELCOMPONENT    6

#define PLUSFRAME 9

#define MAPVK_VK_TO_VSC    0
#define MAPVK_VSC_TO_VK    1
#define MAPVK_VK_TO_CHAR   2
#define MAPVK_VSC_TO_VK_EX 3

#define BP_CHECKBOX        3
#define CBS_UNCHECKEDNORMAL      1
#define CBS_UNCHECKEDHOT         2
#define CBS_UNCHECKEDPRESSED     3
#define CBS_UNCHECKEDDISABLED    4
#define CBS_CHECKEDNORMAL        5
#define CBS_CHECKEDHOT           6
#define CBS_CHECKEDPRESSED       7
#define CBS_CHECKEDDISABLED      8
#define CBS_MIXEDNORMAL          9
#define CBS_MIXEDHOT            10
#define CBS_MIXEDPRESSED        11
#define CBS_MIXEDDISABLED       12

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
   DATA cSubPropList  EXPORTED INIT ""
   DATA cEventList    EXPORTED INIT ""
   DATA hChkTheme     EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD ResetProperties()
   METHOD CheckObjProp()
   METHOD Create()
   METHOD OnHScroll( n)        INLINE ::nScroll := n
   METHOD OnDestroy()          INLINE ::Super:OnDestroy(), ::LevelFont:Delete(), CloseThemeData( ::hChkTheme ), NIL
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
   METHOD GetColorValues()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD OnGetDlgCode() CLASS ObjManager
   IF ::wParam == VK_RETURN
      ::PostMessage( WM_USER+4768 )
      RETURN 0
   ENDIF
   IF ::wParam == VK_SPACE
      RETURN 0
   ENDIF
RETURN DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS


METHOD Init( oParent ) CLASS ObjManager
   LOCAL cColor
   ::ReleaseEditSelection := .F.

   DEFAULT ::__xCtrlName  TO "ObjManager"
   ::Super:Init( oParent )
   ::Border := 0
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
   ::LevelFont := Font( NIL )
   ::LevelFont:Weight     := 700
   ::LevelFont:Create()
   ::SetIndent( 15 )
   ::SetItemHeight( ABS( ::Font:Height ) + 6 )
   ::Action := {|o| ::SetPropDesc(o) }
   ::hChkTheme := OpenThemeData(,"button")
RETURN Self

//---------------------------------------------------------------------------------------------------
METHOD OnSize( nwParam, nlParam ) CLASS ObjManager
   Super:OnSize( nwParam, nlParam )
   ::Columns[1][1] := Int(::ClientWidth/2)-11
   ::Columns[2][1] := Int(::ClientWidth/2)-7
   ::InvalidateRect()
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD OnEraseBkGnd( hDC ) CLASS ObjManager
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
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS ObjManager
   LOCAL tvcd
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
RETURN Super:OnParentNotify( nwParam, nlParam, hdr )

METHOD DrawItem( tvcd ) CLASS ObjManager
   LOCAL n, nState, nBack, nFore, lExpanded, rc, nWidth, nLeft, nRight, nBottom, aAlign, x, y, lHasChildren
   LOCAL aRow, aCol, hOldPen, nAlign, cType, nColor, hOld, hBrush, aRest, cText, nfHeight := ABS( ::Font:Height )+3
   LOCAL nPos, cCap, cProp, nLevel, hDC, oItem, aRect, lVal
   LOCAL hIcon, lDisabled := .F.
   LOCAL lEnabled, hBoldFont, oImageList

   rc       := tvcd:nmcd:rc
   oItem    := FindTreeItem( ::Items, tvcd:nmcd:dwItemSpec )
   IF oItem == NIL .OR. rc:Right == 0 .OR. !IsWindowVisible( ::hWnd )
      RETURN NIL
   ENDIF
   hDC      := tvcd:nmcd:hdc
   nLevel   := tvcd:iLevel
   lEnabled := IsWindowEnabled( ::hWnd )
   nBack := IIF( lEnabled, ::Columns[1][2], ::System:CurrentScheme:ToolStripPanelGradientEnd )
   nFore := IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) )

   nState := ::GetItemState( tvcd:nmcd:dwItemSpec, TVIF_STATE )

   IF nState & TVIS_SELECTED == TVIS_SELECTED .AND. ::CurCol == 1 .AND. ::HasFocus
      nBack := ::System:CurrentScheme:ButtonSelectedGradientEnd
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
   IF cText == "Height" .AND. ::ActiveObject != NIL .AND. ::ActiveObject:__xCtrlName == "Expando" .AND. !::ActiveObject:Expanded
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
      _FillRect( hDC, { rc:left, rc:top, nLeft, rc:bottom }, ::System:CurrentScheme:Brush:ToolStripPanelGradientEnd )
      _FillRect( hDC, { nRight+::Columns[2][1], rc:top, rc:right, rc:bottom }, ::System:CurrentScheme:Brush:ToolStripPanelGradientEnd )

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

   IF ::ActiveObject != NIL .AND. VALTYPE( oItem:ColItems ) == "A"
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

          IF oItem:ColItems[n]:ColType == "OLEVERB"
             IF ( nPos := hScan( ::System:OleVerb, ::ActiveObject:OleVerb ) ) > 0 .AND. LEN( cText ) >= nPos
                cText := cText[ nPos ]
             ENDIF
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
                  lVal := cText
                  cText  := ""
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
          nBack := IIF( lEnabled, ::Columns[n+1][2], ::System:CurrentScheme:ToolStripPanelGradientEnd )
          nFore := IIF( lEnabled, C_BLACK, GetSysColor( COLOR_BTNSHADOW ) )
          IF nState & TVIS_SELECTED != 0 .AND. ::CurCol == n+1 .AND. ::HasFocus
             nBack := ::System:CurrentScheme:ButtonSelectedGradientEnd
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

          IF ASCAN( m->aChangedProps, {|a| a[1] == ::ActiveObject .AND. a[2] == oItem:ColItems[n]:Prop } ) > 0
             SelectObject( hDC, hBoldFont )
          ENDIF

          _ExtTextOut( hDC, x, y, ETO_CLIPPED+ETO_OPAQUE, { nLeft, rc:top, nRight, rc:bottom }, cText  )

          IF cType == "L"
             aRect := { nLeft, rc:top, nLeft+20, rc:top+20 }
             DrawThemeBackground( ::hChkTheme, hDC, BP_CHECKBOX, IIF( lVal, CBS_CHECKEDNORMAL, CBS_UNCHECKEDNORMAL ), aRect, aRect )
          //   RETURN NIL
          ENDIF

          SelectObject( hDC, ::Font:Handle )

          IF oItem:ColItems[n]:ColType == "ICONS"
             IF ::ActiveObject:__hIcon == NIL .AND. VALTYPE( ::ActiveObject:Icon ) == "C"
                ::ActiveObject:__hIcon := LoadImage( ::AppInstance, ::ActiveObject:Icon, IMAGE_ICON,,, LR_LOADFROMFILE )
             ENDIF
             IF ::ActiveObject:__hIcon != NIL
                DrawIconEx( hDC, x-20, y, ::ActiveObject:__hIcon, 16, 16,,,DI_NORMAL )
             ENDIF
          ENDIF

          IF __ObjHasMsg( ::ActiveObject, "ImageList" )
             oImageList := ::ActiveObject:ImageList
             IF ::ActiveObject:Parent != NIL
                DEFAULT oImageList TO ::ActiveObject:Parent:ImageList
             ENDIF
          ENDIF

          IF oItem:ColItems[n]:ColType == "IMAGEINDEX" .AND. oImageList != NIL .AND. ::ActiveObject:&cCap != NIL .AND. ::ActiveObject:&cCap > 0
             hIcon := oImageList:GetImage( ::ActiveObject:&cCap )
             DrawIconEx( hDC, nLeft, y, hIcon, oImageList:IconWidth, 16,,,DI_NORMAL )
          ENDIF

          IF nColor != NIL .AND. cType != "L"
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

METHOD CheckValue( cProp, cRoot, xValue ) CLASS ObjManager
   LOCAL xVal, oItem := ::SearchString( cProp, cRoot )
   IF oItem != NIL
      xVal := oItem:ColItems[1]:Value
      IF !( VALTYPE( xVal ) == VALTYPE( xValue ) )
         xVal := oItem:ColItems[1]:SetValue
      ENDIF
      IF ! ( xValue == xVal ) .AND. VALTYPE( xVal ) == VALTYPE( xValue )

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
   LOCAL /*n,*/ oEditor

   IF !( cOldName == cNewName )

      DEFAULT lProject TO .F.

      oEditor := ::Application:ProjectPrgEditor
      ::Application:ProjectPrgEditor:Select()
      ::Application:ProjectPrgEditor:ReplaceAll( cOldName, cNewName, 0 )

      IF lProject
         cNewName := ::Application:Project:properties:Name
         oEditor:TreeItem:Select()
       ELSE
         IF ::ActiveObject:lCustom
            ::ActiveObject:__NewName := cNewName
            ::ActiveObject:__OldName := cOldName
         ENDIF
         oEditor := ::ActiveObject:Form:Editor
         oEditor:Select()
         oEditor:ReplaceAll( cOldName, cNewName, 0 )

         IF ! Empty( oEditor:FileName )
            oEditor:Path     := ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source
            oEditor:FileName := cNewName +".prg"
            oEditor:File     := oEditor:Path + "\" + oEditor:FileName
         ENDIF
      ENDIF
      oEditor:PrevFile := cOldName

      IF lProject .OR. ::ActiveObject:ClsName == "VXH_FORM_IDE" .OR. ::ActiveObject:ClsName == "CCTL"
         TRY
            oEditor:TreeItem:Text := cNewName +".prg *"
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

      ::Application:Project:SetAction( { { DG_FONTCHANGED, oFont, aFont1, aFont2, oItem } }, ::Application:Project:aUndo )
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
   oFont:Set()
   ::ResetProperties(,,.T., IIF( oItem:Expanded, {oItem:Caption},) )
RETURN Self

//------------------------------------------------------------------------------------------
METHOD SetValue( xValue, cCaption, oItem ) CLASS ObjManager
   LOCAL oObj, cProp, cProp2, xVal

   hb_gcAll()
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

   IF (cProp == "Name" .OR. ( cProp == "Alias" .AND. ! Empty(xValue) .AND. xValue[1] != "&" ) ).AND. !IsValidIdentifier( xValue )
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

   IF cProp2 != NIL .AND. cProp2 == "Dock" .AND. AT( "Margin", cProp ) == 0 .AND. AT( "RightProportional", cProp ) == 0
      xValue := oItem:ColItems[1]:Value[2][ xValue ]
    ELSEIF cProp IN {"ActiveMenuBar","Buddy","FolderView","ImageList","HotImageList","BandChild","DataSource","ImageListSmall","HeaderMenu","ButtonMenu","ContextMenu","PageChild","Socket","BindingSource","SqlConnector"}
      xValue := oItem:ColItems[1]:Value[2][ xValue ]
   ENDIF

   IF cProp == "DataSource" .AND. ::ActiveObject:ClsName == "DataGrid" .AND. ::ActiveObject:DataSource == xValue
      RETURN NIL
   ENDIF

   TRY
      xVal := oObj:&cProp
    CATCH
      xVal := xValue
   END
   //::Application:Project:SetAction( { { DG_PROPERTYCHANGED, ::ActiveObject, xValue, cCaption, oItem, xVal, cProp } }, ::Application:Project:aUndo )
   ::SetObjectValue( ::ActiveObject, xValue, cCaption, oItem, xVal, cProp )
RETURN Self

METHOD SetObjectValue( oActiveObject, xValue, cCaption, oItem ) CLASS ObjManager
   STATIC s_bSetting := .F.

   LOCAL n, cProp, cProp2, oObj, Topic, Event, nCurr, nHost, cVal
   LOCAL oError, aSel

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

   ::Application:Project:Modified := .T.
   IF ::Application:Project:CurrentForm != NIL
      ::Application:Project:CurrentForm:__lModified := .T.
   ENDIF
   IF cProp2 != NIL .AND. cProp2 == "MDIClient" .AND. cProp IN {"AlignLeft","AlignTop","AlignRight","AlignBottom"}
      __objSendMsg( oActiveObject:MDIClient, "_"+cProp, xValue )
      IF xValue != NIL
         oItem:ColItems[1]:Value[1] := xValue:Name
       ELSE
         oItem:ColItems[1]:Value[1] := ""
         oItem:ColItems[1]:Value[2][1] := NIL
      ENDIF
      PostMessage( oActiveObject:hWnd, WM_SIZE, 0, MAKELPARAM( oActiveObject:ClientWidth, oActiveObject:ClientHeight ) )

    ELSEIF cProp2 != NIL .AND. cProp2 == "Dock" .AND. AT( "Margins", cProp ) == 0 .AND. AT( "RightProportional", cProp ) == 0

      __objSendMsg( oActiveObject:Dock, "_"+cProp, xValue )
      IF xValue != NIL
         oItem:ColItems[1]:Value[1] := xValue:Name
       ELSE
         oItem:ColItems[1]:Value[1] := ""
         oItem:ColItems[1]:Value[2][1] := NIL
      ENDIF

    ELSEIF cProp IN {"ActiveMenuBar","Buddy","FolderView","ImageList","HotImageList","BandChild","DataSource","ImageListSmall","HeaderMenu","ButtonMenu","ContextMenu","PageChild","Socket","BindingSource","SqlConnector"}
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
         IF HGetPos( oObj:__OleVars, cProp ) > 0
            IF VALTYPE( oObj:__OleVars[cProp][1] ) == "A"
               oObj:__OleVars[cProp][4] := xValue
             ELSE
               oObj:__OleVars[cProp][1] := xValue
            ENDIF
            __objSendMsg( oObj, "_"+cProp, xValue )
         ENDIF
       ELSEIF ::Application:Project:CurrentForm != NIL
         IF LEN( ::Application:Project:CurrentForm:Selected ) > 1
            FOR EACH aSel IN ::Application:Project:CurrentForm:Selected
                IF cProp2 != NIL
                   __objSendMsg( aSel[1]:&cProp2, "_"+cProp, xValue )
                 ELSE
                   __objSendMsg( aSel[1], "_"+cProp, xValue )

                   IF cProp IN {"Left","Top","Width","Height"}
                      aSel[1]:MoveWindow()
                      aSel[1]:Parent:InvalidateRect()
                   ENDIF
                ENDIF
            NEXT
          ELSE
            __objSendMsg( oObj, "_"+cProp, xValue )
         ENDIF
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
         IF oActiveObject:ClsName != "VXH_FORM_IDE" .OR. cProp IN {"Width","Height"}
            oActiveObject:MoveWindow()
            oActiveObject:Parent:InvalidateRect()
         ENDIF
      ENDIF

      IF oItem:Owner:Caption == "Colors" .OR. oItem:ColItems[1]:Color != NIL
         oItem:ColItems[1]:Color := ::GetColorValues( oActiveObject, cProp, @xValue, oItem:ColItems[1]:Default )
         ::InvalidateRect(,.F.)
         IF oItem:Owner:Caption == "COM Properties" .AND. HGetPos( oObj:__OleVars, cProp ) > 0
            oObj:__OleVars[cProp][1] := oItem:ColItems[1]:Color
            __ObjSendMsg( oObj, "_"+cProp, oItem:ColItems[1]:Color )
         ENDIF
      ENDIF

      cVal := oItem:ColItems[1]:Value

      IF !( VALTYPE( oItem:ColItems[1]:Value ) == "A" )
         oItem:ColItems[1]:Value := xValue
      ENDIF

   ENDIF

   IF cProp == "MDIContainer" .OR. cProp == "MDIChild"
      ::ResetProperties()
   ENDIF

   IF ValType( oActiveObject ) == "O" .AND. __objHasMsg( oActiveObject, "__CreateBkBrush" )
      oActiveObject:__CreateBkBrush()
      oActiveObject:Redraw()
   ENDIF

   ::Application:Project:Modified := .T.
   IF oActiveObject:ClsName != "ProjProp"
      ::Application:Project:CurrentForm:UpdateSelection()

      TRY
         IF cProp == "Name" .AND. oActiveObject:ClsName == "VXH_FORM_IDE"
            ::RenameForm( cVal, xValue,, oActiveObject )

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
   ELSE
      __ObjSendMsg( oObj, "_"+cProp, xValue )
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
   DEFAULT oItem TO ::SearchString( "Text" )
   IF oItem != NIL
      oItem:Select()
      ::PostMessage( WM_USER+4768 )
   ENDIF
RETURN Self

METHOD SetPropDesc( oItem ) CLASS ObjManager
   LOCAL cHelp
   IF oItem != NIL .AND. __ObjHasMsg( oItem, "ColItems" ) .AND. ::Application:ShowObjectProps

      IF !EMPTY( oItem:ColItems )
         IF ::Application:Props[ "ObjPropsCtrlName" ]:Visible
            ::Application:Props[ "ObjPropsCtrlName" ]:Visible := .F.
            ::Application:Props[ "ObjPropsLabel2" ]:Visible := .F.
            ::Application:Props[ "ObjPropsLabel3" ]:Visible := .F.
            ::Application:Props[ "ObjPropsLabel4" ]:Visible := .F.
            ::Application:Props[ "ObjPropsLabel5" ]:Visible := .T.
            ::Application:Props[ "ObjPropsLabel6" ]:Visible := .T.
         ENDIF

         ::Application:Props[ "ObjPropsLabel5" ]:Text := oItem:ColItems[1]:Prop

         IF oItem:Owner:Caption == "COM Properties" .AND. HGetPos( ::ActiveObject:__OleVars, ::Application:Props[ "ObjPropsLabel5" ]:Text ) != 0
            cHelp := ::ActiveObject:__OleVars[ ::Application:Props[ "ObjPropsLabel5" ]:Text ][5]
         ELSE
            cHelp := oItem:ColItems[1]:Help
         ENDIF
         DEFAULT cHelp TO "Visual xHarbour - Object Manager Property Description"
         ::Application:Props[ "ObjPropsLabel6" ]:Text := cHelp
       ELSE
         ::Application:Props[ "ObjPropsLabel5" ]:Text := ""
         ::Application:Props[ "ObjPropsLabel6" ]:Text := ""
      ENDIF

   ENDIF
RETURN Self


//---------------------------------------------------------------------------------------------------

METHOD ResetProperties( aSel, lPaint, lForce, aSubExpand, lRefreshComp ) CLASS ObjManager
   LOCAL cProp, aProp, xValue, n, oItem, nColor, aCol, oSub, lReadOnly, aObjs, oSelf := Self
   LOCAL aProperties, aProperty, aSubProp, cType, Child, xProp, nDefault, aObj, aProps, cHelp, e

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
      FOR n := 1 TO LEN( ::Items )
         ::Items[n]:Cargo := NIL
         ::Items[n]:ColItems := NIL
         ::Items[n]:Delete()
      NEXT
      ::ResetContent()
      RETURN NIL
   ENDIF

   IF ::Application:ShowObjectProps
      IF ! ::Application:Props[ "ObjPropsCtrlName" ]:Visible
         ::Application:Props[ "ObjPropsLabel5" ]:Visible := .F.
         ::Application:Props[ "ObjPropsLabel6" ]:Visible := .F.

         ::Application:Props[ "ObjPropsCtrlName" ]:Visible := .T.
         ::Application:Props[ "ObjPropsLabel2" ]:Visible := .T.
         ::Application:Props[ "ObjPropsLabel3" ]:Visible := .T.
         ::Application:Props[ "ObjPropsLabel4" ]:Visible := .T.
      ENDIF
      IF ::ActiveObject != NIL
         ::Application:Props[ "ObjPropsLabel2" ]:Text := ::ActiveObject:Name
         ::Application:Props[ "ObjPropsLabel4" ]:Text := ::ActiveObject:__xCtrlName
      ENDIF
   ENDIF

   IF ::ActiveObject != NIL .AND. ::ActiveObject == aSel[1][1] .AND. !lForce .AND. LEN( aSel ) == 1
      RETURN Self
   ENDIF

   ::ActiveObject := aSel[1][1]

   IF ::Application:ShowObjectProps
      ::Application:Props[ "ObjPropsLabel2" ]:Text := ::ActiveObject:Name
      ::Application:Props[ "ObjPropsLabel4" ]:Text := ::ActiveObject:__xCtrlName
   ENDIF

   ::Application:Props[ "ComboSelect" ]:SelectControl( ::ActiveObject )

   ::lPaint := .F.
   ::SetRedraw( .F. )
   FOR n := 1 TO LEN( ::Items )
      ::Items[n]:Cargo := NIL
      ::Items[n]:ColItems := NIL
      ::Items[n]:Delete()
   NEXT
   ::ResetContent()

   IF LEN( aSel ) > 1
      aProperties := {}
      aObjs := {}
      FOR EACH aObj IN aSel
          IF ASCAN( aObjs, {|a| a[1]==aObj[1]:ClsName} ) == 0

             aProps := ::GetPropertiesAndValues( aObj[1] )
             AADD( aObjs, { aObj[1]:ClsName, aProps } )

             FOR EACH aProp IN aProps
                 IF ASCAN( aProperties, {|a| a[1]==aProp[1]} ) == 0
                    AADD( aProperties, aProp )
                 ENDIF
             NEXT
          ENDIF
      NEXT
    ELSE
      aProperties := ::GetPropertiesAndValues( ::ActiveObject )
   ENDIF

   aSort( aProperties,,,{|x, y| x[1] < y[1]})

   FOR EACH aProperty IN aProperties
       cProp  := aProperty[1]
       IF LEN( aSel ) > 1
          IF cProp IN {"NAME","TABORDER","DOCK","ANCHOR","SHORTCUTKEY","TOOLTIP"}
             LOOP
          ENDIF
          FOR EACH aObj IN aSel
              n := ASCAN( aObjs, {|a| a[1]==aObj[1]:ClsName} )
              IF n > 0
                 IF ASCAN( aObjs[n][2], {|a| a[1]==cProp}) == 0
                    cProp := NIL
                    EXIT
                 ENDIF
              ENDIF
          NEXT
          IF cProp == NIL
             LOOP
          ENDIF
       ENDIF
       //------------------------------------------------------------------------------------------------------------------------------------------------------
       IF Upper(cProp) IN ::ActiveObject:__aExcludeProperties
          LOOP
       ENDIF
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
       IF cProp IN {"DOCK","ANCHOR"} .AND. ::ActiveObject:Parent:ClsName == "StatusBarPanel"
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
       //------------------------------------------------------------------------------------------------------------------------------------------------------

       //hb_gcStep()

       TRY
          xValue := ::ActiveObject:&cProp
       CATCH
          xValue := NIL
       END
       cType  := VALTYPE( xValue )
       nColor := NIL

       aProp := __objSendMsg( ::ActiveObject, "__a_"+cProp )
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

       nDefault := NIL

       IF aProp[2] == "Colors" .AND. VALTYPE(xValue) != "L"
          cType  := "COLORREF"
          nColor := ::GetColorValues( ::ActiveObject, cProp, @xValue, @nDefault )
       ENDIF

       aCol := { TreeColItem( IIF( VALTYPE(xValue)=="O", "", xValue ), cType, , nColor, cProp, , , aProp[3], nDefault ) }

       IF __ObjHasMsg( ::ActiveObject, "Enum"+cProp )
          aCol[1]:Value    := ::ActiveObject:Enum&cProp[1]
          aCol[1]:ColType  := "ENUM"
          aCol[1]:SetValue := ASCAN( ::ActiveObject:Enum&cProp[2], ::ActiveObject:&cProp,,, .T. )
          aCol[1]:Action   := {|o, n, c| n := o:GetCurSel()-1,;
                                              c := o:Cargo[1],;
                                              o:Destroy(),;
                                              o:Cargo[2]:ColItems[1]:SetValue := n+1,;
                                              oSelf:SetValue( ::ActiveObject:Enum&c[2][n+1] ) }
          xValue := NIL
        ELSE
          DO CASE
             CASE cProp == "ShortcutKey"
                  aCol[1]:Value   := ::ActiveObject:ShortcutKey:GetShortcutText()
                  aCol[1]:ColType := ""

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
                  IF ! EMPTY( ::Application:Project:Forms ) .AND. ::ActiveObject:Form:hWnd != ::Application:Project:Forms[1]:hWnd
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

             CASE cProp == "FolderView"
                  aCol[1]:Value := { "", { NIL } }

                  FOR EACH Child IN ::ActiveObject:Parent:Children
                      IF Child:__xCtrlName == "FolderList"
                         AADD( aCol[1]:Value[2], Child )
                      ENDIF
                  NEXT

                  IF xValue != NIL
                     aCol[1]:Value[1] := xValue:Name
                  ENDIF

                  aCol[1]:ColType  := UPPER( cProp )
                  xValue := NIL


             CASE cProp IN { "Buddy", "PageChild" }
                  aCol[1]:Value := { "", { NIL } }

                  FOR EACH Child IN ::ActiveObject:Parent:Children
                      IF Child:ClsName != REBARCLASSNAME
                         AADD( aCol[1]:Value[2], Child )
                      ENDIF
                  NEXT

                  IF xValue != NIL
                     aCol[1]:Value[1] := xValue:Name
                  ENDIF

                  aCol[1]:ColType  := UPPER( cProp )
                  xValue := NIL

             CASE cProp == "BandChild"

                  aCol[1]:Value := { "", { NIL } }

                  FOR EACH Child IN ::ActiveObject:Parent:Parent:Children
                      IF Child:ClsName != REBARCLASSNAME
                         AADD( aCol[1]:Value[2], Child )
                      ENDIF
                  NEXT

                  IF xValue != NIL
                     aCol[1]:Value[1] := IIF( VALTYPE( xValue ) == "C", xValue, xValue:Name )
                  ENDIF

                  aCol[1]:ColType  := "BANDCHILD"
                  xValue := NIL

             CASE cProp == "ActiveMenuBar"
                  aCol[1]:Value := { "", { NIL } }

                  FOR EACH Child IN ::ActiveObject:Form:Components
                      IF Child:ClsName == "MenuBar"
                         AADD( aCol[1]:Value[2], Child )
                      ENDIF
                  NEXT

                  IF xValue != NIL
                     aCol[1]:Value[1] := IIF( VALTYPE( xValue ) == "C", xValue, xValue:Name )
                  ENDIF

                  aCol[1]:ColType  := "ACTIVEMENUBAR"
                  xValue := NIL


             CASE cProp == "Icon"
                  IF xValue != NIL
                     aCol[1]:Value := xValue
                     xValue := NIL
                  ENDIF
                  aCol[1]:ColType  := "ICONS"

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

             CASE RIGHT(cProp,10) == "ImageIndex"
                  aCol[1]:ColType  := "IMAGEINDEX"
                  xValue := NIL

             CASE cProp == "Alignment"
                  IF ::ActiveObject:__xCtrlName == "ListViewGroup"
                     aCol[1]:Value := ::System:ListViewGroupAlign:Keys
                     aCol[1]:ColType  := "LVGALIGNMENT"
                  ENDIF
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

             CASE cProp == "SysFolder"
                  aCol[1]:Value := ::System:Folders:Keys
                  aCol[1]:ColType  := "SYSFOLDERS"
                  xValue := NIL

             CASE cProp == "Driver" .AND. ::ActiveObject:ComponentType == "DataSource"
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
       ::CheckObjProp( xValue, oSub, cProp, aSubExpand )

       oSub:SortChildren( .T. )
       IF aSubExpand != NIL .AND. ASCAN( aSubExpand, cProp ) > 0
          oSub:Expand()
       ENDIF
       oItem:SortChildren( .T. )
       oItem:Expand()
   NEXT

   IF ::ActiveObject:ClsName == "AtlAxWin" .AND. ::ActiveObject:__OleVars != NIL
      oItem := TreeViewItem( Self )
      oItem:Caption    := "COM Properties"
      oItem:InsertAfter:= TVI_SORT
      oItem:Create()

      FOR EACH cProp IN ::ActiveObject:__OleVars:Keys
          TRY
             xValue    := ::ActiveObject:__OleVars[cProp][1]
             cType     := VALTYPE( xValue )
             lReadOnly := ::ActiveObject:__OleVars[cProp][3]
             cHelp     := ::ActiveObject:__OleVars[cProp][5]

             IF cType == "A"
                cType := "ENUMERATION"
              ELSEIF cType == "O"
                cType := "COLLECTION"
             ENDIF

             IF "Color" IN cProp
                nDefault := GetSysColor( COLOR_BTNFACE )
                nColor   := ::GetColorValues( ::ActiveObject, cProp, @xValue, @nDefault )
                cType    := "COLORREF"
             ENDIF

             aCol     := { TreeColItem( xValue, cType, , nColor, cProp, , lReadOnly, cHelp, nDefault ) }
             oSub     := oItem:AddItem( cProp, 0, aCol )
             nColor   := NIL
             nDefault := NIL

   //             IF "Color" IN cProp
   //                VIEW oSub:ColItems[1]:Default
   //             ENDIF
          catch e
             view e:description
          END
      NEXT
      oItem:SortChildren( .T. )
      oItem:Expand()
   ENDIF
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
//      ::ActiveObject:TreeItem:Expand()
   ENDIF

   //hb_gcall(.T.)
RETURN NIL

METHOD CheckObjProp( xValue, oItem, cProp, aSubExpand ) CLASS ObjManager
   LOCAL aSub, cProp2, xValue2, cType, nColor, aCol, aSubProp, Child, oSub, aProp, nDefault, oSelf := Self
   IF VALTYPE( xValue ) == "O"
      aSub := __ClsGetPropertiesAndValues( xValue )
      FOR EACH aSubProp IN aSub
          ::Application:Yield()

          cProp2  := aSubProp[1]
          xValue2 := __objSendMsg( xValue, cProp2 )

          cType   := VALTYPE( xValue2 )
          nColor  := NIL

          IF ! __ObjHasMsg( xValue, "__a_" + cProp2 )
             EXIT
          ENDIF

          aProp   := __objSendMsg( xValue, "__a_"+cProp2 )
          cProp2  := aProp[1]

          nDefault := NIL
          IF aProp[2] == "Colors" .AND. VALTYPE(xValue2) != "L"
             nColor := ::GetColorValues( xValue, cProp2, @xValue2, @nDefault )
             cType  := "COLORREF"
          ENDIF

          aCol   := { TreeColItem( IIF( VALTYPE(xValue2)=="O", "", xValue2 ), cType, , nColor, cProp2, cProp,,, nDefault ) }

          IF __ObjHasMsg( xValue, "Enum"+cProp2 )
             aCol[1]:Value    := xValue:Enum&cProp2[1]
             aCol[1]:ColType  := "ENUM"
             aCol[1]:SetValue := ASCAN( xValue:Enum&cProp2[2], xValue:&cProp2,,, .T. )
             aCol[1]:Action   := {|o, n, c| n := o:GetCurSel()-1,;
                                                 c := o:Cargo[1],;
                                                 o:Cargo[2]:ColItems[1]:SetValue := n+1,;
                                                 o:Destroy(),;
                                                 oSelf:SetValue( xValue:Enum&c[2][n+1] ) }
             xValue2 := NIL

           ELSEIF cProp == "Dock"
             IF cProp2 IN {"Left","Top","Right","Bottom"}
                aCol[1]:Value := { "", { NIL } }
                AADD( aCol[1]:Value[2], ::ActiveObject:Parent )
                FOR EACH Child IN ::ActiveObject:Parent:Children
                    IF Child:hWnd != ::ActiveObject:hWnd .AND. Child:__xCtrlName != "DataTable" .AND. Child:__xCtrlName != "AdsDataTable" .AND. Child:__xCtrlName != "Splitter"
                       AADD( aCol[1]:Value[2], Child )
                    ENDIF
                NEXT
                TRY
                   IF xValue2 != NIL
                      aCol[1]:Value[1] := IIF( VALTYPE( xValue2 ) == "O", xValue2:Name, xValue2 )
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
          oSub := oItem:AddItem( cProp2, 0, aCol )
          ::CheckObjProp( xValue2, oSub, cProp2, aSubExpand )

          oSub:SortChildren( .T. )
          oItem:SortChildren( .T. )
      NEXT
   ENDIF
RETURN NIL
//--------------------------------------------------------------------------------------------------------------------------------

METHOD GetColorValues( oObj, cProp, xValue, nDefault ) CLASS ObjManager
   LOCAL n, nColor := xValue

   IF nColor == NIL .AND. UPPER( cProp ) == "MASKCOLOR"
      xValue := "None"
    ELSE
      IF nDefault == NIL .AND. __ObjHasMsg( oObj, "__a_" + cProp )
         nDefault := __objSendMsg( oObj, "__a_" + cProp )[4]
      ENDIF
      IF nDefault == NIL .AND. __ObjHasMsg( oObj, "__Sys" + cProp )
         nDefault := __objSendMsg( oObj, "__Sys" + cProp )
      ENDIF
      IF ( n := ASCAN( ::Colors, {|a|a[1]==xValue} ) ) > 0
         xValue := ::Colors[n][2]
       ELSEIF xValue != nDefault
         xValue := "Custom..."
      ENDIF
      DEFAULT nColor TO nDefault
      IF nColor == nDefault
         xValue := "System Default..."
      ENDIF
   ENDIF

   IF nDefault == NIL
      nDefault := nColor
      IF VALTYPE( xValue ) != "C"
         xValue := "System Default..."
      ENDIF
   ENDIF
RETURN nColor

//--------------------------------------------------------------------------------------------------------------------------------

METHOD GetEditBuffer( oItem, nCol ) CLASS ObjManager
   LOCAL cProp, cText := oItem:ColItems[nCol-1]:Value
   cProp := oItem:ColItems[1]:Prop
   IF cProp IN {"Caption","Text"} .AND. cText == NIL .AND. ::ActiveObject:ClsName == "ToolButton"
      cText := ""
   ENDIF
RETURN cText

//--------------------------------------------------------------------------------------------------------------------------------

METHOD OnUserMsg( hWnd, nMsg, nCol, nLeft ) CLASS ObjManager
   LOCAL oImageList, oItem, rc, cType, n, cProp, cText, oFont, cFont, oSelf := Self
   ( hWnd )
   IF nMsg == WM_USER + 4765
      IF ( oItem := ::GetSelected() ) != NIL .AND. ::ActiveObject:HasMessage( "MDIClient" ) .AND. ::ActiveObject:MDIClient != NIL
         oItem:ColItems[1]:Value := ::ActiveObject:MDIClient:Margins
         ::InvalidateRect(,.F.)
      ENDIF

    ELSEIF nMsg == WM_USER + 4767
      IF ( oItem := ::GetSelected() ) != NIL .AND. ::ActiveObject:HasMessage( "Dock" ) .AND. ::ActiveObject:Dock != NIL .AND. oItem:ColItems != NIL
         oItem:ColItems[1]:Value := ::ActiveObject:Dock:Margins
         ::InvalidateRect(,.F.)
      ENDIF

    ELSEIF nMsg == WM_USER + 4766
      IF ( oItem := ::GetSelected() ) != NIL
         TRY
            IF ::ActiveObject:HasMessage( "BackgroundImage" ) .AND. VALTYPE( ::ActiveObject:BackgroundImage ) == "O"
               oItem:ColItems[1]:Value := ::ActiveObject:BackgroundImage:Margins
             ELSE
               oItem:ColItems[1]:Value := ::ActiveObject:Margins
            ENDIF
         CATCH
         END
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
                    ::Application:AccelEnabled := .F.
                    ::ActiveControl := EditBox( Self )
                    WITH OBJECT ::ActiveControl
                       :Left        := nLeft + IIF( cType == "ICONS", 20, 0 )
                       :Top         := rc:top+1
                       :Width       := ::Columns[ nCol ][ 1 ]+2 - IIF( cType == "ICONS", 20, 0 )
                       :Height      := rc:bottom-rc:top-1
                       :Border      := 0
                       :Style       := :Style | ES_AUTOHSCROLL | ES_MULTILINE & NOT( WS_BORDER )

                       GetRealKeyName(::ActiveObject:ShortcutKey:Key, @cText, 40)

                       :Caption     := cText
                       :Cargo       := ::ActiveObject:ShortcutKey:Key

                       :OnWMKeyDown := {|o,n| CheckShortCutKeyDown(o,n,::ActiveObject:ShortcutKey)}
                       :OnWMChar    := {|| 0}
                       :OnWMKillFocus := {|o,cText| cText := IIF( o:Cargo == NIL, 0, o:Cargo ),;
                                                    IIF( ::__xCtrlName == "ObjManager", oSelf:SetValue( cText ), ),;
                                                    o:Application:AccelEnabled := .T.,;
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
                       :Border := 0
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Create()

                       :OnDTNDateTimeChange := {|o, d| d := o:Date,;
                                                       oSelf:SetValue( d, o ) }

                       :Action := {|o, d| d := o:Date,;
                                          oSelf:SetValue( d, o ),;
                                          o:Destroy() }
                       :Date := ::GetEditBuffer( oItem, nCol )
                       :SetFocus()
                       :PostMessage( WM_LBUTTONDOWN, 1, MAKELPARAM( :Width - 10, :Height / 2 ) )
                    END

               CASE cType == "L"
                    ::SetValue( ! oItem:ColItems[nCol-1]:Value )
                    ::InvalidateRect(,.F.)

               CASE cType == "MASKTYPE"
                    ::ActiveControl := ObjCombo( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 70
                       :Border := 0

                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, c| c := o:GetSelString(), o:Destroy(), oSelf:SetValue( c ) }
                       :Create()

                       FOR n := 1 TO LEN( ::ActiveObject:MaskTypes )
                           :AddItem( ::ActiveObject:MaskTypes[n] )
                       NEXT

                       IF ( n := :FindStringExact(, ::ActiveObject:MaskType ) ) > 0
                          :SetCurSel(n)
                        ELSE
                          :SetCurSel(1)
                       ENDIF

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
                       :Border := 0
                       :Cargo  := oItem:Owner:Caption
                       :Owner  := ::ActiveObject

                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, cSel| cSel := o:GetSelString(), o:Destroy(), oSelf:SetValue( cSel ) }
                       :Create()

                       cFont := :Cargo

                       :SetItemHeight( -1, ::GetItemHeight()-5 )

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
                       :Border := 0

                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action        := {|o, n| n := o:GetCurSel()-1, o:Destroy(), oSelf:SetValue(n) }
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
                    cType == "SERVICES" .OR.;
                    cType == "STATES" .OR.;
                    cType == "LVGALIGNMENT" .OR.;
                    cType == "SYSFOLDERS" .OR.;
                    cType == "OLEVERB" .OR.;
                    cType == "ANIMATIONSTYLE" .OR.;
                    ( cType == "IMAGEINDEX" .AND. ( ::ActiveObject:ImageList != NIL .OR. ::ActiveObject:Parent:ImageList != NIL ) )
                    ::ActiveControl := IIF( cType == "IMAGEINDEX", ComboBoxEx( Self ), ObjCombo( Self ) )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 200
                       :Border := 0

                       IF cType != "IMAGEINDEX"
                          :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                          :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                        ELSE
                          :Height := 300
                       ENDIF
                       :Action := oItem:ColItems[nCol-1]:Action
                       IF cType == "OLEVERB"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:OleVerb, n+1 )  ) }

                        ELSEIF cType == "SYSFOLDERS"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:Folders, n+1 )  ) }

                        ELSEIF cType == "ADSDATADRIVERS"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:AdsDataDrivers, n+1 )  ) }

                        ELSEIF cType == "DATADRIVERS"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:DataDrivers, n+1 )  ) }

                        ELSEIF cType == "LVGALIGNMENT"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:ListViewGroupAlign, n+1 )  ) }
                        ELSEIF cType == "DTSFORMATS"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:DateTimeFormat, n+1 )  ) }
                        ELSEIF cType == "TEXTALIGNMENT"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:TextAlignment, n+1 )  ) }
                        ELSEIF cType == "CBDROPDOWN"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:DropDownStyle, n+1 )  ) }
                        ELSEIF cType == "ANIMATIONSTYLE"
                          :Action := {|o, n| n    := o:GetCurSel()-1,;
                                                   o:Destroy(),;
                                                   oSelf:SetValue( HGetValueAt( ::System:WindowAnimation, n+1 )  ) }
                        ELSEIF cType == "SERVICES"
                          :Action := {|o, cSel| cSel := o:GetSelString(),;
                                                        o:Destroy(),;
                                                        oSelf:SetValue( cSel ) }
                        ELSE
                          :Action := {|o, n| n := o:GetCurSel()-1, o:Destroy(), oSelf:SetValue( n + IIF( cType == "PAGE_POSITIONS" .OR. cType == "STATES" .OR. cType == "IMAGEINDEX", 0, 1 ) ) }
                       ENDIF
                       :Create()

                       IF cType != "IMAGEINDEX"
                          FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value )
                              :AddItem( oItem:ColItems[nCol-1]:Value[n] )
                          NEXT
                        ELSEIF cType == "IMAGEINDEX"
                          :AddItem( "None", 0 )

                          oImageList := ::ActiveObject:ImageList
                          DEFAULT oImageList TO ::ActiveObject:Parent:ImageList

                          FOR n := 1 TO oImageList:Count
                              :AddItem( XSTR(n), n )
                          NEXT
                          :ImageList  := oImageList
                       ENDIF

                       IF cType == "IMAGEINDEX"
                          :SendMessage( CB_SETCURSEL, MAX( ::ActiveObject:&cProp, 0 ) )
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
                        ELSEIF cType == "DROPDOWN"
                          :SetCurSel( ::ActiveObject:DropDown )
                        ELSEIF cType == "OLEVERB"
                          n := hScan( ::System:OleVerb, ::ActiveObject:OleVerb )
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
                       :Border := 0
                       :Height := 200
                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := {|o, n| n := o:GetCurSel()-1, o:Destroy(), oSelf:SetValue( n + 1 ) }
                       :Create()
                       :AddItem( "" )
                       FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value[2] )
                           IF VALTYPE( oItem:ColItems[nCol-1]:Value[2][n] ) == "O"
                              :AddItem( oItem:ColItems[nCol-1]:Value[2][n]:Name )
                           ENDIF
                       NEXT

                       :SetItemHeight( -1, ::GetItemHeight()-5 )
                       :SetFocus()
                       :ShowDropDown()

                       IF ( n := :FindStringExact( ,oItem:ColItems[nCol-1]:Value[1] ) ) > 0
                          :SetCurSel(n)
                        ELSE
                          :SetCurSel(1)
                       ENDIF
                    END

               CASE cType IN { "ACTIVEMENUBAR", "IMAGELIST","PAGECHILD","BANDCHILD","DATASOURCE","HEADERMENU","BUTTONMENU","CONTEXTMENU","SOCKET","BINDINGSOURCE","SQLCONNECTOR","BUDDY","FOLDERVIEW" }
                    ::ActiveControl := ObjCombo( Self )
                    WITH OBJECT ::ActiveControl
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 70
                       :Border := 0

                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ), NIL }
                       :Action := {|o, n, c| c := o:GetSelString(), n := o:GetCurSel()-1, o:Destroy(), oSelf:SetValue( n + 1, c ) }
                       :Create()

                       :AddItem( "" )
                       FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value[2] )
                           IF oItem:ColItems[nCol-1]:Value[2][n] != NIL
                              :AddItem( oItem:ColItems[nCol-1]:Value[2][n]:Name )
                           ENDIF
                       NEXT

                       IF ( n := :FindStringExact(, oItem:ColItems[nCol-1]:Value[1] ) ) > 0
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
                                                       oSelf:ChangeCtrlFont( cf, oItem, oFont ),;
                                                       o:Destroy(),;
                                                       ::Application:Project:Modified := .T.}
                          ENDIF
                        ELSE
                          :Action := {|o| StructEditor( oSelf:ActiveObject ),;
                                          oSelf:ResetProperties(,,.T.),;
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
                    WITH OBJECT ::ActiveControl := IIF( UPPER(cProp) == "CURSOR", CursorComboBox( Self ), ObjCombo( Self ) )
                       :Left   := nLeft-1
                       :Top    := rc:top
                       :Width  := ::Columns[ nCol ][ 1 ]+4
                       :Height := 70
                       :Border := 0
                       :Height := 200
                       :OnWMKillFocus := {|o|o:HideDropDown(),o:Destroy() }
                       :OnWMKeyDown   := {|o,n| IIF( n == 27, o:Destroy(),NIL ) }
                       :Action := oItem:ColItems[nCol-1]:Action
                       :Cargo  := { cProp, oItem }
                       :Create()
                       IF :GetCount() == 0
                          FOR n := 1 TO LEN( oItem:ColItems[nCol-1]:Value )
                              :AddItem( oItem:ColItems[nCol-1]:Value[n] )
                          NEXT
                       ENDIF
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
                       :Border := 0
                       :Style := :Style | ES_AUTOHSCROLL | ES_MULTILINE & NOT( WS_BORDER )

                       IF oItem:Caption == "Path" .OR. oItem:Caption IN {"SelectedPath","DefaultPath","Default","IncludePath","SourcePath"} .OR. oItem:Owner:Caption == "Target Folder"
                          :Button := .T.
                          :ButtonAction := {|o| BrowseForFolder(o, Self, oItem)}

                        ELSEIF __ObjHasMsg( ::ActiveObject, "__ExplorerFilter" ) .OR. oItem:Caption IN {"FileName","Icon","ImageName"} .OR. oItem:Owner:Caption == "Font"
                          IF oItem:Caption IN {"FileName","Icon","ImageName"}
                             :Button := .T.
                             :ButtonAction := {|o| BrowseForFile( o, Self, IIF( oItem:Owner:Caption == "Font", ::ActiveObject:Font, ::ActiveObject ), oItem:Caption == "Icon" ) }
                          ENDIF
                        ELSEIF ::ActiveObject:ClassName == "WINDOWEDIT" .AND. oItem:Caption == "ImageName"
                          :Button := .T.
                          :ButtonAction := {|o| BrowseForFile( o, Self, ::ActiveObject:BackgroundImage, .F. ) }
                        ELSEIF oItem:Caption == "ImageName"
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
                          :OnWMKillFocus := {|o,cText| cText := o:Caption,;
                                                       IIF( ::__xCtrlName == "ObjManager", oSelf:SetValue( cText ), ),;
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

                       :Alignment   := 3
                       :MultiLine   := .T.
                       :AutoHScroll := .T.
                       :Border      := 0
                       :Cargo       := :Caption
                       :OnWMKillFocus := {|o,cText| cText := o:Caption,;
                                                    IIF( ::__xCtrlName == "ObjManager", oSelf:SetValue( VAL( cText ) ), ),;
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
                             :SysDefault := ::ActiveObject:__SysBackColor
                            ELSEIF UPPER( oItem:Caption )!= "MASKCOLOR"
                             :SysDefault := ::ActiveObject:__SysForeColor
                          ENDIF
                         catch
                       END

                       IF oItem:ColItems[nCol-1]:Value == "Custom..."
                          :Custom := oItem:ColItems[nCol-1]:Color
                       ENDIF

                       :ColorSelected := oItem:ColItems[nCol-1]:Color

                       :Create( UPPER( oItem:Caption ) == "MASKCOLOR" )
                       :Action := {|o, c| c := o:ColorSelected, o:Destroy(), ::SetValue( c ), ::ActiveObject:InvalidateRect(,.F.) }
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
   LOCAL oForm      := Dialog( ::Application:MainForm )
   oForm:Text       := "Enter Text"
   oForm:ToolWindow := .T.
   oForm:Modal      := .T.
   oForm:Width      := 350
   oForm:height     := 220
   oForm:Center     := .T.
   oForm:OnWMInitDialog := {|o| InitEditText(o, Self, oEdit:Text ) }
   oForm:Create()
RETURN NIL

FUNCTION InitEditText( oForm, oMan, cText )
   LOCAL oCancel, oOK, oEdit
   WITH OBJECT oEdit := EditBox( oForm )
      :Caption     := cText
      :Dock:Left   := oForm
      :Dock:Top    := oForm
      :Dock:Right  := oForm
      :Dock:Bottom := oForm
      :Dock:BottomMargin := 27
      :Height      := 160
      :VertScroll  := .T.
      :HorzScroll  := .T.
      :WantReturn  := .T.
      :Multiline   := .T.
      :Create()
   END
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
RETURN 0

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

STATIC FUNCTION CheckShortCutKeyDown( o, n, oShortcut )
   LOCAL nScanCode, cText, lAsc, aSys := { VK_CONTROL, VK_SHIFT, VK_MENU, VK_CAPITAL, VK_SCROLL }
   IF ! oShortcut:Ctrl .AND. ! oShortcut:Alt .AND. ! oShortcut:Shift .AND. n == 27
      o:Caption := ""
      o:Cargo   := NIL
      RETURN 0
   ENDIF
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

METHOD OnMouseMove( nwParam, nlParam ) CLASS ObjManager
   LOCAL x, y, oItem, rc, pt, aAlign, nfHeight, nPlus := 0
   static cText

   ::Super:OnMouseMove( nwParam, nlParam )

   x := LOWORD(nlParam)
   y := HIWORD(nlParam)

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
             ELSE
               cText := NIL
               ::ToolTip:Text  := NIL
            ENDIF

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

      CASE nKey == VK_SPACE
           ::PostMessage( WM_USER+4768 )

   ENDCASE
RETURN NIL

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS TreeColItem
   DATA SetValue  EXPORTED
   DATA Value     EXPORTED
   DATA Align     EXPORTED
   DATA Default   EXPORTED
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

METHOD Init( xValue, cType, nAlign, nColor, cProp, cProp2, lReadOnly, cHelp, xDefault ) CLASS TreeColItem
   DEFAULT lReadOnly TO ::ReadOnly
   ::Value   := xValue
   ::Align   := nAlign
   ::ColType := cType
   ::Color   := nColor
   ::Prop    := cProp
   ::Prop2   := cProp2
   ::ReadOnly:= lReadOnly
   ::Help    := cHelp
   ::Default := xDefault
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
   LOCAL nPos, aEvents

   ::Application:Project:CurrentForm:Editor:Select()
   ::Application:Project:CurrentForm:Editor:TreeItem:Select()

   WITH OBJECT ::Application:Project:CurrentForm:Editor

      :SetSearchFlags( SCFIND_WHOLEWORD )

      nPos := :FindInPos( "METHOD "+cFuncName, 1 )
      IF nPos == -1
         :AppendText( CRLF + "//----------------------------------------------------------------------------------------------------" + CRLF +;
                      "METHOD " + cFuncName + "( Sender" + IIF( ! Empty( Event[3] ), ", " + Event[3] + " ", " " ) + ") CLASS "+::ActiveObject:Form:Name + CRLF )

         aEvents := { "ONCHAR", "ONKEYDOWN", "ONKEYUP", "ONSYSKEYDOWN", "ONSYSKEYUP", "ONCHILDCHAR", "ONCHILDKEYDOWN", "ONSYSCHAR" }

         IF ASCAN( aEvents, UPPER( cEvent ) ) > 0
            :AppendText( "   LOCAL nKey := Sender:wParam" + CRLF + CRLF )
          ELSE
            :AppendText( "   " + IIF( ::ActiveObject:__xCtrlName == "LinkLabel", "ShellExecute( ::hWnd, 'open', Sender:Url, , , SW_SHOW )", "" ) + CRLF )
         ENDIF

         nPos := :GetTextLen()-2
         :AppendText( "RETURN Self" + CRLF )

      ENDIF
      :GotoPosition( nPos )
      :Owner:SendMessage( SCI_SETFIRSTVISIBLELINE, :SendEditor( SCI_LINEFROMPOSITION, nPos, 0 )-1, 0 )
   END

   IF !::Application:SourceEditor:IsWindowVisible()
      ::Application:EditorPage:Select()
   ENDIF
   ::Application:SourceEditor:SetFocus()
   InvalidateRect( ::Application:SourceEditor:hWnd,, .F. )

   ::Application:ToolBox:Enabled := .F.
   ::Application:Project:Modified := .T.
   ::Application:Project:CurrentForm:__lModified := .T.
RETURN Self

//------------------------------------------------------------------------------------------

METHOD RenameEvent( cEvent, cFuncName, cNewFuncName, lSwitch ) CLASS EventManager
   LOCAL nPos
   (cEvent)
   DEFAULT lSwitch TO .T.

   ::Application:Project:CurrentForm:Editor:Select()
   ::Application:Project:CurrentForm:Editor:TreeItem:Select()

   IF !( cFuncName == cNewFuncName )
      WITH OBJECT ::Application:Project:CurrentForm
         nPos := :Editor:FindInPos( "METHOD "+cFuncName, 1 )
         IF nPos >= 0
            IF :Editor:ReplaceAll( cFuncName, cNewFuncName, SCFIND_WHOLEWORD ) > 0
               ::Application:Project:Modified := .T.
               :__lModified := .T.
               IF lSwitch
                  ::Application:EditorPage:Select()
                  ::Application:SourceEditor:SetFocus()
               ENDIF
               :Editor:GotoPosition( nPos )
               :Editor:Owner:SendMessage( SCI_SETFIRSTVISIBLELINE, :Editor:SendEditor( SCI_LINEFROMPOSITION, nPos, 0 )-1, 0 )
            ENDIF
         ENDIF
      END
   ENDIF
RETURN Self

/*STATIC FUNCTION ChangePrgLine( cLine, cOldVal, cNewVal, lComment )
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

       IF cChar  == " " .OR. cChar == "." .OR. cChar == '"' .OR. cChar == "(" .OR. cChar == ")" .OR. cChar == "=" .OR. cChar == "{" .OR. cChar == ","
          cNewLine += cWord
          cWord := ""
       ENDIF
       cPrev := cChar
       nFor++
   NEXT
   cNewLine += cWord
RETURN cNewLine*/

//------------------------------------------------------------------------------------------
METHOD ResetEvents( aSel ) CLASS EventManager

   LOCAL oItem, Event, Topic, oSub

   DEFAULT aSel TO { { ::ActiveObject,, } }

   IF !::lPaint
      RETURN NIL
   ENDIF

   IF EMPTY( aSel ) .OR. EMPTY( aSel[1] ) .OR. aSel[1][1] == NIL
      aSel := NIL
      ::ResetContent()
      RETURN NIL
   ENDIF

   ::ActiveObject := aSel[1][1]

   ::lPaint := .F.
   ::SetRedraw( .F. )
   ::ResetContent()

   ::cEventList := ""

   IF ::ActiveObject:Events != NIL
      FOR EACH Topic IN ::ActiveObject:Events
          oItem := ::AddItem( Topic[1] )
          FOR EACH Event IN Topic[2]
              oSub := oItem:AddItem( Event[1], 0, { TreeColItem( Event[2], "C", ,NIL , Event[1]) } )
              ::cEventList += Event[1] + " "
          NEXT
          oItem:Expand()
      NEXT
      IF LEN( ::Items ) > 0
         ::Items[1]:EnsureVisible()
      ENDIF
   ENDIF
   ::lPaint := .T.
   ::SetRedraw( .T. )
   ::Application:Yield()
   aSel := NIL
   //hb_gcall(.T.)
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
                 ENDIF
                 IF UPPER( ::ActiveObject:Events[n][2][i][2] ) != UPPER( cName )
                    ::ActiveObject:Events[n][2][i][2] := cName
                    ::Application:Project:Modified := .T.
                    ::Application:Project:CurrentForm:__lModified := .T.
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
   ::ColFont := Font( NIL )
RETURN Self

METHOD Create() CLASS ObjCombo
   ::Super:Create()
   ::ColFont:Bold := .T.
   ::ColFont:Create()
RETURN Self

METHOD OnParentDrawItem( nlParam, nwParam, dis ) CLASS ObjCombo
   LOCAL n, x, lSelected, aRect, aClip, nLen, itemTxt, cText, nField, z, aAlign, y
   ( nlParam, nwParam )
   IF dis:hwndItem == ::hWnd
      DEFAULT ::ColWidth TO ::ClientWidth

      lSelected := dis:itemState & ODS_SELECTED != 0
      aClip     := { dis:rcItem:Left,  dis:rcItem:Top, ;
                     dis:rcItem:Right, dis:rcItem:Bottom  }

      IF dis:itemAction & ODA_DRAWENTIRE != 0 .OR. dis:itemAction & ODA_SELECT != 0
         SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
         itemTxt := left( itemTxt, nLen )

         z := dis:rcItem:Top
         IF dis:itemState & ODS_COMBOBOXEDIT != 0 .AND. dis:itemAction & ODA_SELECT == 0
            z := dis:rcItem:Top - 2
         ENDIF

         cText   := ""
         aRect   := ACLONE(aClip)
         nField  := 1
         aRect[3]:= aRect[1] + ::ColWidth

         FOR n := 1 to nLen + 1
             IF SubStr( itemTxt, n, 1) == chr(9) .or. n == nLen + 1
                x := aRect[1] + 2

                ::Font:Select( dis:hDC )
                IF nField == 1
                   ::ColFont:Select( dis:hDC )
                ENDIF

                aAlign  := _GetTextExtentPoint32( dis:hDC, cText )
                y := dis:rcItem:top + ( ( dis:rcItem:bottom - dis:rcItem:top )/2 ) - ( aAlign[2]/2 )

                _ExtTextOut( dis:hDC, x, y, ETO_OPAQUE + ETO_CLIPPED, aRect, cText )
                cText := ""
                aRect[1] += ::ColWidth
                nField ++

                IF nField  > 1
                   aRect[3] := dis:rcItem:Right
                  ELSE
                   aRect[3] += ::ColWidth
                ENDIF
                LOOP
             ENDIF

             cText += SubStr( itemTxt, n, 1 )
         NEXT
      ENDIF

      IF dis:itemState & ODS_COMBOBOXEDIT == 0
         IF dis:itemState & ODS_FOCUS != 0 .OR. dis:itemAction & ODA_FOCUS != 0
            aClip := { dis:rcItem:Left,  dis:rcItem:Top, ;
                       dis:rcItem:Right, dis:rcItem:Bottom  }
            _DrawfocusRect( dis:hDC, aclip )
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

/*STATIC FUNCTION BrowseFile( o )
   LOCAL n := o:GetCurSel()-1
   o:Parent:SetValue( n + 1 )
   o:Destroy()
RETURN NIL*/

STATIC FUNCTION BrowseForFile( oEdit, oMan, oObj, lIcon, aFilter )
   LOCAL oFile := CFile( oEdit:Text )
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
   IF __ObjHasMsg( oMan:ActiveObject, "Path" ) .AND. !EMPTY( oMan:ActiveObject:Path ) .AND. oMan:ActiveObject:ClsName != "Application"
      oFile:Path := oMan:ActiveObject:Path
   ENDIF
   oFile:OpenDialog()

   oEdit:OnWMKillFocus := {|o|o:Destroy() }
   IF oFile:Result != IDCANCEL
      oEdit:Destroy()

      IF oMan:ActiveObject:ClsName != "Application" .AND. __ObjHasMsg( oMan:ActiveObject, "Path" )
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
