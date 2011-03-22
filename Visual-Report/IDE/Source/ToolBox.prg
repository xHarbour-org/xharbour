/*
 * $Id$
 */

// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!

#include "vxh.ch"
#include "debug.ch"
#include "winuser.ch"

#define MCS_ARROW    10
#define MCS_PASTE    11
#define MCS_DRAGGING 12

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ToolBox INHERIT TreeView
   DATA nScroll      PROTECTED INIT 0
   DATA LevelFont    PROTECTED
   DATA hPen         PROTECTED
   DATA nImage       PROTECTED
   
   DATA ActiveItem   EXPORTED
   DATA HoverItem    EXPORTED
   DATA aButtons     EXPORTED
   DATA ComObjects   EXPORTED INIT {}
   DATA hPenShadow   EXPORTED
   DATA aCursors     EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD OnHScroll( n, nPos ) INLINE ::nScroll := n
   METHOD OnNCDestroy()        INLINE ::LevelFont:Delete(), DeleteObject( ::hPen ), DeleteObject( ::hPenShadow )
   METHOD OnEraseBkGnd()
   METHOD OnParentNotify()
   METHOD DrawItem()
   METHOD OnGetDlgCode()       INLINE DLGC_WANTALLKEYS
   METHOD OnSelChanged()
   METHOD OnMouseLeave()
   METHOD OnMouseMove()
   METHOD OnSysCommand(n) INLINE IIF( n==SC_CLOSE, (::Hide(),0),)
   METHOD Disable()       INLINE ::xEnabled := .F., ::InvalidateRect(,.F.)
   METHOD Enable()        INLINE ::xEnabled := .T., ::InvalidateRect(,.F.)
   METHOD FindItem()
   METHOD SetControlCursor()
   METHOD SetMouseShape()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ToolBox
   LOCAL n, cName, cType, xData, cValue, lPro
   DEFAULT ::__xCtrlName  TO "ToolBox"
   ::Super:Init( oParent )
   ::Caption       := "ToolBox"
   ::ShowSelAlways := .T.
   ::FullRowSelect := .T.
   ::TrackSelect   := .T.
   ::aCursors      := { LoadCursor(, IDC_SIZENWSE ),;
                        LoadCursor(, IDC_SIZEWE   ),;
                        LoadCursor(, IDC_SIZENESW ),;
                        LoadCursor(, IDC_SIZENS   ),;
                        LoadCursor(, IDC_SIZENWSE ),;
                        LoadCursor(, IDC_SIZEWE   ),;
                        LoadCursor(, IDC_SIZENESW ),;
                        LoadCursor(, IDC_SIZENS   ),;
                        LoadCursor(, IDC_SIZEALL  ),;
                        LoadCursor(, IDC_ARROW    ),;
                        LoadCursor( GetModuleHandle(), "CUR_PASTE" ),;
                        LoadCursor( GetModuleHandle(), "CUR_DRAGGING" ),;
                        LoadCursor(, IDC_NO       ) }
RETURN Self
//------------------------------------------------------------------------------------------

METHOD Create() CLASS ToolBox
   LOCAL nIndex, nIcon, o, oPtr, oItem, n, x
   LOCAL hKey, cName, cType, xData, hSub, cValue, oTypeLib, cId, cProgID, cClsID, lPro

   ::VertScroll := .F.
   ::Super:Create()
   ::SetScrollTime(0)
   ::LevelFont := Font()
   ::LevelFont:Weight     := 700
   ::LevelFont:Create()
   ::hPen := CreatePen( PS_SOLID, 0, GetSysColor( COLOR_BTNFACE ) )
   ::hPenShadow := CreatePen( PS_SOLID, 0, GetSysColor( COLOR_BTNSHADOW ) )
   ::SetIndent( 15 )
   ::SetItemHeight( ABS( ::Font:Height ) + 9 )

   ::aButtons := { { "All", {} },;
                   { "Components",             {} },;
                   { "Drawing",                {} } }

   AADD( ::aButtons[2][2], { "Label", .T. } )
   AADD( ::aButtons[2][2], { "Subtotal", .T. } )
   AADD( ::aButtons[2][2], { "Total", .T. } )
   AADD( ::aButtons[2][2], { "Image", .T. } )
   AADD( ::aButtons[2][2], { "DataTable", .T. } )
   AADD( ::aButtons[2][2], { "Formula", .T. } )

   AADD( ::aButtons[3][2], { "Line", .T. } )
//   AADD( ::aButtons[3][2], { "Rectangle", .T. } )
//   AADD( ::aButtons[3][2], { "Circle", .T. } )

   FOR n := 2 TO LEN( ::aButtons )
       aSort(::aButtons[n][2],,,{|x, y| x[1] < y[1]})
       
       // Add to "All"
       AEVAL( ::aButtons[n][2], {|a| AADD( ::aButtons[1][2], { a[1], a[2] } ) } )
   NEXT
   aSort(::aButtons[1][2],,,{|x, y| x[1] < y[1]})
   
   ::ImageList := ImageList( Self, 16, 16 ):Create()
   ::ImageList:AddIcon( "ICO_Pointer" )

   nIndex := 2
   FOR n := 1 TO LEN( ::aButtons )

       oItem := ::AddItem( ::aButtons[n][1] )
       oItem:Cargo := .T.
       
       oPtr := oItem:AddItem( "Pointer", 1 )
       oPtr:Action := {|o|IIF( o:Parent:Enabled, ::SetControlCursor(o),) }
       oPtr:Cargo := .T.

       FOR x := 1 TO LEN( ::aButtons[n][2] )
           
           nIcon := ::ImageList:AddImage( "ICO_" + UPPER( ::aButtons[n][2][x][1] ) )
           
           o := oItem:AddItem( ::aButtons[n][2][x][1], IIF( nIcon != NIL, nIndex, -1 ) )
           o:Cargo := ::aButtons[n][2][x][2]
           
           o:PointerItem := oPtr
           IF nIcon != NIL
              nIndex ++
           ENDIF
       NEXT
   NEXT

   ::ImageList:AddImage( "ICO_REPORT" )

   ::SetImageList()
   ::ExpandAll()
   ::Items[1]:Toggle()
   ::Items[1]:EnsureVisible()
   ::Enabled := .F.

RETURN Self

METHOD FindItem( cText ) CLASS ToolBox
   LOCAL oItem, n, i
   FOR n := 2 TO LEN( ::Items )
       FOR i := 1 TO LEN( ::Items[n]:Items )
           IF ::Items[n]:Items[i]:Caption == cText
              RETURN ::Items[n]:Items[i]
           ENDIF
       NEXT
   NEXT
RETURN NIL

METHOD OnMouseLeave() CLASS ToolBox
   ::HoverItem := NIL
   ::InvalidateRect(,.F.)
RETURN NIL

METHOD OnMouseMove( x, y ) CLASS ToolBox
   LOCAL oItem, pt := (struct POINT)
   GetCursorPos( @pt )
   ScreenToClient( ::hWnd, @pt )
   IF ( oItem := ::HitTest( pt:x, pt:y ) ) != NIL .AND. !(oItem == ::HoverItem)
      ::HoverItem := oItem
      //oItem:Select()
   ENDIF
RETURN Self
//---------------------------------------------------------------------------------------------------

METHOD OnEraseBkGnd( hDC ) CLASS ToolBox
   LOCAL nTop, rc, hItem := ::GetLastVisibleItem()
   nTop := 0
   IF !EMPTY( hItem )
      rc := ::GetItemRect( hItem )
      nTop := rc:bottom
      IF nTop >= ::ClientHeight
         RETURN 1
      ENDIF
   ENDIF
   _FillRect( hDC, {0,nTop,::ClientWidth, ::ClientHeight}, ::System:CurrentScheme:Brush:ToolStripPanelGradientEnd )
RETURN 1

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS ToolBox
   LOCAL oItem, tvcd, tvkd
   Super:OnParentNotify( nwParam, nlParam, hdr )
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
RETURN NIL

METHOD DrawItem( tvcd ) CLASS ToolBox
   LOCAL hItem, n, oItem, nState, nBack, nFore, lExpanded, rc, nWidth, nLeft, nRight, nBottom, aAlign, x, y, lHasChildren
   LOCAL aRow, aCol, hOldPen, nAlign, cType, nColor, hOld, hBrush, aRest, cText, hPen, cPaint, nFlags, hBackBrush, nBackColor, hItemBrush
   LOCAL hBorderPen, hSelBrush, hDC

   hDC        := tvcd:nmcd:hdc
   nBackColor := ::System:CurrentScheme:ToolStripPanelGradientEnd
   hSelBrush  := ::System:CurrentScheme:Brush:ButtonCheckedGradientBegin
   hItemBrush := ::System:CurrentScheme:Brush:MenuItemSelected
   hBackBrush := ::System:CurrentScheme:Brush:ToolStripPanelGradientEnd
   hBorderPen := ::System:CurrentScheme:Pen:MenuItemBorder

   rc         := tvcd:nmcd:rc
   oItem      := FindTreeItem( ::Items, tvcd:nmcd:dwItemSpec )
   IF oItem == NIL .OR. rc:Right == 0 .OR. !IsWindowVisible( ::hWnd )
      RETURN NIL
   ENDIF
   nBack      := nBackColor
   nFore      := GetSysColor( COLOR_BTNTEXT )
   nState     := ::GetItemState( tvcd:nmcd:dwItemSpec, TVIF_STATE )
   lExpanded  := nState & TVIS_EXPANDED != 0

   SelectObject( hDC, ::Font:Handle )
   IF oItem:Level == 0
      nBack := GetSysColor( COLOR_BTNSHADOW )
      SelectObject( hDC, ::LevelFont:Handle )
    ELSEIF nState & TVIS_SELECTED != 0 .AND. ::IsWindowEnabled() .AND. oItem:Cargo
      nBack := ::System:CurrentScheme:MenuItemSelected
   ENDIF

   cText := oItem:Caption
   SetBkColor( hDC, nBack )
   SetTextColor( hDC, nFore )

   nLeft   := -::nScroll+14
   nRight  := rc:right
   nBottom := rc:bottom

   aAlign  := _GetTextExtentPoint32( hDC, cText )

   x := nLeft + 10
   y := rc:top + ( ( rc:bottom - rc:top )/2 ) - ( aAlign[2]/2 )

   IF oItem:Level > 0
      x += 5
   ENDIF
   
   _FillRect( hDC, { rc:left, rc:top, rc:right, rc:bottom }, hBackBrush )
   
   IF oItem:Level == 0
      rc:top ++
      FillGradient( hDC, rc, ::System:CurrentScheme:ToolStripGradientEnd, ::System:CurrentScheme:ToolStripGradientBegin )
      rc:top --
      hBrush := SelectObject( hDC, GetStockObject( NULL_BRUSH ) )
      hPen := SelectObject( hDC, ::hPenShadow )
      Rectangle( hDC, rc:left, rc:bottom-1, rc:right, rc:bottom )
      SelectObject( hDC, hPen )
      SelectObject( hDC, hBrush )
   ENDIF

   IF !::IsWindowEnabled() .OR. !oItem:Cargo
      IF oItem:Level > 0
         SetTextColor( hDC, ::System:Colors:GrayText )
      ENDIF
   ENDIF
   IF oItem:Level > 0
      IF oItem:Cargo .AND. ( ( ::HoverItem != NIL .AND. oItem == ::HoverItem ) .OR. ( ::ActiveItem != NIL .AND. oItem == ::ActiveItem ) )//:Level > 0 .AND. nState & TVIS_SELECTED != 0
         hBrush := SelectObject( hDC, hItemBrush )
         hPen := SelectObject( hDC, hBorderPen )
         Rectangle( hDC, rc:left, rc:top, rc:right, rc:bottom )
         SelectObject( hDC, hPen )
         SelectObject( hDC, hBrush )
         SetBkColor( hDC, ::System:CurrentScheme:MenuItemSelected )
      ENDIF
   ENDIF
   
   nFlags := ETO_CLIPPED | ETO_OPAQUE
   IF oItem:Level == 0
      SetBkMode( hDC, TRANSPARENT )
      nFlags := 0
   ENDIF

   IF !( oItem == ::HoverItem ) .AND. nState & TVIS_SELECTED != 0 .AND. ::IsWindowEnabled() .AND. oItem:Cargo
      hBrush := SelectObject( hDC, hSelBrush )
      hPen := SelectObject( hDC, hBorderPen )
      Rectangle( hDC, rc:left, rc:top, rc:right, rc:bottom )
      SelectObject( hDC, hPen )
      SelectObject( hDC, hBrush )
      SetBkColor( hDC, ::System:CurrentScheme:ButtonCheckedGradientEnd )
   ENDIF

   _ExtTextOut( hDC, x, y, nFlags, { rc:left+1, rc:top+1, rc:right-1, rc:bottom-1 }, cText  )

   SetBkMode( hDC, OPAQUE )

   IF oItem:Level > 0
      IF ::IsWindowEnabled() .AND. oItem:Cargo
         ::ImageList:DrawImage( hDC, oItem:ImageIndex, 5, rc:top+2 )
       ELSE
         ::ImageList:DrawDisabled( hDC, oItem:ImageIndex, 5, rc:top+2 )
      ENDIF
   ENDIF

   aRest := { rc:Left, rc:bottom, rc:right, ::ClientHeight }

   IF oItem:Level == 0
      y := rc:top + ( ( rc:bottom - rc:top ) / 2 ) - ( 9/2 )
      DrawMinusPlus( hDC, 5, y, lExpanded, .T., RGB(0,0,0), 9, 9/2 )
   ENDIF
RETURN 0

METHOD OnSelChanged( oItem ) CLASS ToolBox
   IF ::Enabled .AND. LEN( oItem:Items ) == 0
      ::SetControlCursor( oItem )
   ENDIF
RETURN 0

METHOD SetControlCursor( oItem ) CLASS ToolBox
   LOCAL cClass, n, hClass
   LOCAL oApp := __GetApplication()
   ::ActiveItem := oItem
   cClass := oItem:Caption
   ::SetMouseShape( IIF( cClass != "Pointer", MCS_DRAGGING, 0 ) )
   IF cClass == "Pointer"
      ::ActiveItem := NIL
   ENDIF
RETURN NIL

METHOD SetMouseShape( nPos ) CLASS ToolBox
   ::Application:Cursor := IIF( nPos > 0, ::aCursors[ nPos ], NIL )
   SendMessage( ::hWnd, WM_SETCURSOR )
RETURN Self
