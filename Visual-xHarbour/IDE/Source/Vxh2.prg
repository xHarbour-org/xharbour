/*
 * $Id$
 */

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "Debug.ch"
#include "uxTheme.ch"

#define MCS_NONE     0
#define MCS_SIZENWSE 1
#define MCS_SIZEWE   2
#define MCS_SIZENESW 3
#define MCS_SIZENS   4
//#define MCS_SIZENWSE 5
//#define MCS_SIZEWE   6
//#define MCS_SIZENESW 7
//#define MCS_SIZENS   8
#define MCS_SIZEALL  9
#define MCS_ARROW    10
#define MCS_PASTE    11
#define MCS_DRAGGING 12
#define MCS_NO       13

#define MAXBS_INACTIVE 5
#define MINBS_INACTIVE 5
#define MAXBS_DISINACTIVE 8
#define MINBS_DISINACTIVE 8

#define TS_MIN    1
#define TS_TRUE   2
#define TS_DRAW   3

#define DG_ADDCONTROL      1
#define DG_DELCONTROL      2
#define DG_PROPERTYCHANGED 3
#define DG_MOVESELECTION   4
#define DG_FONTCHANGED     5
#define DG_DELCOMPONENT    6

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

CLASS WindowEdit INHERIT WinForm
   DATA InActive            EXPORTED INIT .F.
   DATA PathName            EXPORTED
   DATA GridColor           PROTECTED INIT C_WHITE//C_BLACK
   DATA CtrlMask            EXPORTED
   DATA Control             PROTECTED
   DATA CtrlOldPt           EXPORTED
   DATA MouseDown           EXPORTED  INIT FALSE
   DATA InRect              EXPORTED  INIT 0
   DATA Selected            EXPORTED  INIT {}
   DATA CtrlHover           PROTECTED INIT 0
   DATA SelInitPoint        EXPORTED
   DATA SelEndPoint         EXPORTED
   DATA SelPointSize        EXPORTED  INIT 7
   DATA CtrlParent          EXPORTED
   DATA NewParent           EXPORTED
   DATA OldParent           EXPORTED
   DATA CtrlPos             PROTECTED
   DATA CtrlSizedMoved      EXPORTED INIT .F.
   DATA CurObj              EXPORTED
   DATA Editor              EXPORTED
   DATA lCustom             EXPORTED
   DATA __SelMoved          EXPORTED INIT .F.
   DATA __PrevSelRect       EXPORTED
   ACCESS CtrlMask          INLINE ::Parent:CtrlMask
   
   DATA __lModified         EXPORTED INIT .F.
   DATA __OldName           EXPORTED
   DATA __NewName           EXPORTED
   DATA __LeftSnap          PROTECTED
   DATA __TopSnap           PROTECTED
   DATA __RightSnap         PROTECTED
   DATA __BottomSnap        PROTECTED

   METHOD Init()            CONSTRUCTOR
   METHOD Create()

   METHOD Show()

   METHOD OnWindowPaint()
   METHOD OnEraseBkGnd()
   METHOD OnUserMsg()
   METHOD OnSize()
   METHOD OnMove()
   METHOD OnNCPaint()

   METHOD ControlSelect()
   METHOD CheckMouse()
   METHOD MaskKeyDown()
   METHOD GetSelRect()
   METHOD GetPoints()
   METHOD UpdateSelection()
   METHOD EditClickEvent()
   METHOD SelectControl()
   METHOD AddDefaultMenuItem()
   METHOD StickLeft()
   METHOD StickTop()
   METHOD StickRight()
   METHOD StickBottom()
   METHOD DrawStickyLines()
   METHOD SetWindowText( cText ) INLINE ::Super:SetWindowText(cText), ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW + RDW_NOCHILDREN + RDW_NOERASE )
   METHOD SetFormIcon( cIcon )   INLINE ::Super:SetFormIcon( cIcon ), ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW + RDW_NOCHILDREN + RDW_NOERASE )
ENDCLASS

METHOD OnSize() CLASS WindowEdit
  ::Parent:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW + RDW_NOCHILDREN + RDW_NOERASE )
  Super:OnSize()
RETURN 0

METHOD OnMove() CLASS WindowEdit
  ::Parent:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW + RDW_NOCHILDREN + RDW_NOERASE )
RETURN 0

FUNCTION CntChildren( oObj )
   LOCAL n, nCnt := LEN( oObj:Children )
   FOR n := 1 TO LEN( oObj:Children )
       nCnt += CntChildren( oObj:Children[n] )
   NEXT
RETURN nCnt
//----------------------------------------------------------------------------

METHOD Init( oParent, cFileName, lNew, lCustom ) CLASS WindowEdit
   LOCAL cInitialBuffer, cMain, cProject, n
   DEFAULT lNew TO .T.
   ::Super:Init( oParent )
   ::__lModified := lNew
   ::__IdeImageIndex := 1

   #ifndef VXH_PROFESSIONAL 
      lCustom := .F.
   #endif
   
   ::lCustom     := lCustom
   IF !lCustom
      ::Style   := WS_CHILD | WS_CAPTION | WS_SYSMENU | WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_THICKFRAME | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
      ::xName   := "Form" + XSTR( LEN( ::Application:Project:Forms )+1 )
      ::ClsName := "VXH_FORM_IDE"
    ELSE
      ::Style   := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS

      n := 1
      WHILE ASCAN( ::Application:Project:Forms, {|o| o:xName == "CustomControl" + XSTR( n ) } ) > 0 .OR. ASCAN( ::Application:CControls, {|c| UPPER( STRTRAN( SplitFile(c)[2], ".xfm" ) )  == "CUSTOMCONTROL" + XSTR( n ) } ) > 0
         n++
      ENDDO
      ::xName   := "CustomControl" + XSTR( n )
      ::ClsName := "CCTL"
      ::__lResizeable :=  {.F.,.F.,.F.,.T.,.T.,.T.,.F.,.F.}
   ENDIF
   ::AutoClose := .F.

   ::__ClassInst := __ClsInst( ::ClassH )
   ::__ClassInst:__IsInstance   := .T.

   ::BackgroundImage := FreeImageRenderer( Self ):Create()

   ::Editor := Editor():New(,,,,, ::Application:SourceEditor:oEditor:oDisplay )

   IF cFileName != NIL
      ::Application:SourceTabs:InsertTab( cFileName )
    ELSE
      ::Application:SourceTabs:InsertTab( "  " + ::Name+".prg * ",,, .T. )
   ENDIF
   
   cInitialBuffer := ""
   IF lNew
      cInitialBuffer += '#include "vxh.ch"' + CRLF +;
                        '#include "' + ::Name + '.xfm"' + CRLF +;
                        "//---------------------------------------- End of system code ----------------------------------------//" + CRLF + CRLF

      ::Editor:Load( , cInitialBuffer, .F., .F. )
      ::Editor:lModified := .T.
   ENDIF
   ::Editor:SetExtension( "prg" )

   cMain := ::Application:ProjectPrgEditor:GetBuffer()
   IF AT( "[BEGIN SYSTEM CODE]", cMain ) == 0
      cProject := "//----------------------------------- [BEGIN SYSTEM CODE] ------------------------------------//" + CRLF
      cProject += "   __" + STRTRAN( ::Application:Project:Properties:Name, " ", "_" ) + "( NIL ):Run( "+::Name+"( NIL ) )" + CRLF
      cProject += "//------------------------------------ [END SYSTEM CODE] -------------------------------------//" + CRLF + CRLF
      cProject += "RETURN NIL"+ CRLF+ CRLF
      cMain := STRTRAN( cMain, "RETURN NIL"+ CRLF, cProject )
      ::Application:ProjectPrgEditor:Load( , cMain, .F., .F. )
      ::Application:ProjectPrgEditor:lModified := .T.
   ENDIF

   IF !::Application:Project:DesignPage
      ::Application:Project:DesignPage := .T.
      ::Application:DesignPage:Enable()
   ENDIF

   ::Selected := {{Self}}

   ::Application:ObjectTree:Set( Self, lNew )

RETURN Self


//----------------------------------------------------------------------------

METHOD Create() CLASS WindowEdit
   IF IsWindow( ::hWnd )
      RETURN Self
   ENDIF
   ::Style := ::Style & NOT( WS_OVERLAPPED )
   ::Style := ::Style & NOT( WS_POPUP )
   ::Style := ::Style | WS_CHILD

   ::Super:Create()
   IF !::__lModified
      ::Hide()
   ENDIF
   IF ::Cargo == NIL
      ::Application:FormsTabs:InsertTab( ::Name,,, .F. )
      ::Application:FormsTabs:SetCurSel( ::Application:FormsTabs:GetItemCount() )
      ::Application:FormsTabs:Redraw()
      ::Application:ObjectTree:Set( Self )
   ENDIF

RETURN Self

//----------------------------------------------------------------------------

METHOD Show() CLASS WindowEdit
   IF ::Application:Project:CurrentForm != NIL
      Super:Show()
   ENDIF
   ::CtrlMask:BringWindowToTop()
RETURN Self

//----------------------------------------------------------------------------

METHOD OnUserMsg() CLASS WindowEdit
   SWITCH ::Msg
      CASE WM_USER + 333
           ::Application:ObjectManager:ResetProperties( ::Selected )
           ::Application:EventManager:ResetEvents( ::Selected )
   END
RETURN 0

//----------------------------------------------------------------------------

METHOD UpdateSelection( nKey, lDes ) CLASS WindowEdit
   DEFAULT lDes TO .T.
   DEFAULT nKey TO -1
   
   ::CtrlMask:Clean( , .F., lDes )
   IF !EMPTY( ::Selected ) .AND. ::Selected[1][1]:hWnd == ::hWnd
    ELSE
      RedrawWindow( ::hWnd, , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------

METHOD SelectControl( oControl, lFocus, lRefreshComp ) CLASS WindowEdit
   LOCAL aRect:={,,,}, lComponent := __clsParent( oControl:ClassH, "COMPONENT" )
   DEFAULT lFocus TO .T.

   ::InRect    := -1
   IF !lComponent
      IF !oControl:ClsName == "Application"
         aRect := oControl:GetRectangle()
         aRect[1] -= 4
         aRect[2] -= 4
         aRect[3] += 4
         aRect[4] += 4
      ENDIF
      ::Selected    := { { oControl, aRect, NIL } }

      ::CtrlOldPt := NIL
      ::InvalidateRect()
      ::MouseDown := .F.

      ::CtrlMask:InvalidateRect()
      ::CtrlMask:UpdateWindow()

      IF lFocus
         ::CtrlMask:SetFocus()
      ENDIF
      IF !oControl:ClsName == "CMenuItem"
         ::UpdateSelection()
         ::CtrlMask:ValidateRect()
      ENDIF
      
      ::Application:ObjectManager:ResetProperties( ::Selected )
      ::Application:EventManager:ResetEvents( ::Selected )

      IF !oControl:ClsName == "CMenuItem"
         ::UpdateSelection()
      ENDIF

      IF oControl:ClsName == "VXH_FORM_IDE"
         ::SelInitPoint := NIL
         ::SelEndPoint  := NIL
      ENDIF

    ELSE
      ::CtrlOldPt := NIL
      ::InvalidateRect()
      ::MouseDown := .F.

      ::Selected := {}

      ::CtrlMask:InvalidateRect()
      ::CtrlMask:UpdateWindow()
      IF lFocus
         ::CtrlMask:SetFocus()
      ENDIF
      ::SelInitPoint := NIL
      ::SelEndPoint  := NIL
      ::UpdateWindow()
      ::Application:Yield()
      ::Application:ObjectManager:ResetProperties( {{oControl}},,,,lRefreshComp  )
      ::Application:EventManager:ResetEvents( {{oControl}} )

      ::CtrlMask:SetMouseShape( MCS_NONE )
      IF lComponent .AND. oControl:Button != NIL //.AND. lFocus
         oControl:Button:Select()
      ENDIF

      IF oControl:__xCtrlName IN { "ContextMenu", "ContextStrip" }
         oControl:Show()
      ENDIF
   ENDIF

RETURN Self

//----------------------------------------------------------------------------

METHOD OnEraseBkGnd( hDC ) CLASS WindowEdit
   IF ::Application:ShowGrid != 1
      RETURN 0
   ENDIF
   IF ::bkBrush != NIL
      _FillRect( hDC, { 0,0,::ClientWidth, ::ClientHeight }, ::bkBrush )
      RETURN 1
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------

METHOD OnWindowPaint() CLASS WindowEdit
   LOCAL nFColor, nBColor
   LOCAL cPaint
   LOCAL hDC
   LOCAL i       := 0
   LOCAL j       := 0

   hDC := _BeginPaint( ::hWnd, @cPaint )

   IF ::Application:ShowGrid == 1

      nBColor := SetBkColor( hDC, ::BackColor )
      nFColor := SetTextColor( hDC, ::ForeColor )
      SetBkMode( hDC, TRANSPARENT )
      DrawGrid( hDC, ::CtrlMask:hBmpGrid, ::CtrlMask:xBmpSize, ::CtrlMask:yBmpSize, ::ClientWidth, ::ClientHeight, SRCCOPY )
      SetTextColor( hDC, nFColor )
      SetBkColor( hDC, nBColor )

    ELSE
      IF ::bkBrush != NIL
         _FillRect( hDC, { 0,0,::ClientWidth, ::ClientHeight }, ::bkBrush )
       ELSE
         _FillRect( hDC, { 0,0,::ClientWidth, ::ClientHeight }, GetSysColorBrush( COLOR_BTNFACE ) )
      ENDIF

   ENDIF
   ::__PaintBakgndImage( hDC )
   _EndPaint( ::hWnd, cPaint )
   ValidateRect( ::hWnd )
RETURN 0

//----------------------------------------------------------------------------

METHOD MaskKeyDown( o, nKey ) CLASS WindowEdit
   LOCAL nLen, x, aControl, aRect, lCtrl, lShift, n := 1, lCheck := .T.
   LOCAL aAction
   ( o )
   lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
   lCtrl  := CheckBit( GetKeyState( VK_CONTROL ) , 32768 )

   IF lCtrl
      n := ::CtrlMask:xGrid
   ENDIF

   IF LEN( ::Selected ) > 1
      lShift := .F.
   ENDIF
   IF nKey != VK_CONTROL .AND. nKey != VK_SHIFT
      ::UpdateSelection()
   ENDIF

   DO CASE
      CASE nKey == VK_TAB
           IF LEN( ::Selected ) > 1
              ::Selected := {{ ::Selected[1][1] }}
            ELSEIF LEN( ::Children ) > 0 .AND. LEN( ::Selected ) == 0
              ::Selected := {{ ::Children[1] }}
           ENDIF
           IF ( n := ASCAN( ::Children, {|o| o:hWnd == ::Selected[1][1]:hWnd } ) ) > 0
              IF !lShift
                 n++
                 IF n > LEN( ::Children )
                    n := 1
                 ENDIF
               ELSE
                 n--
                 IF n <= 0
                    n := LEN( ::Children )
                 ENDIF
              ENDIF
              ::SelectControl( ::Children[n] )
           ENDIF
           RETURN 0

      CASE nKey == VK_DELETE
           lCheck := .F.

           IF __clsParent( ::Application:ObjectManager:ActiveObject:ClassH, "COMPONENT" )
              WITH OBJECT ::Application:ObjectManager:ActiveObject
                 ::Application:Project:SetAction( { { DG_DELCONTROL, NIL, 0, 0, .F., :Owner, :__xCtrlName, hb_qWith(), , 1, , ::Application:Components:Current } }, ::Application:Project:aUndo )
              END
/*
              IF ::Application:Components:Current != NIL
                 RETURN ::Application:Components:Current:OnKeyDown( VK_DELETE )
              ENDIF
*/
              RETURN 0
           ENDIF

           IF LEN( ::Selected ) == 1
              IF ::Selected[1][1]:hWnd == ::hWnd
                 IF ::Parent:Children[1] == Self
                    MessageBox( GetActiveWindow(), "Cannot delete "+::Name+" because it's designed to be Main Window", "Delete Form", MB_ICONEXCLAMATION )
                    RETURN 0
                 ENDIF
                 IF MessageBox( GetActiveWindow(), "This WILL DELETE "+::Name+" and its children from the project" + CHR(13) +;
                                                   "Changes to " + ::Name + ".prg **WILL BE LOST** " + CHR(13) +;
                                                   "are you sure ?", "Delete Form", MB_YESNO ) == IDNO
                    RETURN 0
                 ENDIF
              ENDIF
              ::Application:Project:Modified := .T.
              ::__lModified := .T.
           ENDIF

           nLen := LEN( ::Selected )

           aAction := {}
           FOR x := 1 TO LEN( ::Selected )
               AADD( aAction, { DG_DELCONTROL, NIL, ::Selected[x][1]:Left, ::Selected[x][1]:Top, .F., IIF( ::Selected[x][1]:__xCtrlName == "Splitter", ::Selected[x][1]:Owner, ::Selected[x][1]:Parent), ::Selected[x][1]:__xCtrlName, ::Selected[x][1], , nLen,, } )
           NEXT
           
           ::Application:Project:SetAction( aAction, ::Application:Project:aUndo )
           
           ::Selected := {}
           
           RETURN 0
      CASE nKey == VK_DOWN
           // Move selected controls

           IF !EMPTY( ::Selected ) .AND. ::Selected[1][1]:__xCtrlName == "CoolMenuItem"
              ::Selected[1][1]:Parent:OpenPopup( ::Selected[1][1]:Parent:hWnd )
             ELSE
              FOR EACH aControl IN ::Selected
                  IF aControl[1]:__lMoveable
                     aControl[1]:Top += n

                     IF lShift
                        aControl[1]:Height -= n
                     ENDIF

                     aControl[1]:MoveWindow()
                  ENDIF
              NEXT
           ENDIF

      CASE nKey == VK_UP
           // Move selected controls
           FOR EACH aControl IN ::Selected
               IF aControl[1]:__lMoveable
                  aControl[1]:Top -= n

                  IF lShift
                     aControl[1]:Height += n
                  ENDIF

                  aControl[1]:MoveWindow()
               ENDIF
           NEXT

      CASE nKey == VK_LEFT
           // Move selected controls
           FOR EACH aControl IN ::Selected
               IF aControl[1]:__lMoveable
                  aControl[1]:Left -= n

                  IF lShift
                     aControl[1]:Width += n
                  ENDIF

                  aControl[1]:MoveWindow()
               ENDIF
           NEXT

      CASE nKey == VK_RIGHT
           // Move selected controls
           FOR EACH aControl IN ::Selected
               IF aControl[1]:__lMoveable
                  aControl[1]:Left += n

                  IF lShift
                     aControl[1]:Width -= n
                  ENDIF

                  aControl[1]:MoveWindow()
               ENDIF
           NEXT
      OTHERWISE
           lCheck := .F.
           RETURN NIL
   ENDCASE
   IF lCheck .AND. LEN( ::Selected ) > 0
      IF ::Selected[1][1]:__lMoveable
         ::Application:ObjectManager:CheckValue( "Left",   "Position", ::Selected[1][1]:Left )
         ::Application:ObjectManager:CheckValue( "Top",    "Position", ::Selected[1][1]:Top )
         ::Application:ObjectManager:CheckValue( "Width",  "Size",     ::Selected[1][1]:Width )
         ::Application:ObjectManager:CheckValue( "Height", "Size",     ::Selected[1][1]:Height )
         IF ::Selected[1][1] == Self
            ::Parent:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
         ENDIF
      ENDIF
      ::Application:Project:Modified := .T.
      ::__lModified := .T.
      ::UpdateSelection( nKey )

      aRect := ::GetSelRect(.T.)
      IF LEN( ::Selected ) == 1
         ::Application:MainForm:StatusBarPanel7:Caption := XSTR(::Selected[1][1]:Left)+", "+XSTR(::Selected[1][1]:Top)+", "+XSTR(::Selected[1][1]:Width)+", "+XSTR(::Selected[1][1]:Height)
       ELSE
         ::Application:MainForm:StatusBarPanel7:Caption := XSTR(aRect[1])+", "+XSTR(aRect[2])+", "+XSTR(aRect[3])+", "+XSTR(aRect[4])
      ENDIF
   ENDIF
   hb_gcall()
RETURN 0

//----------------------------------------------------------------------------

METHOD ControlSelect( x, y ) CLASS WindowEdit
   LOCAL oControl, aRect, aPt, n, aControl, lChild := .F., lShift, lCtrl
   LOCAL nInRect, lMouse, pt, lToolItem := .F.
   LOCAL nCursor, rc := (struct RECT)

   
   IF ::Application:CurCursor != NIL .OR. ::Application:Project:PasteOn
      //IF ::CtrlParent == NIL
         ::CheckMouse( x, y )
      //ENDIF
      RETURN NIL
   ENDIF

   lCtrl  := CheckBit( GetKeyState( VK_CONTROL ) , 32768 )

   pt := (struct POINT)
   pt:x := x
   pt:y := y
   ClientToScreen( ::CtrlMask:hWnd, @pt )
   oControl := ::GetChildFromPoint( pt )
   DEFAULT oControl TO Self
   IF oControl:ClsName == "ToolBarWindow32"
      ScreenToClient( oControl:hWnd, @pt )

      IF ( n := SendMessage( oControl:hWnd, TB_HITTEST, 0, pt ) ) >= 0
         lToolItem := .T.
      ENDIF
      pt := (struct POINT)
      pt:x := x
      pt:y := y
      ClientToScreen( ::CtrlMask:hWnd, @pt )
   ENDIF

   IF ::InRect == -1 .AND. LEN( ::Selected ) > 0 .AND. ::Selected[1][1]:hWnd != ::hWnd .AND. ( oControl:hWnd == ::Selected[1][1]:hWnd .AND. !lToolItem ).AND. !lCtrl
      IF oControl:ClsName == WC_TABCONTROL
         pt:x := x
         pt:y := y
         ClientToScreen( ::CtrlMask:hWnd, @pt )
         ScreenToClient( oControl:hWnd, @pt )
         IF ( n := oControl:HitTest( pt:x, pt:y ) ) > 0
            oControl:Children[n]:Select()
            oControl := oControl:Children[n]
            nCursor := MCS_ARROW
         ENDIF
       ELSE
         ::MouseDown := .T.

         aRect := ::GetSelRect(.T.)
         IF LEN( ::Selected ) == 1
            ::Application:MainForm:StatusBarPanel7:Caption := XSTR(::Selected[1][1]:Left)+", "+XSTR(::Selected[1][1]:Top)+", "+XSTR(::Selected[1][1]:Width)+", "+XSTR(::Selected[1][1]:Height)
          ELSE
            ::Application:MainForm:StatusBarPanel7:Caption := XSTR(aRect[1])+", "+XSTR(aRect[2])+", "+XSTR(aRect[3])+", "+XSTR(aRect[4])
         ENDIF

         RETURN NIL
      ENDIF
   ENDIF

   IF ::InRect <= 0
      ::MouseDown := .T.
      // a control is being selected
      aPt := { x, y }

      lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )

      TRY
         IF oControl:Owner != NIL .AND. oControl:Owner:ClsName == "CoolBarBand"
            oControl := oControl:Owner:Parent:GetChildFromPoint( pt )
         ENDIF
       CATCH
      END
      
      IF oControl:hWnd == ::hWnd
         ::SelInitPoint := { x,y }
         ::SelEndPoint  := { x,y }
      ENDIF

      nCursor := MCS_SIZEALL
      IF oControl:ClsName == WC_TABCONTROL //__xCtrlName == "TabControl"
         pt:x := x
         pt:y := y
         ClientToScreen( ::CtrlMask:hWnd, @pt )
         ScreenToClient( oControl:hWnd, @pt )
         IF ( n := oControl:HitTest( pt:x, pt:y ) ) > 0
            oControl:Children[n]:Select()
            oControl := oControl:Children[n]
            nCursor := MCS_ARROW
         ENDIF
      ENDIF

      ::UpdateSelection()

      IF oControl:ClsName == "ToolBarWindow32"
         ScreenToClient( oControl:hWnd, @pt )
         IF ( n := SendMessage( oControl:hWnd, TB_HITTEST, 0, pt ) ) >= 0
            oControl:nPressed := n
            IF oControl:__xCtrlName != "CoolMenu"
               oControl := oControl:aItems[n+1]
             ELSE
               oControl:lKeyboard := .T.
               IF oControl:aItems[n+1]:Caption != "[ Add New Item ]"
                  IF !EMPTY( ::Selected ) .AND. ::Selected[1][1]:__xCtrlName == "CoolMenuItem" 
                     
                     IF ::Selected[1][1]:Id==oControl:aItems[n+1]:Id
                     
                        oControl:OpenPopup( oControl:hWnd )
                        RETURN 0
                      ELSE
                        SendMessage( oControl:hWnd, TB_SETHOTITEM, -1, 0 )
                        SendMessage( oControl:hWnd, TB_SETHOTITEM, n, 0 )
                        oControl := oControl:aItems[n+1]
                     ENDIF
                   ELSE
                     SendMessage( oControl:hWnd, TB_SETHOTITEM, n, 0 )
                     oControl := oControl:aItems[n+1]
                  ENDIF
                ELSE
                  ::AddDefaultMenuItem( oControl:aItems[n+1]:Parent )
               ENDIF
            ENDIF
         ENDIF
       ELSEIF oControl:ClsName == "MenuStripItem" .OR. ( oControl:ClsName == "ToolStripButton" .AND. oControl:DropDown > 1 )
         IF oControl:GenerateMember
            IF ASCAN( oControl:Children, {|o|o:Caption == "[ Add New Item ]" } ) == 0
               WITH OBJECT MenuStripItem()
                  :GenerateMember := .F.
                  :Init( oControl )
                  :Caption   := "[ Add New Item ]"
                  :Font:Bold := .T.
                  :Action    := {|o| ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., o:Parent, "MenuStripItem",,,1, {}, } }, ::Application:Project:aUndo ) }
                  :Create()
               END
            ENDIF
            ::Selected := { { oControl, aRect, NIL } }
            ::Application:ObjectManager:ResetProperties( ::Selected )
            ::Application:EventManager:ResetEvents( ::Selected )
            oControl:__OpenMenu()
          ELSE
            EVAL( oControl:Action, oControl )
         ENDIF
         RETURN 0
       ELSEIF oControl:ClsName == "ToolStripButton" .AND. oControl:Caption == "[ Add New Item ]" .AND. oControl:Action != NIL
         EVAL( oControl:Action, oControl )
         RETURN 0
      ENDIF

      ::CtrlPos := { oControl:Left, oControl:Top }

      IF ( ASCAN( ::Selected, {|a| a[1]:Name == oControl:Name .AND. a[1]:hWnd == oControl:hWnd} ) ) == 0 .OR. lCtrl
         lMouse  := .T.
         nInRect := -1

         IF LEN( ::Selected ) > 0
            IF ( ::Selected[1][1]:__xCtrlName == "CMenuItem" .OR. ::Selected[1][1]:__xCtrlName == "ToolButton" ) .AND. !::MouseDown
               ::MouseDown := .F.
               RETURN 0
            ENDIF
          ELSE
            lCtrl := .F.
         ENDIF

         IF lCtrl
            IF LEN( ::Selected ) > 0 .AND. ( __clsParent( ::Selected[1][1]:ClassH, "COMPONENT" ) .OR. __clsParent( oControl:ClassH, "COMPONENT" ) )
               lCtrl := .F.
            ENDIF
         ENDIF

         IF lShift .AND. ::Selected[1][1]:hWnd != ::hWnd
            IF ASCAN( ::Selected, {|a|a[1]:Parent:hWnd == oControl:Parent:hWnd} ) == 0
               ::MouseDown := .F.
               ::InRect    := nInRect
               RETURN -2
            ENDIF
            
            IF ASCAN( ::Selected, {|a| a[1]:hWnd == oControl:Parent:hWnd} ) == 0
               AADD( ::Selected, { oControl, aRect, NIL } )
            ENDIF
            aRect := ::GetSelRect(.T.,.F.,.F.)
            FOR EACH oControl IN ::Selected[1][1]:Parent:Children
                aControl := oControl:GetRectangle()
                IF _IntersectRect( aControl, aRect ) != NIL .AND. ASCAN( ::Selected, {|a| a[1]:hWnd == oControl:hWnd} ) == 0
                   AADD( ::Selected, { oControl, aControl, NIL } )
                   lChild := .T.
                ENDIF
            NEXT
            
          ELSEIF lCtrl
            IF ASCAN( ::Selected, {|a|a[1]:Parent:hWnd == oControl:Parent:hWnd} ) > 0
               IF ( n := ASCAN( ::Selected, {|a|a[1]:hWnd == oControl:hWnd} ) ) > 0
                  IF LEN( ::Selected ) > 1
                     ADEL( ::Selected, n, TRUE )
                     nInRect := 0
                     lMouse  := .F.
                     lChild  := .T.
                   ELSE
                     ::MouseDown := .F.
                     ::InRect    := nInRect
                     RETURN 0
                  ENDIF
                ELSEIF ASCAN( ::Selected, {|a| a[1]:hWnd == oControl:Parent:hWnd} ) == 0
                  AADD( ::Selected, { oControl, aRect, NIL } )
                  lChild := .T.
               ENDIF
             ELSE
               ::MouseDown := .F.
               ::InRect    := nInRect
               RETURN -2
            ENDIF
          ELSEIF LEN( ::Selected ) == 1
            IF !(::Selected[1][1]:Name == oControl:Name) .OR. !(oControl:hWnd == ::Selected[1][1]:hWnd )
               ::Selected    := { { oControl, aRect, NIL } }
               lChild := .T.
               IF oControl:hWnd == ::hWnd
                  nInRect := -2
                  ::Parent:InvalidateRect()

                  ::InRect := -1
                  pt:x := aPt[1]
                  pt:y := aPt[2]

                  ClientToScreen( ::CtrlMask:hWnd, @pt )
                  ScreenToClient( ::hWnd, @pt )

                  ::UpdateSelection()
                  ::Application:Yield()
                  IF pt:x < 0 .OR. pt:y < 0
                     ::CtrlMask:SetMouseShape( MCS_SIZEALL )
                   ELSE
                     ::MouseDown := .F.
                     ::Application:ObjectManager:ResetProperties( ::Selected )
                     ::Application:EventManager:ResetEvents( ::Selected )
                     RETURN 0
                  ENDIF
               ENDIF
            ENDIF

          ELSE
            ::Selected    := { { oControl, aRect, NIL } }
            lChild := .T.
         ENDIF

         IF lChild
            ::CtrlMask:SetMouseShape( nCursor )
            ::CtrlMask:InvalidateRect(,.F.)
            ::CtrlOldPt := NIL

            ::UpdateWindow()
            ::CtrlMask:UpdateWindow()
            ::Application:Yield()

            ::MouseDown := lMouse
            ::InRect    := nInRect

            IF !lCtrl
               ::Application:ObjectManager:ResetProperties( ::Selected )
               ::Application:EventManager:ResetEvents( ::Selected )
               IF ::MouseDown
                  ::CheckMouse( x, y )
               ENDIF
            ENDIF

         ENDIF
       ELSEIF ::Selected[1][1] == Self .AND. ::InRect != -2
         ::MouseDown := .F.
         RETURN 0
      ENDIF

      ::CtrlMask:SetMouseShape( nCursor )

      IF LEN( ::Selected ) > 0
         aRect := ::GetSelRect(.T.)
         IF LEN( ::Selected ) == 1
            ::Application:MainForm:StatusBarPanel7:Caption := XSTR(::Selected[1][1]:Left)+", "+XSTR(::Selected[1][1]:Top)+", "+XSTR(::Selected[1][1]:Width)+", "+XSTR(::Selected[1][1]:Height)
          ELSE
            ::Application:MainForm:StatusBarPanel7:Caption := XSTR(aRect[1])+", "+XSTR(aRect[2])+", "+XSTR(aRect[3])+", "+XSTR(aRect[4])
         ENDIF
      ENDIF
    ELSEIF LEN( ::Selected ) >= ::CtrlHover
      // a point is clicked, i have to select the control and release the rest
      n := LEN( ::Selected )
      ::Selected := { ACLONE( ::Selected[ ::CtrlHover ] ) }

      IF n > 1
         ::CtrlOldPt := NIL
         ::InvalidateRect()
         ::CtrlMask:InvalidateRect()
         ::CtrlMask:UpdateWindow()
         ::CtrlMask:SetFocus()
      ENDIF
      ::MouseDown := .T.

      ::Application:MainForm:StatusBarPanel7:Caption := XSTR(::Selected[1][1]:Left)+", "+XSTR(::Selected[1][1]:Top)+", "+XSTR(::Selected[1][1]:Width)+", "+XSTR(::Selected[1][1]:Height)
   ENDIF
   ::CtrlMask:SetFocus()

RETURN 0

//----------------------------------------------------------------------------

METHOD GetSelRect( lPure, lMask, lConvert ) CLASS WindowEdit
   LOCAL aControl, aRect, nLeft, nTop, nRight, nBottom, aPoints

   FOR EACH aControl IN ::Selected
       IF aControl[1]:Parent != NIL .AND. ! aControl[1]:ClassName IN {"CMENUITEM", "MENUSTRIPITEM"}
          aPoints := ::GetPoints( aControl[1], lPure, lMask, lConvert )

          DEFAULT nLeft   TO aPoints[1][1]
          DEFAULT nTop    TO aPoints[1][2]
          DEFAULT nRight  TO aPoints[5][1]+aPoints[5][3]
          DEFAULT nBottom TO aPoints[5][2]+aPoints[5][4]

          nLeft   := MIN( nLeft  , aPoints[1][1] )
          nTop    := MIN( nTop   , aPoints[1][2] )
          nRight  := MAX( nRight , aPoints[5][1]+aPoints[5][3] )
          nBottom := MAX( nBottom, aPoints[5][2]+aPoints[5][4] )
       ENDIF
   NEXT
   IF nTop != NIL
      aRect := { nLeft, nTop, nRight, nBottom }
   ENDIF

RETURN aRect

//----------------------------------------------------------------------------

METHOD GetPoints( oControl, lPure, lMask, lConvert ) CLASS WindowEdit
   LOCAL aRect, rc, aPoints, n := ::SelPointSize, pt := (struct POINT)
   DEFAULT lPure TO .F.
   DEFAULT lMask TO .T.
   DEFAULT lConvert TO .T.
   IF lPure
      n := 0
   ENDIF
   
   IF oControl:ClsName  == "ToolBarWindow32"
      oControl:GetWindowRect()
   ENDIF

   aRect := NIL
   IF lPure
      TRY
         aRect := ACLONE( oControl:__TempRect )
         lConvert := .F.
      CATCH   
      END
   ENDIF
   DEFAULT aRect TO ACLONE( oControl:GetRectangle() )

   IF __ObjHasMsg( oControl, "Owner" ) .AND. oControl:Owner != NIL .AND. oControl:__xCtrlName != "Splitter"
      rc := oControl:Owner:GetRectangle()
      aRect[1] := rc[1]
      aRect[2] := rc[2]
   ENDIF

   pt:x := aRect[1]
   pt:y := aRect[2]
   
   IF lConvert
      IF !lMask
         ClientToScreen( oControl:Parent:hWnd, @Pt )
         ScreenToClient( ::hWnd, @Pt )
       ELSE
         ClientToScreen( oControl:Parent:hWnd, @Pt )
         ScreenToClient( ::CtrlMask:hWnd, @Pt )
      ENDIF
   ENDIF

   aRect[1] := pt:x - IIF( oControl:hWnd != ::hWnd, oControl:Parent:HorzScrollPos, 0 )
   aRect[2] := pt:y - IIF( oControl:hWnd != ::hWnd, oControl:Parent:VertScrollPos, 0 )
   aRect[3] := aRect[1] + oControl:Width
   aRect[4] := aRect[2] + oControl:Height

   aPoints := { {aRect[1]-n, aRect[2]-n, n, n                         },; // left top
                {aRect[1]-n, aRect[2]+((aRect[4]-aRect[2]-n)/2), n, n },; // left
                {aRect[1]-n, aRect[4], n, n                           },; // left bottom
                {aRect[1]+((aRect[3]-aRect[1]-n)/2), aRect[4], n, n   },; // bottom
                {aRect[3], aRect[4], n, n                             },; // right bottom
                {aRect[3], aRect[2]+((aRect[4]-aRect[2]-n)/2), n, n   },; // right
                {aRect[3], aRect[2]-n, n, n                           },; // right top
                {aRect[1]+((aRect[3]-aRect[1]-n)/2), aRect[2]-n, n, n } } // top
RETURN aPoints

//----------------------------------------------------------------------------

METHOD CheckMouse( x, y, lRealUp, nwParam, lOrderMode ) CLASS WindowEdit
   LOCAL aControl, aPt := { x, y }, aPoint, n, aRect, aPoints, oControl, aSelRect, xOld, yOld
   LOCAL nLeft, nTop, nRight, nBottom, nWidth, nHeight, nPlus, cClass, lLeft, lTop, lWidth, lHeight
   LOCAL aSel, pt, pt2, oParent, nCursor, nTab, z, aControls, aSnap, nSnap, hDef, nFor, aSelected
   ( nwParam )
   pt := (struct POINT)
   pt:x := x
   pt:y := y

   pt2 := (struct POINT)

   DEFAULT lOrderMode TO .F.
   DEFAULT lRealUp TO .F.
   IF ::Application:CurCursor != NIL .or. ::Application:Project:PasteOn .OR. lOrderMode

      ClientToScreen( ::CtrlMask:hWnd, @pt )

      oControl := ::GetChildFromPoint( pt )
      DEFAULT oControl TO Self

      cClass := oControl:ClsName
      IF oControl:ClsName != REBARCLASSNAME
         cClass := oControl:Parent:ClsName
      ENDIF


      IF !lOrderMode      
         IF ( oControl:IsContainer .OR. ( VALTYPE( ::Application:CurCursor ) == "C" .AND. ( ::Application:CurCursor == "Splitter" .AND. !oControl:ClsName == "TabPage" ) ) )
            IF !( ::CtrlParent == oControl )
               ::CtrlParent := oControl

               ::UpdateSelection()
               ::Selected    := { { oControl, , NIL } }
               ::CtrlOldPt := NIL
               ::InvalidateRect()
               ::InRect    := -1
               oControl:Parent:InvalidateRect()
               ::CtrlMask:InvalidateRect()
               ::CtrlMask:UpdateWindow()
               ::UpdateSelection()

               ::Application:MainForm:StatusBarPanel7:Caption := oControl:Name
            ENDIF
          ELSE
            IF !::Application:MainForm:StatusBarPanel7:Caption == ::Name
               ::Application:MainForm:StatusBarPanel7:Caption := ::Name
            ENDIF
            ::CtrlParent := Self
         ENDIF
       ELSEIF !( ::CurObj == oControl ) .AND. !( oControl == Self )
         ::Application:MainForm:StatusBarPanel7:Caption := "Next Tab " + XSTR( oControl:Parent:__CurrentPos )
         ::CurObj := oControl

         ::UpdateSelection()
         ::Selected    := { { oControl, , NIL } }
         ::CtrlOldPt := NIL
         ::InvalidateRect()
         ::InRect    := -1
         oControl:Parent:InvalidateRect()
         ::CtrlMask:InvalidateRect()
         ::CtrlMask:UpdateWindow()
         ::UpdateSelection()
       ELSEIF lOrderMode .AND. ( oControl == Self )
         ::Application:MainForm:StatusBarPanel7:Caption := "No Order for " + oControl:Name
      ENDIF
      RETURN 0
   ENDIF

   IF LEN( ::Selected ) > 0 .AND. lRealUp .AND. ! __clsParent( ::Selected[1][1]:ClassH, "COMPONENT" ) .AND. ::Selected[1][1]:__xCtrlName != "Application"
      IF ::Selected[1][1]:__Temprect != NIL
         nLeft := ::Selected[1][1]:__Temprect[1]
         nTop  := ::Selected[1][1]:__Temprect[2]
       ELSE
         nLeft := ::Selected[1][1]:Left
         nTop  := ::Selected[1][1]:Top
      ENDIF
      
      lLeft   := ::Application:ObjectManager:CheckValue( "Left",   "Position", nLeft )
      lTop    := ::Application:ObjectManager:CheckValue( "Top",    "Position", nTop  )
      lWidth  := ::Application:ObjectManager:CheckValue( "Width",  "Size",     ::Selected[1][1]:Width )
      lHeight := ::Application:ObjectManager:CheckValue( "Height", "Size",     ::Selected[1][1]:Height )
      
      ::__LeftSnap   := NIL
      ::__TopSnap    := NIL
      ::__RightSnap  := NIL
      ::__BottomSnap := NIL
      
      TRY
         IF ::Selected[1][1] == Self
            ::Parent:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
         ENDIF
       CATCH
      END  
      ::CtrlPos := NIL
      IF ( !lLeft .OR. !lTop .OR. !lWidth .OR. !lHeight ) .AND. ( !::Application:Project:Modified .OR. ::NewParent != NIL .OR. ::OldParent != NIL )

         IF ::NewParent != NIL
            hDef := BeginDeferWindowPos( LEN( ::Selected ) )
            FOR n := 1 TO LEN( ::Selected )
                IF ::Selected[n][1]:__TempRect != NIL
                   pt:x := ::Selected[n][1]:__TempRect[1]
                   pt:y := ::Selected[n][1]:__TempRect[2]
                 ELSE
                   pt:x := ::Selected[n][1]:Left
                   pt:y := ::Selected[n][1]:Top 
                ENDIF

                nTab := ::Selected[n][1]:xTabOrder
                
                ClientToScreen( ::hWnd, @pt )
                ScreenToClient( ::NewParent:hWnd, @pt )
                
                ::Selected[n][1]:SetParent( ::NewParent )
                SetParent( ::Selected[n][1]:hWnd, ::NewParent:hWnd )

                ::Selected[n][1]:TreeItem:SetOwner( ::NewParent:TreeItem )

                DeferWindowPos( hDef, ::Selected[n][1]:hWnd, , pt:x, pt:y, ::Selected[n][1]:Width, ::Selected[n][1]:Height, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER )

                IF ::NewParent:hWnd == ::OldParent:hWnd
                   ::Selected[n][1]:TabOrder := nTab
                ENDIF
            NEXT
            EndDeferWindowPos( hDef )
            
            FOR z := 1 TO LEN( ::NewParent:Children )
                ::NewParent:Children[z]:xTabOrder := z
                ::NewParent:Children[z]:__ClassInst:xTabOrder := z
            NEXT
            ::Application:ObjectManager:CheckValue( "Left",   "Position", ::Selected[1][1]:Left ) //+ IIF( __ObjHasMsg( ::Selected[1][1]:Parent, "HorzScrollPos" ), ::Selected[1][1]:Parent:HorzScrollPos, 0 ) )
            ::Application:ObjectManager:CheckValue( "Top",    "Position", ::Selected[1][1]:Top ) //+ IIF( __ObjHasMsg( ::Selected[1][1]:Parent, "VertScrollPos" ), ::Selected[1][1]:Parent:VertScrollPos, 0 ) )
            ::Application:ObjectManager:CheckValue( "Width",  "Size",     ::Selected[1][1]:Width )
            ::Application:ObjectManager:CheckValue( "Height", "Size",     ::Selected[1][1]:Height )
          ELSEIF ::OldParent != NIL
            hDef := BeginDeferWindowPos( LEN( ::Selected ) )
            FOR n := 1 TO LEN( ::Selected )
                IF ::Selected[n][1]:__TempRect != NIL
                   pt:x := ::Selected[n][1]:__TempRect[1]
                   pt:y := ::Selected[n][1]:__TempRect[2]
                 ELSE
                   pt:x := ::Selected[n][1]:Left
                   pt:y := ::Selected[n][1]:Top 
                ENDIF
                
                ClientToScreen( ::hWnd, @pt )
                ScreenToClient( ::OldParent:hWnd, @pt )
                
                SetParent( ::Selected[n][1]:hWnd, ::OldParent:hWnd )
                //::Selected[n][1]:MoveWindow( pt:x, pt:y,,,.T.)
                DeferWindowPos( hDef, ::Selected[n][1]:hWnd, , pt:x, pt:y, ::Selected[n][1]:Width, ::Selected[n][1]:Height, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER )
                ::Selected[n][1]:__TempRect := NIL
            NEXT
            EndDeferWindowPos( hDef )
         ENDIF
         ::Application:Project:Modified := .T.
         ::__lModified := .T.
      ENDIF

      IF ( !lLeft .OR. !lTop .OR. !lWidth .OR. !lHeight ) .AND. ::__SelMoved
         aRect := {}
         aControls := {}
         FOR n := 1 TO LEN( ::Selected )
             nLeft   := ::Selected[n][1]:Left
             nTop    := ::Selected[n][1]:Top   
             nWidth  := ::Selected[n][1]:Width 
             nHeight := ::Selected[n][1]:Height
             AADD( aRect, { nLeft, nTop, nWidth, nHeight } )
             AADD( aControls, ::Selected[n][1] )
         NEXT
         ::Application:Project:Modified := .T.
         ::__lModified := .T.
   
         ::Application:Project:SetAction( { { DG_MOVESELECTION,;
                                            aControls,;
                                            aRect,;
                                            ::__PrevSelRect,;
                                            0,;
                                            ::OldParent } }, ::Application:Project:aUndo )
      ENDIF
      ::__SelMoved := .F.

      ::NewParent := NIL
      ::OldParent := NIL

   ENDIF
   IF !::MouseDown .AND. !( ::Application:MainForm:StatusBarPanel7:Caption == "Ready" )
      ::Application:MainForm:StatusBarPanel7:Caption := "Ready"
   ENDIF
//   ::Application:MainForm:StatusBarPanel7:Caption := ""

   IF LEN( ::Selected ) > 0 .AND. !::MouseDown .AND. !::Application:Project:PasteOn
      IF lRealUp
         ::CtrlMask:DrawBand := .T.
         IF ::InRect > 0 .OR. ::Selected[1][1]:__CustomOwner
            ::UpdateSelection()
         ENDIF
      ENDIF

      ::InRect := 0
      ::CtrlHover := 1
      
      nFor := 1
      FOR EACH aControl IN ::Selected
          IF aControl[1]:Parent != NIL .AND. ! __clsParent( aControl[1]:ClassH, "COMPONENT" ) .AND. ! aControl[1]:ClassName IN {"CMENUITEM", "MENUSTRIPITEM"}
             aPoints := ::GetPoints( aControl[1] )
             FOR x := 1 TO LEN( aPoints )

                 aPoint := ACLONE( aPoints[x] )

                 aPoint[3] += aPoint[1]
                 aPoint[4] += aPoint[2]

                 IF !aControl[1]:__CustomOwner .AND. aControl[1]:__lResizeable[x] .AND. _PtInRect( aPoint, aPt )
                    ::InRect := x
                    ::CtrlMask:SetMouseShape( x )
                    EXIT
                 ENDIF

                 DEFAULT nLeft   TO aPoint[1]
                 DEFAULT nTop    TO aPoint[2]
                 DEFAULT nRight  TO aPoint[3]
                 DEFAULT nBottom TO aPoint[4]

                 nLeft   := MIN( nLeft  , aPoint[1] )
                 nTop    := MIN( nTop   , aPoint[2] )
                 nRight  := MAX( nRight , aPoint[3] )
                 nBottom := MAX( nBottom, aPoint[4] )

                 IF aControl[1]:__lMoveable .AND. _PtInRect( { nLeft   + ::SelPointSize,;
                                 nTop    + ::SelPointSize,;
                                 nRight  - ::SelPointSize,;
                                 nBottom - ::SelPointSize }, aPt )
                    IF aControl[1]:hWnd != ::hWnd
                       ::InRect := -1
                       ::CtrlMask:SetMouseShape( MCS_SIZEALL )

                     ELSE
                       ::InRect := -1
                       pt:x := aPt[1]
                       pt:y := aPt[2]
                       ClientToScreen( ::CtrlMask:hWnd, @pt )
                       ScreenToClient( ::hWnd, @pt )
                       ::CtrlMask:SetMouseShape( MCS_ARROW)//SIZEALL )
                       IF pt:x < 0 .OR. pt:y < 0
                          ::InRect := -2
                       ENDIF
                       EXIT
                    ENDIF

                    IF aControl[1]:ClsName == WC_TABCONTROL //__xCtrlName == "TabControl"
                       pt:x := aPt[1]
                       pt:y := aPt[2]
                       ClientToScreen( ::CtrlMask:hWnd, @pt )
                       ScreenToClient( aControl[1]:hWnd, @pt )
                       IF ( n := aControl[1]:HitTest( pt:x, pt:y ) ) > 0
                           ::CtrlMask:SetMouseShape( MCS_ARROW )
                           EXIT
                       ENDIF
                    ENDIF

                 ENDIF

             NEXT
             IF ::InRect > 0
                ::CtrlHover := nFor
                EXIT
             ENDIF
          ENDIF
          nFor++
      NEXT
      IF ::InRect == 0 .AND. !::Application:Project:PasteOn
         ::CtrlMask:SetMouseShape( MCS_ARROW )
      ENDIF

      IF lRealUp .AND. LEN( ::Selected ) == 1 .AND. ::Selected[1][1]:hWnd == ::hWnd .AND. ::SelInitPoint != NIL .AND. ::SelEndPoint != NIL
         ::Selected := {}
         aSelRect := { ::SelInitPoint[1], ::SelInitPoint[2], ::SelEndPoint[1], ::SelEndPoint[2] }
         IF aSelRect[3] < aSelRect[1]
            aSelRect[1] := ::SelEndPoint[1]
            aSelRect[3] := ::SelInitPoint[1]
         ENDIF
         IF aSelRect[4] < aSelRect[2]
            aSelRect[2] := ::SelEndPoint[2]
            aSelRect[4] := ::SelInitPoint[2]
         ENDIF

         FOR EACH oControl IN ::Children
             IF oControl != NIL
                aControl := _GetWindowRect( oControl:hWnd )
                aPt := { aControl[1], aControl[2] }
                _ScreenToClient( ::CtrlMask:hWnd, aPt )
                aControl[1] := aPt[1]
                aControl[2] := aPt[2]
                aPt := { aControl[3], aControl[4] }
                _ScreenToClient( ::CtrlMask:hWnd, aPt )
                aControl[3] := aPt[1]
                aControl[4] := aPt[2]

                IF _IntersectRect( aControl, aSelRect ) != NIL
                   aRect := { oControl:Left - ::SelPointSize,;
                              oControl:Top  - ::SelPointSize,;
                              oControl:Left + oControl:Width  + ::SelPointSize,;
                              oControl:Top  + oControl:Height + ::SelPointSize }

                   AADD( ::Selected, { oControl, aRect, NIL } )
                   ::InRect := -1
                   //oControl:InvalidateRect()
                   oControl:RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                ENDIF
             ENDIF
         NEXT
         aSelRect := ::GetSelRect()
         ::SelInitPoint := NIL
         ::SelEndPoint  := NIL
         ::InvalidateRect( ,.F.)
         ::CtrlMask:InValidateRect( aSelRect, .F. )

         ::CtrlMask:UpdateWindow()
         ::Parent:InvalidateRect()

         IF LEN( ::Selected ) == 1 //.AND. ::Selected[1][1]:__xCtrlName == "GroupBox"
            ::Application:ObjectManager:ResetProperties( ::Selected )
            ::Application:EventManager:ResetEvents( ::Selected )
         ENDIF
         IF EMPTY( ::Selected )
            ::Selected := { { Self } }
         ENDIF
      ENDIF

    ELSEIF ::MouseDown

      IF LEN( ::Selected ) > 0
         aSelected := ACLONE( ::Selected )
         IF aSelected[1][1]:__CustomOwner
            aSelected := { { aSelected[1][1]:GetCCTL() } }
         ENDIF
         
         IF ( aSelected[1][1]:__xCtrlName == "CoolMenu" .AND. aSelected[1][1]:Owner != NIL ) .OR. aSelected[1][1]:__xCtrlName == "ToolButton"
            RETURN NIL
         ENDIF
         IF aSelected[1][1]:__xCtrlName == "DataTable" .AND. ::InRect > 0
            RETURN NIL
         ENDIF
         IF LEN( aSelected ) == 1
            IF ::InRect == -1 .AND. !aSelected[1][1]:__lMoveable
               ::CtrlMask:SetMouseShape( MCS_NONE )
               RETURN NIL
            ENDIF
            IF ::InRect > 0 .AND. !aSelected[1][1]:__lResizeable[ ::InRect ]
               ::CtrlMask:SetMouseShape( MCS_NONE )
               RETURN NIL
            ENDIF
         ENDIF

         // select rectangle, must show the rectangle
         IF ::SelInitPoint != NIL .AND. ( ::InRect == 0 .OR. ::InRect == -1 ) .AND. aSelected[1][1]:hWnd == ::hWnd .AND. x - ::Left > 0 .AND. x - ::Left <= ::Width .AND. y - ::Top > 0 .AND. y - ::Top <= ::Height
            IF x-1 <= ::Left .OR. y-1 <= ::Top .OR. x+1 >= ::Left+::Width .OR. y+1 >= ::Top+::Height
               ::CtrlMask:DrawSelRect(.T.)
               ::SelInitPoint := NIL
               ::SelEndPoint := NIL
               RETURN 0
            ENDIF

            ::CtrlMask:DrawSelRect(.F.)
            ::SelEndPoint := { x,y }

            aSelRect := { ::SelInitPoint[1], ::SelInitPoint[2], ::SelEndPoint[1], ::SelEndPoint[2] }
            IF aSelRect[3] < aSelRect[1]
               aSelRect[1] := ::SelEndPoint[1]
               aSelRect[3] := ::SelInitPoint[1]
            ENDIF
            IF aSelRect[4] < aSelRect[2]
               aSelRect[2] := ::SelEndPoint[2]
               aSelRect[4] := ::SelInitPoint[2]
            ENDIF

            FOR EACH oControl IN ::Children
                aControl := _GetWindowRect( oControl:hWnd )
                aPt := { aControl[1], aControl[2] }
                _ScreenToClient( ::CtrlMask:hWnd, aPt )
                aControl[1] := aPt[1]
                aControl[2] := aPt[2]
                aPt := { aControl[3], aControl[4] }
                _ScreenToClient( ::CtrlMask:hWnd, aPt )
                aControl[3] := aPt[1]
                aControl[4] := aPt[2]
            NEXT

            ::CtrlMask:SetMouseShape( MCS_ARROW )
            ::CtrlMask:DrawSelRect(.F.)
            RETURN 0
          ELSE
            IF ::InRect > 0
               ::CtrlMask:SetMouseShape( ::InRect )
            ENDIF
         ENDIF
         
         // move/size the control, selected controls ???
         aPt := { x, y }
         _ClientToScreen( ::CtrlMask:hWnd, aPt )
         _ScreenToClient( aSelected[1][1]:Parent:hWnd, aPt )
         x := aPt[1]
         y := aPt[2]

         DEFAULT ::CtrlOldPt TO { x, y }
         IF ::CtrlOldPt[1] != x .OR. ::CtrlOldPt[2] != y

            ::__lModified := .T.
            ::Application:Project:Modified := .T.

            IF !::__SelMoved
               ::__PrevSelRect := {}
               FOR n := 1 TO LEN( aSelected )
                   nLeft   := aSelected[n][1]:Left
                   nTop    := aSelected[n][1]:Top   
                   nWidth  := aSelected[n][1]:Width 
                   nHeight := aSelected[n][1]:Height
                   AADD( ::__PrevSelRect, { nLeft, nTop, nWidth, nHeight } )
               NEXT
               ::__SelMoved := .T.
            ENDIF
            
            aRect := aSelected[1][1]:GetRectangle()
            
            IF ::Application:ShowGrid == 1 .AND. aSelected[1][1]:hWnd != ::hWnd
               aRect[1] := Snap( aRect[1], ::CtrlMask:xGrid )
               aRect[2] := Snap( aRect[2], ::CtrlMask:xGrid )
            ENDIF
            xOld := x
            yOld := y

            IF ::Application:ShowGrid == 1 .AND. aSelected[1][1]:hWnd != ::hWnd
               x  := Snap( x, ::CtrlMask:xGrid )
               y  := Snap( y, ::CtrlMask:yGrid )
            ENDIF
            
            IF ::CtrlMask:DrawBand
               ::CtrlMask:DrawBand := .F.
               ::UpdateSelection()

               aSel := ::GetSelRect(.F.,.F.)

               IF aSel[3] >= ::ClientWidth .OR. aSel[1] <= 0 .OR. aSel[4] >= ::ClientHeight .OR. aSel[2] <= 0
                  ::RedrawWindow( aSel, , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
               ENDIF
            ENDIF

            nPlus := 1
            IF ::Application:ShowGrid != 1
               nPlus := 0
            ENDIF
            DO CASE
               CASE ::InRect == -2
                    ::Left += ( x - ::CtrlOldPt[1] )
                    ::Top  += ( y - ::CtrlOldPt[2] )
                    ::MoveWindow()
                    ::UpdateWindow()
                    ::CtrlOldPt := { x, y }
                    RETURN 0

               CASE ::InRect == -1 .AND. aSelected[1][1]:hWnd != ::hWnd
                    aSel := ::GetSelRect(.T.,.F.)
                    
                    IF aSelected[1][1]:Parent:IsContainer
                       ClientToScreen( ::CtrlMask:hWnd, @pt )
                       oParent := ::GetChildFromPoint( pt,, aSelected )
                       IF oParent != NIL .AND. oParent:IsContainer
                          IF ::OldParent == NIL
                             ::OldParent := aSelected[1][1]:Parent

                             hDef := BeginDeferWindowPos( LEN( aSelected ) )
                             FOR n := 1 TO LEN( aSelected )
                                 pt:x := aSelected[n][1]:Left
                                 pt:y := aSelected[n][1]:Top

                                 ClientToScreen( aSelected[n][1]:Parent:hWnd, @pt )
                                 ScreenToClient( ::hWnd, @pt )

                                 SetParent( aSelected[n][1]:hWnd, ::hWnd )
                                 TRY
                                    DeleteObject( aSelected[n][1]:__hBrush )
                                    aSelected[n][1]:__hBrush := NIL
                                 CATCH
                                 END
                                 DeferWindowPos( hDef, aSelected[n][1]:hWnd, , pt:x, pt:y, aSelected[n][1]:Width, aSelected[n][1]:Height, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER )
                                 BringWindowToTop( aSelected[n][1]:hWnd )
                             NEXT
                             EndDeferWindowPos( hDef )
                             RETURN 0
                             
                          ENDIF
                          IF ( ::NewParent == NIL .OR. !(oParent:hWnd == ::NewParent:hWnd) ) .AND. !(oParent:hWnd == ::OldParent:hWnd)
                             ::NewParent := oParent
                             nCursor := MCS_DRAGGING
                           ELSEIF oParent:hWnd == ::OldParent:hWnd
                             ::NewParent := NIL
                             nCursor := MCS_NONE
                           ELSEIF ::NewParent != NIL .AND. oParent:hWnd == ::NewParent:hWnd
                             nCursor := MCS_DRAGGING
                          ENDIF
                        ELSE
                          nCursor := MCS_SIZEALL
                       ENDIF
                       IF nCursor != NIL
                          ::CtrlMask:SetMouseShape( nCursor )
                       ENDIF
                    ENDIF

                    pt:x := aSel[1]
                    pt:y := aSel[2]
                    ClientToScreen( ::hWnd, @pt )
                    ScreenToClient( aSelected[1][1]:Parent:hWnd, @pt )
                    aSel[1] := pt:x
                    aSel[2] := pt:y

                    pt:x := aSel[3]
                    pt:y := aSel[4]
                    ClientToScreen( ::hWnd, @pt )
                    ScreenToClient( aSelected[1][1]:Parent:hWnd, @pt )
                    aSel[3] := pt:x
                    aSel[4] := pt:y

                    aSnap := {0,0,0,0}
                    IF ::Application:ShowGrid == 2 .AND. aSelected[1][1]:hWnd != ::hWnd
                       aSnap[1] := ::StickLeft( aSel, x - ::CtrlOldPt[1] )
                       aSnap[2] := ::StickTop( aSel, y - ::CtrlOldPt[2] )
                       aSnap[3] := ::StickRight( aSel, x - ::CtrlOldPt[1] )
                       aSnap[4] := ::StickBottom( aSel, y - ::CtrlOldPt[2] )
                    ENDIF
                    // Move selected controls

                    hDef := BeginDeferWindowPos( LEN( aSelected ) )
                    FOR EACH aControl IN aSelected
                        IF aControl[1]:__lMoveable
                           aRect := NIL
                           TRY
                              aRect := aControl[1]:__TempRect
                           CATCH   
                           END
                           DEFAULT aRect TO aControl[1]:GetRectangle()
                           
                           IF ::Application:ShowGrid == 1 .AND. aSelected[1][1]:hWnd != ::hWnd
                              aRect[1] := Snap( aRect[1], ::CtrlMask:xGrid )
                              aRect[2] := Snap( aRect[2], ::CtrlMask:xGrid )
                           ENDIF
                           
                           IF aControl[1]:__TempRect == NIL
                              aControl[1]:Left := aRect[1] + ( x - ::CtrlOldPt[1] ) + aSnap[1] + aSnap[3]
                              aControl[1]:Top  := aRect[2] + ( y - ::CtrlOldPt[2] ) + aSnap[2] + aSnap[4]
                              //aControl[1]:MoveWindow()
                              DeferWindowPos( hDef, aControl[1]:hWnd, , aControl[1]:Left, aControl[1]:Top, aControl[1]:Width, aControl[1]:Height, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER )
                            ELSE
                             
                              aRect[1] := aRect[1] + ( x - ::CtrlOldPt[1] ) + aSnap[1] + aSnap[3]
                              aRect[2] := aRect[2] + ( y - ::CtrlOldPt[2] ) + aSnap[2] + aSnap[4]

                              DeferWindowPos( hDef, aControl[1]:hWnd, , aRect[1], aRect[2], aControl[1]:Width, aControl[1]:Height, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER )
                              //MoveWindow( aControl[1]:hWnd, aRect[1], aRect[2], aControl[1]:Width, aControl[1]:Height )
                           ENDIF
                        ENDIF
                    NEXT
                    EndDeferWindowPos( hDef )

                    ::CtrlOldPt := { x, y }

                    aRect := ::GetSelRect(.T.)

                    IF LEN( aSelected ) == 1

                       pt:x := aRect[1]
                       pt:y := aRect[2]
                       ClientToScreen( ::hWnd, @pt )
                       ScreenToClient( IIF( ::NewParent == NIL, aSelected[1][1]:Parent:hWnd, ::NewParent:hWnd ), @pt )
                       nLeft := pt:x
                       nTop  := pt:y
                       
                       ::Application:MainForm:StatusBarPanel7:Caption := XSTR(nLeft)+", "+XSTR(nTop)+", "+XSTR(aSelected[1][1]:Width)+", "+XSTR(aSelected[1][1]:Height)
                     ELSE
                       IF ::NewParent != NIL
                          pt:x := aRect[1]
                          pt:y := aRect[2]
                          ClientToScreen( ::hWnd, @pt )
                          ScreenToClient( ::NewParent:hWnd, @pt )
                          aRect[1] := pt:x
                          aRect[2] := pt:y
                       ENDIF
                       ::Application:MainForm:StatusBarPanel7:Caption := XSTR(aRect[1])+", "+XSTR(aRect[2])+", "+XSTR(aRect[3])+", "+XSTR(aRect[4])
                    ENDIF

               CASE ::InRect == 1 // left top
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickLeft( aSel, x - ::CtrlOldPt[1] )
                    aSelected[1][1]:xLeft   := aSelected[1][1]:xLeft + ( x - ::CtrlOldPt[1] ) + nSnap
                    aSelected[1][1]:xWidth  += ( ::CtrlOldPt[1] - x - nSnap )
                    nSnap := ::StickTop( aSel, y - ::CtrlOldPt[2] )
                    aSelected[1][1]:xTop    := aSelected[1][1]:xTop + ( y - ::CtrlOldPt[2] ) + nSnap
                    aSelected[1][1]:xHeight += ( ::CtrlOldPt[2] - y - nSnap )

               CASE ::InRect == 2 // left
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickLeft( aSel, x - ::CtrlOldPt[1] )
                    aSelected[1][1]:xLeft   := aSelected[1][1]:xLeft + ( x - ::CtrlOldPt[1] ) + nSnap
                    aSelected[1][1]:xWidth  += ( ::CtrlOldPt[1] - x - nSnap )

               CASE ::InRect == 3 // left bottom
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickLeft( aSel, x - ::CtrlOldPt[1] )
                    aSelected[1][1]:xLeft   := aSelected[1][1]:xLeft + ( x - ::CtrlOldPt[1] ) + nSnap
                    aSelected[1][1]:xWidth  += ( ::CtrlOldPt[1] - x - nSnap )
                    nSnap := ::StickBottom( aSel, y - ::CtrlOldPt[2] )
                    aSelected[1][1]:xHeight += ( y - ::CtrlOldPt[2] + nSnap )

               CASE ::InRect == 4 // bottom
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickBottom( aSel, y - ::CtrlOldPt[2] )
                    aSelected[1][1]:xHeight += ( y - ::CtrlOldPt[2] + nSnap )

               CASE ::InRect == 5 // right bottom
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickRight( aSel, x - ::CtrlOldPt[1] )
                    aSelected[1][1]:xWidth  += ( x - ::CtrlOldPt[1] + nSnap )
                    nSnap := ::StickBottom( aSel, y - ::CtrlOldPt[2] )
                    aSelected[1][1]:xHeight += ( y - ::CtrlOldPt[2] + nSnap )

               CASE ::InRect == 6 // right
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickRight( aSel, x - ::CtrlOldPt[1] )
                    aSelected[1][1]:xWidth  += ( x - ::CtrlOldPt[1] + nSnap )

               CASE ::InRect == 7 // right top
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickRight( aSel, x - ::CtrlOldPt[1] )
                    aSelected[1][1]:xWidth  += ( x - ::CtrlOldPt[1] + nSnap )
                    nSnap := ::StickTop( aSel, y - ::CtrlOldPt[2] )
                    aSelected[1][1]:xTop    := aSelected[1][1]:xTop + ( y - ::CtrlOldPt[2] ) + nSnap
                    aSelected[1][1]:xHeight += ( ::CtrlOldPt[2] - y - nSnap )

               CASE ::InRect == 8 // top
                    aSel := ::GetSelRect(.T.,.F.)
                    nSnap := ::StickTop( aSel, y - ::CtrlOldPt[2] )
                    aSelected[1][1]:xTop    := aSelected[1][1]:xTop + ( y - ::CtrlOldPt[2] ) + nSnap
                    aSelected[1][1]:xHeight += ( ::CtrlOldPt[2] - y - nSnap )

            ENDCASE

            IF ::InRect >= 1
               ::Application:MainForm:StatusBarPanel7:Caption := XSTR(aSelected[1][1]:Left)+", "+XSTR(aSelected[1][1]:Top)+", "+XSTR(aSelected[1][1]:Width)+", "+XSTR(aSelected[1][1]:Height)
               aSelected[1][1]:SetWindowPos(, aSelected[1][1]:xLeft,;
                                               aSelected[1][1]:xTop,;
                                               aSelected[1][1]:xWidth,;
                                               aSelected[1][1]:xHeight, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_DEFERERASE )
              
               ::CtrlOldPt := { x, y }
            ENDIF
            ::UpdateWindow()
            IF ::Application:ShowRulers
               ::Parent:RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

FUNCTION Snap( x, nGrain )
RETURN ROUND( ( x / nGrain ), 0) * nGrain

METHOD DrawStickyLines() CLASS WindowEdit
   IF ::__LeftSnap != NIL
      ::CtrlMask:Drawing:Rectangle( ::__LeftSnap[1], ::__LeftSnap[2], ::__LeftSnap[3], ::__LeftSnap[4], ::__LeftSnap[5] )
   ENDIF
   IF ::__TopSnap != NIL
      ::CtrlMask:Drawing:Rectangle( ::__TopSnap[1], ::__TopSnap[2], ::__TopSnap[3], ::__TopSnap[4], ::__TopSnap[5] )
   ENDIF
   IF ::__RightSnap != NIL
      ::CtrlMask:Drawing:Rectangle( ::__RightSnap[1], ::__RightSnap[2], ::__RightSnap[3], ::__RightSnap[4], ::__RightSnap[5] )
   ENDIF
   IF ::__BottomSnap != NIL
      ::CtrlMask:Drawing:Rectangle( ::__BottomSnap[1], ::__BottomSnap[2], ::__BottomSnap[3], ::__BottomSnap[4], ::__BottomSnap[5] )
   ENDIF
RETURN NIL

METHOD StickLeft( aRect, xDif ) CLASS WindowEdit
   LOCAL nSnap := 0, aChildren, n, oParent, nDis, x, pt := (struct POINT)
   static nDif := 0
   
   IF ::Application:ShowGrid != 2 .OR. !::Selected[1][1]:__lMoveable .OR. ::__RightSnap != NIL .OR. ::Selected[1][1]:hWnd == ::hWnd
      nDif := 0
      RETURN 0
   ENDIF
   
   oParent   := ::Selected[1][1]:Parent
   aChildren := oParent:Children
   
   FOR n := 1 TO LEN( aChildren )
       IF aChildren[n]:__xCtrlName != "Splitter" .AND. ASCAN( ::Selected, {|a|a[1]:hWnd == aChildren[n]:hWnd} ) == 0 // Target should not be selected
          IF xDif < 0
             nDis := ( aRect[1] - aChildren[n]:Left )
             x    := -9
           ELSE
             nDis := ( aChildren[n]:Left - aRect[1] )
             x    := 9
          ENDIF
          
          IF nDis <= 10 .AND. nDis >= 0
             IF nDif >= 10
                nDif := 0
                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                ::__LeftSnap := NIL
                ::__RightSnap := NIL
                ::DrawStickyLines()
                RETURN x
             ENDIF
             IF nDis == 10 .OR. nDis == 0
                IF xDif <> 0
                   IF nDis == 0
                      nDif += ABS( xDif )
                    ELSE
                      nDif := -5
                   ENDIF
                ENDIF
                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                
                ::__LeftSnap := {0,0,0,0,0}
                pt:x := aChildren[n]:Left-1
                pt:y := IIF( aChildren[n]:Top < aRect[4], aChildren[n]:Top-1, aRect[2] )

                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__LeftSnap[1] := pt:x
                ::__LeftSnap[2] := pt:y

                pt:x := aChildren[n]:Left
                pt:y := IIF( aChildren[n]:Top < aRect[4], aRect[4], aChildren[n]:Top + aChildren[n]:Height )

                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__LeftSnap[3] := pt:x
                ::__LeftSnap[4] := pt:y
                ::__LeftSnap[5] := C_RED

                ::DrawStickyLines()

                RETURN aChildren[n]:Left - aRect[1] - xDif
             ENDIF
          ENDIF
          
       ENDIF
   NEXT
RETURN 0

METHOD StickRight( aRect, xDif ) CLASS WindowEdit
   LOCAL nSnap := 0, aChildren, n, oParent, nDis, x, pt := (struct POINT)
   static nDif := 0

   IF ::Application:ShowGrid != 2 .OR. !::Selected[1][1]:__lMoveable .OR. ::__LeftSnap != NIL .OR. ::Selected[1][1]:hWnd == ::hWnd
      nDif := 0
      RETURN 0
   ENDIF

   oParent   := ::Selected[1][1]:Parent
   aChildren := oParent:Children
   
   FOR n := 1 TO LEN( aChildren )
       IF aChildren[n]:__xCtrlName != "Splitter" .AND. ASCAN( ::Selected, {|a|a[1]:hWnd == aChildren[n]:hWnd} ) == 0 // Target should not be selected
          IF xDif < 0
             nDis := aRect[3] - ( aChildren[n]:Left + aChildren[n]:Width )
             x    := -9
           ELSE
             nDis := ( aChildren[n]:Left + aChildren[n]:Width ) - aRect[3]
             x    := 9
          ENDIF
          IF nDis <= 10 .AND. nDis >= 0
             IF nDif >= 10
                nDif := 0
                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                ::__LeftSnap := NIL
                ::__RightSnap := NIL
                ::DrawStickyLines()
                RETURN x
             ENDIF
             IF nDis == 10 .OR. nDis == 0
                IF xDif <> 0
                   IF nDis == 0
                      nDif += ABS( xDif )
                    ELSE
                      nDif := -5
                   ENDIF
                ENDIF

                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                
                ::__RightSnap := {0,0,0,0,0}
                pt:x := aChildren[n]:Left + aChildren[n]:Width
                pt:y := IIF( aChildren[n]:Top < aRect[4], aChildren[n]:Top-1, aRect[2] )

                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__RightSnap[1] := pt:x
                ::__RightSnap[2] := pt:y

                pt:x := aChildren[n]:Left + aChildren[n]:Width + 1
                pt:y := IIF( aChildren[n]:Top < aRect[4], aRect[4], aChildren[n]:Top + aChildren[n]:Height )

                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__RightSnap[3] := pt:x
                ::__RightSnap[4] := pt:y
                ::__RightSnap[5] := C_RED

                ::DrawStickyLines()

                RETURN ( aChildren[n]:Left + aChildren[n]:Width ) - aRect[3] - xDif
             ENDIF
          ENDIF
          
       ENDIF
   NEXT
RETURN nSnap

METHOD StickTop( aRect, yDif ) CLASS WindowEdit
   LOCAL nSnap := 0, aChildren, n, oParent, nDis, x, pt := (struct POINT)
   static aPrev, nDif := 0

   IF ::Application:ShowGrid != 2 .OR. !::Selected[1][1]:__lMoveable .OR. ::__BottomSnap != NIL .OR. ::Selected[1][1]:hWnd == ::hWnd
      nDif := 0
      RETURN 0
   ENDIF

   oParent   := ::Selected[1][1]:Parent
   aChildren := oParent:Children

   FOR n := 1 TO LEN( aChildren )
       IF aChildren[n]:__xCtrlName != "Splitter" .AND. ASCAN( ::Selected, {|a|a[1]:hWnd == aChildren[n]:hWnd} ) == 0 // Target should not be selected
          IF yDif < 0
             nDis := ( aRect[2] - aChildren[n]:Top )
             x    := -9
           ELSE
             nDis := ( aChildren[n]:Top - aRect[2] )
             x    := 9
          ENDIF
          
          IF nDis <= 10 .AND. nDis >= 0
             IF nDif >= 10
                nDif := 0
                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                ::__TopSnap := NIL
                aPrev := NIL
                ::DrawStickyLines()
                RETURN x
             ENDIF
             IF nDis == 10 .OR. nDis == 0
                IF yDif <> 0
                   IF nDis == 0
                      nDif += ABS( yDif )
                    ELSE
                      nDif := -5
                   ENDIF
                ENDIF
                IF aPrev != NIL
                   AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                   ::RedrawWindow( aPrev, , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                ENDIF                
                ::__TopSnap := {0,0,0,0,0}
                pt:x := IIF( aChildren[n]:Left < aRect[3], aChildren[n]:Left-1, aRect[1] )
                pt:y := aChildren[n]:Top-1
                
                aPrev := {pt:x, pt:y, 0, 0 }
                
                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__TopSnap[1] := pt:x
                ::__TopSnap[2] := pt:y

                pt:x := IIF( aChildren[n]:Left < aRect[3], aRect[3], aChildren[n]:Left + aChildren[n]:Width )
                pt:y := aChildren[n]:Top
                aPrev[3] := pt:x
                aPrev[4] := pt:y
                
                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__TopSnap[3] := pt:x
                ::__TopSnap[4] := pt:y
                ::__TopSnap[5] := C_BLUE

                ::DrawStickyLines()

                RETURN aChildren[n]:Top - aRect[2] - yDif
             ENDIF
          ENDIF
          
       ENDIF
   NEXT
RETURN nSnap

METHOD StickBottom( aRect, yDif ) CLASS WindowEdit
   LOCAL nSnap := 0, aChildren, n, oParent, nDis, x, pt := (struct POINT)
   static nDif := 0
   
   IF ::Application:ShowGrid != 2 .OR. !::Selected[1][1]:__lMoveable .OR. ::__TopSnap != NIL .OR. ::Selected[1][1]:hWnd == ::hWnd
      nDif := 0
      RETURN 0
   ENDIF

   oParent   := ::Selected[1][1]:Parent
   aChildren := oParent:Children

   FOR n := 1 TO LEN( aChildren )
       IF aChildren[n]:__xCtrlName != "Splitter" .AND. ASCAN( ::Selected, {|a|a[1]:hWnd == aChildren[n]:hWnd} ) == 0 // Target should not be selected
          IF yDif < 0
             nDis := aRect[4] - ( aChildren[n]:Top + aChildren[n]:Height )
             x    := -9
           ELSE
             nDis := ( aChildren[n]:Top + aChildren[n]:Height ) - aRect[4]
             x    := 9
          ENDIF
          IF nDis <= 10 .AND. nDis >= 0
             IF nDif >= 10
                nDif := 0
                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                ::__BottomSnap := NIL
                ::DrawStickyLines()
                RETURN x
             ENDIF
             IF nDis == 10 .OR. nDis == 0
                IF yDif <> 0
                   IF nDis == 0
                      nDif += ABS( yDif )
                    ELSE
                      nDif := -5
                   ENDIF
                ENDIF

                AEVAL( ::Selected, {|a| a[1]:InvalidateRect()} )
                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN )
                
                ::__BottomSnap := {0,0,0,0,0}
                pt:x := IIF( aChildren[n]:Left < aRect[3], aChildren[n]:Left-1, aRect[1] )
                pt:y := aChildren[n]:Top + aChildren[n]:Height

                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__BottomSnap[1] := pt:x
                ::__BottomSnap[2] := pt:y

                pt:x := IIF( aChildren[n]:Left < aRect[3], aRect[3], aChildren[n]:Left + aChildren[n]:Width )
                pt:y := aChildren[n]:Top + aChildren[n]:Height+1

                ClientToScreen( oParent:hWnd, @pt )
                ScreenToClient( ::CtrlMask:hWnd, @pt )
                ::__BottomSnap[3] := pt:x
                ::__BottomSnap[4] := pt:y
                ::__BottomSnap[5] := C_BLUE

                ::DrawStickyLines()

                RETURN ( aChildren[n]:Top + aChildren[n]:Height ) - aRect[4] - yDif
             ENDIF
          ENDIF
          
       ENDIF
   NEXT
RETURN nSnap

//----------------------------------------------------------------------------

METHOD EditClickEvent() CLASS WindowEdit
   IF LEN( ::Selected ) > 0 .AND. !::CtrlMask:lOrderMode
      ::Application:EventManager:EditEvent( IIF( ::Selected[1][1]:hWnd==::hWnd, "OnLoad", "OnClick" ) )
      ::Application:Project:Modified := .T.
      ::__lModified := .T.
   ENDIF
RETURN Self

//----------------------------------------------------------------------------

METHOD AddDefaultMenuItem( oParent ) CLASS WindowEdit

   LOCAL oSubItem, oItem := CoolMenuItem( oParent )
   oItem:Create( LEN(oParent:Children) )

   ::SelectControl( oItem )

   oSubItem := CMenuItem()
   oSubItem:GenerateMember := .F.
   oSubItem:Init( oItem, .T. )
   
   oSubItem:Caption   := "[ Add New Item ]"
   oSubItem:Font:Bold := .T.
   oSubItem:Default   := .T.
   oSubItem:Create()

   ::Application:Project:Modified := .T.
   ::__lModified := .T.
RETURN Self

//----------------------------------------------------------------------------

METHOD OnNCPaint() CLASS WindowEdit
   LOCAL hRegion, hdc, nState, hIcon, nCaption, cx, cy, x, y, nBorder, nColor, hFont, hOldFont, aStyle, aRect[4], aBttn, aSize[2]
   IF ::Application:ThemeActive .AND. ::Theming
      IF ( !::CaptionBar .AND. ::FrameStyle == 2 ) .OR. ::lCustom
         RETURN NIL
      ENDIF
      nBorder  := (::Width-::ClientWidth)/2
      IF ::ToolWindow
         aBttn    := { GetSystemMetrics(SM_CXSMSIZE), GetSystemMetrics(SM_CYSMSIZE) }
         aStyle   := { WP_SMALLCAPTION, WP_SMALLFRAMELEFT, WP_SMALLFRAMERIGHT, WP_SMALLFRAMEBOTTOM, TMT_SMALLCAPTIONFONT, WP_SMALLCLOSEBUTTON }
         nCaption := GetSystemMetrics(SM_CYSMCAPTION) + nBorder
       ELSE
         aBttn    := { GetSystemMetrics(SM_CXSIZE), GetSystemMetrics(SM_CYSIZE) }
         aStyle   := { WP_CAPTION, WP_FRAMELEFT, WP_FRAMERIGHT, WP_FRAMEBOTTOM, TMT_CAPTIONFONT, WP_CLOSEBUTTON }
         nCaption := GetSystemMetrics(SM_CYCAPTION) + nBorder
      ENDIF
      hRegion  := CreateRectRgn( 0, 0, ::Width, ::Height )
      hdc      := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_CLIPCHILDREN | DCX_VALIDATE )
      nState   := IIF( ::InActive, CS_INACTIVE, CS_ACTIVE )
      
      ::OpenThemeData()

      _FillRect( hdc, { 0, 0, 5, 5 }, GetStockObject( WHITE_BRUSH ) )
      _FillRect( hdc, { ::Width-5, 0, ::Width, 5 }, GetStockObject( WHITE_BRUSH ) )

      DrawThemeBackground( ::hTheme, hdc, aStyle[1], nState, { 0, 0, ::Width, nCaption } )
      DrawThemeBackground( ::hTheme, hdc, aStyle[2], nState, { 0, nCaption, nBorder, ::Height } )
      DrawThemeBackground( ::hTheme, hdc, aStyle[3], nState, { ::Width-nBorder, nCaption, ::Width, ::Height } )
      DrawThemeBackground( ::hTheme, hdc, aStyle[4], nState, { 0, ::Height-nBorder, ::Width, ::Height } )
      
      GetThemePartSize( ::hTheme, hdc, aStyle[6], nState, { 3, 3, nCaption-5, nCaption-5 }, TS_TRUE, aSize )
      
      aRect[1] := ::Width - aSize[1] - 6
      aRect[2] := ( ( nCaption - aSize[2] ) / 2 ) + 1
      aRect[3] := aRect[1] + aSize[1]
      aRect[4] := aRect[2] + aSize[2]

      IF ::SysMenu
         nState := IIF( ::InActive, 5, CBS_NORMAL )
         DrawThemeBackground( ::hTheme, hdc, aStyle[6], nState, aRect )
         aRect[1] -= (aSize[1]+2)
         aRect[3] -= (aSize[1]+2)

         IF ( ::MaximizeBox .OR. ::MinimizeBox ) .AND. !::ToolWindow
            nState := IIF( ::InActive, MAXBS_INACTIVE, MAXBS_NORMAL )
            IF !::MaximizeBox
               nState := IIF( ::InActive, MAXBS_DISINACTIVE, MAXBS_DISABLED )
            ENDIF
            DrawThemeBackground( ::hTheme, hdc, WP_MAXBUTTON, nState, aRect )
            aRect[1] -= (aSize[1]+2)
            aRect[3] -= (aSize[1]+2)

            nState := IIF( ::InActive, MINBS_INACTIVE, MINBS_NORMAL )
            IF !::MinimizeBox
               nState := IIF( ::InActive, MINBS_DISINACTIVE, MINBS_DISABLED )
            ENDIF
            DrawThemeBackground( ::hTheme, hdc, WP_MINBUTTON, nState, aRect )
         ENDIF

      ENDIF
      
      IF ::ToolWindow .OR. !::SysMenu .OR. ( ::Modal .AND. ::DlgModalFrame )
         cx := 0
         x  := 0
       ELSE
         cx := GetSystemMetrics( SM_CXSMICON )
         cy := GetSystemMetrics( SM_CYSMICON )
         x  := 6
         y  := ( nCaption - cy ) / 2
         hIcon := ::GetIcon( ICON_SMALL )
         IF EMPTY( hIcon )
            hIcon := 65553
         ENDIF
         DrawIconEx( hdc, x, y+1, hIcon, cx, cy, 0, NIL, DI_NORMAL | DI_COMPAT )
      ENDIF

      SetBkMode( hdc, TRANSPARENT )
      IF ( hFont := GetThemeSysFont( ::hTheme, aStyle[5] ) ) != NIL
         hOldFont := SelectObject( hdc, hFont )
      ENDIF
      
      IF ::Application:OsVersion:dwMajorVersion == 5
         IF !::InActive
            DrawThemeText( ::hTheme, hdc, aStyle[1], nState, ::Caption, DT_END_ELLIPSIS | DT_VCENTER | DT_SINGLELINE, { x+cx+4, IIF( !::ToolWindow, 1, 0 )+3, ::Width, nCaption } )
         ENDIF
      ENDIF
      
      /*
      IF ::Application:OsVersion:dwMajorVersion > 5
         pOpt := (struct DTTOPTS)
         pOpt:dwSize  := pOpt:sizeof()
         pOpt:crText  := RGB( 255,0,0 )
         pOpt:dwFlags := DTT_TEXTCOLOR

         DrawThemeTextEx( ::hTheme, hdc, aStyle[1], nState, ::Caption, DT_END_ELLIPSIS | DT_VCENTER | DT_SINGLELINE, { x+cx+4, IIF( !::ToolWindow, 1, 0 )+3, ::Width, nCaption }, pOpt:Value )
      ENDIF
      */

      SetTextColor( hdc, GetThemeSysColor( ::hTheme, COLOR_CAPTIONTEXT ) )
      _DrawText( hdc, ::Caption, { x+cx+4, 3, ::Width, nCaption }, DT_END_ELLIPSIS | DT_VCENTER | DT_SINGLELINE )
      SetTextColor( hdc, nColor )

      IF hOldFont != NIL
         SelectObject( hdc, hOldFont )
         DeleteObject( hFont )
      ENDIF

      ReleaseDC( ::hWnd, hDC )
      DeleteObject( hRegion )

      ::CloseThemeData()
      RETURN 0
   ENDIF
RETURN NIL
