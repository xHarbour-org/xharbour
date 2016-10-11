/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Dialog.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"
#include "commdlg.ch"

#xcommand ODEFAULT <v> TO <x> [, <vN> TO <xN>]         ;
       => IF <v> == nil .OR. VALTYPE( <v> ) == "O"; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]

#define ETDT_DISABLE        0x00000001
#define ETDT_ENABLE         0x00000002
#define ETDT_USETABTEXTURE  0x00000004
#define ETDT_ENABLETAB      (ETDT_ENABLE | ETDT_USETABTEXTURE)

//------------------------------------------------------------------------------------------------

CLASS Dialog INHERIT WinForm
   PROPERTY Modal    DEFAULT .T.

   DATA Template
   DATA MDIClient     PROTECTED
   DATA MdiContainer  PROTECTED INIT .F.
   DATA Result        EXPORTED
   DATA lEmpty        PROTECTED
   DATA __lVertScroll PROTECTED INIT .F.
   DATA __lhORZScroll PROTECTED INIT .F.

   ACCESS Form       INLINE IIF( ::Modal .OR. ::Parent == NIL, Self, ::Parent:Form )

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Close()
   METHOD __WinProc() INLINE 0
   METHOD SetObject()
   METHOD SetDialogRect()
   METHOD ReCreate()

   METHOD OnInitDialog()        VIRTUAL
   METHOD PreInitDialog()
   METHOD PostInitDialog()
ENDCLASS

//------------------------------------------------------------------------------------------------

METHOD Init( oParent, aParameters, cProjectName ) CLASS Dialog
   DEFAULT ::__xCtrlName TO "Dialog"
   DEFAULT ::ClsName   TO "Dialog"
   ::Super:Init( oParent, aParameters, cProjectName )
   ::__WindowStyle := 0
   ::Width   := 600
   ::Height  := 700
RETURN Self

METHOD PostInitDialog() CLASS Dialog
   LOCAL nRet, oObj
   nRet := ExecuteEvent( "OnCreate", Self )

   ODEFAULT nRet TO 0
   IF ::Parent != NIL .AND. ::SetChildren
      AADD( ::Parent:Children, Self )
   ENDIF

   IF ::__xCtrlName == "TabPage"
      RETURN 0
   ENDIF

   IF ::Center
      ::CenterWindow()
   ENDIF

   FOR EACH oObj IN ::Components
       IF oObj:__xCtrlName == "Timer" .AND. oObj:AutoRun
          oObj:Start()
       ENDIF
       IF oObj:__xCtrlName == "NotifyIcon"
          oObj:Visible := oObj:Visible
       ENDIF
   NEXT
   IF EMPTY( ::__hIcon )
      SWITCH VALTYPE( ::Icon )
         CASE "A"
              IF ! ::DesignMode .OR. EMPTY( ::Icon[1] )
                 ::__hIcon := LoadIcon( ::AppInstance, ::Icon[2] )
                 ::xIcon := ::Icon[2]
               ELSE
                 ::__hIcon := LoadImage( ::AppInstance, ::Icon[1], IMAGE_ICON,,, LR_LOADFROMFILE )
                 ::xIcon := ::Icon[1]
              ENDIF
              EXIT

         CASE "C"
              ::__hIcon := LoadIcon( ::AppInstance, ::Icon )
              EXIT

         CASE "N"
              ::__hIcon := ::Icon
              EXIT
      END
   ENDIF
   ::SetIcon( ICON_SMALL, IIF( !EMPTY( ::__hIcon ), ::__hIcon, 0 ) )
   ::SetIcon( ICON_BIG, IIF( !EMPTY( ::__hIcon ), ::__hIcon, 0 ) )

   ::SetOpacity( ::xOpacity )

   AEVAL( ::__aPostCreateProc, {|a| hb_ExecFromArray( a[1], a[2] )} )
   IF ::ActiveMenuBar != NIL
      ::__SetActiveMenuBar( ::ActiveMenuBar )
   ENDIF

   IF ::BackgroundImage != NIL
      ::BackgroundImage:Create()
   ENDIF
/*
   IF !::__lShown
      ::__lShown := .T.
      ::__FixDocking()

      nRet := ExecuteEvent( "OnLoad", Self )
      ODEFAULT nRet TO ::OnLoad( Self )
      IF ::AnimationStyle != 0 .AND. ! ::DesignMode
         RETURN ::Animate( 1000, ::AnimationStyle )
      ENDIF
   ENDIF
*/
   ::Show( ::ShowMode )
RETURN nRet

//------------------------------------------------------------------------------------------------

METHOD PreInitDialog() CLASS Dialog
   LOCAL oCtrl, cClass, hWnd, nStyle, nLeft, nTop, nWidth, nHeight
   ::siv := (struct SCROLLINFO)
   ::siv:cbSize := ::siv:sizeof()
   ::siv:nMin   := 0

   ::sih := (struct SCROLLINFO)
   ::sih:cbSize := ::sih:sizeof()
   ::sih:nMin   := 0

   nLeft   := ::Left
   nTop    := ::Top
   nWidth  := ::Width
   nHeight := ::Height

   ::GetClientRect()
   ::GetWindowRect()

   ::xLeft  := nLeft
   ::xTop   := nTop

   DEFAULT nWidth TO ::xWidth
   DEFAULT nHeight TO ::xHeight

   ::xWidth := nWidth
   ::xHeight:= nHeight

   ::__ClientRect := { nLeft, nTop, ::xWidth, ::xHeight }
   ::__aCltRect   := { nLeft, nTop, ::xWidth, ::xHeight }
   ::OriginalRect := { nLeft, nTop, ::xWidth, ::xHeight }

   __SetWindowObjPtr( Self )

   IF ::Template != NIL

      hWnd := GetWindow( ::hWnd, (GW_CHILD | GW_HWNDFIRST ))
      WHILE hWnd != 0

         cClass := GetClassName( hWnd )

         IF cClass != NIL
            //TraceLog( cClass )
            nStyle := GetWindowLong( hWnd, GWL_STYLE )

            IF cClass == "Static"
               oCtrl := Label( Self )
             ELSEIF cClass == "SysListView32"
               oCtrl := ListView( Self )
             ELSEIF cClass == "Button" .AND. ( nStyle & BS_GROUPBOX ) == BS_GROUPBOX
               oCtrl := GroupBox( Self )
             ELSEIF cClass == "Button" .AND. ( ( nStyle & BS_RADIOBUTTON) == BS_RADIOBUTTON .OR. (nStyle & BS_AUTORADIOBUTTON) == BS_AUTORADIOBUTTON )
               oCtrl := RadioButton( Self )
             ELSEIF cClass == "Button" .AND. ( (nStyle & BS_CHECKBOX) == BS_CHECKBOX .OR. (nStyle & BS_AUTOCHECKBOX) == BS_AUTOCHECKBOX )
               oCtrl := CheckBox( Self )
             ELSE
               oCtrl := &cClass( Self )
            ENDIF

            oCtrl:hWnd := hWnd
            oCtrl:Id   := GetDlgCtrlID( hWnd )
            oCtrl:GetClientRect()
            oCtrl:GetWindowRect()

            __SetWindowObjPtr( oCtrl )

            AADD( ::Children, oCtrl )
         ENDIF

         hWnd   := GetWindow( hWnd, GW_HWNDNEXT )
      ENDDO
   ENDIF
   IF !EMPTY( ::xText )
      ::SetWindowText( ::xText )
   ENDIF
   IF ::lEmpty //.AND. ::Modal
      ::SetDialogRect()
   ENDIF
   IF ::Modal .AND. ! ::DesignMode
      IF ::VertScrollTopMargin > 0
         ::__oDlg := Dialog( Self )
         WITH OBJECT ::__oDlg
            :Style       := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
            :ExStyle     := WS_EX_CONTROLPARENT

            :Top         := ::VertScrollTopMargin

            :VertScroll  := ::__lVertScroll
            :HorzScroll  := ::__lHorzScroll

            :SetChildren := .F.
            :Modal       := .F.
            :BackColor   := ::BackColor
            :Create()
            :OriginalRect[4] := 0
         END
      ENDIF
   ENDIF
   ::__SetScrollBars()
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD Create( hParent ) CLASS Dialog
   LOCAL nRet, cProcStack := ""

   IF ::__hParent != NIL
      hParent := ::__hParent
   ENDIF

   hParent := IIF( ::Parent == NIL, hParent, ::Parent:hWnd )
   DEFAULT ::EventHandler TO Hash()

   ::lEmpty   := ::Template == NIL

   nRet := ExecuteEvent( "OnInit", Self )
   IF VALTYPE( nRet ) == "N" .AND. nRet == 0
      ::__OnInitCanceled := .T.
      ::RemoveProperty()
      IF VALTYPE( ::Font ) == "O" .AND. ! ::Font:Shared
         ::Font:Delete()
      ENDIF
      RETURN Self
   ENDIF

   ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, ::__WndProc ), Self )

   IF !::ShowInTaskBar .AND. ::Parent == NIL .AND. ! ::DesignMode .AND. ::Application:DllInstance == NIL
      ::__TaskBarParent := CreateDialogIndirect( ::Instance, __GetTemplate( Self ), 0, NIL )
      IF ::__hIcon != NIL
         SendMessage( ::__TaskBarParent, WM_SETICON, ICON_BIG, ::__hIcon )
      ENDIF
      hParent := ::__TaskBarParent
   ENDIF

   DEFAULT hParent TO GetActiveWindow()
   IF ::Modal
      IF ! ::DesignMode .AND. ::VertScrollTopMargin > 0
         ::__lVertScroll := ::VertScroll
         ::__lHorzScroll := ::HorzScroll

         ::VertScroll := .F.
         ::HorzScroll := .F.
      ENDIF



      IF ::Template == NIL
         ::Template := __GetTemplate( Self )
         ::Result := DialogBoxIndirect( ::Instance, ::Template, hParent, ::__pCallBackPtr )
      ELSE
         ::Result := DialogBox( ::Instance, ::Template, hParent, ::__pCallBackPtr )
      ENDIF
      RETURN ::Result == IDOK
   ELSE
      IF ::Template == NIL
         ::Template := __GetTemplate( Self )
         ::hWnd := CreateDialogIndirect( ::Instance, ::Template, hParent, ::__pCallBackPtr )
       ELSE
         ::hWnd := CreateDialog( ::Instance, ::Template, hParent, ::__pCallBackPtr )
      ENDIF
   ENDIF
RETURN SELF

//------------------------------------------------------------------------------------------------

METHOD ReCreate() CLASS Dialog
   LOCAL Child
   ::hWnd := NIL
   ::Create()
   FOR EACH Child IN ::Children
       Child:ReCreate()
   NEXT
RETURN Self

//------------------------------------------------------------------------------------------------

FUNCTION __GetTemplate( oDlg )
   LOCAL dt := (struct DLGTEMPLATEX)
   dt:style := (oDlg:Style | WS_CLIPCHILDREN)
   dt:dwExtendedStyle := oDlg:ExStyle
   dt:x  := MulDiv( oDlg:Left,   4, LOWORD( GetDialogBaseUnits() ) )  //Int( ( oDlg:Left * 4 )  /LOWORD(GetDialogBaseUnits()) )
   dt:y  := MulDiv( oDlg:Top,    8, HIWORD( GetDialogBaseUnits() ) )  //Int( ( oDlg:Top * 4 )   /LOWORD(GetDialogBaseUnits()) )
   dt:cx := MulDiv( oDlg:Width,  4, LOWORD( GetDialogBaseUnits() ) )  //Int( ( oDlg:Width * 4 ) /LOWORD(GetDialogBaseUnits()) )
   dt:cy := MulDiv( oDlg:Height, 8, HIWORD( GetDialogBaseUnits() ) )  //Int( ( oDlg:Height * 4 )/LOWORD(GetDialogBaseUnits()) )
RETURN dt

//------------------------------------------------------------------------------------------------
METHOD Close(n) CLASS Dialog
   LOCAL nRet
   DEFAULT n TO IDCANCEL
   ::Template := NIL

   nRet := ExecuteEvent( "OnClose", Self )

   IF VALTYPE( nRet ) $ "UO"
      IF ::Modal
         EndDialog( ::hWnd, n )
       ELSE
         DestroyWindow( ::hWnd )
      ENDIF
   ENDIF
RETURN nRet

//------------------------------------------------------------------------------------------------
METHOD SetObject( cName, nId )
   EXTERNAL Button, Label
   LOCAL oCtrl
   oCtrl := &cName( Self )
   oCtrl:hWnd := GetDlgItem( ::hWnd, nId )
   oCtrl:Id   := VAL( RIGHT( ALLTRIM( STR( oCtrl:hWnd ) ), 4 ) )
   oCtrl:__SubClass()
RETURN Self

METHOD SetDialogRect() CLASS Dialog
   LOCAL rc
   IF VALTYPE( ::Template ) == "C"
      RETURN ::GetWindowRect()
   ENDIF
   rc := (struct RECT)
   rc:left   := ::Template:x
   rc:top    := ::Template:y
   rc:right  := ::Template:cx
   rc:bottom := ::Template:cy

   MapDialogRect( ::hWnd, @rc )

   ::__aCltRect[3]  := rc:right
   ::__aCltRect[4]  := rc:bottom
   ::OriginalRect[3]:= rc:right
   ::OriginalRect[4]:= rc:bottom

   IF ::Width != ::Template:cx .OR. ::Height != ::Template:cy
      GetWindowRect( ::hWnd, @rc )
      IF ::Width == ::Template:cx
         ::xWidth := rc:right-rc:left
      ENDIF
      IF ::Height == ::Template:cy
         ::xHeight := rc:bottom-rc:top
      ENDIF
      ::MoveWindow(,,,, .f. )
   ENDIF
   ::GetWindowRect()
RETURN NIL

FUNCTION __GetFontRatio( cFontFace, nFontSize )
   LOCAL hFont
   LOCAL nWidthFont
   LOCAL nWidthSystem

   hFont        := __FontCreate( cFontFace, nFontSize, .F., .F., .F. )
   nWidthFont   := __GetTrueAveFont( hFont )[1]
   DeleteObject(hFont)

   hFont        := __FontCreate( cFontFace, nFontSize, .T., .F., .F. )
   nWidthSystem := __GetTrueAveFont( hFont )[1]
   DeleteObject(hFont)

RETURN nWidthSystem/nWidthFont

//---------------------------------------------------------------------------------------------------

FUNCTION __GetTrueAveFont( hFont )
   LOCAL hDC := GetDC( 0 )
   LOCAL holdfont := SelectObject( hDC, hFont )
   LOCAL aTextExt := _GetTextExtentPoint32( hDC, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' )

   SelectObject( hDC, holdfont )
   ReleaseDC( 0, hDC )
RETURN( { Int( ( Int( aTextExt[ 1 ] / 26 ) + 1 ) / 2 ), aTextExt[2] } )

