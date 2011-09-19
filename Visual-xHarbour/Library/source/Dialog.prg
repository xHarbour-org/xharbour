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

//------------------------------------------------------------------------------------------------

CLASS Dialog INHERIT WinForm
   DATA Template
   DATA MDIClient    PROTECTED
   DATA MdiContainer PROTECTED INIT .F.
   DATA Result       EXPORTED
   DATA Modal        AS LOGIC  PUBLISHED INIT .T.
   DATA lEmpty       PROTECTED
   ACCESS Form       INLINE IIF( ::Modal .OR. ::Parent == NIL, Self, ::Parent:Form )
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Close()
   METHOD __WinProc() INLINE 0
   METHOD SetObject()
   METHOD InitDialogBox()
   METHOD SetDialogRect()
   METHOD ReCreate()
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

//------------------------------------------------------------------------------------------------

METHOD InitDialogBox()
   LOCAL oCtrl, cClass, hWnd, nStyle

   hWnd := GetWindow( ::hWnd, GW_CHILD | GW_HWNDFIRST )
   WHILE hWnd != 0

      cClass := GetClassName( hWnd )

      IF cClass != NIL
         //TraceLog( cClass )
         nStyle := GetWindowLong( hWnd, GWL_STYLE )

         IF cClass == "Static"
            oCtrl := Label( Self )
          ELSEIF cClass == "SysListView32"
            oCtrl := ListView( Self )
          ELSEIF cClass == "Button" .AND. nStyle & BS_GROUPBOX == BS_GROUPBOX
            oCtrl := GroupBox( Self )
          ELSEIF cClass == "Button" .AND. ( nStyle & BS_RADIOBUTTON == BS_RADIOBUTTON .OR. nStyle & BS_AUTORADIOBUTTON == BS_AUTORADIOBUTTON )
            oCtrl := RadioButton( Self )
          ELSEIF cClass == "Button" .AND. ( nStyle & BS_CHECKBOX == BS_CHECKBOX .OR. nStyle & BS_AUTOCHECKBOX == BS_AUTOCHECKBOX )
            oCtrl := CheckBox( Self )
          ELSE
            oCtrl := &cClass( Self )
         ENDIF

         oCtrl:hWnd := hWnd
         oCtrl:Id   := GetDlgCtrlID( hWnd )
         oCtrl:GetClientRect()
         oCtrl:GetWindowRect()

         IF oCtrl:__ArrayPointer != NIL
            ReleaseArrayPointer( oCtrl:__ArrayPointer )
         ENDIF
         oCtrl:__ArrayPointer := ARRAYPOINTER( oCtrl )
         SetProp( hWnd, "PROP_CLASSOBJECT", oCtrl:__ArrayPointer )

         AADD( ::Children, oCtrl )
      ENDIF

      hWnd   := GetWindow( hWnd, GW_HWNDNEXT )
   ENDDO
   IF !EMPTY( ::xCaption )
      SetWindowText( ::hWnd, ::xCaption )
   ENDIF
   IF ::lEmpty //.AND. ::Modal
      ::SetDialogRect()
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD Create( hParent ) CLASS Dialog
   LOCAL nRet, cProcStack := ""

   IF ::__hParent != NIL
      hParent := ::__hParent
   ENDIF

   hParent := IIF( ::Parent == NIL, hParent, ::Parent:hWnd )
   DEFAULT hParent TO GetDesktopWindow()
   DEFAULT ::EventHandler TO Hash()
  
   ::__lOnPaint   := __ClsMsgAssigned( Self, "OnPaint" ) .OR. HGetPos( ::EventHandler, "OnPaint" ) != 0
   ::__lOnWindowPaint := __ClsMsgAssigned( Self, "OnWindowPaint" ) .OR. HGetPos( ::EventHandler, "OnWindowPaint" ) != 0

   ::ClsName  := "Dialog"
   ::lEmpty   := ::Template == NIL

   nRet := ExecuteEvent( "OnInit", Self )
   IF VALTYPE( nRet ) == "N" .AND. nRet == 0
      ::__OnInitCanceled := .T.
      ::RemoveProperty()
      RETURN Self
   ENDIF

   ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, ::__WndProc ), Self )
   
   IF !::ShowInTaskBar .AND. ::Parent == NIL .AND. ::__ClassInst == NIL .AND. ::Application:DllInstance == NIL
      ::__TaskBarParent := CreateDialogIndirect( ::Instance, __GetTemplate( Self ), 0, NIL )
      IF ::__hIcon != NIL
         SendMessage( ::__TaskBarParent, WM_SETICON, ICON_BIG, ::__hIcon )
      ENDIF
      hParent := ::__TaskBarParent
   ENDIF

   IF ::Modal
      IF ::Template == NIL
         ::Template := __GetTemplate( Self )
         ::Result := DialogBoxIndirect( ::Instance, ::Template, hParent, ::__pCallBackPtr )
         RETURN ::Result == IDOK
       ELSE
         RETURN ( ::Result := DialogBox( ::Instance, ::Template, hParent, ::__pCallBackPtr )) == IDOK
      ENDIF
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
   dt:style := oDlg:Style
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

