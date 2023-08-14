/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MDIChild.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

#define ATL_IDM_FIRST_MDICHILD 50000
#define IDM_MDI_BASE      (ATL_IDM_FIRST_MDICHILD - 5)
#define IDM_MDI_ICON      (IDM_MDI_BASE + 0)
#define IDM_MDI_GAP       (IDM_MDI_BASE + 1)
#define IDM_MDI_MINIMIZE  (IDM_MDI_BASE + 2)
#define IDM_MDI_RESTORE   (IDM_MDI_BASE + 3)
#define IDM_MDI_CLOSE     (IDM_MDI_BASE + 4)
#define IDM_MDI_FIRST     IDM_MDI_ICON
#define IDM_MDI_LAST      IDM_MDI_CLOSE

#xcommand ODEFAULT <v> TO <x> [, <vN> TO <xN>]         ;
       => IF <v> == nil .OR. VALTYPE( <v> ) == "O"; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]

//-----------------------------------------------------------------------------------------------

CLASS MDIChildWindow INHERIT WinForm
   ACCESS Form       INLINE Self

   CLASSDATA __oCoolMenu  PROTECTED
   CLASSDATA __oWinMenu   PROTECTED

   METHOD Init()  CONSTRUCTOR
   METHOD __WinProc( hWnd, nMsg, nwParam, nlParam ) INLINE DefMDIChildProc( hWnd, nMsg, nwParam, nlParam )
   METHOD Create()//                                INLINE ::Super:Create(), ::InvalidateRect(), Self
   METHOD Show()
   METHOD OnNCDestroy()
   METHOD SetWindowText()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent, aParams, cProjectName ) CLASS MDIChildWindow
   DEFAULT ::__xCtrlName TO "MDIChild"
   ::xMDIChild   := .T.
   ::Modal       := .F.
   ::ClsName     := "MDIChild"
   ::Super:Init( oParent:MDIClient, aParams, cProjectName )
   ::ClassStyle  := (CS_VREDRAW | CS_HREDRAW)
   ::Style       := (WS_CHILD | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_THICKFRAME)
   ::IsContainer := .F.
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS MDIChildWindow
   LOCAL n

   ::ControlParent := .T.

   ::ExStyle := (::ExStyle | WS_EX_MDICHILD)
   ::Super:Create()

   AADD( ::Parent:Children, Self )

   ::SetWindowLong( GWL_STYLE, ::Style )
   //IF ::IsWindowVisible()
   //   ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER)
   //   ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   //ENDIF

   IF ::__hParent != NIL
      InvalidateRect( ::hWnd )
   ENDIF
   IF ::__oCoolMenu == NIL .AND. ::Parent:WindowsMenu .AND. ( n := ASCAN( ::Parent:Parent:Children, {|o| o:__xCtrlName == "CoolMenu"} ) ) > 0
      ::__oCoolMenu := ::Parent:Parent:Children[n]
   ENDIF
   IF ::__oWinMenu == NIL

      IF ::__oCoolMenu != NIL .AND. ( n := ASCAN( ::__oCoolMenu:aItems, {|o| o:MDIList } ) ) > 0
         ::__oWinMenu := ::__oCoolMenu:aItems[n]
       ELSEIF ::Parent:Parent:ActiveMenuBar != NIL .AND. ( n := ASCAN( ::Parent:Parent:ActiveMenuBar:Children, {|o| o:MDIList } ) ) > 0
         ::__oWinMenu := ::Parent:Parent:ActiveMenuBar:Children[n]
      ENDIF

      IF ::__oWinMenu != NIL
         SendMessage( ::Parent:hWnd, WM_MDISETMENU,, ::__oWinMenu:hMenu )
      ENDIF
   ENDIF
RETURN Self

METHOD Show( nShow ) CLASS MDIChildWindow
   LOCAL nRet
   DEFAULT nShow TO ::ShowMode

   IF nShow == SW_HIDE
      RETURN ::Hide()
   ENDIF

   IF ::hWnd != NIL
      IF !::__lShown
         ::__lShown := .T.
         nRet := ExecuteEvent( "OnLoad", Self )
         ODEFAULT nRet TO ::OnLoad( Self )
      ENDIF
      ShowWindow( ::hWnd, IIF( ! ::DesignMode, nShow, SW_SHOW ) )
   ENDIF

   DO CASE
      CASE nShow == SW_SHOWMAXIMIZED
           SendMessage( ::Parent:hWnd, WM_MDIMAXIMIZE, ::hWnd )
      OTHERWISE
           ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
   ENDCASE
   ::Style := (::Style | WS_VISIBLE)
RETURN Self

METHOD OnNCDestroy() CLASS MDIChildWindow
   LOCAL n
   IF ( n := ASCAN( ::Parent:Children, {|o|o:hWnd == ::hWnd} ) ) > 0
      ADEL( ::Parent:Children, n, .T. )
   ENDIF
RETURN Super:OnNCDestroy()

METHOD SetWindowText( cText ) CLASS MDIChildWindow
   Super:SetWindowText( cText )
   SendMessage( ::Parent:hWnd, WM_MDIREFRESHMENU, 0, 0 )
RETURN Self
