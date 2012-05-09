/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// UpDown.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"


//-----------------------------------------------------------------------------------------------

CLASS UpDown INHERIT Control
   PROPERTY Buddy GET __ChkComponent( Self, ::xBuddy ) SET __SetBuddy
   DATA __lResizeable          EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.T.,.F.,.F.}
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD __SetBuddy()
ENDCLASS

METHOD Init( oParent ) CLASS UpDown
   ::ClsName := "msctls_updown32"
   DEFAULT ::Style   TO WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | UDS_ALIGNRIGHT | UDS_SETBUDDYINT | UDS_NOTHOUSANDS | UDS_ARROWKEYS
   DEFAULT ::__xCtrlName TO "UpDown"
   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 22
   ::__lMoveable := .T.

RETURN Self

METHOD Create() CLASS UpDown
   ::Super:Create()
   ::__SetBuddy()
RETURN Self

METHOD __SetBuddy( oBuddy ) CLASS UpDown
   IF ::IsWindow()
      DEFAULT oBuddy TO ::Buddy
      IF VALTYPE( oBuddy ) != "C"
         ::SendMessage( UDM_SETBUDDY, IIF( oBuddy != NIL, oBuddy:hWnd, NIL ) )
      ENDIF
   ENDIF
RETURN Self
