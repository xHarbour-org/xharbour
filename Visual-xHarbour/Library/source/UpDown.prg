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
   DATA __lResizeable          EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.T.,.F.,.F.}
   METHOD Init()  CONSTRUCTOR
ENDCLASS

METHOD Init( oParent ) CLASS UpDown
   ::ClsName      := "msctls_updown32"
   DEFAULT ::Style   TO WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   DEFAULT ::__xCtrlName TO "UpDown"
   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 22
   ::__lMoveable := .T.

RETURN Self

