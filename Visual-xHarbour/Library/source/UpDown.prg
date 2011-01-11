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
   METHOD Init()  CONSTRUCTOR
ENDCLASS

METHOD Init( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight, nStyle, lCreate ) CLASS UpDown
   DEFAULT ::__xCtrlName TO "UpDown"
   DEFAULT nWidth  TO 50
   DEFAULT nHeight TO 13
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := nWidth
   ::Height    := nHeight
   ::Super:Init( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight, nStyle, lCreate )
   ::ClsName      := "msctls_updown32"
RETURN Self

