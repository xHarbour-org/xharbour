/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// RichTextBox.prg                                                                                      *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include 'vxh.ch'
#include "debug.ch"
#include "commdlg.ch"
#include 'inkey.ch'
#include "RichEdit.ch"

//#define GTL_DEFAULT     0  //* do the default (return # of chars)
//#define GTL_USECRLF     1  //* compute answer using CRLFs for paragraphs
//#define GTL_PRECISE     2  //* compute a precise answer
//#define GTL_CLOSE       4  //* fast computation of a "close" answer
//#define GTL_NUMCHARS    8  //* return the number of characters
//#define GTL_NUMBYTES   16  //* return the number of _bytes_
#define CP_ACP            0  //* default to ANSI code page
#define CP_OEMCP          1  //* default to OEM  code page

//-----------------------------------------------------------------------------------------------

CLASS RichTextBox INHERIT EditBox
   DATA __EditStreamPtr PROTECTED
   METHOD Init()     CONSTRUCTOR
   METHOD Create()
   METHOD FindText()
   METHOD GetTextLength()
   METHOD SetSelectionColor()
   METHOD EditStreamCallback()
   METHOD OnDestroy() INLINE ::Super:OnDestroy(), IIF( ::__EditStreamPtr != NIL, FreeCallBackPointer( ::__EditStreamPtr ), ), NIL
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS RichTextBox
   DEFAULT ::__xCtrlName TO "RichTextBox"
   ::Super:Init( oParent )
   ::Width       := 80
   ::Height      := 80
   ::xWantReturn := .T.
   ::xMultiLine  := .T.
   ::xClientEdge := .F.
   ::Style       := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | ES_WANTRETURN | ES_MULTILINE
   ::ClsName     := RICHEDIT_CLASS
RETURN Self

METHOD Create() CLASS RichTextBox
   LOCAL es := (struct EDITSTREAM)
   ::Super:Create()
   IF !::xClientEdge
      ::ClientEdge := .F.
   ENDIF
   ::__EditStreamPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, "EditStreamCallback" ), Self )
   es:pfnCallback := ::__EditStreamPtr
   ::SendMessage( EM_STREAMIN, SF_TEXT, es )
RETURN Self
   
//-----------------------------------------------------------------------------------------------
METHOD FindText( cText, nOpt ) CLASS RichTextBox
   LOCAL ft := (struct FINDTEXT)
   DEFAULT nOpt TO FR_DOWN
   ft:lpstrText  := cText
   ft:chrg:cpMin := 0
   ft:chrg:cpMax := -1

RETURN SendMessage( ::hWnd, EM_FINDTEXT, nOpt, ft )

//-----------------------------------------------------------------------------------------------
METHOD GetTextLength( nFlags ) CLASS RichTextBox
   LOCAL tl    := (struct GETTEXTLENGTHEX)
   DEFAULT nFlags TO GTL_DEFAULT
   tl:flags    := nFlags
   tl:codepage := CP_ACP
RETURN SendMessage( ::hWnd, EM_GETTEXTLENGTHEX, tl )

//-----------------------------------------------------------------------------------------------
METHOD SetSelectionColor( nColor ) CLASS RichTextBox
   LOCAL cf := (struct CHARFORMAT)
   cf:cbSize      := cf:SizeOf()
   cf:dwMask      := CFM_COLOR
   cf:crTextColor := nColor
RETURN ::SendMessage( EM_SETCHARFORMAT, SCF_SELECTION, cf )

//-----------------------------------------------------------------------------------------------
METHOD EditStreamCallback( dwCookie, pbBuff, cb, pcb ) CLASS RichTextBox
RETURN cb

#endif