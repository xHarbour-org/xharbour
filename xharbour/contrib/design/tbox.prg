/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Text Mode Screen Designer
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef __CLIPPER__
#Include "hbclass.CH"
#else
#Include "Objects.CH"
#endif
#define BOX_CHARS   "³ ³³ÙÄÀ³ "

CLASS BaseBox

    METHOD New()
    METHOD Display()
    METHOD SetName()
    METHOD Hide()
    METHOD Activate()
    METHOD DeActivate()
    METHOD AddText()

    DATA   aTextBox
    DATA   InBoxColor
    DATA   IsActive
    DATA   nTop
    DATA   nLeft
    DATA   nRight
    DATA   nBottom
    DATA   Shadow
    DATA   Frame
    DATA   BoxColor
    DATA   cTitle
    DATA   TitleColor
    DATA   ButtonColor
    DATA   TopBar
    DATA   cBuffer
    DATA   Cargo
    DATA   BarColor
    DATA   cTitlepos
    DATA   aBuffer

ENDCLASS

********************************************************************************
METHOD New( _cTitle, _nTop, _nLeft, _nBottom, _nRight, _nHeight, _nWidth, _cWinColor ) CLASS BaseBox
::aTextBox     := {}
::IsActive     := .F.
::cTitle       := _cTitle
::nTop         := _nTop
::nLeft        := _nLeft
::nBottom      := _nBottom
::nRight       := _nRight
Return Self

********************************************************************************
METHOD AddText( _aText ) CLASS BaseBox
AADD( ::aTextBox, _aText )
Return Self

********************************************************************************
METHOD Activate() CLASS BaseBox
::IsActive := .T.
Return Self

********************************************************************************
METHOD DeActivate() CLASS BaseBox
::IsActive := .F.
Return Self

********************************************************************************
METHOD display() CLASS BaseBox
LOCAL nTop      := ::nTop
LOCAL nLeft     := ::nLeft
LOCAL nRight    := ::nRight
LOCAL nBottom   := ::nBottom
LOCAL cBoxColor := "W+/B"
LOCAL i, ButtColor

::cTitlePos     := If(::cTitlePos==NIL,"MIDDLE",::cTitlePos )
::TopBar        := If(::TopBar==NIL,.T.,::TopBar)
::Shadow        := If(::Shadow==NIL,.T.,::Shadow)
::Frame         := If(::Frame==NIL,BOX_CHARS,::Frame)
::BoxColor      := If(::BoxColor==NIL,cBoxColor,::BoxColor)
::InBoxColor    := If(::InBoxColor==NIL,::BoxColor,::InBoxColor)
::TitleColor    := If(::TitleColor==NIL,"N/W*",::TitleColor)
::ButtonColor   := If(::ButtonColor==NIL,"W+/N",::ButtonColor)
::cTitle        := If(empty(::cTitle)," ",::cTitle)
::BarColor      := If(::BarColor==NIL,pickcolor(cBoxColor,1,"/")+"/W*",::BarColor)
::aBuffer       := {nTop,nLeft,nBottom+If(::Shadow,1,0),nRight+If(::Shadow,2,0)}
ButtColor       := ::ButtonColor
::cBuffer := SaveScreen(::aBuffer[1],::aBuffer[2],::aBuffer[3],::aBuffer[4])
DISPBEGIN()
  DispBox(nTop,nLeft,nBottom,nRight,::Frame,::BoxColor)
  If ::TopBar
    if ::cTitlePos == "MIDDLE"
      DispOutAt(nTop,nLeft+1,PADC(::cTitle,nRight-nLeft-1),::TiTleColor)
    else
      DispOutAt(nTop,nLeft+1,PADR(::cTitle,nRight-nLeft-1),::TiTleColor )
    Endif
  Endif
  If !Empty(::aTextBox)
     AEval( ::aTextBox, { |e| DispOutAt( e[1], e[2],e[3], ::InBoxColor ) } )
  Endif
  If !( ::Cargo == NIL )
     ::Cargo:Eval()
  Endif
  If ::Shadow
    ShadowOnly( ::nTop, ::nLeft, ::nBottom, ::nRight )
  Endif
DISPEND()
RETURN SELF

********************************************************************************
METHOD SetName( cNewTitle, cPos ) CLASS BaseBox
if cpos == NIL
   cpos := "MIDDLE"
endif
::cTitlePos := Upper( cpos )
::cTitle    := If(!(cNewTitle==NIL),cNewTitle,::cTitle)
RETURN SELF

********************************************************************************
METHOD Hide() CLASS BaseBox
RestScreen(::aBuffer[1],::aBuffer[2],::aBuffer[3],::aBuffer[4],::cBuffer)
RETURN SELF
