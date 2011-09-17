/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Alert.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

static nButWidth

#include "debug.ch"
#include "vxh.ch"

FUNCTION Alert( cMsg, aChoices, lModal, nBackColor, nForeColor, lVert, cFace, nPointSize, nDefault )
   LOCAL o
   DEFAULT lModal TO .T.
   o := AlertDialog( cMsg, aChoices, lModal, nBackColor, nForeColor, lVert, cFace, nPointSize, nDefault )
RETURN IIF( lModal, o:Result, o )

CLASS AlertDialog INHERIT Dialog
   DATA aChoices  EXPORTED
   DATA MsgHeight EXPORTED
   DATA Message   EXPORTED
   DATA _Back
   DATA _Fore
   DATA _Vert
   DATA _Face
   DATA _PointSize
   DATA _Default
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OnCommand( nId ) INLINE ::Close( nId )
ENDCLASS

METHOD Init( cMsg, aChoices, lModal, nBackColor, nForeColor, lVert, cFace, nPointSize, nDefault ) CLASS AlertDialog
   Local n, aMsg, hFont, hOldFont, i
   Local hWnd, hDC
   Local nWidth, nMsgHeight
   LOCAL aSize, oParent
   
   nButWidth := 0
   
   DEFAULT lVert    TO .F.
   DEFAULT nDefault TO 1
   DEFAULT lModal   TO .T.
   
   ::_Vert      := lVert
   ::_Back      := nBackColor
   ::_Fore      := nForeColor
   ::_PointSize := nPointSize
   ::_Face      := cFace
   ::_Default   := nDefault
   
   IF VALTYPE( cMsg ) != "C"
      IF VALTYPE( cMsg)=="A"
         cMsg:=__a2str(cMsg,";")
       ELSE
         cMsg = __asstring( cMsg )
      ENDIF
   ENDIF

   DEFAULT aChoices TO { "&OK" }

   cMsg     := StrTran( cMsg, ";", CHR(13) )

   hFont    := __GetMessageFont( 700 )
   hDC      := GetDC( 0 )
   hOldFont := SelectObject( hDC, hFont )

   aSize    := _GetTextExtentPoint32( hDC, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" )
   nWidth   := 0
   
   aMsg := __str2a( cMsg, CHR(13) )

   AEVAL( aMsg, {|x| nWidth := Max( nWidth, _GetTextExtentPoint32( hDC, AllTrim(x) )[1] ) } )

   n := Len( aChoices )

   nMsgHeight := Len( aMsg ) * aSize[2]
   FOR i = 1 To n
       nButWidth := MAX( nButWidth, _GetTextExtentPoint32( hDC, aChoices[i] )[1]+4 )
   NEXT i

   SelectObject( hDC, hOldFont )
   DeleteObject( hFont )
   ReleaseDC( 0, hDC )

   IF !lVert
      nWidth  := Max( nWidth, (nButWidth*n)+5  )
   ENDIF

   hWnd := GetFocus()
   oParent := NIL //ObjFromHandle( GetActiveWindow() )

   Super:Init( oParent )
   ::Modal    := lModal
   ::Style    := DS_MODALFRAME | WS_VISIBLE | WS_POPUP | DS_SETFONT | WS_CAPTION
   ::ExStyle  := WS_EX_TOOLWINDOW
   ::Message  := cMsg
   ::MsgHeight:= nMsgHeight
   ::aChoices := aChoices
   ::Width    := nWidth + 40
   ::Height   := nMsgHeight + 55 + IIF( !lVert, 0, 24 * (Len( aChoices )-1) )
   ::Create() 
 
   SetFocus( hWnd )
RETURN Self

METHOD OnInitDialog() CLASS AlertDialog
   LOCAL n, i, o, nLeft, nWidth, nTop, aMsg, aSize, oButton
   
   o := Label( Self )
   o:Font:Bold := .T.
   o:Style   := SS_CENTER | WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   o:ExStyle := WS_EX_STATICEDGE
   IF ::_Back != NIL
      o:BackColor := ::_Back
   ENDIF
   IF ::_Fore != NIL
      o:ForeColor := ::_Fore
   ENDIF

   IF ::_Face != NIL
      o:Font:FaceName := ::_Face
   ENDIF
   IF ::_PointSize != NIL
      o:Font:PointSize := ::_PointSize
      aSize := o:Drawing:GetTextExtentPoint32( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" )
      aMsg := __str2a( ::Message, CHR(13) )
      ::MsgHeight := Len( aMsg ) * aSize[2]
      ::Height    := ::MsgHeight + 55 + IIF( !::_Vert, 0, 24 * (Len( ::aChoices )-1) )
      nWidth      := 0
      AEVAL( aMsg, {|x| nWidth := Max( nWidth, o:Drawing:GetTextExtentPoint32( AllTrim(x) )[1] ) } )
      IF !::_Vert
         nWidth  := Max( nWidth, (nButWidth*Len( ::aChoices ))+5  )
      ENDIF
      ::Width    := nWidth + 40
   ENDIF

   o:Caption := ::Message
   o:Left    := 2
   o:Top     := 2
   o:Width   := ::ClientWidth - 4
   o:Height  := ::MsgHeight + 5
   o:Create()

   nLeft  := 1
   nTop   := ::MsgHeight + 8 //::ClientHeight - 24
   nWidth := IIF( ::_Vert, ::ClientWidth-2, ::ClientWidth / LEN( ::aChoices ) )
   
   n := LEN( ::aChoices )
   For i = 1 To n
       o := Button( Self )
       o:Caption := ::aChoices[ i ]
       o:Left    := nLeft
       o:Top     := nTop
       o:Width   := nWidth - 1
       o:Id      := i
       o:Height  := 24
       IF i ==  ::_Default
          o:DefaultButton := .T.
          DEFAULT oButton TO o
       ENDIF
       o:Create()
       IF ::_Vert
          nTop += o:Height
        ELSE
          nLeft += nWidth
       ENDIF
   Next i
   ::CenterWindow()
   //IF !::Modal
   //   ::height := oButton:Top + oButton:Height + 25
   //ENDIF
   /*
   IF ::Left < 0
      ::Left := 0
   ENDIF
   IF ::Top < 0
      ::Top := 0
   ENDIF
   */
   oButton:SetFocus()
RETURN .T.


