/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HRichEdit class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HRichEdit INHERIT HControl

   CLASS VAR winclass   INIT "RICHEDIT"
   DATA lChanged    INIT .F.

   METHOD New( oWndParent,nId,vari,nStyle,nLeft,nTop,nWidth,nHeight, ;
         oFont,bInit,bSize,bPaint,bGfocus,bLfocus,ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()
   METHOD SetText( cText )

ENDCLASS

METHOD New( oWndParent,nId,vari,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  oFont,bInit,bSize,bPaint,bGfocus,bLfocus,ctoolt, ;
                  tcolor,bcolor ) CLASS HRichEdit

   // ::classname:= "HRICHEDIT"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := vari
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE+WS_TABSTOP+WS_BORDER )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,Iif( bcolor==Nil,GetSysColor( COLOR_BTNHIGHLIGHT ),bcolor ) )

   ::Activate()
   ::oParent:AddControl( Self )

   IF bGfocus != Nil
      ::oParent:AddEvent( EN_SETFOCUS,::id,bGfocus )
   ENDIF
   IF bLfocus != Nil
      ::oParent:AddEvent( EN_KILLFOCUS,::id,bLfocus )
   ENDIF

Return Self

METHOD Activate CLASS HRichEdit
   IF ::oParent:handle != 0
      ::handle := CreateRichEdit( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Init()  CLASS HRichEdit
   IF !::lInit
      Super:Init()
      Hwg_InitRichProc( ::handle )
   ENDIF
Return Nil

METHOD SetText( cText )  CLASS HRichEdit

   SetDlgItemText( ::oParent:handle,::id,cText )
Return Nil

Function DefRichProc( hEdit, msg, wParam, lParam )
Local oEdit
   // writelog( "RichProc: " + Str(hEdit,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   oEdit := FindSelf( hEdit )
   IF msg == WM_CHAR
      oEdit:lChanged := .T.
   ELSEIF msg == WM_KEYDOWN 
      IF wParam == 46     // Del
         oEdit:lChanged := .T.
      ENDIF
   ELSEIF oEdit:bOther != Nil
      Return Eval( oEdit:bOther, oEdit, msg, wParam, lParam )
   ENDIF
Return -1
