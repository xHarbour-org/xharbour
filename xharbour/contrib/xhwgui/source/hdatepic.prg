/*
 *$Id: hdatepic.prg,v 1.1 2004/02/19 02:35:44 lculik Exp $
 *
 * HWGUI - Harbour Win32 GUI library source code:
 * HDatePicker class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#undef  DTN_DATETIMECHANGE
#undef  NM_KILLFOCUS
#undef  NM_SETFOCUS
#define DTN_DATETIMECHANGE    -759
#define NM_KILLFOCUS          -8
#define NM_SETFOCUS           -7

CLASS HDatePicker INHERIT HControl

   CLASS VAR winclass   INIT "SYSDATETIMEPICK32"
   DATA bSetGet
   DATA value

   METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  oFont,bInit,bGfocus,bLfocus,ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  oFont,bInit,bGfocus,bLfocus,ctoolt,tcolor,bcolor ) CLASS HDatePicker

   // ::classname:= "HDATEPICKER"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::value   := Iif( vari==Nil .OR. Valtype(vari)!="D",CTOD(SPACE(8)),vari )
   ::bSetGet := bSetGet
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::SetColor( tcolor,bcolor )

   HWG_InitCommonControlsEx()
   ::Activate()
   ::oParent:AddControl( Self )

   IF bGfocus != Nil
      ::oParent:AddEvent( NM_SETFOCUS,::id,bGfocus,.T. )
   ENDIF
   IF bSetGet != Nil
      ::bLostFocus := bLFocus
      ::oParent:AddEvent( DTN_DATETIMECHANGE,::id,{|o,id|__Valid(o:FindControl(id),.F.)},.T. )
      ::oParent:AddEvent( NM_KILLFOCUS,::id,{|o,id|__Valid(o:FindControl(id),.T.)},.T. )
   ELSE
      IF bLfocus != Nil
         ::oParent:AddEvent( NM_KILLFOCUS,::id,bLfocus,.T. )
      ENDIF
   ENDIF

Return Self

METHOD Activate CLASS HDatePicker
   IF ::oParent:handle != 0
      ::handle := CreateDatePicker( ::oParent:handle, ::id, ;
                  ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init() CLASS HDatePicker
   IF !::lInit
      Super:Init()
      IF !Empty( ::value )
         SetDatePicker( ::handle,::value )
      ENDIF
   ENDIF
Return Nil

Static Function __Valid( oCtrl,lLostFocus )

   oCtrl:value := GetDatePicker( oCtrl:handle )

   IF oCtrl:bSetGet != Nil
      Eval( oCtrl:bSetGet,oCtrl:value )
   ENDIF
   IF lLostFocus .AND. oCtrl:bLostFocus != Nil .AND. !Eval( oCtrl:bLostFocus, oCtrl:value, oCtrl )
      // SetFocus( oCtrl:handle )
      Return .F.
   ENDIF

Return .T.
