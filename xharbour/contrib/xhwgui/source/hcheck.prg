/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HCheckButton class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"


CLASS HCheckButton INHERIT HControl

   CLASS VAR winclass   INIT "BUTTON"
   DATA bSetGet
   DATA value

   METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Redefine( oWnd,nId,vari,bSetGet,oFont,bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor )
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor ) CLASS HCheckButton

   // ::classname:= "HCHECKBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::value   := Iif( vari==Nil .OR. Valtype(vari)!="L",.F.,vari )
   ::bSetGet := bSetGet
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_AUTOCHECKBOX+WS_CHILD+WS_VISIBLE+WS_TABSTOP )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

   IF bSetGet != Nil
      ::bLostFocus := bClick
      ::oParent:AddEvent( BN_CLICKED,::id,{|o,id|__Valid(o:FindControl(id))} )
   ELSE
      IF bClick != Nil
         ::oParent:AddEvent( BN_CLICKED,::id,bClick )
      ENDIF
   ENDIF
Return Self

METHOD Activate CLASS HCheckButton
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Redefine( oWndParent,nId,vari,bSetGet,oFont,bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor ) CLASS HCheckButton
   // ::classname:= "HCHECKBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::value   := Iif( vari==Nil .OR. Valtype(vari)!="L",.F.,vari )
   ::bSetGet := bSetGet
   ::style   := ::nLeft := ::nTop := ::nWidth := ::nHeight := 0
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::oParent:AddControl( Self )
   IF bSetGet != Nil
      ::bLostFocus := bClick
      ::oParent:AddEvent( BN_CLICKED,::id,{|o,id|__Valid(o:FindControl(id))} )
   ELSE
      IF bClick != Nil
         ::oParent:AddEvent( BN_CLICKED,::id,bClick )
      ENDIF
   ENDIF
Return Self

METHOD Init() CLASS HCheckButton
   IF !::lInit
      Super:Init()
      IF ::value
         CheckDlgButton( ::oParent:handle,::id,.T. )
      ENDIF
   ENDIF
Return Nil


Static Function __Valid( oCtrl )

   oCtrl:value := IsDlgButtonChecked( oCtrl:oParent:handle, oCtrl:id )

   IF oCtrl:bSetGet != Nil
      Eval( oCtrl:bSetGet,oCtrl:value )
   ENDIF
   IF oCtrl:bLostFocus != Nil .AND. !Eval( oCtrl:bLostFocus, oCtrl:value, oCtrl )
      SetFocus( oCtrl:handle )
   ENDIF

Return .T.